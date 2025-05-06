import asyncio
import logging
import time
from typing import Callable, Dict, Optional

from asgiref.sync import sync_to_async
from django.utils import timezone

logger = logging.getLogger(__name__)


class BackgroundService:

    """
    A class to manage background services that run periodically.
    Each service can be started, stopped, and registered with a name and interval.
    """

    def __init__(self, name: str, interval: int, fn: Callable, run_immediately: bool = True):
        self.name = name
        self.interval = interval
        self.fn = fn
        self.run_immediately = run_immediately
        self.task: Optional[asyncio.Task] = None
        self.should_continue = True

        from khoj.database.models import BackgroundServiceConfig

        self.config: BackgroundServiceConfig = None

    async def loadConfig(self) -> None:
        """
        Load the configuration for the service from the database.
        This is a placeholder for loading any necessary configuration.
        """
        from khoj.database.models import BackgroundServiceConfig

        try:
            # Remove old config
            if self.config:
                self.config = None

            self.config = await sync_to_async(BackgroundServiceConfig.objects.get)(task_name=self.name)

            self.should_continue = self.config.task_is_enabled
            self.interval = self.config.task_interval

            if not self.config.task_is_enabled:
                logger.info(f"[{self.name}] Service is disabled.")
                self.run_immediately = False
                self.should_continue = False
                return

            will_save = False

            # Time checks
            current_time = timezone.now().timestamp()

            if self.config.task_last_run and (current_time - self.config.task_last_run.timestamp()) < self.interval:
                self.run_immediately = True
            else:
                will_save = True

            if self.config.task_next_run:
                diff = self.config.task_next_run.timestamp() - current_time
                if diff > 0:
                    self.run_immediately = False

                    self.interval = diff

                else:
                    self.run_immediately = True
            else:
                self.config.task_next_run = timezone.now() + timezone.timedelta(seconds=self.interval)
                will_save = True

            if will_save:
                await sync_to_async(self.config.save)()

            logger.info(f"[{self.name}] Loaded config: {self.config}")
        except BackgroundServiceConfig.DoesNotExist:
            logger.warning(f"[{self.name}] No config found. Creating default entry.")
            await sync_to_async(BackgroundServiceConfig.objects.create)(
                task_id=f"default-{self.name}",
                task_name=self.name,
                task_interval=self.interval,
                task_last_run=timezone.now(),
                task_next_run=timezone.now() + timezone.timedelta(seconds=self.interval),
                task_is_enabled=True,
            )
        except Exception as e:
            logger.error(f"[{self.name}] Error loading config: {e}")
            self.should_continue = False
            self.run_immediately = False
            logger.info(f"[{self.name}] Service is disabled due to error.")

    async def updateConfig(self) -> bool:
        """
        Update the configuration for the service in the database.
        This is a placeholder for updating any necessary configuration.
        """
        from khoj.database.models import BackgroundServiceConfig

        result = True
        try:
            self.config = await sync_to_async(BackgroundServiceConfig.objects.get)(task_name=self.name)

            self.config.task_last_run = timezone.now()
            self.config.task_next_run = timezone.now() + timezone.timedelta(seconds=self.interval)
            self.interval = self.config.task_interval

            if not self.config.task_is_enabled:
                result = False

            await sync_to_async(self.config.save)()
        except Exception as e:
            logger.error(f"[{self.name}] Error updating config: {e}")

        return result

    async def _safe_call(self) -> bool:
        try:
            # Check if the service function is async or sync
            if asyncio.iscoroutinefunction(self.fn):
                result = await self.fn()
            else:
                result = await sync_to_async(self.fn)()

            result = bool(result)  # Treat None or False as stop signal

            if result:
                # if the function returns True, the configuration is updated
                # if the function returns False, the service was disabled
                # and the configuration is updated
                if not await self.updateConfig():
                    result = False

            return result
        except Exception as e:
            logger.error(f"[{self.name}] Error: {e}")
            return True  # Keep trying even after failure, unless you want to halt

    async def start(self):
        if not self.config:
            await self.loadConfig()

        if self.run_immediately:
            self.should_continue = await self._safe_call()

        while self.should_continue:
            await asyncio.sleep(self.interval)
            if not self.should_continue:
                break
            self.should_continue = await self._safe_call()

        logger.info(f"[{self.name}] Stopped.")
        if self.task:
            self.task.cancel()
            self.task = None
            logger.info(f"[{self.name}] Task cancelled.")

    async def stop(self):
        if self.task:
            self.task.cancel()
            self.should_continue = False
            logger.info(f"[{self.name}] Stopped.")
        self.task = None


class ServiceManager:
    def __init__(self):
        self.services: Dict[str, BackgroundService] = {}

        self.register_service(
            name="service_manager", interval=60, fn=self._service_manager_task, run_immediately=False  # 1 minute
        )

    async def _service_manager_task(self) -> bool:
        """
        This task runs every minute to check the status of all registered services.
        It can be used to perform any necessary maintenance or updates.
        """

        logger.info("Service Manager Task running...")
        for service in self.services.values():
            if service.name == "service_manager":
                continue

            await service.loadConfig()

            if service.task and not service.task.done():
                if service.config and service.config.task_is_enabled:
                    logger.info(f"Service '{service.name}' is running and should be.")
                elif service.config and not service.config.task_is_enabled:
                    logger.info(f"Service '{service.name}' is running, but should not be.")
                    await service.stop()
                else:
                    logger.info(f"Service '{service.name}' is running, but no config found.")

            elif service.config and service.config.task_is_enabled and not service.task:
                logger.info(f"Service '{service.name}' is not running, but should be.")

                service.task = asyncio.create_task(service.start())
            elif service.config and not service.config.task_is_enabled and not service.task:
                logger.info(f"Service '{service.name}' is not running and should not be.")
                # service.config = None

        return True

    def register_service(
        self, name: str, interval: int, fn: Callable, run_immediately: bool = True
    ) -> BackgroundService:
        if name in self.services:
            raise ValueError(f"Service '{name}' already registered.")
        service = BackgroundService(name, interval, fn, run_immediately)
        self.services[name] = service

        return service

    def get_service(self, name: str) -> Optional[BackgroundService]:
        return self.services.get(name)

    def start_all(self) -> None:
        for service in self.services.values():
            service.task = asyncio.create_task(service.start())

    def delete_service(self, name: str) -> Optional[BackgroundService]:
        if name in self.services:
            service = self.services.pop(name)
            if service.task:
                service.task.cancel()
                logger.info(f"Service '{name}' stopped.")
            return service
        else:
            logger.warning(f"Service '{name}' not found.")
            return None

    def stop_all(self) -> None:
        for service in self.services.values():
            if service.task:
                service.task.cancel()
                logger.info(f"Service '{service.name}' stopped.")
        self.services.clear()
        logger.info("All services stopped and cleared.")
