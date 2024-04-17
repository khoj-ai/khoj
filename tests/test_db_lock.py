import time

import pytest

from khoj.database.adapters import ProcessLockAdapters
from khoj.database.models import ProcessLock
from tests.helpers import ProcessLockFactory


@pytest.mark.django_db(transaction=True)
def test_process_lock(default_process_lock):
    # Arrange
    lock: ProcessLock = default_process_lock

    # Assert
    assert True == ProcessLockAdapters.is_process_locked(lock.name)


@pytest.mark.django_db(transaction=True)
def test_expired_process_lock():
    # Arrange
    lock: ProcessLock = ProcessLockFactory(name="test_expired_lock", max_duration_in_seconds=2)

    # Act
    time.sleep(3)

    # Assert
    assert False == ProcessLockAdapters.is_process_locked(lock.name)


@pytest.mark.django_db(transaction=True)
def test_in_progress_lock(default_process_lock):
    # Arrange
    lock: ProcessLock = default_process_lock

    # Act
    ProcessLockAdapters.run_with_lock(lock.name, lambda: time.sleep(2))

    # Assert
    assert True == ProcessLockAdapters.is_process_locked(lock.name)


@pytest.mark.django_db(transaction=True)
def test_run_with_completed():
    # Arrange
    ProcessLockAdapters.run_with_lock("test_run_with", lambda: time.sleep(2))

    # Act
    time.sleep(4)

    # Assert
    assert False == ProcessLockAdapters.is_process_locked("test_run_with")


@pytest.mark.django_db(transaction=True)
def test_nonexistent_lock():
    # Assert
    assert False == ProcessLockAdapters.is_process_locked("nonexistent_lock")
