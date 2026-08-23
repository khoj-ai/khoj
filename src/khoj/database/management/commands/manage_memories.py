import asyncio
from datetime import datetime, timedelta
from typing import List, Optional

from django.core.management.base import BaseCommand
from django.db.models import Q
from django.utils import timezone

from khoj.configure import initialize_server
from khoj.database.adapters import UserMemoryAdapters
from khoj.database.models import (
    Conversation,
    DataStore,
    KhojUser,
)
from khoj.routers.helpers import extract_facts_from_query


class Command(BaseCommand):
    help = "Manage user memories - generate from conversations or delete existing memories"

    def add_arguments(self, parser):
        parser.add_argument(
            "--lookback-days",
            type=int,
            default=None,
            help="Number of days to look back. For generation: defaults to 7 days. For deletion: if not specified, deletes ALL memories",
        )
        parser.add_argument(
            "--users",
            type=str,
            help="Process specific users (comma-separated usernames or emails)",
        )
        parser.add_argument(
            "--batch-size",
            type=int,
            default=10,
            help="Number of conversations to process in each batch (default: 10)",
        )
        parser.add_argument(
            "--apply",
            action="store_true",
            help="Actually perform the operation. Without this flag, only shows what would be processed.",
        )
        parser.add_argument(
            "--delete",
            action="store_true",
            help="Delete all memories for specified users instead of generating new ones",
        )
        parser.add_argument(
            "--resume",
            action="store_true",
            help="Resume from last checkpoint if process was interrupted",
        )
        parser.add_argument(
            "--force",
            action="store_true",
            help="Force regenerate memories even if already processed",
        )

    def handle(self, *args, **options):
        """Main entry point for the command"""
        initialize_server()
        asyncio.run(self.async_handle(*args, **options))

    async def async_handle(self, *args, **options):
        """Async handler for memory management"""
        lookback_days = options["lookback_days"]
        usernames = options["users"]
        batch_size = options["batch_size"]
        apply = options["apply"]
        delete = options["delete"]
        resume = options["resume"]
        force = options["force"]

        mode = "APPLY" if apply else "DRY RUN"

        # Handle deletion mode
        if delete:
            # For deletion, only use cutoff_date if lookback_days is explicitly provided
            cutoff_date = timezone.now() - timedelta(days=lookback_days) if lookback_days else None
            await self.handle_delete_memories(usernames, cutoff_date, apply)
            return

        # Handle generation mode
        # For generation, default to 7 days if not specified
        if lookback_days is None:
            lookback_days = 7
        cutoff_date = timezone.now() - timedelta(days=lookback_days)
        self.stdout.write(f"[{mode}] Generating memories for conversations from the last {lookback_days} days")

        # Get users to process
        users = await self.get_users_to_process(usernames)
        if not users:
            self.stdout.write("No users found to process")
            return

        self.stdout.write(f"Found {len(users)} users to process")

        # Initialize or retrieve checkpoint
        checkpoint = await self.get_or_create_checkpoint(resume)

        total_conversations = 0
        total_memories = 0

        for user in users:
            # Check if user already processed in checkpoint
            if not force and user.id in checkpoint.get("processed_users", []):
                self.stdout.write(f"Skipping already processed user: {user.username}")
                continue

            self.stdout.write(f"\nProcessing user: {user.username} (ID: {user.id})")

            # Get conversations for this user
            conversations = await self.get_user_conversations(user, cutoff_date, checkpoint, force)

            if not conversations:
                self.stdout.write(f"  No conversations to process for {user.username}")
                # Mark user as processed
                if apply:
                    await self.update_checkpoint(checkpoint, user_id=user.id)
                continue

            self.stdout.write(f"  Found {len(conversations)} conversations to process")

            # Process conversations in batches
            user_memories = 0
            for i in range(0, len(conversations), batch_size):
                batch = conversations[i : i + batch_size]
                batch_memories = await self.process_conversation_batch(user, batch, apply, checkpoint)
                user_memories += batch_memories
                total_conversations += len(batch)

                # Update progress
                progress = min(i + batch_size, len(conversations))
                self.stdout.write(
                    f"    Processed {progress}/{len(conversations)} conversations, generated {batch_memories} memories"
                )

            total_memories += user_memories
            self.stdout.write(
                f"  Completed user {user.username}: "
                f"processed {len(conversations)} conversations, "
                f"generated {user_memories} memories"
            )

            # Mark user as processed
            if apply:
                await self.update_checkpoint(checkpoint, user_id=user.id)

        # Clear checkpoint on successful completion
        if apply:
            await self.clear_checkpoint()

        action = "Generated" if apply else "Would generate"
        self.stdout.write(
            self.style.SUCCESS(f"\n{action} {total_memories} memories from {total_conversations} conversations")
        )

    async def get_users_to_process(self, users_str: Optional[str]) -> List[KhojUser]:
        """Get list of users to comma separated usernames or emails to process"""
        if users_str:
            usernames = [u.strip() for u in users_str.split(",") if u.strip()]
            # Process specific users
            users = [user async for user in KhojUser.objects.filter(Q(username__in=usernames) | Q(email__in=usernames))]
            return users
        else:
            # Process all users with conversations
            return [user async for user in KhojUser.objects.filter(conversation__isnull=False).distinct()]

    async def get_user_conversations(
        self, user: KhojUser, cutoff_date: Optional[datetime], checkpoint: dict, force: bool
    ) -> List[Conversation]:
        """Get conversations for a user that need processing"""
        if cutoff_date is None:
            query = Conversation.objects.filter(user=user).order_by("updated_at")
        else:
            query = Conversation.objects.filter(user=user, updated_at__gte=cutoff_date).order_by("updated_at")

        # Filter out already processed conversations if resuming
        if not force and user.id in checkpoint.get("processed_conversations", {}):
            processed_ids = checkpoint["processed_conversations"][user.id]
            query = query.exclude(id__in=processed_ids)

        return [conv async for conv in query]

    async def process_conversation_batch(
        self, user: KhojUser, conversations: List[Conversation], apply: bool, checkpoint: dict
    ) -> int:
        """Process a batch of conversations and generate memories"""
        total_memories = 0

        for conversation in conversations:
            try:
                # Get conversation messages using sync_to_async for property access
                from asgiref.sync import sync_to_async

                # Access conversation_log synchronously
                @sync_to_async
                def get_messages():
                    return conversation.messages

                messages = await get_messages()
                if not messages:
                    continue

                # Get agent if conversation has one
                @sync_to_async
                def get_agent():
                    return conversation.agent

                agent = await get_agent()

                # Get existing memories for context
                # Process each conversation turn
                conversation_memories = 0
                i = 0
                while i + 1 < len(messages):
                    # Only process user-assistant pairs as a valid turn for memory extraction
                    if messages[i].by != "you" or messages[i + 1].by != "khoj":
                        i += 1
                        continue

                    # Get the conversation history up to this point
                    history = messages[: i + 2]

                    # Extract user query text for memory search
                    q = ""
                    if messages[i].message is None:
                        i += 1
                        continue
                    elif isinstance(messages[i].message, str):
                        q = messages[i].message
                    elif isinstance(messages[i].message, list):
                        q = "\n\n".join(
                            content.get("text", "")
                            for content in messages[i].message
                            if isinstance(content, dict) and content.get("text")
                        )

                    if not q or not q.strip():
                        i += 1
                        continue

                    # Get unique recent and long term relevant memories
                    recent_memories = await UserMemoryAdapters.pull_memories(user=user, agent=agent)
                    long_term_memories = await UserMemoryAdapters.search_memories(query=q, user=user, agent=agent)
                    relevant_memories = list({m.id: m for m in recent_memories + long_term_memories}.values())

                    if apply:
                        # Ensure agent is fully loaded with its chat_model
                        if agent:

                            @sync_to_async
                            def load_agent_with_chat_model():
                                # Force load the chat_model relationship
                                _ = agent.chat_model
                                return agent

                            agent = await load_agent_with_chat_model()

                        # Update memories based on latest conversation turn
                        memory_updates = await extract_facts_from_query(
                            user=user,
                            conversation_history=history,
                            existing_facts=relevant_memories,
                            agent=agent,
                            tracer={},
                        )

                        # Save new memories
                        for memory in memory_updates.create:
                            await UserMemoryAdapters.save_memory(user, memory, agent=agent)
                            conversation_memories += 1
                            self.stdout.write(f"Created memory for user {user.id}: {memory[:50]}...")

                        # Delete outdated memories
                        for memory in memory_updates.delete:
                            await UserMemoryAdapters.delete_memory(user, memory)
                            self.stdout.write(f"Deleted memory for user {user.id}: {memory[:50]}...")
                    else:
                        # Dry run - estimate memories that would be created
                        conversation_memories += 1  # Rough estimate

                    # Move to next conversation turn pair
                    i += 2

                total_memories += conversation_memories

                # Update checkpoint after each conversation
                if apply:
                    await self.update_checkpoint(checkpoint, user_id=user.id, conversation_id=str(conversation.id))
            except Exception as e:
                import traceback

                self.stderr.write(
                    f"Error processing conversation {conversation.id} for user {user.id}: {e}\n"
                    f"Traceback: {traceback.format_exc()}"
                )
                continue

        return total_memories

    async def get_or_create_checkpoint(self, resume: bool) -> dict:
        """Get or create checkpoint for resumable processing"""
        checkpoint_key = "memory_generation_checkpoint"

        if resume:
            # Try to retrieve existing checkpoint
            checkpoint_store = await DataStore.objects.filter(key=checkpoint_key, private=True).afirst()
            if checkpoint_store:
                self.stdout.write("Resuming from checkpoint...")
                return checkpoint_store.value

        # Create new checkpoint
        return {"started_at": timezone.now().isoformat(), "processed_users": [], "processed_conversations": {}}

    async def update_checkpoint(
        self, checkpoint: dict, user_id: Optional[int] = None, conversation_id: Optional[str] = None
    ):
        """Update checkpoint with progress"""
        if user_id and user_id not in checkpoint["processed_users"]:
            checkpoint["processed_users"].append(user_id)

        if user_id and conversation_id:
            if user_id not in checkpoint["processed_conversations"]:
                checkpoint["processed_conversations"][user_id] = []
            if conversation_id not in checkpoint["processed_conversations"][user_id]:
                checkpoint["processed_conversations"][user_id].append(conversation_id)

        # Save checkpoint to database
        await DataStore.objects.aupdate_or_create(
            key="memory_generation_checkpoint", defaults={"value": checkpoint, "private": True}
        )

    async def clear_checkpoint(self):
        """Clear checkpoint after successful completion"""
        await DataStore.objects.filter(key="memory_generation_checkpoint").adelete()
        self.stdout.write("Checkpoint cleared")

    async def handle_delete_memories(self, usernames: Optional[str], cutoff_date: Optional[datetime], apply: bool):
        """Handle deletion of user memories"""
        from khoj.database.models import UserMemory

        # Get users to process
        users = await self.get_users_to_process(usernames)
        if not users:
            self.stdout.write("No users found to process")
            return

        mode = "APPLY" if apply else "DRY RUN"
        if cutoff_date:
            # Calculate days from cutoff date
            days_back = (timezone.now() - cutoff_date).days
            self.stdout.write(f"[{mode}] Deleting memories created in the last {days_back} days for {len(users)} users")
        else:
            self.stdout.write(f"[{mode}] Deleting ALL memories for {len(users)} users")

        total_deleted = 0
        for user in users:
            # Count memories for this user
            if cutoff_date is None:
                user_memories = UserMemory.objects.filter(user=user)
            else:
                user_memories = UserMemory.objects.filter(user=user, created_at__gte=cutoff_date)

            memories_count = await user_memories.acount()
            if memories_count == 0:
                self.stdout.write(f"  User {user.username} has no memories to delete")
                continue

            self.stdout.write(f"\n  User {user.username} (ID: {user.id}): {memories_count} memories")

            if apply:
                # Delete memories for this user (with date filter if specified)
                deleted_count, _ = await user_memories.adelete()
                self.stdout.write(f"    Deleted {deleted_count} memories")
                total_deleted += deleted_count
            else:
                self.stdout.write(f"    Would delete {memories_count} memories")
                total_deleted += memories_count

        action = "Deleted" if apply else "Would delete"
        self.stdout.write(self.style.SUCCESS(f"\n{action} {total_deleted} memories total"))
