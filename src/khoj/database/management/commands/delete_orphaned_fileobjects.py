from django.core.management.base import BaseCommand
from django.db import transaction
from django.db.models import Exists, OuterRef

from khoj.database.models import Entry, FileObject


class Command(BaseCommand):
    help = "Deletes FileObjects that have no associated Entries"

    def add_arguments(self, parser):
        parser.add_argument(
            "--apply",
            action="store_true",
            help="Actually perform the deletion. Without this flag, only shows what would be deleted.",
        )

    def handle(self, *args, **options):
        apply = options["apply"]

        mode = "UPDATE" if apply else "DRY RUN"
        self.stdout.write(f"[{mode}] Processing entries with null file_objects...")

        # Create lookup dictionary of all file objects
        file_objects_map = {(fo.user_id, fo.file_name): fo for fo in FileObject.objects.all()}

        chunk_size = 1000
        processed = 0
        processed_entry_ids = set()

        while True:
            entries = list(
                Entry.objects.select_related("user")
                .filter(file_object__isnull=True)
                .exclude(id__in=processed_entry_ids)
                .only("id", "user", "file_path")[:chunk_size]
            )

            if not entries:
                break

            processed_entry_ids.update([entry.id for entry in entries])
            entries_to_update = []

            for entry in entries:
                try:
                    file_object = file_objects_map.get((entry.user_id, entry.file_path))
                    if file_object:
                        entry.file_object = file_object
                        entries_to_update.append(entry)
                except Exception as e:
                    self.stdout.write(self.style.WARNING(f"Error processing entry {entry.id}: {str(e)}"))
                    continue

            if entries_to_update and apply:
                with transaction.atomic():
                    Entry.objects.bulk_update(entries_to_update, ["file_object"], batch_size=chunk_size)

            processed += len(entries)
            self.stdout.write(f"Processed {processed} entries")

        action = "Updated" if apply else "Would update"
        self.stdout.write(self.style.SUCCESS(f"{action} {len(processed_entry_ids)} entries"))

        # Find FileObjects with no related entries using subquery
        orphaned_files = FileObject.objects.annotate(
            has_entries=Exists(Entry.objects.filter(file_object=OuterRef("pk")))
        ).filter(has_entries=False)

        total_orphaned = orphaned_files.count()
        mode = "DELETE" if options["apply"] else "DRY RUN"
        self.stdout.write(f"[{mode}] Found {total_orphaned} orphaned FileObjects")

        if total_orphaned == 0:
            self.stdout.write("No orphaned FileObjects to process")
            return

        # Process in batches of 1000
        batch_size = 1000
        processed = 0

        while processed < total_orphaned:
            # Get batch of IDs to process
            batch_ids = list(orphaned_files.values_list("id", flat=True)[:batch_size])
            if not batch_ids:
                break

            if options["apply"]:
                # Delete by ID to avoid slice/limit issues
                count = FileObject.objects.filter(id__in=batch_ids).delete()[0]
                processed += count
                self.stdout.write(f"Deleted {processed}/{total_orphaned} orphaned FileObjects")
            else:
                processed += len(batch_ids)
                self.stdout.write(f"Would delete {processed}/{total_orphaned} orphaned FileObjects")

            # Re-query to get fresh state
            orphaned_files = FileObject.objects.annotate(
                has_entries=Exists(Entry.objects.filter(file_object=OuterRef("pk")))
            ).filter(has_entries=False)

        action = "Deleted" if options["apply"] else "Would delete"
        self.stdout.write(self.style.SUCCESS(f"{action} {processed} orphaned FileObjects"))
