from typing import List

from django.core.management.base import BaseCommand
from tqdm import tqdm

from khoj.database.models import Agent, Entry, SearchModelConfig, UserSearchModelConfig
from khoj.processor.embeddings import EmbeddingsModel


class Command(BaseCommand):
    help = "Convert all existing Entry objects to use a new default Search model."

    def add_arguments(self, parser):
        # Pass default SearchModelConfig ID
        parser.add_argument(
            "--id",
            action="store",
            help="ID of the SearchModelConfig object to set as the default search model for all existing Entry objects and UserSearchModelConfig objects.",
        )

        # Set the apply flag to apply the new default Search model to all existing Entry objects and UserSearchModelConfig objects.
        parser.add_argument(
            "--apply",
            action="store_true",
            help="Apply the new default Search model to all existing Entry objects and UserSearchModelConfig objects. Otherwise, only display the number of Entry objects and UserSearchModelConfig objects that will be affected.",
        )

    def handle(self, *args, **options):
        def regenerate_entry(entry: List[Entry], embeddings_model: EmbeddingsModel):
            compiled_entries = [entry.compiled for entry in entry]
            embeddings = embeddings_model.embed_documents(compiled_entries)
            for i, entry in enumerate(tqdm(entry)):
                entry.embeddings = embeddings[i]
                entry.save()

        search_model_config_id = options.get("id")
        apply = options.get("apply")

        embeddings_model = dict()

        search_models = SearchModelConfig.objects.all()
        for model in search_models:
            embeddings_model.update(
                {
                    model.name: EmbeddingsModel(
                        model.bi_encoder,
                        model.embeddings_inference_endpoint,
                        model.embeddings_inference_endpoint_api_key,
                        query_encode_kwargs=model.bi_encoder_query_encode_config,
                        docs_encode_kwargs=model.bi_encoder_docs_encode_config,
                        model_kwargs=model.bi_encoder_model_config,
                    )
                }
            )

        new_default_search_model_config = SearchModelConfig.objects.get(id=search_model_config_id)
        user_search_model_configs_to_update = UserSearchModelConfig.objects.exclude(
            setting_id=search_model_config_id
        ).all()
        print(f"Number of UserSearchModelConfig objects to update: {user_search_model_configs_to_update.count()}")

        for user_config in user_search_model_configs_to_update:
            affected_user = user_config.user
            relevant_entries = Entry.objects.filter(user=affected_user).all()
            print(f"Number of Entry objects to update for user {affected_user}: {relevant_entries.count()}")

            if apply:
                regenerate_entry(relevant_entries, embeddings_model[new_default_search_model_config.name])
                user_config.setting = new_default_search_model_config
                user_config.save()

                print(
                    f"Updated UserSearchModelConfig object for user {affected_user} to use the new default Search model."
                )
                print(
                    f"Updated {relevant_entries.count()} Entry objects for user {affected_user} to use the new default Search model."
                )
            else:
                print("Run the command with the --apply flag to apply the new default Search model.")
                break

        all_agents = Agent.objects.all()
        for agent in all_agents:
            relevant_entries = Entry.objects.filter(agent=agent).all()
            print(f"Number of Entry objects to update for agent {agent}: {relevant_entries.count()}")

            if apply:
                regenerate_entry(relevant_entries, embeddings_model[new_default_search_model_config.name])
                print(
                    f"Updated {relevant_entries.count()} Entry objects for agent {agent} to use the new default Search model."
                )
            else:
                print("Run the command with the --apply flag to apply the new default Search model.")
                break
