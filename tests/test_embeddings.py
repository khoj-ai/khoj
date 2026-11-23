from unittest.mock import MagicMock, Mock, call

import pytest
from django.forms import modelform_factory

from khoj import configure
from khoj.database.management.commands import change_default_model
from khoj.database.models import SearchModelConfig
from khoj.processor.embeddings import EmbeddingsModel
from khoj.utils import state


@pytest.mark.parametrize(
    "endpoint_type,api_method",
    [
        (SearchModelConfig.ApiType.OPENAI, "embed_with_openai"),
        (SearchModelConfig.ApiType.HUGGINGFACE, "embed_with_hf"),
    ],
)
@pytest.mark.parametrize(
    "document_count,batch_size,expected_batch_sizes",
    [
        (0, 2, []),
        (3, 1, [1, 1, 1]),
        (1, 2, [1]),
        (2, 2, [2]),
        (4, 2, [2, 2]),
        (5, 2, [2, 2, 1]),
        (7, 3, [3, 3, 1]),
        (1000, None, [1000]),
        (1001, None, [1000, 1]),
    ],
    ids=[
        "empty",
        "minimum-limit",
        "small",
        "exact",
        "multiple",
        "partial",
        "different-limit",
        "default-exact",
        "default-partial",
    ],
)
def test_embed_documents_respects_configured_batch_size(
    monkeypatch, endpoint_type, api_method, document_count, batch_size, expected_batch_sizes
):
    documents = [f"doc-{index}" for index in range(document_count)]
    embeddings_by_document = {document: [float(index)] for index, document in enumerate(documents)}
    api_mock = Mock(side_effect=lambda batch: [embeddings_by_document[document] for document in batch])
    monkeypatch.setattr(EmbeddingsModel, api_method, api_mock)
    progress_factory = MagicMock()
    monkeypatch.setattr("khoj.processor.embeddings.tqdm.tqdm", progress_factory)
    model_kwargs = {"embeddings_inference_endpoint_type": endpoint_type}
    if batch_size is not None:
        model_kwargs["embeddings_batch_size"] = batch_size
    model = EmbeddingsModel(**model_kwargs)

    result = model.embed_documents(documents)

    assert [len(api_call.args[0]) for api_call in api_mock.call_args_list] == expected_batch_sizes
    assert result == [embeddings_by_document[document] for document in documents]
    progress_factory.assert_called_once_with(total=document_count)
    progress_bar = progress_factory.return_value.__enter__.return_value
    assert progress_bar.update.call_args_list == [call(size) for size in expected_batch_sizes]


@pytest.mark.parametrize(
    "endpoint_type,api_method",
    [
        (SearchModelConfig.ApiType.OPENAI, "embed_with_openai"),
        (SearchModelConfig.ApiType.HUGGINGFACE, "embed_with_hf"),
    ],
)
def test_remote_batch_failure_is_propagated_without_sending_remaining_documents(monkeypatch, endpoint_type, api_method):
    failure = RuntimeError("Provider failed on the second batch")
    api_mock = Mock(side_effect=[[[0.0], [1.0]], failure])
    monkeypatch.setattr(EmbeddingsModel, api_method, api_mock)
    model = EmbeddingsModel(embeddings_inference_endpoint_type=endpoint_type, embeddings_batch_size=2)

    with pytest.raises(RuntimeError) as error:
        model.embed_documents(["doc-0", "doc-1", "doc-2", "doc-3", "doc-4"])

    assert error.value is failure
    assert api_mock.call_args_list == [call(["doc-0", "doc-1"]), call(["doc-2", "doc-3"])]


@pytest.mark.parametrize("batch_size", [0, -1, None, 1.5, "2", True, False])
def test_embedding_model_rejects_invalid_batch_size(batch_size):
    with pytest.raises(ValueError, match="embeddings_batch_size must be a positive integer"):
        EmbeddingsModel(
            embeddings_inference_endpoint_type=SearchModelConfig.ApiType.OPENAI,
            embeddings_batch_size=batch_size,
        )


@pytest.mark.parametrize("document_count", [0, 2])
def test_local_encoding_keeps_its_own_batch_size(monkeypatch, document_count):
    documents = [f"doc-{index}" for index in range(document_count)]
    expected_embeddings = [[float(index)] for index in range(document_count)]
    encoder = Mock()
    encoder.encode.return_value.tolist.return_value = expected_embeddings
    monkeypatch.setattr("khoj.processor.embeddings.SentenceTransformer", Mock(return_value=encoder))
    model = EmbeddingsModel(embeddings_batch_size=1, docs_encode_kwargs={"batch_size": 7})

    assert model.embed_documents(documents) == expected_embeddings
    if documents:
        encoder.encode.assert_called_once_with(
            documents, batch_size=7, show_progress_bar=True, normalize_embeddings=True
        )
    else:
        encoder.encode.assert_not_called()


def test_search_models_store_independent_batch_sizes_with_existing_default():
    default_config = SearchModelConfig.objects.create(name="batch-default")
    custom_config = SearchModelConfig.objects.create(name="batch-custom", embeddings_batch_size=7)

    default_config.refresh_from_db()
    custom_config.refresh_from_db()

    assert default_config.embeddings_batch_size == 1000
    assert custom_config.embeddings_batch_size == 7


@pytest.mark.parametrize("batch_size", [0, -1, "invalid", 1.5, None])
def test_search_model_form_rejects_invalid_batch_size(batch_size):
    form_class = modelform_factory(SearchModelConfig, fields=["embeddings_batch_size"])
    form = form_class(data={"embeddings_batch_size": batch_size})

    assert not form.is_valid()
    assert "embeddings_batch_size" in form.errors


def test_search_model_form_accepts_positive_batch_size():
    form_class = modelform_factory(SearchModelConfig, fields=["embeddings_batch_size"])
    form = form_class(data={"embeddings_batch_size": "2"})

    assert form.is_valid(), form.errors
    assert form.save(commit=False).embeddings_batch_size == 2


def test_server_initialization_uses_each_search_models_batch_size(monkeypatch):
    models = [
        SearchModelConfig(
            name="first",
            embeddings_inference_endpoint_type=SearchModelConfig.ApiType.OPENAI,
            embeddings_batch_size=2,
        ),
        SearchModelConfig(
            name="second",
            embeddings_inference_endpoint_type=SearchModelConfig.ApiType.HUGGINGFACE,
            embeddings_batch_size=3,
        ),
    ]
    monkeypatch.setattr(configure.ConversationAdapters, "has_valid_ai_model_api", Mock(return_value=False))
    monkeypatch.setattr(configure, "get_or_create_search_models", Mock(return_value=models))
    monkeypatch.setattr(configure, "CrossEncoderModel", Mock())
    monkeypatch.setattr(configure, "configure_search_types", Mock(return_value=state.SearchType))
    monkeypatch.setattr(configure, "setup_default_agent", Mock())
    monkeypatch.setattr(state, "embeddings_model", {})
    monkeypatch.setattr(state, "cross_encoder_model", {})
    monkeypatch.setattr(state, "SearchType", state.SearchType)

    configure.initialize_server()

    assert state.embeddings_model["first"].embeddings_batch_size == 2
    assert state.embeddings_model["second"].embeddings_batch_size == 3


def test_change_default_model_uses_configured_batch_sizes(monkeypatch):
    models = [
        SearchModelConfig.objects.create(
            name="first",
            bi_encoder="first-encoder",
            embeddings_inference_endpoint_type=SearchModelConfig.ApiType.OPENAI,
            embeddings_batch_size=2,
        ),
        SearchModelConfig.objects.create(
            name="second",
            bi_encoder="second-encoder",
            embeddings_inference_endpoint_type=SearchModelConfig.ApiType.OPENAI,
            embeddings_batch_size=3,
        ),
    ]
    initialized_models = []

    def create_embedding_model(*args, **kwargs):
        model = EmbeddingsModel(*args, **kwargs)
        initialized_models.append(model)
        return model

    monkeypatch.setattr(SearchModelConfig.objects, "all", Mock(return_value=models))
    monkeypatch.setattr(change_default_model, "EmbeddingsModel", create_embedding_model)

    change_default_model.Command().handle(search_model_id=models[1].pk, apply=False)

    assert [(model.model_name, model.embeddings_batch_size) for model in initialized_models] == [
        ("first-encoder", 2),
        ("second-encoder", 3),
    ]
