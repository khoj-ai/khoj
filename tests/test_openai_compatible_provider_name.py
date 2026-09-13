import unittest
from khoj.utils.initialization import infer_openai_compatible_provider_name


class TestOpenAICompatibleProviderName(unittest.TestCase):
    def test_infer_provider_name_api_route(self):
        self.assertEqual(infer_openai_compatible_provider_name("https://global.api-route.com/v1"), "API Route")
        self.assertEqual(infer_openai_compatible_provider_name("https://api-route.com/v1/"), "API Route")
        self.assertEqual(infer_openai_compatible_provider_name("https://GLOBAL.API-ROUTE.COM/V1"), "API Route")

    def test_infer_provider_name_ollama(self):
        self.assertEqual(infer_openai_compatible_provider_name("http://localhost:11434/v1/"), "Ollama")
        self.assertEqual(infer_openai_compatible_provider_name("http://127.0.0.1:11434/v1"), "Ollama")

    def test_infer_provider_name_openrouter(self):
        self.assertEqual(infer_openai_compatible_provider_name("https://openrouter.ai/api/v1"), "OpenRouter")

    def test_infer_provider_name_orcarouter(self):
        self.assertEqual(infer_openai_compatible_provider_name("https://api.orcarouter.ai/v1"), "OrcaRouter")

    def test_infer_provider_name_lmstudio(self):
        self.assertEqual(infer_openai_compatible_provider_name("http://localhost:1234/v1/"), "LM Studio")

    def test_infer_provider_name_litellm(self):
        self.assertEqual(infer_openai_compatible_provider_name("http://localhost:4000/v1/"), "LiteLLM")

    def test_infer_provider_name_default_openai(self):
        self.assertEqual(infer_openai_compatible_provider_name(None), "OpenAI")
        self.assertEqual(infer_openai_compatible_provider_name(""), "OpenAI")

    def test_infer_provider_name_generic_compatible(self):
        self.assertEqual(infer_openai_compatible_provider_name("https://my-custom-proxy.internal/v1"), "OpenAI Compatible")


if __name__ == "__main__":
    unittest.main()
