import yaml

# Do not emit tags when dumping to YAML
yaml.emitter.Emitter.process_tag = lambda self, *args, **kwargs: None  # type: ignore[assignment]


def yaml_dump(data):
    return yaml.dump(data, allow_unicode=True, sort_keys=False, default_flow_style=False)
