# Obsidian Development

You can use the [Obsidian Khoj plugin](https://obsidian.md/plugins?id=khoj) to share your Obsidian knowledge base with your AI assistant. The plugin will index your notes and make them searchable by your AI assistant.

## Development

In `src/interface/obsidian`, run:

```bash
yarn install
yarn build
```

You'll want to link the built plugin to your Obsidian vault. You can do this by symlinking the built folder to your Obsidian vault's plugins folder. For example:

```bash
ln -s /path/to/khoj-assistant/src/interface/obsidian /path/to/obsidian-vault/.obsidian/plugins/obsidian-assistant
```

Now, once you reload your Obsidian, you should see your updated plugin code running.
