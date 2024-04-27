# Github integration

The Github integration allows you to index as many repositories as you want. It's currently default configured to index Issues, Commits, and all Markdown/Org files in each repository. For large repositories, this takes a fairly long time, but it works well for smaller projects.

# Configure your settings

1. Go to [https://app.khoj.dev/config](https://app.khoj.dev/config) and enter in settings for the data sources you want to index. You'll have to specify the file paths.

## Use the Github plugin

1. Generate a [classic PAT (personal access token)](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens) from [Github](https://github.com/settings/tokens) with `repo` and `admin:org` scopes at least.
2. Navigate to [https://app.khoj.dev/config/content-source/github](https://app.khoj.dev/config/content-source/github) to configure your Github settings. Enter in your PAT, along with details for each repository you want to index.
3. Click `Save`. Go back to the settings page and click `Configure`.
4. Go to [https://app.khoj.dev/](https://app.khoj.dev/) and start searching!
