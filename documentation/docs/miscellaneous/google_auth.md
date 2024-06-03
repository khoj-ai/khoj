# Setting up Google Auth

To set up your self-hosted Khoj with Google Auth, you need to create a project in the Google Cloud Console and enable the Google Auth API.


To implement this, you'll need to:
1. You must use the `python` package or build from source, because you'll need to install additional packages for the google auth libraries (`prod`). The syntax to install the right packages is
```
pip install khoj-assistant[prod]
```
2. [Create authorization credentials](https://developers.google.com/identity/sign-in/web/sign-in) for your application.
3. Go to your [Google cloud console](https://console.developers.google.com/apis/credentials) and create a configuration like below for the relevant `OAuth 2.0 Client IDs` project:
![Google auth login project settings](https://github.com/khoj-ai/khoj/assets/65192171/9bcbf6f4-197d-4d0c-973a-c10b1331c892)

4. Configure these environment variables: `GOOGLE_CLIENT_SECRET`, and `GOOGLE_CLIENT_ID`. You can find these values in the Google cloud console, in the same place where you configured the authorized origins and redirect URIs.

That's it! That should be all you have to do. Now, when you reload Khoj without `--anonymous-mode`, you should be able to use your Google account to sign in.
