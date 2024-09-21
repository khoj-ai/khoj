# Authenticate

:::info
This is only helpful for self-hosted users or teams. If you're using [Khoj Cloud](https://app.khoj.dev), both Magic Links and Google OAuth work.
:::

By default, most of the instructions for self-hosting Khoj assume a single user, and so the default configuration is to run in anonymous mode. However, if you want to enable authentication, you can do so either with with [Magic Links](#using-magic-links) or [Google OAuth](#using-google-oauth) as shown below. This can be helpful to make Khoj securely accessible to you and your team.

:::tip[Note]
Remove the `--anonymous-mode` flag from your khoj start up command or docker-compose file to enable authentication.
:::

## Using Magic Links
The most secure way to do this is to integrate with [Resend](https://resend.com) by setting up an account and adding an environment variable for `RESEND_API_KEY`. You can get your API key [here](https://resend.com/api-keys). This will allow you to automatically send sign-in links to users who want to log in.

It's still possible to use the magic links feature without Resend, but you'll need to manually send the magic links to users who want to log in.

## Manually sending magic links

1. The user will have to enter their email address in the login form.
They'll click `Send Magic Link`. Without the Resend API key, this will just create an unverified account for them in the backend
<img src="/img/magic_link.png" alt="Magic link login form" width="400"/>

2. You can get their magic link using the admin panel
Go to the [admin panel](http://localhost:42110/server/admin/database/khojuser/). You'll see a list of users. Search for the user you want to send a magic link to. Tick the checkbox next to their row, and use the action drop down at the top to 'Get email login URL'. This will generate a magic link that you can send to the user, which will appear at the top of the admin interface.

| Get email login URL | Retrieved login URL |
|---------------------|---------------------|
| <img src="/img/admin_get_emali_login.png" alt="Get user magic sign in link" width="400" />| <img src="/img/admin_successful_login_url.png" alt="Successfully retrieved a login URL" width="400" />|

3. Send the magic link to the user. They can click on it to log in.

Once they click on the link, they'll automatically be logged in. They'll have to repeat this process for every new device they want to log in from, but they shouldn't have to repeat it on the same device.

A given magic link can only be used once. If the user tries to use it again, they'll be redirected to the login page to get a new magic link.

## Using Google OAuth

To set up your self-hosted Khoj with Google Auth, you need to create a project in the Google Cloud Console and enable the Google Auth API.

To implement this, you'll need to:
1. You must use the `python` package or build from source, because you'll need to install additional packages for the google auth libraries (`prod`). The syntax to install the right packages is
   ```
   pip install khoj[prod]
   ```
2. [Create authorization credentials](https://developers.google.com/identity/sign-in/web/sign-in) for your application.
3. Open your [Google cloud console](https://console.developers.google.com/apis/credentials) and create a configuration like below for the relevant `OAuth 2.0 Client IDs` project:
![Google auth login project settings](https://github.com/khoj-ai/khoj/assets/65192171/9bcbf6f4-197d-4d0c-973a-c10b1331c892)

4. Configure these environment variables: `GOOGLE_CLIENT_SECRET`, and `GOOGLE_CLIENT_ID`. You can find these values in the Google cloud console, in the same place where you configured the authorized origins and redirect URIs.

That's it! That should be all you have to do. Now, when you reload Khoj without `--anonymous-mode`, you should be able to use your Google account to sign in.
