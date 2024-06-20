# Authentication when Self-Hosting

By default, most of the instructions for self-hosting Khoj assume a single user, and so the default configuration is to run in anonymous mode. However, if you want to enable authentication, you can do so either with [Google Auth](/miscellaneous/google_auth) or with magic links, as shown below. This can be helpful if you want to make sure your Khoj instance is only accessible to you and your team.

:::tip[Note]
Remove the `--anonymous-mode` flag in your start up command to enable authentication.
:::

The most secure way to do this is to integrate with [Resend](https://resend.com) by setting up an account and adding an environment variable for `RESEND_API_KEY`. You can get your API key [here](https://resend.com/api-keys). This will allow you to automatically send sign-in links to users who want to log in.

It's still possible to use the magic links feature without Resend, but you'll need to manually send the magic links to users who want to log in.

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
