# Remote Access

By default self-hosted AlphaMind is only accessible on the machine it is running. To securely access it from a remote machine:
- Set the `ALPHAMIND_DOMAIN` environment variable to your remotely accessible ip or domain via shell or docker-compose.yml.
  Examples: `ALPHAMIND_DOMAIN=my.alphamind-domain.com`, `ALPHAMIND_DOMAIN=192.168.0.4`.
- Ensure the AlphaMind Admin password and `ALPHAMIND_DJANGO_SECRET_KEY` environment variable are securely set.
- Setup [Authentication](/advanced/authentication).
- Open access to the AlphaMind port (default: 42110) from your OS and Network firewall.

:::warning[Use HTTPS certificate]
To expose AlphaMind on a custom domain over the public internet, use of an SSL certificate is strongly recommended. You can use [Let's Encrypt](https://letsencrypt.org/) to get a free SSL certificate for your domain.

To disable HTTPS, set the `ALPHAMIND_NO_HTTPS` environment variable to `True`. This can be useful if AlphaMind is only accessible behind a secure, private network.
:::

:::info[Try Tailscale]
You can use [Tailscale](https://tailscale.com/) for easy, secure access to your self-hosted AlphaMind over the network.
1. Set `ALPHAMIND_DOMAIN` to your machines [tailscale ip](https://tailscale.com/kb/1452/connect-to-devices#identify-your-devices) or [fqdn on tailnet](https://tailscale.com/kb/1081/magicdns#fully-qualified-domain-names-vs-machine-names). E.g `ALPHAMIND_DOMAIN=100.4.2.0` or `ALPHAMIND_DOMAIN=alphamind.tailfe8c.ts.net`
2. Access AlphaMind by opening `http://tailscale-ip-of-server:42110` or `http://fqdn-of-server:42110` from any device on your tailscale network
:::
