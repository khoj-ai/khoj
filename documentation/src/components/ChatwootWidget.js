import ExecutionEnvironment from '@docusaurus/ExecutionEnvironment';

// Only execute on client-side
if (ExecutionEnvironment.canUseDOM) {
  (function (d, t) {
    var BASE_URL = "https://app.chatwoot.com";
    var g = d.createElement(t), s = d.getElementsByTagName(t)[0];
    g.src = BASE_URL + "/packs/js/sdk.js";
    g.defer = true;
    g.async = true;
    s.parentNode.insertBefore(g, s);
    g.onload = function () {
      window.chatwootSDK.run({
        websiteToken: 'cFxvnLSjfE2UF4UUiPCA5NsF',
        baseUrl: BASE_URL
      })
    }
  })(document, 'script');
}
