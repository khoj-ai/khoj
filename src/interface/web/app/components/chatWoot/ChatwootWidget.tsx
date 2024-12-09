"use client";

import Script from "next/script";

export function ChatwootWidget() {
    return (
        <Script
            id="chatwoot-widget"
            strategy="afterInteractive"
            dangerouslySetInnerHTML={{
                __html: `
          window.chatwootSettings = {
            position: "right",
            type: "standard",
            launcherTitle: "Chat with us"
          };
          (function(d,t) {
            var BASE_URL="https://app.chatwoot.com";
            var g=d.createElement(t),s=d.getElementsByTagName(t)[0];
            g.src=BASE_URL+"/packs/js/sdk.js";
            g.defer = true;
            g.async = true;
            s.parentNode.insertBefore(g,s);
            g.onload=function(){
              window.chatwootSDK.run({
                websiteToken: '5uV59Ay2pvMJenJary2hvvVM',
                baseUrl: BASE_URL
              })
            }
          })(document,"script");
        `,
            }}
        />
    );
}
