export function ContentSecurityPolicy() {
    return (
        <meta
            httpEquiv="Content-Security-Policy"
            content="default-src 'self' https://assets.khoj.dev;
               media-src * blob:;
               script-src 'self' https://assets.khoj.dev https://app.chatwoot.com https://accounts.google.com 'unsafe-inline' 'unsafe-eval';
               connect-src 'self' blob: https://ipapi.co/json ws://localhost:42110 https://accounts.google.com;
               style-src 'self' https://assets.khoj.dev 'unsafe-inline' https://fonts.googleapis.com https://accounts.google.com;
               img-src 'self' data: blob: https://*.khoj.dev https://accounts.google.com https://*.googleusercontent.com https://*.google.com/ https://*.gstatic.com;
               font-src 'self' https://assets.khoj.dev https://fonts.gstatic.com;
               frame-src 'self' https://accounts.google.com https://app.chatwoot.com;
               child-src 'self' https://app.chatwoot.com;
               object-src 'none';"
        ></meta>
    );
}
