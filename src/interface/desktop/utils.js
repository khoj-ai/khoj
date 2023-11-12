console.log(`%c %s`, "font-family:monospace", `
 __  __     __  __     ______       __        _____      __
/\\ \\/ /    /\\ \\_\\ \\   /\\  __ \\     /\\ \\      /\\  __ \\   /\\ \\
\\ \\  _"-.  \\ \\  __ \\  \\ \\ \\/\\ \\   _\\_\\ \\     \\ \\  __ \\  \\ \\ \\
 \\ \\_\\ \\_\\  \\ \\_\\ \\_\\  \\ \\_____\\ /\\_____\\     \\ \\_\\ \\_\\  \\ \\_\\
  \\/_/\\/_/   \\/_/\\/_/   \\/_____/ \\/_____/      \\/_/\\/_/   \\/_/

Greetings traveller,

I am ✨Khoj✨, your open-source, personal AI copilot.

See my source code at https://github.com/khoj-ai/khoj
Read my operating manual at https://docs.khoj.dev
`);


window.appInfoAPI.getInfo((_, info) => {
    let khojVersionElement = document.getElementById("about-page-version");
    if (khojVersionElement) {
        khojVersionElement.innerHTML = `<code>${info.version}</code>`;
    }
    let khojTitleElement = document.getElementById("about-page-title");
    if (khojTitleElement) {
        khojTitleElement.innerHTML = '<b>Khoj for ' + (info.platform === 'win32' ? 'Windows' : info.platform === 'darwin' ? 'macOS' : 'Linux') + '</b>';
    }
});
