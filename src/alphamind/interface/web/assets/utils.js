// Toggle the navigation menu
function toggleMenu() {
    let menu = document.getElementById("alphamind-nav-menu");
    menu.classList.toggle("show");
}

// Close the dropdown menu if the user clicks outside of it
document.addEventListener('click', function(event) {
    let menu = document.getElementById("alphamind-nav-menu");
    let menuContainer = document.getElementById("alphamind-nav-menu-container");
    if (menuContainer) {
        let isClickOnMenu = menuContainer.contains(event.target) || menuContainer === event.target;
        if (isClickOnMenu === false && menu.classList.contains("show")) {
            menu.classList.remove("show");
        }
    }
});

console.log(`%c %s`, "font-family:monospace", `
 __  __     __  __     ______       __        _____      __
/\\ \\/ /    /\\ \\_\\ \\   /\\  __ \\     /\\ \\      /\\  __ \\   /\\ \\
\\ \\  _"-.  \\ \\  __ \\  \\ \\ \\/\\ \\   _\\_\\ \\     \\ \\  __ \\  \\ \\ \\
 \\ \\_\\ \\_\\  \\ \\_\\ \\_\\  \\ \\_____\\ /\\_____\\     \\ \\_\\ \\_\\  \\ \\_\\
  \\/_/\\/_/   \\/_/\\/_/   \\/_____/ \\/_____/      \\/_/\\/_/   \\/_/


Greetings traveller,

I am ✨AlphaMind✨, your open-source, personal AI copilot.

See my source code at https://github.com/alphamind-ai/alphamind
Read my operating manual at https://docs.alphamind.dev
`);
