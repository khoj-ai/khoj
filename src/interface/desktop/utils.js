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


window.appInfoAPI.getInfo((_, info) => {
    let alphamindVersionElement = document.getElementById("about-page-version");
    if (alphamindVersionElement) {
        alphamindVersionElement.innerHTML = `<code>${info.version}</code>`;
    }
    let alphamindTitleElement = document.getElementById("about-page-title");
    if (alphamindTitleElement) {
        alphamindTitleElement.innerHTML = '<b>AlphaMind for ' + (info.platform === 'win32' ? 'Windows' : info.platform === 'darwin' ? 'macOS' : 'Linux') + '</b>';
    }
});

function toggleNavMenu() {
    let menu = document.getElementById("alphamind-nav-menu");
    menu.classList.toggle("show");
}

// Close the dropdown menu if the user clicks outside of it
document.addEventListener('click', function (event) {
    let menu = document.getElementById("alphamind-nav-menu");
    let menuContainer = document.getElementById("alphamind-nav-menu-container");
    let isClickOnMenu = menuContainer?.contains(event.target) || menuContainer === event.target;
    if (menu && isClickOnMenu === false && menu.classList.contains("show")) {
        menu.classList.remove("show");
    }
});

async function populateHeaderPane() {
    let userInfo = null;
    try {
        userInfo = await window.userInfoAPI.getUserInfo();
    } catch (error) {
        console.log("User not logged in");
    }

    let username = userInfo?.username ?? "?";
    let user_photo = userInfo?.photo;
    let is_active = userInfo?.is_active;
    let has_documents = userInfo?.has_documents;

    // Populate the header element with the navigation pane
    return `
        <a class="alphamind-logo" href="/">
            <img class="alphamind-logo" src="./assets/icons/alphamind_logo.png" alt="AlphaMind"></img>
        </a>
        <nav class="alphamind-nav">
        ${userInfo && userInfo.email
            ? `<div class="alphamind-status-box">
              <span class="alphamind-status-connected"></span>
               <span class="alphamind-status-text">Connected to server</span>
               </div>`
            : `<div class="alphamind-status-box">
              <span class="alphamind-status-not-connected"></span>
               <span class="alphamind-status-text">Not connected to server</span>
               </div>`
        }
            ${username ? `
                <div id="alphamind-nav-menu-container" class="alphamind-nav dropdown">
                    ${user_photo && user_photo != "None" ? `
                        <img id="profile-picture" class="${is_active ? 'circle subscribed' : 'circle'}" src="${user_photo}" alt="${username[0].toUpperCase()}" referrerpolicy="no-referrer">
                    ` : `
                        <div id="profile-picture" class="${is_active ? 'circle user-initial subscribed' : 'circle user-initial'}" alt="${username[0].toUpperCase()}">${username[0].toUpperCase()}</div>
                    `}
                    <div id="alphamind-nav-menu" class="alphamind-nav-dropdown-content">
                        <div class="alphamind-nav-username"> ${username} </div>
                        <a onclick="window.navigateAPI.navigateToWebHome()" class="alphamind-nav-link">
                        <img class="alphamind-nav-icon" src="./assets/icons/open-link.svg" alt="Open Host Url"></img>
                        Open App
                        </a>
                    </div>
                </div>
            ` : ''}
        </nav>
    `;
}
