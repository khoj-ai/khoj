// Tauri migration: replace Electron preload bridges with @tauri-apps/api
import { invoke } from '@tauri-apps/api/tauri';
import { open as openShell } from '@tauri-apps/api/shell';
import { getVersion } from '@tauri-apps/api/app';
import { platform as getPlatform } from '@tauri-apps/api/os';

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

(async function initAboutInfo() {
    try {
        const ver = await getVersion();
        const plat = await getPlatform();
        let khojVersionElement = document.getElementById("about-page-version");
        if (khojVersionElement) {
            khojVersionElement.innerHTML = `<code>${ver}</code>`;
        }
        let khojTitleElement = document.getElementById("about-page-title");
        if (khojTitleElement) {
            khojTitleElement.innerHTML = '<b>Khoj for ' + (plat === 'win32' ? 'Windows' : plat === 'darwin' ? 'macOS' : 'Linux') + '</b>';
        }
    } catch (e) {
        console.warn('Failed to init about info', e);
    }
})();

export function toggleNavMenu() {
    let menu = document.getElementById("khoj-nav-menu");
    menu.classList.toggle("show");
}

// Close the dropdown menu if the user clicks outside of it
document.addEventListener('click', function (event) {
    let menu = document.getElementById("khoj-nav-menu");
    let menuContainer = document.getElementById("khoj-nav-menu-container");
    let isClickOnMenu = menuContainer?.contains(event.target) || menuContainer === event.target;
    if (menu && isClickOnMenu === false && menu.classList.contains("show")) {
        menu.classList.remove("show");
    }
});

export async function populateHeaderPane() {
    let userInfo = null;
    try {
        userInfo = await invoke('get_user_info');
    } catch (error) {
        console.log("User not logged in");
    }

    let username = userInfo?.username ?? "?";
    let user_photo = userInfo?.photo;
    let is_active = userInfo?.is_active;
    let has_documents = userInfo?.has_documents;

    // Populate the header element with the navigation pane
    return `
        <a class="khoj-logo" href="/">
            <img class="khoj-logo" src="./assets/icons/khoj_logo.png" alt="Khoj"></img>
        </a>
        <nav class="khoj-nav">
        ${userInfo && userInfo.email
            ? `<div class="khoj-status-box">
              <span class="khoj-status-connected"></span>
               <span class="khoj-status-text">Connected to server</span>
               </div>`
            : `<div class="khoj-status-box">
              <span class="khoj-status-not-connected"></span>
               <span class="khoj-status-text">Not connected to server</span>
               </div>`
        }
            ${username ? `
                <div id="khoj-nav-menu-container" class="khoj-nav dropdown">
                    ${user_photo && user_photo != "None" ? `
                        <img id="profile-picture" class="${is_active ? 'circle subscribed' : 'circle'}" src="${user_photo}" alt="${username[0].toUpperCase()}" referrerpolicy="no-referrer">
                    ` : `
                        <div id="profile-picture" class="${is_active ? 'circle user-initial subscribed' : 'circle user-initial'}" alt="${username[0].toUpperCase()}">${username[0].toUpperCase()}</div>
                    `}
                    <div id="khoj-nav-menu" class="khoj-nav-dropdown-content">
                        <div class="khoj-nav-username"> ${username} </div>
                        <a href="#" class="khoj-nav-link" data-action="open-app">
                        <img class="khoj-nav-icon" src="./assets/icons/open-link.svg" alt="Open Host Url"></img>
                        Open App
                        </a>
                    </div>
                </div>
            ` : ''}
        </nav>
    `;
}

// Attach navigation handlers to elements created by populateHeaderPane
export async function attachHeaderNavHandlers(root = document) {
    const link = root.querySelector('.khoj-nav-link[data-action="open-app"]');
    if (link) {
        link.addEventListener('click', async (e) => {
            e.preventDefault();
            try {
                const url = await invoke('get_url');
                await openShell(url);
            } catch (err) {
                console.warn('Failed to open app URL', err);
            }
        });
    }
}

// Open any external anchors in given container via OS shell
export function openExternalLinksIn(root = document) {
    root.addEventListener('click', async (e) => {
        const a = e.target.closest('a');
        if (!a) return;
        const href = a.getAttribute('href') || '';
        const isExternal = href.startsWith('http://') || href.startsWith('https://');
        if (isExternal) {
            e.preventDefault();
            try { await openShell(href); } catch (err) { console.warn(err); }
        }
    });
}
