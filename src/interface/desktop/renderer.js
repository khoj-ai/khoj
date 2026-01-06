// Tauri migration: replace Electron preload bridges with @tauri-apps/api
import { invoke } from '@tauri-apps/api/tauri';
import { listen } from '@tauri-apps/api/event';
import { open as openDialog } from '@tauri-apps/api/dialog';

const setFolderButton = document.getElementById('update-folder');
const setFileButton = document.getElementById('update-file');
const loadingBar = document.getElementById('loading-bar');

async function removeFile(filePath) {
    const updatedFiles = await invoke('remove_file', { path: filePath });

    let currentFilesElement = document.getElementById("current-files");
    currentFilesElement.innerHTML = '';
    for (const file of updatedFiles) {
        console.log(file);
        let fileElement = makeFileElement(file);
        currentFilesElement.appendChild(fileElement);
    }
}

async function removeFolder(folderPath) {
    const updatedFolders = await invoke('remove_folder', { path: folderPath });

    let currentFoldersElement = document.getElementById("current-folders");
    currentFoldersElement.innerHTML = '';
    for (const folder of updatedFolders) {
        console.log(folder);
        let folderElement = makeFolderElement(folder);
        currentFoldersElement.appendChild(folderElement);
    }
}

const currentFiles = document.getElementById('current-files');

const currentFolders = document.getElementById('current-folders');

function makeFileElement(file) {
    let fileElement = document.createElement("div");
    fileElement.classList.add("file-element");

    let fileNameElement = document.createElement("div");
    fileNameElement.classList.add("content-name");
    fileNameElement.innerHTML = file.path;
    fileNameElement.style.cursor = "pointer";

    fileNameElement.addEventListener("click", () => {
        invoke('open_file', { path: file.path }).catch(console.error);
    });

    fileElement.appendChild(fileNameElement);

    let buttonContainer = document.createElement("div");
    buttonContainer.classList.add("remove-button-container");
    let removeFileButton = document.createElement("button");
    let fileSyncedImage = document.createElement("img");
    fileSyncedImage.classList.add("file-synced-image");
    fileSyncedImage.src = "./assets/icons/file-synced.svg";

    // Create trash icon image
    let trashIcon = document.createElement("img");
    trashIcon.src = "./assets/icons/trash-solid.svg";
    trashIcon.classList.add("trash-icon");

    removeFileButton.classList.add("remove-file-button");
    removeFileButton.appendChild(trashIcon);
    removeFileButton.addEventListener("click", () => {
        removeFile(file.path);
    });

    buttonContainer.appendChild(removeFileButton);
    buttonContainer.insertAdjacentElement("afterbegin", fileSyncedImage);
    fileElement.appendChild(buttonContainer);
    return fileElement;
}

function makeFolderElement(folder) {
    let folderElement = document.createElement("div");
    folderElement.classList.add("folder-element");

    let folderNameElement = document.createElement("div");
    folderNameElement.classList.add("content-name");
    folderNameElement.innerHTML = folder.path;
    folderNameElement.style.cursor = "pointer";

    folderNameElement.addEventListener("click", () => {
        invoke('open_file', { path: folder.path }).catch(console.error);
    });

    folderElement.appendChild(folderNameElement);

    let buttonContainer = document.createElement("div");
    buttonContainer.classList.add("remove-button-container");
    let removeFolderButton = document.createElement("button");
    removeFolderButton.classList.add("remove-folder-button");

    // Create trash icon image
    let trashIcon = document.createElement("img");
    trashIcon.src = "./assets/icons/trash-solid.svg";
    trashIcon.classList.add("trash-icon");

    removeFolderButton.appendChild(trashIcon);

    removeFolderButton.addEventListener("click", () => {
        removeFolder(folder.path);
    });
    buttonContainer.appendChild(removeFolderButton);
    folderElement.appendChild(buttonContainer);
    return folderElement;
}

(async function () {
    const files = await invoke('get_files');
    let currentFilesElement = document.getElementById("current-files");
    for (const file of files) {
        console.log(file);
        let fileElement = makeFileElement(file);
        currentFilesElement.appendChild(fileElement);
    }

    const folders = await invoke('get_folders');
    let currentFoldersElement = document.getElementById("current-folders");
    for (const folder of folders) {
        let folderElement = makeFolderElement(folder);
        currentFoldersElement.appendChild(folderElement);
    }
})();

setFolderButton.addEventListener('click', async () => {
    await handleFileOpen('folder');
});

setFileButton.addEventListener('click', async () => {
    await handleFileOpen('file');
});

async function handleFileOpen(type) {
    let selected = null;
    try {
        selected = await openDialog({
            multiple: true,
            directory: type === 'folder',
        });
    } catch (e) {
        console.warn('Dialog open failed:', e);
    }

    const paths = Array.isArray(selected) ? selected : (selected ? [selected] : []);
    const value = await invoke('handle_file_open', { type, paths });
    console.log(value);
    let currentFilesElement = document.getElementById("current-files");
    let currentFoldersElement = document.getElementById("current-folders");

    if (value.files) {
        currentFilesElement.innerHTML = '';
        value.files.forEach((file) => {
            let fileElement = makeFileElement(file);
            currentFilesElement.appendChild(fileElement);
        });
    }

    if (value.folders) {
        currentFoldersElement.innerHTML = '';
        value.folders.forEach((folder) => {
            let folderElement = makeFolderElement(folder);
            currentFoldersElement.appendChild(folderElement);
        });
    }
}

listen('update-state', ({ payload: state }) => {
    const fileSyncedImage = document.querySelectorAll(".file-synced-image");
    console.log("state was updated", state);
    loadingBar.style.display = 'none';
    let syncStatusElement = document.getElementById("sync-status");
    syncStatusElement.innerHTML = '';
    const currentTime = new Date();
    nextSyncTime = new Date();
    nextSyncTime.setMinutes(Math.ceil((nextSyncTime.getMinutes() + 1) / 10) * 10);
    if (state.completed == false) {

        fileSyncedImage.forEach((image) => {
            image.style.display = "block"
            image.src = "./assets/icons/file-not-synced.svg"
        })
        if (state.error) syncStatusElement.innerHTML = state.error;
        return;
    } else {
        fileSyncedImage.forEach((image) => {
            image.style.display = "block"
            image.src = "./assets/icons/file-synced.svg"
        })

    }
    const options = { hour: '2-digit', minute: '2-digit' };

    const clockElement = document.createElement("div");
    const clockIcon = document.createElement("img");
    clockIcon.src = "./assets/icons/clock.svg";
    clockIcon.classList.add("clock-icon");

    clockElement.appendChild(clockIcon);
    syncStatusElement.appendChild(clockElement);
    syncStatusElement.innerHTML += ` Synced at ${currentTime.toLocaleTimeString(undefined, options)}. Next sync at ${nextSyncTime.toLocaleTimeString(undefined, options)}.`;
});

listen('needsSubscription', ({ payload: needsSubscription }) => {
    console.log("needs subscription", needsSubscription);
    if (needsSubscription) {
        window.alert("Looks like you're out of space to sync your files. Upgrade your plan to unlock more space here: https://app.khoj.dev/settings#subscription");
        const needsSubscriptionElement = document.getElementById('needs-subscription');
        if (needsSubscriptionElement) needsSubscriptionElement.style.display = 'block';
    }
});

const urlInput = document.getElementById('khoj-host-url');
(async function () {
    try {
        const url = await invoke('get_url');
        if (urlInput) urlInput.value = url;
    } catch (e) { console.warn(e); }
})();

urlInput.addEventListener('blur', async () => {
    const urlInputValue = urlInput.value;

    // Check if it's a valid URL
    try {
        new URL(urlInputValue);
    } catch (e) {
        console.log(e);
        alert('Please enter a valid URL');
        return;
    }

    const url = await invoke('set_url', { url: urlInput.value.trim() });
    urlInput.value = url;
});

const khojKeyInput = document.getElementById('khoj-access-key');
(async function () {
    try {
        const token = await invoke('get_token');
        if (khojKeyInput) khojKeyInput.value = token;
    } catch (e) { console.warn(e); }
})();

khojKeyInput.addEventListener('blur', async () => {
    const token = await invoke('set_token', { token: khojKeyInput.value.trim() });
    khojKeyInput.value = token;
});

const syncForceButton = document.getElementById('sync-force');
syncForceButton.addEventListener('click', async () => {
    loadingBar.style.display = 'block';
    await invoke('sync_data', { regenerate: true });
});

const deleteAllButton = document.getElementById('delete-all');
deleteAllButton.addEventListener('click', async () => {
    loadingBar.style.display = 'block';
    await invoke('delete_all_files');
});
