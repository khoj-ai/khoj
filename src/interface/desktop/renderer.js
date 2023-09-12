const getButton = document.getElementById('update-data')
const showKey = document.getElementById('show-key');

async function removeFile(filePath) {
    const updatedFiles = await window.removeFileAPI.removeFile(filePath);

    let currentFilesElement = document.getElementById("current-files");
    currentFilesElement.innerHTML = '';
    for (const file of updatedFiles) {
        console.log(file);
        let fileElement = makeFileElement(file);
        currentFilesElement.appendChild(fileElement);
    }
}

async function removeFolder(folderPath) {
    const updatedFolders = await window.removeFolderAPI.removeFolder(folderPath);

    let currentFoldersElement = document.getElementById("current-folders");
    currentFoldersElement.innerHTML = '';
    for (const folder of updatedFolders) {
        console.log(folder);
        let folderElement = makeFolderElement(folder);
        currentFoldersElement.appendChild(folderElement);
    }
}

const toggleFilesButton = document.getElementById('toggle-files');
const currentFiles = document.getElementById('current-files');

const toggleFilesSVG = document.getElementById('toggle-files-svg');

toggleFilesButton.addEventListener('click', () => {
    if (currentFiles.style.display === 'none') {
        currentFiles.style.display = 'block';
        toggleFilesSVG.style.transform = 'rotate(0deg)';
    } else {
        currentFiles.style.display = 'none';
        toggleFilesSVG.style.transform = 'rotate(180deg)';
    }
});

const toggleFoldersButton = document.getElementById('toggle-folders');
const currentFolders = document.getElementById('current-folders');

const toggleFoldersSVG = document.getElementById('toggle-folders-svg');


toggleFoldersButton.addEventListener('click', () => {
    if (currentFolders.style.display === 'none') {
        currentFolders.style.display = 'block';
        toggleFoldersSVG.style.transform = 'rotate(0deg)';
    } else {
        currentFolders.style.display = 'none';
        toggleFoldersSVG.style.transform = 'rotate(180deg)';
    }
});

function makeFileElement(file) {
    let fileElement = document.createElement("div");
    fileElement.classList.add("file-element");
    let fileNameElement = document.createElement("div");
    fileNameElement.classList.add("content-name");
    fileNameElement.innerHTML = file.path;
    fileElement.appendChild(fileNameElement);

    let buttonContainer = document.createElement("div");
    buttonContainer.classList.add("remove-button-container");
    let removeFileButton = document.createElement("button");
    removeFileButton.classList.add("remove-file-button");
    removeFileButton.innerHTML = "🗑️";
    removeFileButton.addEventListener("click", () => {
        removeFile(file.path);
    });
    buttonContainer.appendChild(removeFileButton);
    fileElement.appendChild(buttonContainer);
    return fileElement;
}

function makeFolderElement(folder) {
    let folderElement = document.createElement("div");
    folderElement.classList.add("folder-element");
    let folderNameElement = document.createElement("div");
    folderNameElement.classList.add("content-name");
    folderNameElement.innerHTML = folder.path;
    folderElement.appendChild(folderNameElement);

    let buttonContainer = document.createElement("div");
    buttonContainer.classList.add("remove-button-container");
    let removeFolderButton = document.createElement("button");
    removeFolderButton.classList.add("remove-folder-button");
    removeFolderButton.innerHTML = "🗑️";
    removeFolderButton.addEventListener("click", () => {
        removeFolder(folder.path);
    });
    buttonContainer.appendChild(removeFolderButton);
    folderElement.appendChild(buttonContainer);
    return folderElement;
}

(async function() {
    const files = await window.getFilesAPI.getFiles();
    let currentFilesElement = document.getElementById("current-files");
    for (const file of files) {
        console.log(file);
        let fileElement = makeFileElement(file);
        currentFilesElement.appendChild(fileElement);
    }

    const folders = await window.getFoldersAPI.getFolders();
    let currentFoldersElement = document.getElementById("current-folders");
    for (const folder of folders) {
        let folderElement = makeFolderElement(folder);
        currentFoldersElement.appendChild(folderElement);
    }
})();

getButton.addEventListener('click', async () => {
    const key = 'foo';
    const value = await window.storeValueAPI.getStoreValue(key);
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
});

window.updateStateAPI.onUpdateState((event, state) => {
    console.log("state was updated", state);
    let syncStatusElement = document.getElementById("sync-status");
    const currentTime = new Date();
    if (state.completed == false) {
        syncStatusElement.innerHTML = `Sync was unsuccessful at ${currentTime.toLocaleTimeString()}. Contact team@khoj.dev to report this issue.`;
        return;
    }
    syncStatusElement.innerHTML = `Last synced at ${currentTime.toLocaleTimeString()}`;
});

const urlInput = document.getElementById('khoj-host-url');
(async function() {
    const url = await window.hostURLAPI.getURL();
    urlInput.value = url;
})();

urlInput.addEventListener('blur', async () => {
    const urlInputValue = urlInput.value;

    // Check if it's a valid URL
    try {
        new URL(urlInputValue);
    } catch (e) {
        console.log(e);
        return;
    }

    const url = await window.hostURLAPI.setURL(urlInput.value.trim());
    urlInput.value = url;
});

const syncButton = document.getElementById('sync-data');
const syncForceToggle = document.getElementById('sync-force');
syncButton.addEventListener('click', async () => {
    const regenerate = syncForceToggle.checked;
    await window.syncDataAPI.syncData(regenerate);
});
