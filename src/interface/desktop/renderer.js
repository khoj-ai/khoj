const setFolderButton = document.getElementById("update-folder");
const setFileButton = document.getElementById("update-file");
const loadingBar = document.getElementById("loading-bar");

async function removeFile(filePath) {
  const updatedFiles = await window.removeFileAPI.removeFile(filePath);

  let currentFilesElement = document.getElementById("current-files");
  currentFilesElement.innerHTML = "";
  for (const file of updatedFiles) {
    console.log(file);
    let fileElement = makeFileElement(file);
    currentFilesElement.appendChild(fileElement);
  }
}

async function removeFolder(folderPath) {
  const updatedFolders = await window.removeFolderAPI.removeFolder(folderPath);

  let currentFoldersElement = document.getElementById("current-folders");
  currentFoldersElement.innerHTML = "";
  for (const folder of updatedFolders) {
    console.log(folder);
    let folderElement = makeFolderElement(folder);
    currentFoldersElement.appendChild(folderElement);
  }
}

const toggleFilesButton = document.getElementById("toggle-files");
const currentFiles = document.getElementById("current-files");

const toggleFilesSVG = document.getElementById("toggle-files-svg");

toggleFilesButton.addEventListener("click", () => {
  if (currentFiles.style.display === "none") {
    currentFiles.style.display = "block";
    toggleFilesSVG.style.transform = "rotate(0deg)";
  } else {
    currentFiles.style.display = "none";
    toggleFilesSVG.style.transform = "rotate(180deg)";
  }
});

const toggleFoldersButton = document.getElementById("toggle-folders");
const currentFolders = document.getElementById("current-folders");

const toggleFoldersSVG = document.getElementById("toggle-folders-svg");

toggleFoldersButton.addEventListener("click", () => {
  if (currentFolders.style.display === "none") {
    currentFolders.style.display = "block";
    toggleFoldersSVG.style.transform = "rotate(0deg)";
  } else {
    currentFolders.style.display = "none";
    toggleFoldersSVG.style.transform = "rotate(180deg)";
  }
});

function makeFileElement(file) {
  let fileParentElement = document.createElement("div");
  fileParentElement.classList.add("file-parent-element");

  let fileElement = document.createElement("div");
  fileElement.classList.add("file-element");

  let fileErrorElement = document.createElement("p");
  fileErrorElement.classList.add("file-error");
  let fileNameElement = document.createElement("div");
  fileNameElement.classList.add("content-name");
  fileNameElement.innerHTML = file.path;
  fileElement.appendChild(fileNameElement);
  fileParentElement.insertAdjacentElement("beforeend", fileElement);

  fileParentElement.insertAdjacentElement("beforeend", fileErrorElement);

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
  return fileParentElement;
}

function makeFolderElement(folder) {
  let folderParentElement = document.createElement("div");
  folderParentElement.classList.add("folder-parent-element");

  let folderElement = document.createElement("div");
  folderElement.classList.add("folder-element");

  let folderErrorElement = document.createElement("p");
  folderErrorElement.classList.add("folder-error");
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
  folderParentElement.insertAdjacentElement("beforeend", folderElement);

  folderParentElement.insertAdjacentElement("beforeend", folderErrorElement);
  return folderParentElement;
}

(async function () {
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

setFolderButton.addEventListener("click", async () => {
  await handleFileOpen("folder");
});

setFileButton.addEventListener("click", async () => {
  await handleFileOpen("file");
});

async function handleFileOpen(type) {
  const value = await window.storeValueAPI.handleFileOpen(type);
  console.log(value);
  let currentFilesElement = document.getElementById("current-files");
  let currentFoldersElement = document.getElementById("current-folders");

  if (value.files) {
    currentFilesElement.innerHTML = "";
    value.files.forEach((file) => {
      let fileElement = makeFileElement(file);
      currentFilesElement.appendChild(fileElement);
    });
  }

  if (value.folders) {
    currentFoldersElement.innerHTML = "";
    value.folders.forEach((folder) => {
      let folderElement = makeFolderElement(folder);
      currentFoldersElement.appendChild(folderElement);
    });
  }
}

window.updateStateAPI.onUpdateState((event, state) => {
  const allFileErrorElement = document.querySelectorAll(".file-error");
  const allFolderErrorElement = document.querySelectorAll(".folder-error");
  console.log("state was updated", state);
  loadingBar.style.display = "none";
  let syncStatusElement = document.getElementById("sync-status");
  const currentTime = new Date();
  nextSyncTime = new Date();
  nextSyncTime.setMinutes(Math.ceil((nextSyncTime.getMinutes() + 1) / 10) * 10);
  if (state.completed == false) {
    allFileErrorElement.forEach(
      (element) => (element.textContent = "Sync failed, try again")
    );
    allFolderErrorElement.forEach(
      (element) => (element.textContent = "Sync failed, try again")
    );
    // fileErrorElement.textContent = "Sync failed, try again";
    if (state.error) syncStatusElement.innerHTML = state.error;
    return;
  } else {
    allFileErrorElement.forEach(
      (element) => (element.textContent = "Sync Successfull")
    );
    allFolderErrorElement.forEach(
      (element) => (element.textContent = "Sync Successfull")
    );
  }
  const options = { hour: "2-digit", minute: "2-digit" };
  syncStatusElement.innerHTML = `⏱️ Synced at ${currentTime.toLocaleTimeString(
    undefined,
    options
  )}. Next sync at ${nextSyncTime.toLocaleTimeString(undefined, options)}.`;
});

window.needsSubscriptionAPI.onNeedsSubscription((event, needsSubscription) => {
  console.log("needs subscription", needsSubscription);
  if (needsSubscription) {
    window.alert(
      "Looks like you're out of space to sync your files. Upgrade your plan to unlock more space here: https://app.khoj.dev/config"
    );
    needsSubscriptionElement.style.display = "block";
  }
});

const urlInput = document.getElementById("khoj-host-url");
(async function () {
  const url = await window.hostURLAPI.getURL();
  urlInput.value = url;
})();

urlInput.addEventListener("blur", async () => {
  const urlInputValue = urlInput.value;

  // Check if it's a valid URL
  try {
    new URL(urlInputValue);
  } catch (e) {
    console.log(e);
    alert("Please enter a valid URL");
    return;
  }

  const url = await window.hostURLAPI.setURL(urlInput.value.trim());
  urlInput.value = url;
});

const khojKeyInput = document.getElementById("khoj-access-key");
(async function () {
  const token = await window.tokenAPI.getToken();
  khojKeyInput.value = token;
})();

khojKeyInput.addEventListener("blur", async () => {
  const token = await window.tokenAPI.setToken(khojKeyInput.value.trim());
  khojKeyInput.value = token;
});

const syncForceButton = document.getElementById("sync-force");
syncForceButton.addEventListener("click", async () => {
  loadingBar.style.display = "block";
  await window.syncDataAPI.syncData(true);
});

const deleteAllButton = document.getElementById("delete-all");
deleteAllButton.addEventListener("click", async () => {
  loadingBar.style.display = "block";
  await window.syncDataAPI.deleteAllFiles();
});
