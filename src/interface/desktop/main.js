const { app, BrowserWindow, ipcMain } = require('electron');
const todesktop = require("@todesktop/runtime");

todesktop.init();

const fs = require('fs');
const {dialog} = require('electron');

const cron = require('cron').CronJob;
const axios = require('axios');

const KHOJ_URL = 'http://127.0.0.1:42110'

const Store = require('electron-store');

const validFileTypes = ['org', 'md', 'markdown', 'txt', 'html', 'xml', 'pdf']

const binaryFileTypes = ['pdf', 'png', 'jpg', 'jpeg']

const schema = {
    files: {
        type: 'array',
        items: {
            type: 'object',
            properties: {
                path: {
                    type: 'string'
                }
            }
        },
        default: []
    },
    folders: {
        type: 'array',
        items: {
            type: 'object',
            properties: {
                path: {
                    type: 'string'
                }
            }
        },
        default: []
    },
    hostURL: {
        type: 'string',
        default: KHOJ_URL
    },
    lastSync: {
        type: 'array',
        items: {
            type: 'object',
            properties: {
                path: {
                    type: 'string'
                },
                datetime: {
                    type: 'string'
                }
            }
        }
    }
};

var state = {}

const store = new Store({ schema });

console.log(store);

// include the Node.js 'path' module at the top of your file
const path = require('path');

function handleSetTitle (event, title) {
    const webContents = event.sender
    const win = BrowserWindow.fromWebContents(webContents)
    win.setTitle(title)
    dialog.showOpenDialog({properties: ['openFile', 'openDirectory'] }).then(function (response) {
        if (!response.canceled) {
            // handle fully qualified file name
          console.log(response.filePaths[0]);
        } else {
          console.log("no file selected");
        }
    });
}

function filenameToMimeType (filename) {
    const extension = filename.split('.').pop();
    switch (extension) {
        case 'pdf':
            return 'application/pdf';
        case 'png':
            return 'image/png';
        case 'jpg':
        case 'jpeg':
            return 'image/jpeg';
        case 'md':
        case 'markdown':
            return 'text/markdown';
        case 'org':
            return 'text/org';
        default:
            return 'text/plain';
    }
}

function pushDataToKhoj (regenerate = false) {
    let filesToPush = [];
    const files = store.get('files') || [];
    const folders = store.get('folders') || [];
    state = { completed: true }

    for (const file of files) {
        filesToPush.push(file.path);
    }

    for (const folder of folders) {
        const files = fs.readdirSync(folder.path, { withFileTypes: true });
        for (const file of files) {
            if (file.isFile() && validFileTypes.includes(file.name.split('.').pop())) {
                filesToPush.push(path.join(folder.path, file.name));
            }
        }
    }

    const lastSync = store.get('lastSync') || [];
    const formData = new FormData();
    for (const file of filesToPush) {
        const stats = fs.statSync(file);
        if (!regenerate) {
            if (stats.mtime.toISOString() < lastSync.find((syncedFile) => syncedFile.path === file)?.datetime) {
                continue;
            }
        }

        try {
            encoding = binaryFileTypes.includes(file.split('.').pop()) ? "binary" : "utf8";
            mimeType = filenameToMimeType(file) + (encoding === "utf8" ? "; charset=UTF-8" : "");
            fileObj = new Blob([fs.createReadStream(file, encoding)], { type: mimeType });
            formData.append('files', fileObj, file);
            state[file] = {
                success: true,
            }
        } catch (err) {
            console.error(err);
            state[file] = {
                success: false,
                error: err
            }
        }
    }

    for (const syncedFile of lastSync) {
        if (!filesToPush.includes(syncedFile.path)) {
            fileObj = new Blob([""], { type: filenameToMimeType(syncedFile.path) });
            formData.append('files', fileObj, syncedFile.path);
        }
    }

    if (!!formData?.entries()?.next().value) {
        const hostURL = store.get('hostURL') || KHOJ_URL;
        const headers = {
            'x-api-key': 'secret'
        };
        axios.post(`${hostURL}/api/v1/indexer/batch?regenerate=${regenerate}`, formData, { headers })
            .then(response => {
                console.log(response.data);
                const win = BrowserWindow.getAllWindows()[0];
                win.webContents.send('update-state', state);
                let lastSync = [];
                for (const file of filesToPush) {
                    lastSync.push({
                        path: file,
                        datetime: new Date().toISOString()
                    });
                }
                store.set('lastSync', lastSync);
            })
            .catch(error => {
                console.error(error);
                state['completed'] = false
                const win = BrowserWindow.getAllWindows()[0];
                win.webContents.send('update-state', state);
            });
    }
}

pushDataToKhoj();

async function handleFileOpen (type) {
    let { canceled, filePaths } = {canceled: true, filePaths: []};
    if (type === 'file') {
        ({ canceled, filePaths } = await dialog.showOpenDialog({properties: ['openFile' ], filters: [{ name: "Valid Khoj Files", extensions: validFileTypes}] }));
    } else if (type === 'folder') {
        ({ canceled, filePaths } = await dialog.showOpenDialog({properties: ['openDirectory' ]}));
    }
    if (!canceled) {
        const files = store.get('files') || [];
        const folders = store.get('folders') || [];

        for (const filePath of filePaths) {
            console.log(filePath);
            if (fs.existsSync(filePath)) {
                const stats = fs.statSync(filePath);
                if (stats.isFile()) {
                    console.log(`${filePath} is a file.`);

                    if (files.find((file) => file.path === filePath)) {
                        continue;
                    }

                    files.push({path: filePath});
                    store.set('files', files);
                } else if (stats.isDirectory()) {
                    console.log(`${filePath} is a directory.`);

                    if (folders.find((folder) => folder.path === filePath)) {
                        continue;
                    }

                    folders.push({path: filePath});
                    store.set('folders', folders);
                }

            } else {
                console.log(`${filePath} does not exist.`);
            }
        }
        return {
            files: store.get('files'),
            folders: store.get('folders')
        }
    }
}

async function getFiles () {
    return store.get('files');
}

async function getFolders () {
    return store.get('folders');
}

async function setURL (event, url) {
    store.set('hostURL', url);
    return store.get('hostURL');
}

async function getURL () {
    return store.get('hostURL');
}

async function removeFile (event, filePath) {
    const files = store.get('files');
    const newFiles = files.filter((file) => file.path !== filePath);
    store.set('files', newFiles);
    return newFiles;
}

async function removeFolder (event, folderPath) {
    const folders = store.get('folders');
    const newFolders = folders.filter((folder) => folder.path !== folderPath);
    store.set('folders', newFolders);
    return newFolders;
}

async function syncData (regenerate = false) {
    try {
        pushDataToKhoj(regenerate);
        const date = new Date();
        console.log('Pushing data to Khoj at: ', date);
    } catch (err) {
        console.error(err);
    }
}

const createWindow = () => {
    const win = new BrowserWindow({
      width: 800,
      height: 800,
    //   titleBarStyle: 'hidden',
      webPreferences: {
        preload: path.join(__dirname, 'preload.js'),
        nodeIntegration: true,
      }
    })

    const job = new cron('0 */10 * * * *', function() {
        try {
            pushDataToKhoj();
            const date = new Date();
            console.log('Pushing data to Khoj at: ', date);
            win.webContents.send('update-state', state);
        } catch (err) {
            console.error(err);
        }
    });

    win.setResizable(true);
    win.setOpacity(0.95);
    win.setBackgroundColor('#FFFFFF');
    win.setHasShadow(true);

    job.start();

    win.loadFile('index.html')
}

app.whenReady().then(() => {
    ipcMain.on('set-title', handleSetTitle);

    ipcMain.handle('handleFileOpen', (event, type) => {
        return handleFileOpen(type);
    });

    ipcMain.on('update-state', (event, arg) => {
        console.log(arg);
        event.reply('update-state', arg);
    });

    ipcMain.handle('getFiles', getFiles);
    ipcMain.handle('getFolders', getFolders);

    ipcMain.handle('removeFile', removeFile);
    ipcMain.handle('removeFolder', removeFolder);

    ipcMain.handle('setURL', setURL);
    ipcMain.handle('getURL', getURL);

    ipcMain.handle('syncData', (event, regenerate) => {
        syncData(regenerate);
    });

    createWindow()

    app.setAboutPanelOptions({
        applicationName: "Khoj",
        applicationVersion: "0.0.1",
        version: "0.0.1",
        authors: "Khoj Team",
        website: "https://khoj.dev",
        iconPath: path.join(__dirname, 'assets', 'khoj.png')
    });

    app.on('ready', async() => {
        try {
            const result = await todesktop.autoUpdater.checkForUpdates();
            if (result.updateInfo) {
              console.log("Update found:", result.updateInfo.version);
              todesktop.autoUpdater.restartAndInstall();
            }
          } catch (e) {
            console.log("Update check failed:", e);
        }
    })

    app.on('activate', () => {
      if (BrowserWindow.getAllWindows().length === 0) createWindow()
    })
})

app.on('window-all-closed', () => {
    if (process.platform !== 'darwin') app.quit()
})
