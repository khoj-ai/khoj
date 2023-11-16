const { app, BrowserWindow, ipcMain, Tray, Menu, nativeImage, shell } = require('electron');
const todesktop = require("@todesktop/runtime");
const khojPackage = require('./package.json');

todesktop.init();

const fs = require('fs');
const {dialog} = require('electron');

const cron = require('cron').CronJob;
const axios = require('axios');

const KHOJ_URL = 'https://app.khoj.dev';

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
    khojToken: {
        type: 'string',
        default: ''
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

let syncing = false;
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
    // Don't sync if token or hostURL is not set or if already syncing
    if (store.get('khojToken') === '' || store.get('hostURL') === '' || syncing === true) {
        const win = BrowserWindow.getAllWindows()[0];
        if (win) win.webContents.send('update-state', state);
        return;
    } else {
        syncing = true;
    }

    let filesToPush = [];
    const files = store.get('files') || [];
    const folders = store.get('folders') || [];
    state = { completed: true }

    // Collect paths of all configured files to index
    for (const file of files) {
        filesToPush.push(file.path);
    }

    // Collect paths of all indexable files in configured folders
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
            // Only push files that have been modified since last sync
            if (stats.mtime.toISOString() < lastSync.find((syncedFile) => syncedFile.path === file)?.datetime) {
                continue;
            }
        }

        // Collect all updated or newly created files since last sync to index on Khoj server
        try {
            let encoding = binaryFileTypes.includes(file.split('.').pop()) ? "binary" : "utf8";
            let mimeType = filenameToMimeType(file) + (encoding === "utf8" ? "; charset=UTF-8" : "");
            let fileContent = Buffer.from(fs.readFileSync(file, { encoding: encoding }), encoding);
            let fileObj = new Blob([fileContent], { type: mimeType });
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

    // Mark deleted files for removal from index on Khoj server
    for (const syncedFile of lastSync) {
        if (!filesToPush.includes(syncedFile.path)) {
            fileObj = new Blob([""], { type: filenameToMimeType(syncedFile.path) });
            formData.append('files', fileObj, syncedFile.path);
        }
    }

    // Send collected files to Khoj server for indexing
    if (!!formData?.entries()?.next().value) {
        const hostURL = store.get('hostURL') || KHOJ_URL;
        const headers = {
            'Authorization': `Bearer ${store.get("khojToken")}`
        };
        axios.post(`${hostURL}/api/v1/index/update?force=${regenerate}&client=desktop`, formData, { headers })
            .then(response => {
                console.log(response.data);
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
            })
            .finally(() => {
                // Syncing complete
                syncing = false;
                const win = BrowserWindow.getAllWindows()[0];
                if (win) win.webContents.send('update-state', state);
            });
    } else {
        // Syncing complete
        syncing = false;
        const win = BrowserWindow.getAllWindows()[0];
        if (win) win.webContents.send('update-state', state);
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

async function getToken () {
    return store.get('khojToken');
}

async function setToken (event, token) {
    store.set('khojToken', token);
    return store.get('khojToken');
}

async function getFiles () {
    return store.get('files');
}

async function getFolders () {
    return store.get('folders');
}

async function setURL (event, url) {
    // Sanitize the URL. Remove trailing slash if present. Add http:// if not present.
    url = url.replace(/\/$/, "");
    if (!url.match(/^[a-zA-Z]+:\/\//)) {
        url = `http://${url}`;
    }

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

async function deleteAllFiles () {
    try {
        store.set('files', []);
        store.set('folders', []);
        pushDataToKhoj(true);
        const date = new Date();
        console.log('Pushing data to Khoj at: ', date);
    } catch (err) {
        console.error(err);
    }
}


let firstRun = true;
let win = null;
const createWindow = (tab = 'chat.html') => {
    win = new BrowserWindow({
      width: 800,
      height: 800,
      show: false,
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
    win.setBackgroundColor('#f5f4f3');
    win.setHasShadow(true);

    job.start();

    win.loadFile(tab)

    if (firstRun === true) {
        firstRun = false;

        // Create splash screen
        var splash = new BrowserWindow({width: 400, height: 400, transparent: true, frame: false, alwaysOnTop: true});
        splash.setOpacity(1.0);
        splash.setBackgroundColor('#d16b4e');
        splash.loadFile('splash.html');

        // Show splash screen on app load
        win.once('ready-to-show', () => {
            setTimeout(function(){ splash.close(); win.show(); }, 4500);
        });
    } else {
        // Show main window directly if not first run
        win.once('ready-to-show', () => { win.show(); });
    }
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

    ipcMain.on('navigate', (event, page) => {
        win.loadFile(page);
    });

    ipcMain.on('navigateToWebApp', (event, page) => {
        shell.openExternal(`${store.get('hostURL')}/${page}`);
    });

    ipcMain.handle('getFiles', getFiles);
    ipcMain.handle('getFolders', getFolders);

    ipcMain.handle('removeFile', removeFile);
    ipcMain.handle('removeFolder', removeFolder);

    ipcMain.handle('setURL', setURL);
    ipcMain.handle('getURL', getURL);

    ipcMain.handle('setToken', setToken);
    ipcMain.handle('getToken', getToken);

    ipcMain.handle('syncData', (event, regenerate) => {
        syncData(regenerate);
    });
    ipcMain.handle('deleteAllFiles', deleteAllFiles);

    createWindow()

    app.setAboutPanelOptions({
        applicationName: "Khoj",
        applicationVersion: khojPackage.version,
        version: khojPackage.version,
        authors: "Saba Imran, Debanjum Singh Solanky and contributors",
        website: "https://khoj.dev",
        copyright: "GPL v3",
        iconPath: path.join(__dirname, 'assets', 'icons', 'favicon-128x128.png')
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

/*
** About Page
*/

let aboutWindow;

function openAboutWindow() {
    if (aboutWindow) { aboutWindow.focus(); return; }

    aboutWindow = new BrowserWindow({
        width: 400,
        height: 400,
        titleBarStyle: 'hidden',
        show: false,
        webPreferences: {
            preload: path.join(__dirname, 'preload.js'),
            nodeIntegration: true,
        },
    });

    aboutWindow.loadFile('about.html');

    // Pass OS, Khoj version to About page
    aboutWindow.webContents.on('did-finish-load', () => {
        aboutWindow.webContents.send('appInfo', { version: khojPackage.version, platform: process.platform });
    });

    // Open links in external browser
    aboutWindow.webContents.setWindowOpenHandler(({ url }) => {
        shell.openExternal(url);
        return { action: 'deny' };
    });

    aboutWindow.once('ready-to-show', () => { aboutWindow.show(); });
    aboutWindow.on('closed', () => { aboutWindow = null; });
}

/*
**  System Tray Icon
*/

let tray

openWindow = (page) => {
    if (BrowserWindow.getAllWindows().length === 0) {
        createWindow(page);
    } else {
        win.loadFile(page); win.show();
    }
}

app.whenReady().then(() => {
    const icon = nativeImage.createFromPath('assets/icons/favicon-20x20.png')
    tray = new Tray(icon)

    const contextMenu = Menu.buildFromTemplate([
        { label: 'Chat', type: 'normal', click: () => { openWindow('chat.html'); }},
        { label: 'Search', type: 'normal', click: () => { openWindow('search.html') }},
        { label: 'Configure', type: 'normal', click: () => { openWindow('config.html') }},
        { type: 'separator' },
        { label: 'About Khoj', type: 'normal', click: () => { openAboutWindow(); } },
        { label: 'Quit', type: 'normal', click: () => { app.quit() } }
    ])

    tray.setToolTip('Khoj')
    tray.setContextMenu(contextMenu)
})
