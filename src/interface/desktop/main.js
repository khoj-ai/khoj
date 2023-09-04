const { app, BrowserWindow, ipcMain } = require('electron');
const todesktop = require("@todesktop/runtime");

todesktop.init();

const fs = require('fs');
const {dialog} = require('electron');

const cron = require('cron').CronJob;
const axios = require('axios');
const { Readable } = require('stream');

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
    foo: {
        type: 'number',
        maximum: 100,
        minimum: 0,
        default: 50
    }
};

var state = {}

const store = new Store({schema});

console.log(store.get('foo'));
//=> 50

store.set('foo', 70);
// [Error: Config schema violation: `foo` should be number]

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

function pushDataToKhoj () {
    let filesToPush = [];
    const files = store.get('files');
    const folders = store.get('folders');
    state = {};

    if (files) {
        for (file of files) {
            filesToPush.push(file.path);
        }
    }
    if (folders) {
        for (folder of folders) {
            const files = fs.readdirSync(folder.path, { withFileTypes: true });
            for (file of files) {
                if (file.isFile() && validFileTypes.includes(file.name.split('.').pop())) {
                    filesToPush.push(path.join(folder.path, file.name));
                }
            }
        }
    }

    let data = {
        files: []
    }

    for (file of filesToPush) {
        try {
            let rawData;
            // If the file is a PDF or IMG file, read it as a binary file
            if (binaryFileTypes.includes(file.split('.').pop())) {
                rawData = fs.readFileSync(file).toString('base64');
            } else {
                rawData = fs.readFileSync(file, 'utf8');
            }

            data.files.push({
                path: file,
                content: rawData
            });
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

    const headers = { 'x-api-key': 'secret', 'Content-Type': 'application/json' };

    const stream = new Readable({
        read() {
            this.push(JSON.stringify(data));
            this.push(null);
        }
    });

    axios.post(`${state.hostURL}/indexer/batch`, stream, { headers })
        .then(response => {
            console.log(response.data);
        })
        .catch(error => {
            console.error(error);
    });

}

async function handleFileOpen (event, key) {
    const { canceled, filePaths } = await dialog.showOpenDialog({properties: ['openFile', 'openDirectory'], filters: [{ name: "Valid Khoj Files", extensions: validFileTypes}] });
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

    ipcMain.handle('getStoreValue', handleFileOpen);

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
