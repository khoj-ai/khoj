const { app, BrowserWindow, globalShortcut, clipboard} = require('electron');
const path = require('path');

let mainWindow;

function createWindow() {
  mainWindow = new BrowserWindow({
    width: 800,
    height: 600,
    webPreferences: {
      contextIsolation: true,
      enableRemoteModule: false,
    },
  });

  mainWindow.loadFile('index.html');
}

let isControlCPressed = false;

app.on('ready', () => {
  createWindow();

  // Register a global shortcut for Ctrl+K
  globalShortcut.register('CommandOrControl+Shift+K', () => {
    console.log('Ctrl+K pressed');
    const clipboardText = clipboard.readText();
    console.log('Clipboard Text:', clipboardText);
    mainWindow.webContents.executeJavaScript(`document.getElementById('clipboardText').innerHTML = '<p>Clipboard Text: ${clipboardText}</p>';`); // Update HTML content directly
  });
});

app.on('will-quit', () => {
  // Unregister all global shortcuts when the app is about to quit
  globalShortcut.unregisterAll();
});

app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') {
    app.quit();
  }
});

app.on('activate', () => {
  if (BrowserWindow.getAllWindows().length === 0) {
    createWindow();
  }
});
