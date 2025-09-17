/**
 * Jest tests for Tauri-migrated renderer.js
 */

/** @jest-environment jsdom */

jest.mock('@tauri-apps/api/tauri', () => ({
  invoke: jest.fn(async () => ([])),
}));

jest.mock('@tauri-apps/api/event', () => ({
  listen: jest.fn(async () => ({ unlisten: () => {} })),
}));

jest.mock('@tauri-apps/api/dialog', () => ({
  open: jest.fn(async () => undefined),
}));

jest.mock('@tauri-apps/api/shell', () => ({
  open: jest.fn(async () => undefined),
}));

describe('renderer.js (Tauri)', () => {
  beforeEach(() => {
    document.body.innerHTML = `
      <div id="current-files"></div>
      <div id="current-folders"></div>
      <button id="update-file"></button>
      <button id="update-folder"></button>
      <div id="loading-bar"></div>
      <div id="sync-status"></div>
      <input id="khoj-host-url" />
      <input id="khoj-access-key" />
      <button id="sync-force"></button>
      <button id="delete-all"></button>
    `;

    jest.resetModules();
  });

  test('clicking Add File calls invoke(handle_file_open)', async () => {
    const { invoke } = require('@tauri-apps/api/tauri');
    invoke.mockImplementation(async (cmd, args) => {
      if (cmd === 'get_files' || cmd === 'get_folders') return [];
      if (cmd === 'handle_file_open') return { files: [], folders: [] };
      return [];
    });

    await import('./renderer.js');

    document.getElementById('update-file').click();

    // Allow any async handlers to run
    await new Promise((r) => setTimeout(r, 0));

    expect(invoke).toHaveBeenCalledWith('handle_file_open', expect.objectContaining({ type: 'file' }));
  });
});

describe('utils.js (Tauri)', () => {
  test('external link click uses shell.open', async () => {
    const { open } = require('@tauri-apps/api/shell');
    document.body.innerHTML = `
      <div id="container">
        <a id="ext" href="https://example.com">Example</a>
      </div>
    `;

    const utils = await import('./utils.js');
    utils.openExternalLinksIn(document);

    document.getElementById('ext').click();

    await new Promise((r) => setTimeout(r, 0));
    expect(open).toHaveBeenCalledWith('https://example.com');
  });
});
