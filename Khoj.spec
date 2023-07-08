# -*- mode: python ; coding: utf-8 -*-
from os.path import join
from platform import system
from PyInstaller.utils.hooks import copy_metadata
import sysconfig

datas = [
    ('src/khoj/interface/web', 'khoj/interface/web'),
    (f'{sysconfig.get_paths()["purelib"]}/transformers', 'transformers'),
    (f'{sysconfig.get_paths()["purelib"]}/langchain', 'langchain'),
    (f'{sysconfig.get_paths()["purelib"]}/PIL', 'PIL')
]
datas += copy_metadata('torch')
datas += copy_metadata('tqdm')
datas += copy_metadata('regex')
datas += copy_metadata('requests')
datas += copy_metadata('packaging')
datas += copy_metadata('filelock')
datas += copy_metadata('numpy')
datas += copy_metadata('tokenizers')
datas += copy_metadata('pillow')

block_cipher = None

a = Analysis(
    ['src/khoj/main.py'],
    pathex=[],
    binaries=[],
    datas=datas,
    hiddenimports=['huggingface_hub.repository', 'PIL', 'PIL._tkinter_finder'],
    hookspath=[],
    hooksconfig={},
    runtime_hooks=[],
    excludes=[],
    win_no_prefer_redirects=False,
    win_private_assemblies=False,
    cipher=block_cipher,
    noarchive=False,
)

# Filter out unused and/or duplicate shared libs
torch_lib_paths = {
    join('torch', 'lib', 'libtorch_cuda.so'),
    join('torch', 'lib', 'libtorch_cpu.so'),
}
a.datas = [entry for entry in a.datas if not entry[0] in torch_lib_paths]

os_path_separator = '\\' if system() == 'Windows' else '/'
a.datas = [entry for entry in a.datas if not f'torch{os_path_separator}_C.cp' in entry[0]]
a.datas = [entry for entry in a.datas if not f'torch{os_path_separator}_dl.cp' in entry[0]]

pyz = PYZ(a.pure, a.zipped_data, cipher=block_cipher)

if system() != 'Darwin':
    # Add Splash screen to show on app launch
    splash = Splash(
        'src/khoj/interface/web/assets/icons/favicon-128x128.png',
        binaries=a.binaries,
        datas=a.datas,
        text_pos=(10, 160),
        text_size=12,
        text_color='black',
        minify_script=True,
        always_on_top=True
    )

    exe = EXE(
        pyz,
        a.scripts,
        a.binaries,
        a.zipfiles,
        a.datas,
        splash,
        splash.binaries,
        [],
        name='Khoj',
        debug=False,
        bootloader_ignore_signals=False,
        strip=False,
        upx=True,
        upx_exclude=[],
        runtime_tmpdir=None,
        console=False,
        disable_windowed_traceback=False,
        argv_emulation=False,
        target_arch='x86_64',
        codesign_identity=None,
        entitlements_file=None,
        icon='src/khoj/interface/web/assets/icons/favicon-128x128.ico',
    )
else:
    exe = EXE(
        pyz,
        a.scripts,
        a.binaries,
        a.zipfiles,
        a.datas,
        [],
        name='Khoj',
        debug=False,
        bootloader_ignore_signals=False,
        strip=False,
        upx=True,
        upx_exclude=[],
        runtime_tmpdir=None,
        console=False,
        disable_windowed_traceback=False,
        argv_emulation=False,
        target_arch='x86_64',
        codesign_identity=None,
        entitlements_file=None,
        icon='src/khoj/interface/web/assets/icons/favicon.icns',
    )
    app = BUNDLE(
        exe,
        name='Khoj.app',
        icon='src/khoj/interface/web/assets/icons/favicon.icns',
        bundle_identifier=None,
    )
