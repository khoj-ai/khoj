use std::{
    collections::HashMap,
    path::Path,
    sync::{Arc, Mutex},
    time::{Duration, SystemTime},
};

use chrono::{DateTime, Utc};
use reqwest::multipart::{Form, Part};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use tauri::{AppHandle, Emitter, State};
use walkdir::WalkDir;

pub const EVENT_UPDATE_STATE: &str = "update-state";
pub const EVENT_NEEDS_SUBSCRIPTION: &str = "needsSubscription";

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct FileEntry {
    pub path: String,
}

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct AppStateInner {
    pub host_url: String,
    pub khoj_token: String,
    pub files: Vec<FileEntry>,
    pub folders: Vec<FileEntry>,
}

#[derive(Clone)]
pub struct AppState(pub Arc<Mutex<AppStateInner>>);

impl Default for AppState {
    fn default() -> Self {
        Self(Arc::new(Mutex::new(AppStateInner {
            host_url: default_host(),
            khoj_token: String::new(),
            files: vec![],
            folders: vec![],
        })))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct SyncedFile {
    pub path: String,
    pub datetime: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct StoreData {
    #[serde(default = "default_host")]
    hostURL: String,
    #[serde(default)]
    khojToken: String,
    #[serde(default)]
    files: Vec<FileEntry>,
    #[serde(default)]
    folders: Vec<FileEntry>,
    #[serde(default)]
    lastSync: Vec<SyncedFile>,
}

fn default_host() -> String {
    "https://app.khoj.dev".into()
}
fn store_path() -> std::path::PathBuf {
    if let Ok(p) = std::env::var("KHOJ_STORE_PATH") {
        return std::path::PathBuf::from(p);
    }
    std::path::PathBuf::from("khoj.json")
}
fn read_store() -> StoreData {
    let p = store_path();
    if let Ok(s) = std::fs::read_to_string(&p) {
        if let Ok(v) = serde_json::from_str::<StoreData>(&s) {
            return v;
        }
    }
    StoreData {
        hostURL: default_host(),
        khojToken: String::new(),
        files: vec![],
        folders: vec![],
        lastSync: vec![],
    }
}
fn write_store(mut s: StoreData) -> Result<(), String> {
    if s.hostURL.is_empty() {
        s.hostURL = default_host();
    }
    let p = store_path();
    if let Some(parent) = p.parent() {
        std::fs::create_dir_all(parent).map_err(|e| e.to_string())?;
    }
    let data = serde_json::to_string_pretty(&s).map_err(|e| e.to_string())?;
    std::fs::write(&p, data).map_err(|e| e.to_string())
}

pub fn init_store_defaults(_app: &tauri::App) -> Result<(), String> {
    let s = read_store();
    write_store(s)
}

#[tauri::command]
pub async fn open_file(_app: AppHandle, path: String) -> Result<Value, String> {
    #[allow(unused)]
    fn os_open(p: &str) -> Result<(), String> {
        #[cfg(target_os = "macos")]
        {
            std::process::Command::new("open")
                .arg(p)
                .spawn()
                .map_err(|e| e.to_string())
                .map(|_| ())
        }
        #[cfg(target_os = "linux")]
        {
            std::process::Command::new("xdg-open")
                .arg(p)
                .spawn()
                .map_err(|e| e.to_string())
                .map(|_| ())
        }
        #[cfg(target_os = "windows")]
        {
            std::process::Command::new("cmd")
                .args(["/C", "start", p])
                .spawn()
                .map_err(|e| e.to_string())
                .map(|_| ())
        }
    }
    let _ = os_open(&path);
    Ok(json!({ "ok": true, "path": path }))
}

#[tauri::command]
pub async fn handle_file_open(
    _app: AppHandle,
    state: State<'_, AppState>,
    r#type: String,
    paths: Option<Vec<String>>,
) -> Result<Value, String> {
    let mut st = state.0.lock().map_err(|_| "state poisoned")?;
    let mut store = read_store();
    let mut files = store.files.clone();
    let mut folders = store.folders.clone();
    if let Some(list) = paths.as_ref() {
        for p in list {
            let path = Path::new(p);
            if path.is_file() && (r#type == "file" || r#type == "auto") {
                if !files.iter().any(|f| f.path == *p) {
                    files.push(FileEntry { path: p.clone() });
                }
            } else if path.is_dir() && (r#type == "folder" || r#type == "auto") {
                if !folders.iter().any(|f| f.path == *p) {
                    folders.push(FileEntry { path: p.clone() });
                }
            }
        }
    }
    st.files = files.clone();
    st.folders = folders.clone();
    store.files = files.clone();
    store.folders = folders.clone();
    let _ = write_store(store);
    Ok(json!({ "files": st.files, "folders": st.folders }))
}

#[tauri::command]
pub async fn get_files(_app: AppHandle, state: State<'_, AppState>) -> Result<Value, String> {
    let files: Vec<FileEntry> = read_store().files;
    let mut st = state.0.lock().map_err(|_| "state poisoned")?;
    st.files = files.clone();
    Ok(json!(files))
}

#[tauri::command]
pub async fn get_folders(_app: AppHandle, state: State<'_, AppState>) -> Result<Value, String> {
    let folders: Vec<FileEntry> = read_store().folders;
    let mut st = state.0.lock().map_err(|_| "state poisoned")?;
    st.folders = folders.clone();
    Ok(json!(folders))
}

#[tauri::command]
pub async fn remove_file(
    _app: AppHandle,
    state: State<'_, AppState>,
    path: String,
) -> Result<Value, String> {
    let mut s = read_store();
    let mut files = s.files.clone();
    files.retain(|f| f.path != path);
    s.files = files.clone();
    let _ = write_store(s);
    let mut st = state.0.lock().map_err(|_| "state poisoned")?;
    st.files = files.clone();
    Ok(json!(files))
}

#[tauri::command]
pub async fn remove_folder(
    _app: AppHandle,
    state: State<'_, AppState>,
    path: String,
) -> Result<Value, String> {
    let mut s = read_store();
    let mut folders = s.folders.clone();
    folders.retain(|f| f.path != path);
    s.folders = folders.clone();
    let _ = write_store(s);
    let mut st = state.0.lock().map_err(|_| "state poisoned")?;
    st.folders = folders.clone();
    Ok(json!(folders))
}

#[tauri::command]
pub async fn set_url(
    _app: AppHandle,
    state: State<'_, AppState>,
    mut url: String,
) -> Result<Value, String> {
    if url.ends_with('/') {
        url.pop();
    }
    if !url.contains("://") {
        url = format!("http://{}", url);
    }
    let mut st = state.0.lock().map_err(|_| "state poisoned")?;
    st.host_url = url.clone();
    let mut s = read_store();
    s.hostURL = st.host_url.clone();
    let _ = write_store(s);
    Ok(json!(st.host_url))
}

#[tauri::command]
pub async fn get_url(_app: AppHandle, state: State<'_, AppState>) -> Result<Value, String> {
    let url: String = read_store().hostURL;
    let mut st = state.0.lock().map_err(|_| "state poisoned")?;
    st.host_url = url.clone();
    Ok(json!(url))
}

#[tauri::command]
pub async fn set_token(
    _app: AppHandle,
    state: State<'_, AppState>,
    token: String,
) -> Result<Value, String> {
    let mut st = state.0.lock().map_err(|_| "state poisoned")?;
    st.khoj_token = token.clone();
    let mut s = read_store();
    s.khojToken = st.khoj_token.clone();
    let _ = write_store(s);
    Ok(json!(st.khoj_token))
}

#[tauri::command]
pub async fn get_token(_app: AppHandle, state: State<'_, AppState>) -> Result<Value, String> {
    let token: String = read_store().khojToken;
    let mut st = state.0.lock().map_err(|_| "state poisoned")?;
    st.khoj_token = token.clone();
    Ok(json!(token))
}

#[tauri::command]
pub async fn get_user_info(_app: AppHandle, _state: State<'_, AppState>) -> Result<Value, String> {
    let s = read_store();
    let host = s.hostURL;
    let token = s.khojToken;
    if host.starts_with("mock://") {
        return Ok(
            json!({"username":"demo","email":"demo@example.com","photo":null,"is_active":false,"has_documents":false}),
        );
    }
    if token.is_empty() {
        return Err("missing token".into());
    }
    let url = format!("{}/api/v1/user?client=desktop", host);
    let client = reqwest::Client::new();
    let res = client
        .get(url)
        .header(reqwest::header::AUTHORIZATION, format!("Bearer {}", token))
        .send()
        .await
        .map_err(|e| e.to_string())?;
    if !res.status().is_success() {
        return Err(format!("status {}", res.status()).into());
    }
    let v: Value = res.json().await.map_err(|e| e.to_string())?;
    Ok(v)
}

#[tauri::command]
pub async fn sync_data(
    app: AppHandle,
    _state: State<'_, AppState>,
    regenerate: Option<bool>,
) -> Result<Value, String> {
    sync_data_impl(&app, regenerate.unwrap_or(false)).await
}

#[tauri::command]
pub async fn delete_all_files(app: AppHandle, state: State<'_, AppState>) -> Result<Value, String> {
    {
        let mut st = state.0.lock().map_err(|_| "state poisoned")?;
        st.files.clear();
        st.folders.clear();
    }
    let mut s = read_store();
    s.files.clear();
    s.folders.clear();
    let _ = write_store(s);
    let res = sync_data_impl(&app, true).await?;
    Ok(json!({"deleted":true,"result":res}))
}

fn is_supported_file(path: &Path) -> bool {
    const TEXT_EXTS: &[&str] = &[
        "org",
        "md",
        "markdown",
        "txt",
        "html",
        "xml",
        "appleplist",
        "asm",
        "asp",
        "batch",
        "c",
        "cs",
        "css",
        "csv",
        "eml",
        "go",
        "ini",
        "internetshortcut",
        "java",
        "javascript",
        "json",
        "latex",
        "lisp",
        "makefile",
        "mht",
        "mum",
        "pem",
        "perl",
        "php",
        "powershell",
        "python",
        "rdf",
        "rst",
        "rtf",
        "ruby",
        "rust",
        "scala",
        "shell",
        "smali",
        "sql",
        "svg",
        "symlinktext",
        "vba",
        "winregistry",
        "yaml",
    ];
    const BIN_EXTS: &[&str] = &["pdf", "jpg", "jpeg", "png", "webp"];
    let ext = path
        .extension()
        .and_then(|s| s.to_str())
        .unwrap_or("")
        .to_ascii_lowercase();
    TEXT_EXTS.contains(&ext.as_str()) || BIN_EXTS.contains(&ext.as_str())
}

fn filename_to_mime(path: &Path) -> &'static str {
    match path
        .extension()
        .and_then(|s| s.to_str())
        .unwrap_or("")
        .to_ascii_lowercase()
        .as_str()
    {
        "pdf" => "application/pdf",
        "png" => "image/png",
        "jpg" | "jpeg" => "image/jpeg",
        "webp" => "image/webp",
        "md" | "markdown" => "text/markdown",
        "org" => "text/org",
        _ => "text/plain",
    }
}

fn parse_rfc3339(s: &str) -> Option<SystemTime> {
    DateTime::parse_from_rfc3339(s)
        .ok()
        .map(|dt| dt.with_timezone(&Utc).into())
}
fn to_rfc3339(st: SystemTime) -> String {
    let dt: DateTime<Utc> = st.into();
    dt.to_rfc3339()
}

async fn sync_data_impl(app: &AppHandle, regenerate: bool) -> Result<Value, String> {
    let s = read_store();
    let host_url = s.hostURL.clone();
    let token = s.khojToken.clone();
    let files_cfg = s.files.clone();
    let folders_cfg = s.folders.clone();
    let last_sync_vec = s.lastSync.clone();

    let mut file_status: HashMap<String, (bool, Option<String>)> = HashMap::new();
    if token.is_empty() || host_url.is_empty() {
        let _ = app.emit(
            EVENT_UPDATE_STATE,
            build_sync_state_json(true, &file_status, None),
        );
        return Ok(json!({ "queued": false, "skipped": true }));
    }

    let mut files_to_push: Vec<String> = Vec::new();
    for f in files_cfg {
        if Path::new(&f.path).is_file() {
            files_to_push.push(f.path);
        }
    }
    for folder in folders_cfg {
        let base = Path::new(&folder.path);
        if base.is_dir() {
            for entry in WalkDir::new(base).into_iter().filter_map(Result::ok) {
                let p = entry.path();
                if let Some(name) = p.file_name().and_then(|s| s.to_str()) {
                    if name.starts_with('.') {
                        continue;
                    }
                }
                if p.is_file() && is_supported_file(p) {
                    if let Some(s) = p.to_str() {
                        files_to_push.push(s.to_string());
                    }
                }
            }
        }
    }

    let mut last_sync_map: HashMap<String, SystemTime> = HashMap::new();
    for s in last_sync_vec {
        if let Some(t) = parse_rfc3339(&s.datetime) {
            last_sync_map.insert(s.path, t);
        }
    }

    let mut batches: Vec<Vec<(String, Vec<u8>, String)>> = Vec::new();
    let mut current: Vec<(String, Vec<u8>, String)> = Vec::new();
    let now = SystemTime::now();
    for path_str in &files_to_push {
        let p = Path::new(path_str);
        let meta = std::fs::metadata(p).map_err(|e| e.to_string())?;
        let modified = meta.modified().unwrap_or(now - Duration::from_secs(3600));
        if !regenerate {
            if let Some(prev) = last_sync_map.get(path_str) {
                if modified <= *prev {
                    continue;
                }
            }
        }
        let bytes = std::fs::read(p).map_err(|e| e.to_string())?;
        let mime = filename_to_mime(p).to_string();
        current.push((path_str.clone(), bytes, mime));
        if current.len() >= 1000 {
            batches.push(current);
            current = Vec::new();
        }
        file_status.insert(path_str.clone(), (true, None));
    }
    if !current.is_empty() {
        batches.push(current);
    }

    let files_set: std::collections::HashSet<&String> = files_to_push.iter().collect();
    let mut del_batch: Vec<(String, Vec<u8>, String)> = Vec::new();
    for (path, _) in last_sync_map.iter() {
        if !files_set.contains(path) {
            del_batch.push((
                path.clone(),
                Vec::new(),
                filename_to_mime(Path::new(path)).to_string(),
            ));
            file_status.insert(path.clone(), (true, None));
        }
        if del_batch.len() >= 1000 {
            batches.push(del_batch);
            del_batch = Vec::new();
        }
    }
    if !del_batch.is_empty() {
        batches.push(del_batch);
    }

    if batches.is_empty() {
        let _ = app.emit(
            EVENT_UPDATE_STATE,
            build_sync_state_json(true, &file_status, None),
        );
        return Ok(json!({"queued":false,"skipped":true}));
    }
    let _ = app.emit(
        EVENT_UPDATE_STATE,
        build_sync_state_json(false, &file_status, None),
    );

    let is_mock = host_url.starts_with("mock://");
    let mut any_429 = false;
    if !is_mock {
        let client = reqwest::Client::new();
        let mut headers = reqwest::header::HeaderMap::new();
        use reqwest::header::{HeaderValue, AUTHORIZATION};
        headers.insert(
            AUTHORIZATION,
            HeaderValue::from_str(&format!("Bearer {}", token)).map_err(|e| e.to_string())?,
        );
        for batch in batches.iter() {
            let mut form = Form::new();
            for (path, bytes, mime) in batch.iter() {
                let part = Part::bytes(bytes.clone())
                    .file_name(path.clone())
                    .mime_str(mime)
                    .map_err(|e| e.to_string())?;
                form = form.part("files", part);
            }
            let url = format!("{}/api/content?client=desktop", host_url);
            let req = if regenerate {
                client.put(url)
            } else {
                client.patch(url)
            };
            let res = req
                .headers(headers.clone())
                .multipart(form)
                .send()
                .await
                .map_err(|e| e.to_string())?;
            if res.status().as_u16() == 429 {
                any_429 = true;
            }
            if !res.status().is_success() {
                let text = res.text().await.unwrap_or_default();
                for (path, _, _) in batch.iter() {
                    if let Some(entry) = file_status.get_mut(path) {
                        *entry = (false, Some(text.clone()));
                    }
                }
            }
        }
    }
    if any_429 {
        let _ = app.emit(EVENT_NEEDS_SUBSCRIPTION, json!(true));
    }

    let new_last_sync: Vec<SyncedFile> = files_to_push
        .into_iter()
        .map(|p| SyncedFile {
            path: p,
            datetime: to_rfc3339(SystemTime::now()),
        })
        .collect();
    let mut s2 = read_store();
    s2.lastSync = new_last_sync;
    let _ = write_store(s2);
    let _ = app.emit(
        EVENT_UPDATE_STATE,
        build_sync_state_json(true, &file_status, None),
    );
    Ok(json!({"queued":true,"sent_batches":true}))
}

fn build_sync_state_json(
    completed: bool,
    file_status: &HashMap<String, (bool, Option<String>)>,
    top_error: Option<String>,
) -> Value {
    let mut root = serde_json::Map::new();
    root.insert("completed".into(), Value::Bool(completed));
    if let Some(e) = top_error {
        root.insert("error".into(), Value::String(e));
    }
    for (path, (ok, err)) in file_status.iter() {
        let mut m = serde_json::Map::new();
        m.insert("success".into(), Value::Bool(*ok));
        if let Some(e) = err {
            m.insert("error".into(), Value::String(e.clone()));
        }
        root.insert(path.clone(), Value::Object(m));
    }
    Value::Object(root)
}
