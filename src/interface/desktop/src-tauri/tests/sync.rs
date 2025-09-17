use serde_json::Value;
use std::{
    fs,
    path::PathBuf,
    sync::{Arc, Mutex},
};
use tauri::Manager;
use tempfile::tempdir;

fn build_app() -> tauri::App {
    tauri::Builder::default()
        .manage(khoj_app::AppState::default())
        .plugin(tauri_plugin_store::Builder::default().build())
        .invoke_handler(tauri::generate_handler![
            khoj_app::sync_data,
            khoj_app::delete_all_files,
            khoj_app::get_user_info,
        ])
        .build(tauri::generate_context!())
        .expect("failed to build app")
}

#[test]
fn sync_emits_update_events_and_sets_last_sync() {
    let app = build_app();
    let handle = app.handle();

    // Prepare temp folder with a file
    let tmp = tempdir().expect("tempdir");
    let file_path: PathBuf = tmp.path().join("note.md");
    fs::write(&file_path, b"hello khoj").unwrap();

    // Seed store file via env var for isolation
    let store_path = tmp.path().join("khoj.json");
    std::env::set_var("KHOJ_STORE_PATH", &store_path);
    let seed = serde_json::json!({
        "hostURL": "mock://khoj",
        "khojToken": "token123",
        "files": [],
        "folders": [{"path": tmp.path().to_string_lossy().to_string()}],
        "lastSync": []
    });
    std::fs::write(&store_path, serde_json::to_string(&seed).unwrap()).unwrap();

    // Listen for update-state events
    let updates: Arc<Mutex<Vec<Value>>> = Arc::new(Mutex::new(Vec::new()));
    let updates_clone = updates.clone();
    let _unlisten = handle.listen_global(khoj_app::EVENT_UPDATE_STATE, move |e| {
        if let Some(p) = e.payload() {
            if let Ok(v) = serde_json::from_str::<Value>(p) {
                updates_clone.lock().unwrap().push(v);
            }
        }
    });

    // Run sync
    let state = app.state::<khoj_app::AppState>();
    let res =
        tauri::async_runtime::block_on(khoj_app::sync_data(handle.clone(), state, Some(false)))
            .unwrap();
    assert!(res.get("queued").and_then(|v| v.as_bool()).unwrap_or(false));

    // Expect at least one update-state event with completed true
    let vec = updates.lock().unwrap().clone();
    assert!(!vec.is_empty());
    assert!(vec.iter().any(|v| v
        .get("completed")
        .and_then(|b| b.as_bool())
        .unwrap_or(false)));

    // lastSync should be written into the store file
    let v: Value = serde_json::from_str(&std::fs::read_to_string(&store_path).unwrap()).unwrap();
    assert!(v.get("lastSync").is_some());

    // get_user_info should return dummy data under mock host
    let state2 = app.state::<khoj_app::AppState>();
    let user =
        tauri::async_runtime::block_on(khoj_app::get_user_info(app.handle(), state2)).unwrap();
    assert_eq!(user.get("username").and_then(|v| v.as_str()), Some("demo"));
}

#[test]
fn delete_all_clears_files_and_folders() {
    let app = build_app();
    let handle = app.handle();

    // Seed store file
    let store_path = tempdir().unwrap().path().join("khoj.json");
    std::env::set_var("KHOJ_STORE_PATH", &store_path);
    let seed = serde_json::json!({
        "hostURL": "mock://khoj",
        "khojToken": "token123",
        "files": [{"path": "/tmp/a.txt"}],
        "folders": [{"path": "/tmp/f"}],
        "lastSync": []
    });
    std::fs::write(&store_path, serde_json::to_string(&seed).unwrap()).unwrap();

    let state = app.state::<khoj_app::AppState>();
    let _ =
        tauri::async_runtime::block_on(khoj_app::delete_all_files(handle.clone(), state)).unwrap();

    // Verify cleared
    let v: Value = serde_json::from_str(&std::fs::read_to_string(&store_path).unwrap()).unwrap();
    assert_eq!(v.get("files").cloned(), Some(serde_json::json!([])));
    assert_eq!(v.get("folders").cloned(), Some(serde_json::json!([])));
}
