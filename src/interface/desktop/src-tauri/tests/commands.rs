// Integration tests for Tauri command stubs
// These validate that commands can be called and serialize/deserialize JSON.

use serde_json::{json, Value};
use tauri::{async_runtime, Manager};

// Helper to build an app instance with state and commands registered.
fn build_app() -> tauri::App {
    tauri::Builder::default()
        .manage(khoj_app::AppState::default())
        .invoke_handler(tauri::generate_handler![
            khoj_app::handle_file_open,
            khoj_app::get_files,
            khoj_app::get_folders,
            khoj_app::remove_file,
            khoj_app::remove_folder,
            khoj_app::set_url,
            khoj_app::get_url,
            khoj_app::set_token,
            khoj_app::get_token,
            khoj_app::sync_data,
            khoj_app::delete_all_files,
            khoj_app::open_file,
        ])
        .build(tauri::generate_context!())
        .expect("failed to build tauri app for tests")
}

#[test]
fn test_get_set_url_roundtrip() {
    let app = build_app();
    let state = app.state::<khoj_app::AppState>();
    let result = async_runtime::block_on(khoj_app::set_url(
        app.handle(),
        state.clone(),
        "https://example.com/".to_string(),
    ))
    .expect("set_url failed");
    assert!(result.is_string());

    let state = app.state::<khoj_app::AppState>();
    let got =
        async_runtime::block_on(khoj_app::get_url(app.handle(), state)).expect("get_url failed");
    assert_eq!(got, Value::String("https://example.com".into()));
}

#[test]
fn test_get_set_token_roundtrip() {
    let app = build_app();
    let state = app.state::<khoj_app::AppState>();
    let set = async_runtime::block_on(khoj_app::set_token(
        app.handle(),
        state.clone(),
        "abc123".into(),
    ))
    .expect("set_token failed");
    assert_eq!(set, Value::String("abc123".into()));

    let state = app.state::<khoj_app::AppState>();
    let got = async_runtime::block_on(khoj_app::get_token(app.handle(), state))
        .expect("get_token failed");
    assert_eq!(got, Value::String("abc123".into()));
}

#[test]
fn test_files_and_folders_list_shape() {
    let app = build_app();
    let handle = app.handle();
    let state = app.state::<khoj_app::AppState>();

    // Initially empty
    let files = async_runtime::block_on(khoj_app::get_files(app.handle(), state.clone())).unwrap();
    assert!(files.is_array());
    let folders =
        async_runtime::block_on(khoj_app::get_folders(app.handle(), state.clone())).unwrap();
    assert!(folders.is_array());

    // Simulate adding via handle_file_open stub
    let res = async_runtime::block_on(khoj_app::handle_file_open(
        handle.clone(),
        state.clone(),
        "file".into(),
        Some(vec!["/tmp/demo.txt".into()]),
    ))
    .unwrap();
    assert!(res.get("files").is_some());
    assert!(res.get("folders").is_some());

    let res = async_runtime::block_on(khoj_app::handle_file_open(
        handle,
        state.clone(),
        "folder".into(),
        Some(vec!["/tmp".into()]),
    ))
    .unwrap();
    assert!(res.get("files").is_some());
    assert!(res.get("folders").is_some());
}

#[test]
fn test_remove_items_and_json_serialization() {
    let app = build_app();
    let handle = app.handle();
    let state = app.state::<khoj_app::AppState>();

    // Add one file and one folder via stub
    let _ = async_runtime::block_on(khoj_app::handle_file_open(
        handle.clone(),
        state.clone(),
        "file".into(),
        None,
    ));
    let _ = async_runtime::block_on(khoj_app::handle_file_open(
        handle,
        state.clone(),
        "folder".into(),
        None,
    ));

    // Remove using stubbed paths
    let files = async_runtime::block_on(khoj_app::get_files(app.handle(), state.clone())).unwrap();
    if let Some(first) = files.as_array().and_then(|a| a.first()) {
        let path = first
            .get("path")
            .and_then(|v| v.as_str())
            .unwrap_or_default();
        let after = async_runtime::block_on(khoj_app::remove_file(
            app.handle(),
            state.clone(),
            path.to_string(),
        ))
        .unwrap();
        assert!(after.is_array());
    }

    let folders =
        async_runtime::block_on(khoj_app::get_folders(app.handle(), state.clone())).unwrap();
    if let Some(first) = folders.as_array().and_then(|a| a.first()) {
        let path = first
            .get("path")
            .and_then(|v| v.as_str())
            .unwrap_or_default();
        let after = async_runtime::block_on(khoj_app::remove_folder(
            app.handle(),
            state.clone(),
            path.to_string(),
        ))
        .unwrap();
        assert!(after.is_array());
    }

    // JSON roundtrip sanity
    let sample = json!({ "a": 1, "b": [1,2,3] });
    let s = serde_json::to_string(&sample).unwrap();
    let back: Value = serde_json::from_str(&s).unwrap();
    assert_eq!(back["a"], 1);
}

#[test]
fn test_sync_and_delete_all_files_commands() {
    let app = build_app();
    let handle = app.handle();
    let state = app.state::<khoj_app::AppState>();

    let res = async_runtime::block_on(khoj_app::sync_data(
        handle.clone(),
        state.clone(),
        Some(false),
    ))
    .unwrap();
    assert!(res.get("queued").and_then(|v| v.as_bool()).unwrap_or(false));

    let res = async_runtime::block_on(khoj_app::delete_all_files(handle, state)).unwrap();
    assert!(res
        .get("deleted")
        .and_then(|v| v.as_bool())
        .unwrap_or(false));
}

#[test]
fn test_open_file_returns_json() {
    let app = build_app();
    let handle = app.handle();
    let res = async_runtime::block_on(khoj_app::open_file(handle, "/tmp/demo.txt".into())).unwrap();
    assert!(res.get("ok").and_then(|v| v.as_bool()).unwrap_or(false));
}
