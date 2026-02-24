use tauri::Manager;

// Build a minimal app with tray and plugins; if this panics, test fails.
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
        .expect("failed to build app")
}

#[test]
fn builds_app_without_panic() {
    let app = build_app();
    // Sanity: window exists per config
    let _ = app.get_webview_window("settings");
}
