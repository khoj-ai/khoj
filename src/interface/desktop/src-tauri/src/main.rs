#![cfg_attr(
    all(not(debug_assertions), target_os = "windows"),
    windows_subsystem = "windows"
)]

use std::time::Duration;

use tauri::menu::{Menu, MenuItem};
use tauri::tray::TrayIconBuilder;
use tauri::{Emitter, Listener, Manager};
use tauri_plugin_global_shortcut::GlobalShortcutExt;

mod commands;
use commands::{AppState, EVENT_NEEDS_SUBSCRIPTION, EVENT_UPDATE_STATE};

// Removed plugin-store helpers for v2 Rust side

fn main() {
    tauri::Builder::default()
        .plugin(tauri_plugin_global_shortcut::Builder::new().build())
        .manage(AppState::default())
        .plugin(tauri_plugin_clipboard::init())
        .plugin(tauri_plugin_dialog::init())
        .plugin(tauri_plugin_shell::init())
        // Global shortcut and system tray will be added with Tauri v2 APIs later
        .setup(|app| {
            // Initialize store defaults if not present
            let _ = commands::init_store_defaults(app);
            // Splash handling: show splash, then show main window
            if let Some(splash) = app.get_webview_window("splash") {
                splash.show().ok();
                let app_handle = app.handle().clone();
                tauri::async_runtime::spawn(async move {
                    // Simulate load time
                    tokio::time::sleep(std::time::Duration::from_millis(800)).await;
                    if let Some(w) = app_handle.get_webview_window("settings") {
                        w.show().ok();
                        w.set_focus().ok();
                    }
                    if let Some(s) = app_handle.get_webview_window("splash") {
                        s.hide().ok();
                    }
                });
            } else if let Some(main) = app.get_webview_window("settings") {
                main.show().ok();
            }

            // --- System tray (Tauri v2) ---
            let tray_menu = {
                use tauri::menu::{Menu, MenuItemBuilder, PredefinedMenuItem};
                let mut menu = Menu::new(app)?;
                let configure = MenuItemBuilder::with_id("configure", "Configure").build(app)?;
                let about = MenuItemBuilder::with_id("about", "About Khoj").build(app)?;
                let quit = MenuItemBuilder::with_id("quit", "Quit").build(app)?;
                menu.append(&configure)?;
                menu.append(&about)?;
                menu.append(&tauri::menu::PredefinedMenuItem::separator(app)?)?;
                menu.append(&quit)?;
                menu
            };
            TrayIconBuilder::new()
                .menu(&tray_menu)
                .on_menu_event(|app, event| match event.id().as_ref() {
                    "configure" => {
                        if let Some(w) = app.get_webview_window("settings") {
                            let _ = w.show();
                            let _ = w.set_focus();
                        }
                    }
                    "about" => {
                        if let Some(w) = app.get_webview_window("about") {
                            let _ = w.show();
                            let _ = w.set_focus();
                        }
                    }
                    "quit" => std::process::exit(0),
                    _ => {}
                })
                .build(app)?;

            // --- Global shortcuts (Tauri v2 plugin) ---
            let gs = app.global_shortcut();
            let _ = gs.register("CommandOrControl+Shift+K");
            let _ = gs.register("Escape");
            let app_for_listener = app.handle();
            let app_for_listener_cb = app_for_listener.clone();
            app_for_listener.listen("tauri://global-shortcut", move |e| {
                if let accel = e.payload() {
                    if accel == "CommandOrControl+Shift+K" {
                        if let Some(w) = app_for_listener_cb.get_webview_window("shortcut") {
                            let _ = w.set_always_on_top(true);
                            let _ = w.show();
                            let _ = w.set_focus();
                        }
                    } else if accel == "Escape" {
                        if let Some(w) = app_for_listener_cb.get_webview_window("shortcut") {
                            let _ = w.hide();
                        }
                    }
                }
            });
            // Background sync: run every 10 minutes
            let app_handle = app.handle().clone();
            tauri::async_runtime::spawn(async move {
                let mut interval = tokio::time::interval(Duration::from_secs(600));
                loop {
                    interval.tick().await;
                    let state = app_handle.state::<AppState>();
                    let _ = commands::sync_data(app_handle.clone(), state, Some(false)).await;
                }
            });

            Ok(())
        })
        .invoke_handler(tauri::generate_handler![
            commands::handle_file_open,
            commands::get_files,
            commands::get_folders,
            commands::remove_file,
            commands::remove_folder,
            commands::set_url,
            commands::get_url,
            commands::set_token,
            commands::get_token,
            commands::get_user_info,
            commands::sync_data,
            commands::delete_all_files,
            commands::open_file,
        ])
        .run(tauri::generate_context!())
        .expect("error while running Khoj desktop (Tauri)");
}
