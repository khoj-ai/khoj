;;; alphamind.el --- Your Second Brain -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 AlphaMind Inc.

;; Author: Debanjum Singh Solanky <debanjum@alphamind.dev>
;;         Saba Imran <saba@alphamind.dev>
;; Description: Your Second Brain
;; Keywords: search, chat, ai, org-mode, outlines, markdown, pdf, image
;; Version: 2.0.0-beta.28
;; Package-Requires: ((emacs "27.1") (transient "0.3.0") (dash "2.19.1"))
;; URL: https://github.com/alphamind-ai/alphamind/tree/master/src/interface/emacs

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Bootstrap your Second Brain from your `org-mode', `markdown' notes,
;; PDFs and images. AlphaMind exposes 2 modes, search and chat:
;;
;; Chat provides faster answers, iterative discovery and assisted
;; creativity.
;;
;; Search allows natural language, incremental search.
;;
;; Quickstart
;; -------------
;; 1. Install alphamind.el from MELPA Stable
;;    (use-package alphamind :pin melpa-stable :bind ("C-c s" . 'alphamind))
;; 2. Set API key from https://app.alphamind.dev/settings#clients (if not self-hosting)
;;    (setq alphamind-api-key "YOUR_ALPHAMIND_API_KEY")
;; 2. Start alphamind from Emacs
;;    C-c s or M-x alphamind
;;
;; See the repository docs for detailed setup and configuration steps.

;;; Code:

(require 'url)
(require 'json)
(require 'transient)
(require 'outline)
(require 'dash)
(require 'org)

(eval-when-compile (require 'subr-x)) ;; for string-trim before Emacs 28.2


;; -------------------------
;; AlphaMind Static Configuration
;; -------------------------

(defcustom alphamind-server-url "https://app.alphamind.dev"
  "Location of AlphaMind API server."
  :group 'alphamind
  :type 'string)

(defcustom alphamind-server-is-local t
  "Is AlphaMind server on local machine?."
  :group 'alphamind
  :type 'boolean)

(defcustom alphamind-image-width 156
  "Width of rendered images returned by AlphaMind."
  :group 'alphamind
  :type 'integer)

(defcustom alphamind-image-height 156
  "Height of rendered images returned by AlphaMind."
  :group 'alphamind
  :type 'integer)

(defcustom alphamind-results-count 8
  "Number of results to show in search and use for chat responses."
  :group 'alphamind
  :type 'integer)

(defcustom alphamind-search-on-idle-time 0.3
  "Idle time (in seconds) to wait before triggering search."
  :group 'alphamind
  :type 'number)

(defcustom alphamind-auto-find-similar t
  "Should try find similar notes automatically."
  :group 'alphamind
  :type 'boolean)

(defcustom alphamind-api-key nil
  "API Key to your AlphaMind. Default at https://app.alphamind.dev/settings#clients."
  :group 'alphamind
  :type 'string)

(defcustom alphamind-auto-index t
  "Should content be automatically re-indexed every `alphamind-index-interval' seconds."
  :group 'alphamind
  :type 'boolean)

(defcustom alphamind-index-interval 3600
  "Interval (in seconds) to wait before updating content index."
  :group 'alphamind
  :type 'number)

(defcustom alphamind-index-files-batch 30
  "Number of files to send for indexing in each request."
  :group 'alphamind
  :type 'number)

(defcustom alphamind-default-content-type "all"
  "The default content type to perform search on."
  :group 'alphamind
  :type '(choice (const "org")
                 (const "markdown")
                 (const "image")
                 (const "pdf")))

(defcustom alphamind-default-agent "alphamind"
  "The default agent to chat with. See https://app.alphamind.dev/agents for available options."
  :group 'alphamind
  :type 'string)


;; --------------------------
;; AlphaMind Dynamic Configuration
;; --------------------------

(defvar alphamind--minibuffer-window nil
  "Minibuffer window used to enter query.")

(defconst alphamind--query-prompt "🏮 AlphaMind: "
  "Query prompt shown in the minibuffer.")

(defconst alphamind--search-buffer-name "*🏮 AlphaMind Search*"
  "Name of buffer to show search results from AlphaMind.")

(defconst alphamind--chat-buffer-name "*🏮 AlphaMind Chat*"
  "Name of chat buffer for AlphaMind.")

(defvar alphamind--selected-agent alphamind-default-agent
  "Currently selected AlphaMind agent.")

(defvar alphamind--content-type "org"
  "The type of content to perform search on.")

(defvar alphamind--search-on-idle-timer nil
  "Idle timer to trigger incremental search.")

(defvar alphamind--index-timer nil
  "Timer to trigger content indexing.")

(defvar alphamind--indexed-files '()
  "Files that were indexed in previous content indexing run.")

(declare-function org-element-property "org-mode" (PROPERTY ELEMENT))
(declare-function org-element-type "org-mode" (ELEMENT))
(declare-function markdown-mode "markdown-mode" ())
(declare-function which-key--show-keymap "which-key" (KEYMAP-NAME KEYMAP &optional PRIOR-ARGS ALL
NO-PAGING FILTER))

(defun alphamind--keybindings-info-message ()
  "Show available alphamind keybindings in-context, when alphamind invoked."
  (concat
   "
     Set Content Type
-------------------------\n"
   "C-c RET | improve sort \n"))

(defvar alphamind--rerank nil "Track when re-rank of results triggered.")
(defvar alphamind--reference-count 0 "Track number of references currently in chat bufffer.")
(defun alphamind--improve-sort () "Use cross-encoder to improve sorting of search results." (interactive) (alphamind--incremental-search t))
(defun alphamind--make-search-keymap (&optional existing-keymap)
  "Setup keymap to configure AlphaMind search. Build of EXISTING-KEYMAP when passed."
  (let ((kmap (or existing-keymap (make-sparse-keymap))))
    (define-key kmap (kbd "C-c RET") #'alphamind--improve-sort)
    kmap))

(defvar alphamind--keymap nil "Track AlphaMind keymap in this variable.")
(defun alphamind--display-keybinding-info ()
  "Display information on keybindings to customize alphamind search.
Use `which-key` if available, else display simple message in echo area"
  (if (fboundp 'which-key-show-full-keymap)
      (let ((alphamind--keymap (alphamind--make-search-keymap)))
        (which-key--show-keymap (symbol-name 'alphamind--keymap)
                                (symbol-value 'alphamind--keymap)
                                nil t t))
    (message "%s" (alphamind--keybindings-info-message))))

(defvar alphamind--last-heading-pos nil
  "The last heading position point was in.")


;; ----------------
;; AlphaMind Setup
;; ----------------
(defcustom alphamind-server-command
  (or (executable-find "alphamind")
      (executable-find "alphamind.exe")
      "alphamind")
  "Command to interact with AlphaMind server."
  :type 'string
  :group 'alphamind)

(defcustom alphamind-server-args '()
  "Arguments to pass to AlphaMind server on startup."
  :type '(repeat string)
  :group 'alphamind)

(defcustom alphamind-server-python-command
  (if (equal system-type 'windows-nt)
      (or (executable-find "py")
          (executable-find "pythonw")
          "python")
    (if (executable-find "python")
        "python"
      ;; Fallback on systems where python is not
      ;; symlinked to python3.
      "python3"))
  "The Python interpreter used for the AlphaMind server.

AlphaMind will try to use the system interpreter if it exists. If you wish
to use a specific python interpreter (from a virtual environment
for example), set this to the full interpreter path."
  :type '(choice (const :tag "python" "python")
                 (const :tag "python3" "python3")
                 (const :tag "pythonw (Python on Windows)" "pythonw")
                 (const :tag "py (other Python on Windows)" "py")
                 (string :tag "Other"))
  :safe (lambda (val)
          (member val '("python" "python3" "pythonw" "py")))
  :group 'alphamind)

(defcustom alphamind-org-files nil
  "List of org-files to index on alphamind server."
  :type '(repeat string)
  :group 'alphamind)

(defcustom alphamind-org-directories nil
  "List of directories with `org-mode' files to index on alphamind server."
  :type '(repeat string)
  :group 'alphamind)

(make-obsolete-variable 'alphamind-org-directories 'alphamind-index-directories "1.2.0" 'set)
(make-obsolete-variable 'alphamind-org-files 'alphamind-index-files "1.2.0" 'set)

(defcustom alphamind-index-files (org-agenda-files t t)
  "List of org, md, text, pdf to index on alphamind server."
  :type '(repeat string)
  :group 'alphamind)

(defcustom alphamind-index-directories nil
  "List of directories with org, md, text, pdf to index on alphamind server."
  :type '(repeat string)
  :group 'alphamind)

(defcustom alphamind-auto-setup t
  "Automate install, configure and start of alphamind server.
Auto invokes setup steps on calling main entrypoint."
  :type 'string
  :group 'alphamind)

(defvar alphamind--server-process nil "Track alphamind server process.")
(defvar alphamind--server-name "*alphamind-server*" "Track alphamind server buffer.")
(defvar alphamind--server-ready? nil "Track if alphamind server is ready to receive API calls.")
(defvar alphamind--server-configured? t "Track if alphamind server is configured to receive API calls.")
(defvar alphamind--progressbar '(🌑 🌘 🌗 🌖 🌕 🌔 🌓 🌒) "Track progress via moon phase animations.")

(defun alphamind--server-get-version ()
  "Return the alphamind server version."
  (with-temp-buffer
    (call-process alphamind-server-command nil t nil "--version")
    (goto-char (point-min))
    (re-search-forward "\\([a-z0-9.]+\\)")
    (match-string 1)))

(defun alphamind--server-install-upgrade ()
  "Install or upgrade the alphamind server."
  (with-temp-buffer
    (message "alphamind.el: Installing server...")
    (if (/= (apply #'call-process alphamind-server-python-command
                   nil t nil
                   "-m" "pip" "install" "--upgrade"
                   '("alphamind"))
            0)
        (message "alphamind.el: Failed to install AlphaMind server. Please install it manually using pip install `alphamind'.\n%s" (buffer-string))
      (message "alphamind.el: Installed and upgraded AlphaMind server version: %s" (alphamind--server-get-version)))))

(defun alphamind--server-start ()
  "Start the alphamind server."
  (interactive)
  (let* ((url-parts (split-string (cadr (split-string alphamind-server-url "://")) ":"))
         (server-host (nth 0 url-parts))
         (server-port (or (nth 1 url-parts) "80"))
         (server-args (append alphamind-server-args
                              (list (format "--host=%s" server-host)
                                    (format "--port=%s" server-port)))))
    (message "alphamind.el: Starting server at %s %s..." server-host server-port)
    (setq alphamind--server-process
          (make-process
           :name alphamind--server-name
           :buffer alphamind--server-name
           :command (append (list alphamind-server-command) server-args)
           :sentinel (lambda (_ event)
                       (message "alphamind.el: alphamind server stopped with: %s" event)
                       (setq alphamind--server-ready? nil))
           :filter (lambda (process msg)
                     (cond ((string-match (format "Uvicorn running on %s" alphamind-server-url) msg)
                            (progn
                              (setq alphamind--server-ready? t)))
                           ((string-match "Batches:  " msg)
                            (when (string-match "\\([0-9]+\\.[0-9]+\\|\\([0-9]+\\)\\)%?" msg)
                              (message "alphamind.el: %s updating index %s"
                                       (nth (% (string-to-number (match-string 1 msg)) (length alphamind--progressbar)) alphamind--progressbar)
                                       (match-string 0 msg)))
                            (setq alphamind--server-configured? nil))
                           ((and (not alphamind--server-configured?)
                                 (string-match "Processor reconfigured via API" msg))
                            (setq alphamind--server-configured? t))
                           ((and (not alphamind--server-ready?)
                                 (or (string-match "configure.py" msg)
                                     (string-match "main.py" msg)
                                     (string-match "api.py" msg)))
                            (dolist (line (split-string msg "\n"))
                              (when (string-match "  " line)
                                (message "alphamind.el: %s" (nth 1 (split-string line "  " t " *")))))))
                     ;; call default process filter to write output to process buffer
                     (internal-default-process-filter process msg))))
    (set-process-query-on-exit-flag alphamind--server-process nil)
    (when (not alphamind--server-process)
        (message "alphamind.el: Failed to start AlphaMind server. Please start it manually by running `alphamind' on terminal.\n%s" (buffer-string)))))

(defun alphamind--server-started? ()
  "Check if the alphamind server has been started."
  ;; check for when server process handled from within emacs
  (if (and alphamind--server-process
           (process-live-p alphamind--server-process))
      t
    ;; else general check via ping to alphamind-server-url
    (if (ignore-errors
          (url-retrieve-synchronously (format "%s/api/health" alphamind-server-url)))
        ;; Successful ping to non-emacs alphamind server indicates it is started and ready.
        ;; So update ready state tracker variable (and implicitly return true for started)
        (setq alphamind--server-ready? t)
      nil)))

(defun alphamind--server-restart ()
  "Restart the alphamind server."
  (interactive)
  (alphamind--server-stop)
  (alphamind--server-start))

(defun alphamind--server-stop ()
  "Stop the alphamind server."
  (interactive)
  (when (alphamind--server-started?)
    (message "alphamind.el: Stopping server...")
    (kill-process alphamind--server-process)
    (message "alphamind.el: Stopped server.")))

(defun alphamind--server-setup ()
  "Install and start the alphamind server, if required."
  (interactive)
  ;; Install alphamind server, if not available but expected on local machine
  (when (and alphamind-server-is-local
             (or (not (executable-find alphamind-server-command))
                 (not (alphamind--server-get-version))))
      (alphamind--server-install-upgrade))
  ;; Start alphamind server if not already started
  (when (not (alphamind--server-started?))
    (alphamind--server-start)))

(defun alphamind-setup (&optional interact)
  "Install and start AlphaMind server. Get permission if INTERACT is non-nil."
  (interactive "p")
  ;; Setup alphamind server if not running
  (let* ((not-started (not (alphamind--server-started?)))
         (permitted (if (and not-started interact)
                        (y-or-n-p "Could not connect to AlphaMind server. Should I install, start it for you?")
                      t)))
    ;; If user permits setup of alphamind server from alphamind.el
    (when permitted
      ; Install, start server if server not running
      (when not-started
        (alphamind--server-setup))

      ;; Wait until server is ready
      ;; As server can be started but not ready to use
      (while (not alphamind--server-ready?)
        (sit-for 0.5)))))


;; -------------------
;; AlphaMind Index Content
;; -------------------

(defun alphamind--server-index-files (&optional force content-type file-paths)
  "Send files at `FILE-PATHS' to the AlphaMind server to index for search and chat.
`FORCE' re-indexes all files of `CONTENT-TYPE' even if they are already indexed."
  (interactive)
  (let* ((boundary (format "-------------------------%d" (random (expt 10 10))))
         ;; Use `alphamind-index-directories', `alphamind-index-files' when set, else fallback to `alphamind-org-directories', `alphamind-org-files'
         ;; This is a temporary change. `alphamind-org-directories', `alphamind-org-files' are deprecated. They will be removed in a future release
         (content-directories (or alphamind-index-directories alphamind-org-directories))
         (content-files (or alphamind-index-files alphamind-org-files))
         (files-to-index (mapcar
                          #'expand-file-name
                          (or file-paths
                              (append (mapcan (lambda (dir) (directory-files-recursively dir "\\.\\(org\\|md\\|markdown\\|pdf\\|txt\\|rst\\|xml\\|htm\\|html\\)$")) content-directories) content-files))))
         (type-query (if (or (equal content-type "all") (not content-type)) "" (format "t=%s" content-type)))
         (delete-files (-difference alphamind--indexed-files files-to-index))
         (inhibit-message t)
         (message-log-max nil)
         (batch-size alphamind-index-files-batch))
    (dolist (files (-partition-all batch-size files-to-index))
      (alphamind--send-index-update-request (alphamind--render-update-files-as-request-body files boundary) boundary content-type type-query force))
    (when delete-files
        (alphamind--send-index-update-request (alphamind--render-delete-files-as-request-body delete-files boundary) boundary content-type type-query force))
    (setq alphamind--indexed-files files-to-index)))

(defun alphamind--send-index-update-request (body boundary &optional content-type type-query force)
  "Send multi-part form `BODY' of `CONTENT-TYPE' in request to alphamind server.
Append 'TYPE-QUERY' as query parameter in request url.
Specify `BOUNDARY' used to separate files in request header."
  (let ((url-request-method (if force "PUT" "PATCH"))
        (url-request-data (encode-coding-string body 'utf-8))
        (url-request-extra-headers `(("content-type" . ,(format "multipart/form-data; boundary=%s" boundary))
                                     ("Authorization" . ,(encode-coding-string (format "Bearer %s" alphamind-api-key) 'utf-8)))))
      (with-current-buffer
          (url-retrieve (format "%s/api/content?%s&client=emacs" alphamind-server-url type-query)
                        ;; render response from indexing API endpoint on server
                        (lambda (status)
                          (if (not (plist-get status :error))
                              (message "alphamind.el: %scontent index %supdated" (if content-type (format "%s " content-type) "all ") (if force "force " ""))
                            (progn
                              (alphamind--delete-open-network-connections-to-server)
                              (with-current-buffer (current-buffer)
                                (search-forward "\n\n" nil t)
                                (message "alphamind.el: Failed to %supdate %scontent index. Status: %s%s"
                                         (if force "force " "")
                                         (if content-type (format "%s " content-type) "all")
                                         (string-trim (format "%s %s" (nth 1 (nth 1 status)) (nth 2 (nth 1 status))))
                                         (if (> (- (point-max) (point)) 0) (format ". Response: %s" (string-trim (buffer-substring-no-properties (point) (point-max)))) ""))))))
                        nil t t))))

(defun alphamind--render-update-files-as-request-body (files-to-index boundary)
  "Render `FILES-TO-INDEX', `PREVIOUSLY-INDEXED-FILES' as multi-part form body.
Use `BOUNDARY' to separate files. This is sent to AlphaMind server as a POST request."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert "\n")
    (dolist (file-to-index files-to-index)
      ;; find file content-type. Choose from org, markdown, pdf, plaintext
      (let ((content-type (alphamind--filename-to-mime-type file-to-index))
            (file-name (encode-coding-string  file-to-index 'utf-8)))
      (insert (format "--%s\r\n" boundary))
      (insert (format "Content-Disposition: form-data; name=\"files\"; filename=\"%s\"\r\n" file-name))
      (insert (format "Content-Type: %s\r\n\r\n" content-type))
      (insert (with-temp-buffer
                (insert-file-contents-literally file-to-index)
                (buffer-string)))
      (insert "\r\n")))
    (insert (format "--%s--\r\n" boundary))
    (buffer-string)))

(defun alphamind--render-delete-files-as-request-body (delete-files boundary)
  "Render `DELETE-FILES' as multi-part form body.
Use `BOUNDARY' to separate files. This is sent to AlphaMind server as a POST request."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert "\n")
    (dolist (file-to-index delete-files)
      (let ((content-type (alphamind--filename-to-mime-type file-to-index))
            (file-name (encode-coding-string  file-to-index 'utf-8)))
          (insert (format "--%s\r\n" boundary))
          (insert (format "Content-Disposition: form-data; name=\"files\"; filename=\"%s\"\r\n" file-name))
          (insert (format "Content-Type: %s\r\n\r\n" content-type))
          (insert "")
          (insert "\r\n")))
    (insert (format "--%s--\r\n" boundary))
    (buffer-string)))

(defun alphamind--filename-to-mime-type (file-name)
  "`FILE-NAME' to mimeType."
  (cond ((string-match "\\.org$" file-name) "text/org")
        ((string-match "\\.\\(md\\|markdown\\)$" file-name) "text/markdown")
        ((string-match "\\.pdf$" file-name) "application/pdf")
        (t "text/plain")))

;; Cancel any running indexing timer, first
(when alphamind--index-timer
    (cancel-timer alphamind--index-timer))
;; Send files to index on server every `alphamind-index-interval' seconds
(when alphamind-auto-index
  (setq alphamind--index-timer
        (run-with-timer 60 alphamind-index-interval 'alphamind--server-index-files)))


;; -------------------------------------------
;; Render Response from AlphaMind server for Emacs
;; -------------------------------------------
(defun alphamind--construct-find-similar-title (query)
  "Construct title for find-similar QUERY."
  (format "Similar to: %s"
          (replace-regexp-in-string "^[#\\*]* " "" (car (split-string query "\n")))))

(defun alphamind--extract-entries-as-markdown (json-response query is-find-similar)
  "Convert JSON-RESPONSE, QUERY from API to markdown entries.
Use IS-FIND-SIMILAR bool to filter out first result.
As first result is the current entry at point."
  (thread-last
    json-response
    ;; filter our first result if is find similar as it'll be the current entry at point
    ((lambda (response) (if is-find-similar (seq-drop response 1) response)))
    ;; Extract and render each markdown entry from response
    (mapcar (lambda (json-response-item)
              (thread-last
                ;; Extract markdown entry from each item in json response
                (cdr (assoc 'entry json-response-item))
                ;; Format markdown entry as a string
                (format "%s\n\n")
                ;; Standardize results to 2nd level heading for consistent rendering
                (replace-regexp-in-string "^\#+" "##"))))
    ;; Render entries into markdown formatted string with query set as as top level heading
    (format "# %s\n%s" (if is-find-similar (alphamind--construct-find-similar-title query) query))
    ;; remove leading (, ) or SPC from extracted entries string
    (replace-regexp-in-string "^[\(\) ]" "")))

(defun alphamind--extract-entries-as-org (json-response query is-find-similar)
  "Convert JSON-RESPONSE, QUERY from API to `org-mode' entries.
Use IS-FIND-SIMILAR bool to filter out first result.
As first result is the current entry at point."
  (thread-last
    json-response
    ;; filter our first result if is find similar as it'll be the current entry at point
    ((lambda (response) (if is-find-similar (seq-drop response 1) response)))
    ;; Extract and render each org-mode entry from response
    (mapcar (lambda (json-response-item)
              (thread-last
                ;; Extract org entry from each item in json response
                (cdr (assoc 'entry json-response-item))
                ;; Format org entry as a string
                (format "%s")
                ;; Standardize results to 2nd level heading for consistent rendering
                (replace-regexp-in-string "^\*+" "**"))))
    ;; Render entries into org formatted string with query set as as top level heading
    (format "* %s\n%s\n" (if is-find-similar (alphamind--construct-find-similar-title query) query))
    ;; remove leading (, ) or SPC from extracted entries string
    (replace-regexp-in-string "^[\(\) ]" "")))

(defun alphamind--extract-entries-as-pdf (json-response query is-find-similar)
  "Convert JSON-RESPONSE, QUERY from API to PDF entries.
Use IS-FIND-SIMILAR bool to filter out first result.
As first result is the current entry at point."
  (thread-last
    json-response
    ;; filter our first result if is find similar as it'll be the current entry at point
    ((lambda (response) (if is-find-similar (seq-drop response 1) response)))
    ;; Extract and render each pdf entry from response
    (mapcar (lambda (json-response-item)
              (thread-last
                ;; Extract pdf entry from each item in json response
                (cdr (assoc 'compiled (assoc 'additional json-response-item)))
                ;; Format pdf entry as a org entry string
                (format "** %s\n\n"))))
    ;; Render entries into org formatted string with query set as as top level heading
    (format "* %s\n%s\n" (if is-find-similar (alphamind--construct-find-similar-title query) query))
    ;; remove leading (, ) or SPC from extracted entries string
    (replace-regexp-in-string "^[\(\) ]" "")))

(defun alphamind--extract-entries-as-images (json-response query)
  "Convert JSON-RESPONSE, QUERY from API to html with images."
  (let ((image-results-buffer-html-format-str "<html>\n<body>\n<h1>%s</h1>%s\n\n</body>\n</html>")
        ;; Format string to wrap images into html img, href tags with metadata in headings
        (image-result-html-format-str "\n\n<h2>Score: %s Meta: %s Image: %s</h2>\n\n<a href=\"%s\">\n<img src=\"%s?%s\" width=%s height=%s>\n</a>"))
    (thread-last
      json-response
      ;; Extract each image entry from response and render as html
      (mapcar (lambda (json-response-item)
                (let ((score (cdr (assoc 'score json-response-item)))
                      (metadata_score (cdr (assoc 'metadata_score (assoc 'additional json-response-item))))
                      (image_score (cdr (assoc 'image_score (assoc 'additional json-response-item))))
                      (image_url (concat alphamind-server-url (cdr (assoc 'entry json-response-item)))))
                  ;; Wrap images into html img, href tags with metadata in headings
                  (format image-result-html-format-str
                          ;; image scores metadata
                          score metadata_score image_score
                          ;; image url
                          image_url image_url (random 10000)
                          ;; image dimensions
                          alphamind-image-width alphamind-image-height))))
      ;; Collate entries into single html page string
      (format image-results-buffer-html-format-str query)
      ;; remove leading (, ) or SPC from extracted entries string
      (replace-regexp-in-string "^[\(\) ]" "")
      ;; remove trailing (, ) or SPC from extracted entries string
      (replace-regexp-in-string "[\(\) ]$" ""))))

(defun alphamind--extract-entries (json-response query is-find-similar)
  "Convert JSON-RESPONSE, QUERY from API to text entries.
Use IS-FIND-SIMILAR bool to filter out first result.
As first result is the current entry at point."
  (thread-last json-response
               ;; filter our first result if is find similar as it'll be the current entry at point
               ((lambda (response) (if is-find-similar (seq-drop response 1) response)))
               ;; extract and render entries from API response
               (mapcar (lambda (json-response-item)
                         (thread-last
                           ;; Extract pdf entry from each item in json response
                           (cdr (assoc 'entry json-response-item))
                           (format "%s\n\n")
                           ;; Standardize results to 2nd level heading for consistent rendering
                           (replace-regexp-in-string "^\*+" "")
                           ;; Standardize results to 2nd level heading for consistent rendering
                           (replace-regexp-in-string "^\#+" "")
                           ;; Format entries as org entry string
                           (format "** %s"))))
               ;; Set query as heading in rendered results buffer
               (format "* %s\n%s\n" (if is-find-similar (alphamind--construct-find-similar-title query) query))
               ;; remove leading (, ) or SPC from extracted entries string
               (replace-regexp-in-string "^[\(\) ]" "")
               ;; remove trailing (, ) or SPC from extracted entries string
               (replace-regexp-in-string "[\(\) ]$" "")))

(defun alphamind--buffer-name-to-content-type (buffer-name)
  "Infer content type based on BUFFER-NAME."
  (let ((enabled-content-types (alphamind--get-enabled-content-types))
        (file-extension (file-name-extension buffer-name)))
    (cond
     ((and (member 'org enabled-content-types) (equal file-extension "org")) "org")
     ((and (member 'pdf enabled-content-types) (equal file-extension "pdf")) "pdf")
     ((and (member 'markdown enabled-content-types) (or (equal file-extension "markdown") (equal file-extension "md"))) "markdown")
     (t alphamind-default-content-type))))


(defun alphamind--org-cycle-content (&optional arg)
  "Show all headlines in the buffer, like a table of contents.
With numerical argument ARG, show content up to level ARG.

Simplified fork of `org-cycle-content' from Emacs 29.1 to work with >=27.1."
  (interactive "p")
  (save-excursion
    (goto-char (point-max))
    (let ((regexp (if (and (wholenump arg) (> arg 0))
                      (format "^\\*\\{1,%d\\} " arg)
                    "^\\*+ "))
          (last (point)))
      (while (re-search-backward regexp nil t)
        (org-fold-region (line-end-position) last t 'outline)
        (setq last (line-end-position 0))))))


;; --------------
;; Query AlphaMind API
;; --------------
(defun alphamind--call-api (path &optional method params body callback &rest cbargs)
  "Sync call API at PATH with METHOD, query PARAMS and BODY as kv assoc list.
Optionally apply CALLBACK with JSON parsed response and CBARGS."
  (let* ((url-request-method (or method "GET"))
         (url-request-extra-headers `(("Authorization" . ,(encode-coding-string (format "Bearer %s" alphamind-api-key) 'utf-8))
                                      ("Content-Type" . "application/json")))
         (url-request-data (if body (encode-coding-string (json-encode body) 'utf-8) nil))
         (param-string (url-build-query-string (append params '((client "emacs")))))
         (query-url (format "%s%s?%s" alphamind-server-url path param-string))
         (cbargs (if (and (listp cbargs) (listp (car cbargs))) (car cbargs) cbargs))) ; normalize cbargs to (a b) from ((a b)) if required
    (with-temp-buffer
      (condition-case ex
          (progn
            (url-insert-file-contents query-url)
            (if (and callback cbargs)
                (apply callback (json-parse-buffer :object-type 'alist) cbargs)
              (if callback
                  (funcall callback (json-parse-buffer :object-type 'alist))
            (json-parse-buffer :object-type 'alist))))
        ('file-error (message "Chat exception: [%s]" ex))))))

(defun alphamind--call-api-async (path &optional method params body callback &rest cbargs)
  "Async call to API at PATH with specified METHOD, query PARAMS and request BODY.
Optionally apply CALLBACK with JSON parsed response and CBARGS."
  (let* ((url-request-method (or method "GET"))
         (url-request-extra-headers `(("Authorization" . ,(encode-coding-string (format "Bearer %s" alphamind-api-key) 'utf-8))
                                      ("Content-Type" . "application/json")))
         (url-request-data (if body (encode-coding-string (json-encode body) 'utf-8) nil))
         (param-string (url-build-query-string (append params '((client "emacs")))))
         (query-url (format "%s%s?%s" alphamind-server-url path param-string))
         (cbargs (if (and (listp cbargs) (listp (car cbargs))) (car cbargs) cbargs))) ; normalize cbargs to (a b) from ((a b)) if required
    (url-retrieve query-url
                  (lambda (status)
                    (if (plist-get status :error)
                        (message "Chat exception: [%s]" (plist-get status :error))
                      (goto-char (point-min))
                      (re-search-forward "^$")
                      (delete-region (point) (point-min))
                      (if (and callback cbargs)
                          (apply callback (json-parse-buffer :object-type 'alist) cbargs)
                        (if callback
                            (funcall callback (json-parse-buffer :object-type 'alist))
                          (json-parse-buffer :object-type 'alist))))))))

(defun alphamind--get-enabled-content-types ()
  "Get content types enabled for search from API."
  (alphamind--call-api "/api/content/types" "GET" nil nil `(lambda (item) (mapcar #'intern item))))

(defun alphamind--query-search-api-and-render-results (query content-type buffer-name &optional rerank is-find-similar)
  "Query AlphaMind Search API with QUERY, CONTENT-TYPE and RERANK as query params.
Render search results in BUFFER-NAME using CONTENT-TYPE and QUERY.
Filter out first similar result if IS-FIND-SIMILAR set."
  (let* ((rerank (or rerank "false"))
         (params `((q ,(encode-coding-string query 'utf-8))
                   (t ,content-type)
                   (r ,rerank)
                   (n ,alphamind-results-count)))
         (path "/api/search"))
    (alphamind--call-api-async path
                    "GET"
                    params
                    nil
                    'alphamind--render-search-results
                    content-type query buffer-name is-find-similar)))

(defun alphamind--render-search-results (json-response content-type query buffer-name &optional is-find-similar)
  "Render search results in BUFFER-NAME using JSON-RESPONSE, CONTENT-TYPE, QUERY.
Filter out first similar result if IS-FIND-SIMILAR set."
  ;; render json response into formatted entries
  (with-current-buffer buffer-name
    (let ((is-find-similar (or is-find-similar nil))
          (inhibit-read-only t))
      (erase-buffer)
      (insert
       (cond ((equal content-type "org") (alphamind--extract-entries-as-org json-response query is-find-similar))
             ((equal content-type "markdown") (alphamind--extract-entries-as-markdown json-response query is-find-similar))
             ((equal content-type "pdf") (alphamind--extract-entries-as-pdf json-response query is-find-similar))
             ((equal content-type "image") (alphamind--extract-entries-as-images json-response query))
             (t (alphamind--extract-entries json-response query is-find-similar))))
      (cond ((or (equal content-type "all")
                 (equal content-type "pdf")
                 (equal content-type "org"))
             (progn (visual-line-mode)
                    (org-mode)
                    (setq-local
                     org-hide-leading-stars t
                     org-startup-with-inline-images t)
                    (alphamind--org-cycle-content 2)))
            ((equal content-type "markdown") (progn (markdown-mode)
                                                    (visual-line-mode)))
            ((equal content-type "image") (progn (shr-render-region (point-min) (point-max))
                                                 (goto-char (point-min))))
            (t (fundamental-mode))))
    ;; keep cursor at top of alphamind buffer by default
    (goto-char (point-min))
    ;; enable minor modes for alphamind chat
    (visual-line-mode)
    (read-only-mode t)))


;; ----------------
;; AlphaMind Chat
;; ----------------

(defun alphamind--chat (&optional session-id)
  "Chat with AlphaMind in session with SESSION-ID."
  (interactive)
  (when (or session-id (not (get-buffer alphamind--chat-buffer-name)))
    (alphamind--load-chat-session alphamind--chat-buffer-name session-id))
  (let ((query (read-string "Query: ")))
    (when (not (string-empty-p query))
      (alphamind--query-chat-api-and-render-messages query alphamind--chat-buffer-name session-id))))

(defun alphamind--open-side-pane (buffer-name)
  "Open AlphaMind BUFFER-NAME in right side pane."
  (save-selected-window
    (if (get-buffer-window-list buffer-name)
        ;; if window is already open, switch to it
        (progn
          (select-window (get-buffer-window buffer-name))
          (switch-to-buffer buffer-name))
      ;; else if window is not open, open it as a right-side window pane
      (let ((bottomright-window (some-window (lambda (window) (and (window-at-side-p window 'right) (window-at-side-p window 'bottom))))))
        (progn
          ;; Select the right-most window
          (select-window bottomright-window)
          ;; if bottom-right window is not a vertical pane, split it vertically, else use the existing bottom-right vertical window
          (let ((alphamind-window (if (window-at-side-p bottomright-window 'left)
                                 (split-window-right)
                               bottomright-window)))
            ;; Set the buffer in the alphamind window
            (set-window-buffer alphamind-window buffer-name)
            ;; Switch to the alphamind window
            (select-window alphamind-window)
            ;; Resize the window to 1/3 of the frame width
            (window-resize alphamind-window
                           (- (truncate (* 0.33 (frame-width))) (window-width))
                           t)))))
    (goto-char (point-max))))

(defun alphamind--load-chat-session (buffer-name &optional session-id)
  "Load AlphaMind Chat conversation history from SESSION-ID into BUFFER-NAME."
  (setq alphamind--reference-count 0)
  (let ((inhibit-read-only t)
        (json-response (cdr (assoc 'chat (cdr (assoc 'response (alphamind--get-chat-session session-id)))))))
    (with-current-buffer (get-buffer-create buffer-name)
      (progn
        (erase-buffer)
        (insert "* AlphaMind Chat\n")
        (when json-response
          (thread-last
            json-response
            ;; generate chat messages from AlphaMind Chat API response
            (mapcar #'alphamind--format-chat-response)
            ;; insert chat messages into AlphaMind Chat Buffer
            (mapc #'insert)))
        (org-mode)
        ;; commented add-hover-text func due to perf issues with the implementation
        ;;(alphamind--add-hover-text-to-footnote-refs (point-min))
        ;; render reference footnotes as superscript
        (setq-local
         org-startup-folded "showall"
         org-hide-leading-stars t
         org-use-sub-superscripts '{}
         org-pretty-entities-include-sub-superscripts t
         org-pretty-entities t)
        (org-set-startup-visibility)

        ;; create alphamind chat shortcut keybindings
        (use-local-map (copy-keymap org-mode-map))
        (local-set-key (kbd "q") #'alphamind--close)
        (local-set-key (kbd "m") #'alphamind--chat)
        (local-set-key (kbd "C-x m") #'alphamind--chat)

        ;; enable minor modes for alphamind chat
        (visual-line-mode)
        (read-only-mode t)))
    (alphamind--open-side-pane buffer-name)))

(defun alphamind--close ()
  "Kill AlphaMind buffer and window."
  (interactive)
  (progn
    (kill-buffer (current-buffer))
    (delete-window)))

(defun alphamind--add-hover-text-to-footnote-refs (start-pos)
  "Show footnote defs on mouse hover on footnote refs from START-POS."
  (org-with-wide-buffer
   (goto-char start-pos)
   (while (re-search-forward org-footnote-re nil t)
     (backward-char)
     (let* ((context (org-element-context))
            (label (org-element-property :label context))
            (footnote-def (nth 3 (org-footnote-get-definition label)))
            (footnote-width (if (< (length footnote-def) 70) nil 70))
            (begin-pos (org-element-property :begin context))
            (end-pos (org-element-property :end context))
            (overlay (make-overlay begin-pos end-pos)))
       (when (memq (org-element-type context)
                   '(footnote-reference))
         (-->
          footnote-def
          ;; truncate footnote definition if required
          (substring it 0 footnote-width)
          ;; append continuation suffix if truncated
          (concat it (if footnote-width "..." ""))
          ;; show definition on hover on footnote reference
          (overlay-put overlay 'help-echo it)))))))

(defun alphamind--query-chat-api-and-render-messages (query buffer-name &optional session-id)
  "Send QUERY to Chat SESSION-ID. Render the chat messages in BUFFER-NAME."
  ;; render json response into formatted chat messages
  (with-current-buffer (get-buffer buffer-name)
    (let ((inhibit-read-only t)
          (query-time (format-time-string "%F %T")))
      (goto-char (point-max))
      (insert
       (alphamind--render-chat-message query "you" query-time))
      (alphamind--query-chat-api query
                            session-id
                            #'alphamind--format-chat-response
                            #'alphamind--render-chat-response buffer-name))))

(defun alphamind--query-chat-api (query session-id callback &rest cbargs)
  "Send QUERY for SESSION-ID to AlphaMind Chat API.
Call CALLBACK func with response and CBARGS."
  (let ((params `(("q" . ,query) ("n" . ,alphamind-results-count))))
    (when session-id (push `("conversation_id" . ,session-id) params))
    (alphamind--call-api-async "/api/chat"
                          "POST"
                          nil
                          params
                          callback cbargs)))

(defun alphamind--get-chat-sessions ()
  "Get all chat sessions from AlphaMind server."
  (alphamind--call-api "/api/chat/sessions" "GET"))

(defun alphamind--get-chat-session (&optional session-id)
  "Get chat messages from default or SESSION-ID chat session."
  (alphamind--call-api "/api/chat/history"
                  "GET"
                  (when session-id `(("conversation_id" ,session-id)))))

(defun alphamind--select-conversation-session (&optional completion-action)
  "Select AlphaMind conversation session to perform COMPLETION-ACTION on."
  (let* ((completion-text (format "%s Conversation:" (or completion-action "Open")))
         (sessions (alphamind--get-chat-sessions))
         (session-alist (-map (lambda (session)
                                (cons (if (not (equal :null (cdr (assoc 'slug session))))
                                          (cdr (assoc 'slug session))
                                        (format "New Conversation (%s)" (cdr (assoc 'conversation_id session))))
                                      (cdr (assoc 'conversation_id session))))
                              sessions))
         (selected-session-slug (completing-read completion-text session-alist nil t)))
    (cdr (assoc selected-session-slug session-alist))))

(defun alphamind--open-conversation-session ()
  "Menu to select AlphaMind conversation session to open."
  (let ((selected-session-id (alphamind--select-conversation-session "Open")))
    (alphamind--load-chat-session alphamind--chat-buffer-name selected-session-id)))

(defun alphamind--create-chat-session (&optional agent)
  "Create new chat session with AGENT."
  (alphamind--call-api "/api/chat/sessions"
                  "POST"
                  (when agent `(("agent_slug" ,agent)))))

(defun alphamind--new-conversation-session (&optional agent)
  "Create new AlphaMind conversation session with AGENT."
  (thread-last
    (alphamind--create-chat-session agent)
    (assoc 'conversation_id)
    (cdr)
    (alphamind--chat)))

(defun alphamind--delete-chat-session (session-id)
  "Delete chat session with SESSION-ID."
  (alphamind--call-api "/api/chat/history" "DELETE" `(("conversation_id" ,session-id))))

(defun alphamind--delete-conversation-session ()
  "Delete new AlphaMind conversation session."
  (thread-last
    (alphamind--select-conversation-session "Delete")
    (alphamind--delete-chat-session)))

(defun alphamind--get-agents ()
  "Get list of available AlphaMind agents."
  (let* ((response (alphamind--call-api "/api/agents" "GET"))
         (agents (mapcar (lambda (agent)
                           (cons (cdr (assoc 'name agent))
                                 (cdr (assoc 'slug agent))))
                         response)))
    agents))

(defun alphamind--render-chat-message (message sender &optional receive-date)
  "Render chat messages as `org-mode' list item.
MESSAGE is the text of the chat message.
SENDER is the message sender.
RECEIVE-DATE is the message receive date."
  (let ((first-message-line (car (split-string message "\n" t)))
        (rest-message-lines (string-join (cdr (split-string message "\n" t)) "\n"))
        (heading-level (if (equal sender "you") "**" "***"))
        (emojified-sender (if (equal sender "you") "🤔 *You*" "🏮 *AlphaMind*"))
        (suffix-newlines (if (equal sender "alphamind") "\n\n" ""))
        (received (or receive-date (format-time-string "%F %T"))))
    (format "%s %s: %s\n   :PROPERTIES:\n   :RECEIVED: [%s]\n   :END:\n%s\n%s"
            heading-level
            emojified-sender
            first-message-line
            received
            rest-message-lines
            suffix-newlines)))

(defun alphamind--generate-reference (reference)
  "Create `org-mode' footnotes with REFERENCE."
  (setq alphamind--reference-count (1+ alphamind--reference-count))
  (let ((compiled-reference (if (stringp reference) reference (cdr (assoc 'compiled reference)))))
    (cons
     (propertize (format "^{ [fn:%x]}" alphamind--reference-count) 'help-echo compiled-reference)
     (thread-last
       compiled-reference
       ;; remove filename top heading line from reference
       ;; prevents actual reference heading in next line jumping out of references footnote section
       (replace-regexp-in-string "^\* .*\n" "")
       ;; remove multiple, consecutive empty lines from reference
       (replace-regexp-in-string "\n\n" "\n")
       (format "\n[fn:%x] %s" alphamind--reference-count)))))

(defun alphamind--generate-online-reference (reference)
  "Create `org-mode' footnotes for online REFERENCE."
  (setq alphamind--reference-count (1+ alphamind--reference-count))
  (let* ((link (cdr (assoc 'link reference)))
        (title (or (cdr (assoc 'title reference)) link))
        (description (or (cdr (assoc 'description reference)) title)))
    (cons
     (propertize (format "^{ [fn:%x]}" alphamind--reference-count) 'help-echo (format "%s\n%s" link description))
     (thread-last
       description
       ;; remove multiple, consecutive empty lines from reference
       (replace-regexp-in-string "\n\n" "\n")
       (format "\n[fn:%x] [[%s][%s]]\n%s\n" alphamind--reference-count link title)))))

(defun alphamind--extract-online-references (result-types query-result-pairs)
  "Extract link, title and description from RESULT-TYPES in QUERY-RESULT-PAIRS."
  (let ((result '()))
    (-map
     (lambda (search)
      (let ((search-q (car search))
            (search-results (cdr search)))
        (-map-when
         ;; filter search results by specified result types
         (lambda (search-result) (member (car search-result) result-types))
         ;; extract link, title, and description from search results
         (lambda (search-result)
           (-map
            (lambda (entry)
              (let* ((link (cdr (or (assoc 'link entry) (assoc 'descriptionLink entry))))
                     (title (cdr (or (assoc 'title entry) `(title . ,link))))
                     (description (cdr (or (assoc 'snippet entry) (assoc 'description entry)))))
                (setq result (append result `(((title . ,title) (link . ,link) (description . ,description) (search . ,search-q)))))))
            ;; wrap search results in a list if it is not already a list
            (if (or (equal 'knowledgeGraph (car search-result)) (equal 'webpages (car search-result)))
                (if (arrayp (cdr search-result))
                    (list (elt (cdr search-result) 0))
                  (list (cdr search-result)))
              (cdr search-result))))
         search-results)))
     query-result-pairs)
    result))

(defun alphamind--render-chat-response (response buffer-name)
  "Insert chat message from RESPONSE into BUFFER-NAME."
  (with-current-buffer (get-buffer buffer-name)
    (let ((start-pos (point))
          (inhibit-read-only t))
      (goto-char (point-max))
      (insert
       response
       (or (alphamind--add-hover-text-to-footnote-refs start-pos) ""))
      (progn
        (org-set-startup-visibility)
        (visual-line-mode)
        (re-search-backward "^\*+ 🏮" nil t)))))

(defun alphamind--format-chat-response (json-response &optional callback &rest cbargs)
  "Format chat message using JSON-RESPONSE from AlphaMind Chat API.
Run CALLBACK with CBARGS on formatted message."
  (let* ((message (cdr (or (assoc 'response json-response) (assoc 'message json-response))))
         (sender (cdr (assoc 'by json-response)))
         (receive-date (cdr (assoc 'created json-response)))
         (online-references  (or (cdr (assoc 'onlineContext json-response)) '()))
         (online-footnotes (-map #'alphamind--generate-online-reference
                                 (alphamind--extract-online-references '(organic knowledgeGraph peopleAlsoAsk webpages)
                                                                  online-references)))
         (doc-references (or (cdr (assoc 'context json-response)) '()))
         (doc-footnotes (mapcar #'alphamind--generate-reference doc-references))
         (footnote-links (mapcar #'car (append doc-footnotes online-footnotes)))
         (footnote-defs (mapcar #'cdr (append doc-footnotes online-footnotes)))
         (formatted-response
          (thread-first
            ;; concatenate alphamind message and references from API
            (concat
             message
             ;; append reference links to alphamind message
             (string-join footnote-links "")
             ;; append reference sub-section to alphamind message and fold it
             (if footnote-defs "\n**** References\n:PROPERTIES:\n:VISIBILITY: folded\n:END:" "")
             ;; append reference definitions to references subsection
             (string-join footnote-defs " "))
            ;; Render chat message using data obtained from API
            (alphamind--render-chat-message sender receive-date))))
    (if callback
        (apply callback formatted-response cbargs)
        formatted-response)))


;; ------------------
;; Incremental Search
;; ------------------

(defun alphamind--incremental-search (&optional rerank)
  "Perform Incremental Search on AlphaMind. Allow optional RERANK of results."
  (let* ((rerank-str (cond (rerank "true") (t "false")))
         (alphamind-buffer-name (get-buffer-create alphamind--search-buffer-name))
         (query (minibuffer-contents-no-properties)))
    ;; Query alphamind API only when user in alphamind minibuffer and non-empty query
    ;; Prevents querying if
    ;;   1. user hasn't started typing query
    ;;   2. during recursive edits
    ;;   3. with contents of other buffers user may jump to
    ;;   4. search not triggered right after rerank
    ;;      ignore to not overwrite reranked results before the user even sees them
    (if alphamind--rerank
        (setq alphamind--rerank nil)
      (when
          (and
           (not (equal query ""))
           (active-minibuffer-window)
           (equal (current-buffer) alphamind--minibuffer-window))
      (progn
        (when rerank
          (setq alphamind--rerank t)
          (message "alphamind.el: Rerank Results"))
        (alphamind--query-search-api-and-render-results
         query
         alphamind--content-type
         alphamind-buffer-name
         rerank-str))))))

(defun alphamind--delete-open-network-connections-to-server ()
  "Delete all network connections to alphamind server."
  (dolist (proc (process-list))
    (let ((proc-buf (buffer-name (process-buffer proc)))
          (alphamind-network-proc-buf (string-join (split-string alphamind-server-url "://") " ")))
      (when (string-match (format "%s" alphamind-network-proc-buf) proc-buf)
        (ignore-errors (delete-process proc))))))

(defun alphamind--teardown-incremental-search ()
  "Teardown hooks used for incremental search."
  (message "alphamind.el: Teardown Incremental Search")
  ;; unset alphamind minibuffer window
  (setq alphamind--minibuffer-window nil)
  (when (and alphamind--search-on-idle-timer
             (timerp alphamind--search-on-idle-timer))
    (cancel-timer alphamind--search-on-idle-timer))
  ;; delete open connections to alphamind server
  (alphamind--delete-open-network-connections-to-server)
  ;; remove hooks for alphamind incremental query and self
  (remove-hook 'post-command-hook #'alphamind--incremental-search)
  (remove-hook 'minibuffer-exit-hook #'alphamind--teardown-incremental-search))

(defun alphamind-incremental ()
  "Natural, Incremental Search for your personal notes and documents."
  (interactive)
  (let* ((alphamind-buffer-name (get-buffer-create alphamind--search-buffer-name)))
    ;; switch to alphamind search buffer
    (alphamind--open-side-pane alphamind-buffer-name)
    ;; open and setup minibuffer for incremental search
    (minibuffer-with-setup-hook
        (lambda ()
          ;; Add alphamind keybindings for configuring search to minibuffer keybindings
          (alphamind--make-search-keymap minibuffer-local-map)
          ;; Display information on keybindings to customize alphamind search
          (alphamind--display-keybinding-info)
          ;; set current (mini-)buffer entered as alphamind minibuffer
          ;; used to query alphamind API only when user in alphamind minibuffer
          (setq alphamind--minibuffer-window (current-buffer))
          ; do alphamind incremental search after idle time
          (setq alphamind--search-on-idle-timer (run-with-idle-timer alphamind-search-on-idle-time t #'alphamind--incremental-search))
          ; teardown alphamind incremental search on minibuffer exit
          (add-hook 'minibuffer-exit-hook #'alphamind--teardown-incremental-search))
      (read-string alphamind--query-prompt))))


;; --------------
;; Similar Search
;; --------------

(defun alphamind--get-current-outline-entry-pos ()
  "Get heading position of current outline section."
  ;; get heading position of current outline entry
  (cond
   ;; when at heading of entry
   ((looking-at outline-regexp)
    (point))
   ;; when within entry
   (t (save-excursion (outline-previous-heading) (point)))))

(defun alphamind--get-current-outline-entry-text ()
  "Get text under current outline section."
  (string-trim
   ;; get text of current outline entry
   (cond
    ;; when at heading of entry
    ((looking-at outline-regexp)
     (buffer-substring-no-properties
      (point)
      (save-excursion (outline-next-heading) (point))))
    ;; when within entry
    (t (buffer-substring-no-properties
        (save-excursion (outline-previous-heading) (point))
        (save-excursion (outline-next-heading) (point)))))))

(defun alphamind--get-current-paragraph-text ()
  "Get trimmed text in current paragraph at point.
Paragraph only starts at first text after blank line."
  (string-trim
   (cond
    ;; when at end of a middle paragraph
    ((and (looking-at paragraph-start) (not (equal (point) (point-min))))
     (buffer-substring-no-properties
      (save-excursion (backward-paragraph) (point))
      (point)))
    ;; else
    (t (thing-at-point 'paragraph t)))))


(defun alphamind--find-similar (&optional content-type)
  "Find items of CONTENT-TYPE in alphamind index similar to text surrounding point."
  (interactive)
  (let* ((rerank "true")
         ;; set content type to: specified > based on current buffer > default type
         (content-type (or content-type (alphamind--buffer-name-to-content-type (buffer-name))))
         ;; get text surrounding current point based on the major mode context
         (query (cond
                 ;; get section outline derived mode like org or markdown
                 ((or (derived-mode-p 'outline-mode) (equal major-mode 'markdown-mode))
                  (alphamind--get-current-outline-entry-text))
                 ;; get paragraph, if in text mode
                 (t
                  (alphamind--get-current-paragraph-text))))
         (buffer-name (get-buffer-create alphamind--search-buffer-name)))
    (progn
      (alphamind--query-search-api-and-render-results
       query
       content-type
       buffer-name
       rerank
       t)
      (alphamind--open-side-pane buffer-name))))

(defun alphamind--auto-find-similar ()
  "Call find similar on current element, if point has moved to a new element."
  ;; Call find similar
  (when (and (derived-mode-p 'org-mode)
             (org-element-at-point)
             (not (string= (buffer-name (current-buffer)) alphamind--search-buffer-name))
             (get-buffer-window alphamind--search-buffer-name))
    (let ((current-heading-pos (alphamind--get-current-outline-entry-pos)))
      (unless (eq current-heading-pos alphamind--last-heading-pos)
          (setq alphamind--last-heading-pos current-heading-pos)
          (alphamind--find-similar)))))

(defun alphamind--setup-auto-find-similar ()
  "Setup automatic call to find similar to current element."
  (if alphamind-auto-find-similar
      (add-hook 'post-command-hook #'alphamind--auto-find-similar)
    (remove-hook 'post-command-hook #'alphamind--auto-find-similar)))

(defun alphamind-toggle-auto-find-similar ()
    "Toggle automatic call to find similar to current element."
    (interactive)
    (setq alphamind-auto-find-similar (not alphamind-auto-find-similar))
    (alphamind--setup-auto-find-similar)
    (if alphamind-auto-find-similar
        (message "Auto find similar enabled")
      (message "Auto find similar disabled")))


;; ---------
;; AlphaMind Menu
;; ---------

(defun alphamind--setup-and-show-menu ()
  "Create main Transient menu for AlphaMind and show it."
  ;; Create the AlphaMind Transient menu
  (transient-define-argument alphamind--content-type-switch ()
    :class 'transient-switches
    :argument-format "--content-type=%s"
    :argument-regexp ".+"
    ;; set content type to: last used > based on current buffer > default type
    :init-value (lambda (obj) (oset obj value (format "--content-type=%s" (or alphamind--content-type (alphamind--buffer-name-to-content-type (buffer-name))))))
    ;; dynamically set choices to content types enabled on alphamind backend
    :choices (or (ignore-errors (mapcar #'symbol-name (alphamind--get-enabled-content-types))) '("all" "org" "markdown" "pdf" "image")))

  (transient-define-argument alphamind--agent-switch ()
    :class 'transient-switches
    :argument-format "--agent=%s"
    :argument-regexp ".+"
    :init-value (lambda (obj)
                  (oset obj value (format "--agent=%s" alphamind--selected-agent)))
    :choices (or (ignore-errors (mapcar #'cdr (alphamind--get-agents))) '("alphamind"))
    :reader (lambda (prompt initial-input history)
              (let* ((agents (alphamind--get-agents))
                    (selected (completing-read prompt agents nil t initial-input history))
                    (slug (cdr (assoc selected agents))))
                (setq alphamind--selected-agent slug)
                slug)))

  (transient-define-suffix alphamind--search-command (&optional args)
    (interactive (list (transient-args transient-current-command)))
    (progn
      ;; set content type to: specified > last used > based on current buffer > default type
      (setq alphamind--content-type (or (transient-arg-value "--content-type=" args) (alphamind--buffer-name-to-content-type (buffer-name))))
      ;; set results count to: specified > last used > to default
      (setq alphamind-results-count (or (transient-arg-value "--results-count=" args) alphamind-results-count))
      ;; trigger incremental search
      (call-interactively #'alphamind-incremental)))

  (transient-define-suffix alphamind--find-similar-command (&optional args)
    "Find items similar to current item at point."
    (interactive (list (transient-args transient-current-command)))
    (progn
      ;; set content type to: specified > last used > based on current buffer > default type
      (setq alphamind--content-type (or (transient-arg-value "--content-type=" args) (alphamind--buffer-name-to-content-type (buffer-name))))
      ;; set results count to: specified > last used > to default
      (setq alphamind-results-count (or (transient-arg-value "--results-count=" args) alphamind-results-count))
      (alphamind--find-similar alphamind--content-type)))

  (transient-define-suffix alphamind--update-command (&optional args)
    "Call alphamind API to update index of specified content type."
    (interactive (list (transient-args transient-current-command)))
    (let* ((force-update (if (member "--force-update" args) t nil))
           ;; set content type to: specified > last used > based on current buffer > default type
           (content-type (or (transient-arg-value "--content-type=" args) (alphamind--buffer-name-to-content-type (buffer-name))))
           (url-request-method "GET"))
      (progn
        (setq alphamind--content-type content-type)
        (alphamind--server-index-files force-update content-type))))

  (transient-define-suffix alphamind--chat-command (&optional _)
    "Command to Chat with AlphaMind."
    (interactive (list (transient-args transient-current-command)))
    (alphamind--chat))

  (transient-define-suffix alphamind--open-conversation-session-command (&optional _)
    "Command to select AlphaMind conversation sessions to open."
    (interactive (list (transient-args transient-current-command)))
    (alphamind--open-conversation-session))

  (transient-define-suffix alphamind--new-conversation-session-command (&optional args)
    "Command to select AlphaMind conversation sessions to open."
    (interactive (list (transient-args transient-current-command)))
    (let ((agent-slug (transient-arg-value "--agent=" args)))
      (alphamind--new-conversation-session agent-slug)))

  (transient-define-suffix alphamind--delete-conversation-session-command (&optional _)
    "Command to select AlphaMind conversation sessions to delete."
    (interactive (list (transient-args transient-current-command)))
    (alphamind--delete-conversation-session))

  (transient-define-prefix alphamind--chat-menu ()
    "Create the AlphaMind Chat Menu and Execute Commands."
    [["Configure"
      ("a" "Select Agent" alphamind--agent-switch)]]
    [["Act"
      ("c" "Chat" alphamind--chat-command)
      ("o" "Open Conversation" alphamind--open-conversation-session-command)
      ("n" "New Conversation" alphamind--new-conversation-session-command)
      ("d" "Delete Conversation" alphamind--delete-conversation-session-command)
      ("q" "Quit" transient-quit-one)]])

  (transient-define-prefix alphamind--menu ()
    "Create AlphaMind Menu to Configure and Execute Commands."
    [["Configure Search"
      ("-n" "Results Count" "--results-count=" :init-value (lambda (obj) (oset obj value (format "%s" alphamind-results-count))))
      ("t" "Content Type" alphamind--content-type-switch)]
     ["Configure Update"
      ("-f" "Force Update" "--force-update")]]
    [["Act"
      ("c" "Chat" alphamind--chat-menu)
      ("s" "Search" alphamind--search-command)
      ("f" "Find Similar" alphamind--find-similar-command)
      ("u" "Update" alphamind--update-command)
      ("q" "Quit" transient-quit-one)]])

  ;; Show the AlphaMind Transient menu
  (alphamind--menu))


;; ----------
;; Entrypoint
;; ----------

;;;###autoload
(defun alphamind ()
  "Search and chat with your knowledge base using your personal AI copilot.

Collaborate with AlphaMind to search, create, review and update your knowledge base.
Research across the internet & your documents from the comfort of Emacs."
  (interactive)
  (when alphamind-auto-setup
    (alphamind-setup t))
  (alphamind--setup-and-show-menu))

(provide 'alphamind)

;;; alphamind.el ends here
