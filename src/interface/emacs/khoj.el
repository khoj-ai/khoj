;;; khoj.el --- AI copilot for your Second Brain -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Khoj Inc.

;; Author: Debanjum Singh Solanky <debanjum@khoj.dev>
;;         Saba Imran <saba@khoj.dev>
;; Description: An AI copilot for your Second Brain
;; Keywords: search, chat, org-mode, outlines, markdown, pdf, image
;; Version: 1.12.1
;; Package-Requires: ((emacs "27.1") (transient "0.3.0") (dash "2.19.1"))
;; URL: https://github.com/khoj-ai/khoj/tree/master/src/interface/emacs

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

;; Create an AI copilot to your `org-mode', `markdown' notes,
;; PDFs and images. The copilot exposes 2 modes, search and chat:
;;
;; Chat provides faster answers, iterative discovery and assisted
;; creativity. It requires your OpenAI API key to access GPT models
;;
;; Search allows natural language, incremental and local search.
;; It relies on AI models that run locally on your machine.
;;
;; Quickstart
;; -------------
;; 1. Install khoj.el from MELPA Stable
;;    (use-package khoj :pin melpa-stable :bind ("C-c s" . 'khoj))
;; 2. Start khoj from Emacs
;;    C-c s or M-x khoj
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
;; Khoj Static Configuration
;; -------------------------

(defcustom khoj-server-url "https://app.khoj.dev"
  "Location of Khoj API server."
  :group 'khoj
  :type 'string)

(defcustom khoj-server-is-local t
  "Is Khoj server on local machine?."
  :group 'khoj
  :type 'boolean)

(defcustom khoj-image-width 156
  "Width of rendered images returned by Khoj."
  :group 'khoj
  :type 'integer)

(defcustom khoj-image-height 156
  "Height of rendered images returned by Khoj."
  :group 'khoj
  :type 'integer)

(defcustom khoj-results-count 5
  "Number of results to show in search and use for chat responses."
  :group 'khoj
  :type 'integer)

(defcustom khoj-search-on-idle-time 0.3
  "Idle time (in seconds) to wait before triggering search."
  :group 'khoj
  :type 'number)

(defcustom khoj-api-key nil
  "API Key to your Khoj. Default at https://app.khoj.dev/config#clients."
  :group 'khoj
  :type 'string)

(defcustom khoj-auto-index t
  "Should content be automatically re-indexed every `khoj-index-interval' seconds."
  :group 'khoj
  :type 'boolean)

(defcustom khoj-index-interval 3600
  "Interval (in seconds) to wait before updating content index."
  :group 'khoj
  :type 'number)

(defcustom khoj-default-content-type "org"
  "The default content type to perform search on."
  :group 'khoj
  :type '(choice (const "org")
                 (const "markdown")
                 (const "image")
                 (const "pdf")))


;; --------------------------
;; Khoj Dynamic Configuration
;; --------------------------

(defvar khoj--minibuffer-window nil
  "Minibuffer window used to enter query.")

(defconst khoj--query-prompt "🏮 Khoj: "
  "Query prompt shown in the minibuffer.")

(defconst khoj--search-buffer-name "*🏮 Khoj Search*"
  "Name of buffer to show search results from Khoj.")

(defconst khoj--chat-buffer-name "*🏮 Khoj Chat*"
  "Name of chat buffer for Khoj.")

(defvar khoj--content-type "org"
  "The type of content to perform search on.")

(defvar khoj--search-on-idle-timer nil
  "Idle timer to trigger incremental search.")

(defvar khoj--index-timer nil
  "Timer to trigger content indexing.")

(defvar khoj--indexed-files '()
  "Files that were indexed in previous content indexing run.")

(declare-function org-element-property "org-mode" (PROPERTY ELEMENT))
(declare-function org-element-type "org-mode" (ELEMENT))
(declare-function markdown-mode "markdown-mode" ())
(declare-function which-key--show-keymap "which-key" (KEYMAP-NAME KEYMAP &optional PRIOR-ARGS ALL
NO-PAGING FILTER))

(defun khoj--keybindings-info-message ()
  "Show available khoj keybindings in-context, when khoj invoked."
  (let ((enabled-content-types (khoj--get-enabled-content-types)))
    (concat
     "
     Set Content Type
-------------------------\n"
     (when (member 'markdown enabled-content-types)
       "C-x m  | markdown\n")
     (when (member 'org enabled-content-types)
       "C-x o  | org-mode\n")
     (when (member 'image enabled-content-types)
       "C-x i  | image\n")
     (when (member 'pdf enabled-content-types)
       "C-x p  | pdf\n"))))

(defvar khoj--rerank nil "Track when re-rank of results triggered.")
(defvar khoj--reference-count 0 "Track number of references currently in chat bufffer.")
(defun khoj--search-markdown () "Set content-type to `markdown'." (interactive) (setq khoj--content-type "markdown"))
(defun khoj--search-org () "Set content-type to `org-mode'." (interactive) (setq khoj--content-type "org"))
(defun khoj--search-images () "Set content-type to image." (interactive) (setq khoj--content-type "image"))
(defun khoj--search-pdf () "Set content-type to pdf." (interactive) (setq khoj--content-type "pdf"))
(defun khoj--improve-rank () "Use cross-encoder to rerank search results." (interactive) (khoj--incremental-search t))
(defun khoj--make-search-keymap (&optional existing-keymap)
  "Setup keymap to configure Khoj search. Build of EXISTING-KEYMAP when passed."
  (let ((enabled-content-types (khoj--get-enabled-content-types))
        (kmap (or existing-keymap (make-sparse-keymap))))
    (define-key kmap (kbd "C-c RET") #'khoj--improve-rank)
    (when (member 'markdown enabled-content-types)
      (define-key kmap (kbd "C-x m") #'khoj--search-markdown))
    (when (member 'org enabled-content-types)
      (define-key kmap (kbd "C-x o") #'khoj--search-org))
    (when (member 'image enabled-content-types)
      (define-key kmap (kbd "C-x i") #'khoj--search-images))
    (when (member 'pdf enabled-content-types)
      (define-key kmap (kbd "C-x p") #'khoj--search-pdf))
    kmap))

(defvar khoj--keymap nil "Track Khoj keymap in this variable.")
(defun khoj--display-keybinding-info ()
  "Display information on keybindings to customize khoj search.
Use `which-key` if available, else display simple message in echo area"
  (if (fboundp 'which-key-show-full-keymap)
      (let ((khoj--keymap (khoj--make-search-keymap)))
        (which-key--show-keymap (symbol-name 'khoj--keymap)
                                (symbol-value 'khoj--keymap)
                                nil t t))
    (message "%s" (khoj--keybindings-info-message))))


;; ----------------
;; Khoj Setup
;; ----------------
(defcustom khoj-server-command
  (or (executable-find "khoj")
      (executable-find "khoj.exe")
      "khoj")
  "Command to interact with Khoj server."
  :type 'string
  :group 'khoj)

(defcustom khoj-server-args '()
  "Arguments to pass to Khoj server on startup."
  :type '(repeat string)
  :group 'khoj)

(defcustom khoj-server-python-command
  (if (equal system-type 'windows-nt)
      (or (executable-find "py")
          (executable-find "pythonw")
          "python")
    (if (executable-find "python")
        "python"
      ;; Fallback on systems where python is not
      ;; symlinked to python3.
      "python3"))
  "The Python interpreter used for the Khoj server.

Khoj will try to use the system interpreter if it exists. If you wish
to use a specific python interpreter (from a virtual environment
for example), set this to the full interpreter path."
  :type '(choice (const :tag "python" "python")
                 (const :tag "python3" "python3")
                 (const :tag "pythonw (Python on Windows)" "pythonw")
                 (const :tag "py (other Python on Windows)" "py")
                 (string :tag "Other"))
  :safe (lambda (val)
          (member val '("python" "python3" "pythonw" "py")))
  :group 'khoj)

(defcustom khoj-org-files nil
  "List of org-files to index on khoj server."
  :type '(repeat string)
  :group 'khoj)

(defcustom khoj-org-directories nil
  "List of directories with `org-mode' files to index on khoj server."
  :type '(repeat string)
  :group 'khoj)

(make-obsolete-variable 'khoj-org-directories 'khoj-index-directories "1.2.0" 'set)
(make-obsolete-variable 'khoj-org-files 'khoj-index-files "1.2.0" 'set)

(defcustom khoj-index-files (org-agenda-files t t)
  "List of org, markdown, pdf and other plaintext to index on khoj server."
  :type '(repeat string)
  :group 'khoj)

(defcustom khoj-index-directories nil
  "List of directories with org, markdown, pdf and other plaintext files to index on khoj server."
  :type '(repeat string)
  :group 'khoj)

(defcustom khoj-auto-setup t
  "Automate install, configure and start of khoj server.
Auto invokes setup steps on calling main entrypoint."
  :type 'string
  :group 'khoj)

(defvar khoj--server-process nil "Track khoj server process.")
(defvar khoj--server-name "*khoj-server*" "Track khoj server buffer.")
(defvar khoj--server-ready? nil "Track if khoj server is ready to receive API calls.")
(defvar khoj--server-configured? t "Track if khoj server is configured to receive API calls.")
(defvar khoj--progressbar '(🌑 🌘 🌗 🌖 🌕 🌔 🌓 🌒) "Track progress via moon phase animations.")

(defun khoj--server-get-version ()
  "Return the khoj server version."
  (with-temp-buffer
    (call-process khoj-server-command nil t nil "--version")
    (goto-char (point-min))
    (re-search-forward "\\([a-z0-9.]+\\)")
    (match-string 1)))

(defun khoj--server-install-upgrade ()
  "Install or upgrade the khoj server."
  (with-temp-buffer
    (message "khoj.el: Installing server...")
    (if (/= (apply #'call-process khoj-server-python-command
                   nil t nil
                   "-m" "pip" "install" "--upgrade"
                   '("khoj-assistant"))
            0)
        (message "khoj.el: Failed to install Khoj server. Please install it manually using pip install `khoj-assistant'.\n%s" (buffer-string))
      (message "khoj.el: Installed and upgraded Khoj server version: %s" (khoj--server-get-version)))))

(defun khoj--server-start ()
  "Start the khoj server."
  (interactive)
  (let* ((url-parts (split-string (cadr (split-string khoj-server-url "://")) ":"))
         (server-host (nth 0 url-parts))
         (server-port (or (nth 1 url-parts) "80"))
         (server-args (append khoj-server-args
                              (list (format "--host=%s" server-host)
                                    (format "--port=%s" server-port)))))
    (message "khoj.el: Starting server at %s %s..." server-host server-port)
    (setq khoj--server-process
          (make-process
           :name khoj--server-name
           :buffer khoj--server-name
           :command (append (list khoj-server-command) server-args)
           :sentinel (lambda (_ event)
                       (message "khoj.el: khoj server stopped with: %s" event)
                       (setq khoj--server-ready? nil))
           :filter (lambda (process msg)
                     (cond ((string-match (format "Uvicorn running on %s" khoj-server-url) msg)
                            (progn
                              (setq khoj--server-ready? t)))
                           ((string-match "Batches:  " msg)
                            (when (string-match "\\([0-9]+\\.[0-9]+\\|\\([0-9]+\\)\\)%?" msg)
                              (message "khoj.el: %s updating index %s"
                                       (nth (% (string-to-number (match-string 1 msg)) (length khoj--progressbar)) khoj--progressbar)
                                       (match-string 0 msg)))
                            (setq khoj--server-configured? nil))
                           ((and (not khoj--server-configured?)
                                 (string-match "Processor reconfigured via API" msg))
                            (setq khoj--server-configured? t))
                           ((and (not khoj--server-ready?)
                                 (or (string-match "configure.py" msg)
                                     (string-match "main.py" msg)
                                     (string-match "api.py" msg)))
                            (dolist (line (split-string msg "\n"))
                              (when (string-match "  " line)
                                (message "khoj.el: %s" (nth 1 (split-string line "  " t " *")))))))
                     ;; call default process filter to write output to process buffer
                     (internal-default-process-filter process msg))))
    (set-process-query-on-exit-flag khoj--server-process nil)
    (when (not khoj--server-process)
        (message "khoj.el: Failed to start Khoj server. Please start it manually by running `khoj' on terminal.\n%s" (buffer-string)))))

(defun khoj--server-started? ()
  "Check if the khoj server has been started."
  ;; check for when server process handled from within emacs
  (if (and khoj--server-process
           (process-live-p khoj--server-process))
      t
    ;; else general check via ping to khoj-server-url
    (if (ignore-errors
          (url-retrieve-synchronously (format "%s/api/health" khoj-server-url)))
        ;; Successful ping to non-emacs khoj server indicates it is started and ready.
        ;; So update ready state tracker variable (and implicitly return true for started)
        (setq khoj--server-ready? t)
      nil)))

(defun khoj--server-restart ()
  "Restart the khoj server."
  (interactive)
  (khoj--server-stop)
  (khoj--server-start))

(defun khoj--server-stop ()
  "Stop the khoj server."
  (interactive)
  (when (khoj--server-started?)
    (message "khoj.el: Stopping server...")
    (kill-process khoj--server-process)
    (message "khoj.el: Stopped server.")))

(defun khoj--server-setup ()
  "Install and start the khoj server, if required."
  (interactive)
  ;; Install khoj server, if not available but expected on local machine
  (when (and khoj-server-is-local
             (or (not (executable-find khoj-server-command))
                 (not (khoj--server-get-version))))
      (khoj--server-install-upgrade))
  ;; Start khoj server if not already started
  (when (not (khoj--server-started?))
    (khoj--server-start)))

(defun khoj-setup (&optional interact)
  "Install and start Khoj server. Get permission if INTERACT is non-nil."
  (interactive "p")
  ;; Setup khoj server if not running
  (let* ((not-started (not (khoj--server-started?)))
         (permitted (if (and not-started interact)
                        (y-or-n-p "Could not connect to Khoj server. Should I install, start it for you?")
                      t)))
    ;; If user permits setup of khoj server from khoj.el
    (when permitted
      ; Install, start server if server not running
      (when not-started
        (khoj--server-setup))

      ;; Wait until server is ready
      ;; As server can be started but not ready to use
      (while (not khoj--server-ready?)
        (sit-for 0.5)))))


;; -------------------
;; Khoj Index Content
;; -------------------

(defun khoj--server-index-files (&optional force content-type file-paths)
  "Send files at `FILE-PATHS' to the Khoj server to index for search and chat.
`FORCE' re-indexes all files of `CONTENT-TYPE' even if they are already indexed."
  (interactive)
  (let* ((boundary (format "-------------------------%d" (random (expt 10 10))))
         ;; Use `khoj-index-directories', `khoj-index-files' when set, else fallback to `khoj-org-directories', `khoj-org-files'
         ;; This is a temporary change. `khoj-org-directories', `khoj-org-files' are deprecated. They will be removed in a future release
         (content-directories (or khoj-index-directories khoj-org-directories))
         (content-files (or khoj-index-files khoj-org-files))
         (files-to-index (or file-paths
                             (append (mapcan (lambda (dir) (directory-files-recursively dir "\\.\\(org\\|md\\|markdown\\|pdf\\|txt\\|rst\\|xml\\|htm\\|html\\)$")) content-directories) content-files)))
         (type-query (if (or (equal content-type "all") (not content-type)) "" (format "t=%s" content-type)))
         (inhibit-message t)
         (message-log-max nil))
    (let ((url-request-method "POST")
          (url-request-data (khoj--render-files-as-request-body files-to-index khoj--indexed-files boundary))
          (url-request-extra-headers `(("content-type" . ,(format "multipart/form-data; boundary=%s" boundary))
                                       ("Authorization" . ,(format "Bearer %s" khoj-api-key)))))
      (with-current-buffer
          (url-retrieve (format "%s/api/v1/index/update?%s&force=%s&client=emacs" khoj-server-url type-query (or force "false"))
                        ;; render response from indexing API endpoint on server
                        (lambda (status)
                          (if (not (plist-get status :error))
                              (message "khoj.el: %scontent index %supdated" (if content-type (format "%s " content-type) "all ") (if force "force " ""))
                            (progn
                              (khoj--delete-open-network-connections-to-server)
                              (with-current-buffer (current-buffer)
                                (search-forward "\n\n" nil t)
                                (message "khoj.el: Failed to %supdate %scontent index. Status: %s%s"
                                         (if force "force " "")
                                         (if content-type (format "%s " content-type) "all")
                                         (string-trim (format "%s %s" (nth 1 (nth 1 status)) (nth 2 (nth 1 status))))
                                         (if (> (- (point-max) (point)) 0) (format ". Response: %s" (string-trim (buffer-substring-no-properties (point) (point-max)))) ""))))))
                        nil t t)))
    (setq khoj--indexed-files files-to-index)))

(defun khoj--render-files-as-request-body (files-to-index previously-indexed-files boundary)
  "Render `FILES-TO-INDEX', `PREVIOUSLY-INDEXED-FILES' as multi-part form body.
Use `BOUNDARY' to separate files. This is sent to Khoj server as a POST request."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert "\n")
    (dolist (file-to-index files-to-index)
      ;; find file content-type. Choose from org, markdown, pdf, plaintext
      (let ((content-type (cond ((string-match "\\.org$" file-to-index) "text/org")
                                ((string-match "\\.\\(md\\|markdown\\)$" file-to-index) "text/markdown")
                                ((string-match "\\.pdf$" file-to-index) "application/pdf")
                                (t "text/plain"))))
      (insert (format "--%s\r\n" boundary))
      (insert (format "Content-Disposition: form-data; name=\"files\"; filename=\"%s\"\r\n" file-to-index))
      (insert (format "Content-Type: %s\r\n\r\n" content-type))
      (insert (with-temp-buffer
                (insert-file-contents-literally file-to-index)
                (buffer-string)))
      (insert "\r\n")))
    (dolist (file-to-index previously-indexed-files)
      (when (not (member file-to-index files-to-index))
        ;; find file content-type. Choose from org, markdown, pdf, plaintext
        (let ((content-type (cond ((string-match "\\.org$" file-to-index) "text/org")
                                  ((string-match "\\.\\(md\\|markdown\\)$" file-to-index) "text/markdown")
                                  ((string-match "\\.pdf$" file-to-index) "application/pdf")
                                  (t "text/plain"))))
          (insert (format "--%s\r\n" boundary))
          (insert (format "Content-Disposition: form-data; name=\"files\"; filename=\"%s\"\r\n" file-to-index))
          (insert "Content-Type: text/org\r\n\r\n")
          (insert "")
          (insert "\r\n"))))
    (insert (format "--%s--\r\n" boundary))
    (buffer-string)))

;; Cancel any running indexing timer, first
(when khoj--index-timer
    (cancel-timer khoj--index-timer))
;; Send files to index on server every `khoj-index-interval' seconds
(when khoj-auto-index
  (setq khoj--index-timer
        (run-with-timer 60 khoj-index-interval 'khoj--server-index-files)))


;; -------------------------------------------
;; Render Response from Khoj server for Emacs
;; -------------------------------------------

(defun khoj--extract-entries-as-markdown (json-response query)
  "Convert JSON-RESPONSE, QUERY from API to markdown entries."
  (thread-last
    json-response
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
    (format "# %s\n%s" query)
    ;; remove leading (, ) or SPC from extracted entries string
    (replace-regexp-in-string "^[\(\) ]" "")))

(defun khoj--extract-entries-as-org (json-response query)
  "Convert JSON-RESPONSE, QUERY from API to `org-mode' entries."
  (thread-last
    json-response
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
    (format "* %s\n%s\n" query)
    ;; remove leading (, ) or SPC from extracted entries string
    (replace-regexp-in-string "^[\(\) ]" "")))

(defun khoj--extract-entries-as-pdf (json-response query)
  "Convert QUERY, JSON-RESPONSE from API with PDF results to `org-mode' entries."
  (thread-last
    json-response
    ;; Extract and render each pdf entry from response
    (mapcar (lambda (json-response-item)
              (thread-last
                ;; Extract pdf entry from each item in json response
                (cdr (assoc 'compiled (assoc 'additional json-response-item)))
                ;; Format pdf entry as a org entry string
                (format "** %s\n\n"))))
    ;; Render entries into org formatted string with query set as as top level heading
    (format "* %s\n%s\n" query)
    ;; remove leading (, ) or SPC from extracted entries string
    (replace-regexp-in-string "^[\(\) ]" "")))

(defun khoj--extract-entries-as-images (json-response query)
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
                      (image_url (concat khoj-server-url (cdr (assoc 'entry json-response-item)))))
                  ;; Wrap images into html img, href tags with metadata in headings
                  (format image-result-html-format-str
                          ;; image scores metadata
                          score metadata_score image_score
                          ;; image url
                          image_url image_url (random 10000)
                          ;; image dimensions
                          khoj-image-width khoj-image-height))))
      ;; Collate entries into single html page string
      (format image-results-buffer-html-format-str query)
      ;; remove leading (, ) or SPC from extracted entries string
      (replace-regexp-in-string "^[\(\) ]" "")
      ;; remove trailing (, ) or SPC from extracted entries string
      (replace-regexp-in-string "[\(\) ]$" ""))))

(defun khoj--extract-entries (json-response query)
  "Convert JSON-RESPONSE, QUERY from API to text entries."
  (thread-last json-response
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
               (format "* %s\n%s\n" query)
               ;; remove leading (, ) or SPC from extracted entries string
               (replace-regexp-in-string "^[\(\) ]" "")
               ;; remove trailing (, ) or SPC from extracted entries string
               (replace-regexp-in-string "[\(\) ]$" "")))

(defun khoj--buffer-name-to-content-type (buffer-name)
  "Infer content type based on BUFFER-NAME."
  (let ((enabled-content-types (khoj--get-enabled-content-types))
        (file-extension (file-name-extension buffer-name)))
    (cond
     ((and (member 'org enabled-content-types) (equal file-extension "org")) "org")
     ((and (member 'pdf enabled-content-types) (equal file-extension "pdf")) "pdf")
     ((and (member 'markdown enabled-content-types) (or (equal file-extension "markdown") (equal file-extension "md"))) "markdown")
     (t khoj-default-content-type))))


;; --------------
;; Query Khoj API
;; --------------
(defun khoj--get-enabled-content-types ()
  "Get content types enabled for search from API."
  (let ((config-url (format "%s/api/config/types" khoj-server-url))
        (url-request-method "GET")
        (url-request-extra-headers `(("Authorization" . ,(format "Bearer %s" khoj-api-key)))))
    (with-temp-buffer
      (url-insert-file-contents config-url)
      (thread-last
        (json-parse-buffer :object-type 'alist)
        (mapcar #'intern)))))

(defun khoj--construct-search-api-query (query content-type &optional rerank)
  "Construct Search API Query.
Use QUERY, CONTENT-TYPE and (optional) RERANK as query params"
  (let ((rerank (or rerank "false"))
        (encoded-query (url-hexify-string query)))
    (format "%s/api/search?q=%s&t=%s&r=%s&n=%s&client=emacs" khoj-server-url encoded-query content-type rerank khoj-results-count)))

(defun khoj--query-search-api-and-render-results (query-url content-type query buffer-name)
  "Query Khoj Search with QUERY-URL.
Render results in BUFFER-NAME using QUERY, CONTENT-TYPE."
  ;; get json response from api
  (with-current-buffer buffer-name
    (let ((inhibit-read-only t)
          (url-request-method "GET")
          (url-request-extra-headers `(("Authorization" . ,(format "Bearer %s" khoj-api-key)))))
      (erase-buffer)
      (url-insert-file-contents query-url)))
  ;; render json response into formatted entries
  (with-current-buffer buffer-name
    (let ((inhibit-read-only t)
          (json-response (json-parse-buffer :object-type 'alist)))
      (erase-buffer)
      (insert
       (cond ((equal content-type "org") (khoj--extract-entries-as-org json-response query))
             ((equal content-type "markdown") (khoj--extract-entries-as-markdown json-response query))
             ((equal content-type "pdf") (khoj--extract-entries-as-pdf json-response query))
             ((equal content-type "image") (khoj--extract-entries-as-images json-response query))
             (t (khoj--extract-entries json-response query))))
      (cond ((or (equal content-type "all")
                 (equal content-type "pdf")
                 (equal content-type "org"))
             (progn (visual-line-mode)
                    (org-mode)
                   (setq-local
                    org-startup-folded "showall"
                    org-hide-leading-stars t
                    org-startup-with-inline-images t)
                   (org-set-startup-visibility)))
            ((equal content-type "markdown") (progn (markdown-mode)
                                                    (visual-line-mode)))
            ((equal content-type "image") (progn (shr-render-region (point-min) (point-max))
                                                (goto-char (point-min))))
            (t (fundamental-mode))))
    (read-only-mode t)))


;; ----------------
;; Khoj Chat
;; ----------------

(defun khoj--chat ()
  "Chat with Khoj."
  (interactive)
  (when (not (get-buffer khoj--chat-buffer-name))
      (khoj--load-chat-history khoj--chat-buffer-name))
  (switch-to-buffer khoj--chat-buffer-name)
  (let ((query (read-string "Query: ")))
    (when (not (string-empty-p query))
      (khoj--query-chat-api-and-render-messages query khoj--chat-buffer-name))))

(defun khoj--load-chat-history (buffer-name)
  "Load Khoj Chat conversation history into BUFFER-NAME."
  (let ((json-response (cdr (assoc 'response (khoj--get-chat-history-api)))))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert "* Khoj Chat\n")
      (thread-last
        json-response
        ;; generate chat messages from Khoj Chat API response
        (mapcar #'khoj--render-chat-response)
        ;; insert chat messages into Khoj Chat Buffer
        (mapc #'insert))
      (progn
        (org-mode)
        (khoj--add-hover-text-to-footnote-refs (point-min))

        ;; render reference footnotes as superscript
        (setq-local
         org-startup-folded "showall"
         org-hide-leading-stars t
         org-use-sub-superscripts '{}
         org-pretty-entities-include-sub-superscripts t
         org-pretty-entities t)
        (org-set-startup-visibility)

        ;; create khoj chat shortcut keybindings
        (use-local-map (copy-keymap org-mode-map))
        (local-set-key (kbd "m") #'khoj--chat)
        (local-set-key (kbd "C-x m") #'khoj--chat)

        ;; enable minor modes for khoj chat
        (visual-line-mode)
        (read-only-mode t)))))

(defun khoj--add-hover-text-to-footnote-refs (start-pos)
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

(defun khoj--query-chat-api-and-render-messages (query buffer-name)
  "Send QUERY to Khoj Chat. Render the chat messages from exchange in BUFFER-NAME."
  ;; render json response into formatted chat messages
  (with-current-buffer (get-buffer buffer-name)
    (let ((inhibit-read-only t)
          (new-content-start-pos (point-max))
          (query-time (format-time-string "%F %T"))
          (json-response (khoj--query-chat-api query)))
      (goto-char new-content-start-pos)
      (insert
       (khoj--render-chat-message query "you" query-time)
       (khoj--render-chat-response json-response))
      (khoj--add-hover-text-to-footnote-refs new-content-start-pos))
    (progn
      (org-set-startup-visibility)
      (visual-line-mode)
      (re-search-backward "^\*+ 🏮" nil t))))

(defun khoj--query-chat-api (query)
  "Send QUERY to Khoj Chat API."
  (let* ((url-request-method "GET")
         (encoded-query (url-hexify-string query))
         (url-request-extra-headers `(("Authorization" . ,(format "Bearer %s" khoj-api-key))))
         (query-url (format "%s/api/chat?q=%s&n=%s&client=emacs" khoj-server-url encoded-query khoj-results-count)))
    (with-temp-buffer
      (condition-case ex
          (progn
            (url-insert-file-contents query-url)
            (json-parse-buffer :object-type 'alist))
        ('file-error (cond ((string-match "Internal server error" (nth 2 ex))
                      (message "Chat processor not configured. Configure OpenAI API key and restart it. Exception: [%s]" ex))
                     (t (message "Chat exception: [%s]" ex))))))))


(defun khoj--get-chat-history-api ()
  "Send QUERY to Khoj Chat History API."
  (let* ((url-request-method "GET")
         (url-request-extra-headers `(("Authorization" . ,(format "Bearer %s" khoj-api-key))))
         (query-url (format "%s/api/chat/history?client=emacs" khoj-server-url)))
    (with-temp-buffer
      (condition-case ex
          (progn
            (url-insert-file-contents query-url)
            (json-parse-buffer :object-type 'alist))
        ('file-error (cond ((string-match "Internal server error" (nth 2 ex))
                      (message "Chat processor not configured. Configure OpenAI API key and restart it. Exception: [%s]" ex))
                     (t (message "Chat exception: [%s]" ex))))))))


(defun khoj--render-chat-message (message sender &optional receive-date)
  "Render chat messages as `org-mode' list item.
MESSAGE is the text of the chat message.
SENDER is the message sender.
RECEIVE-DATE is the message receive date."
  (let ((first-message-line (car (split-string message "\n" t)))
        (rest-message-lines (string-join (cdr (split-string message "\n" t)) "\n"))
        (heading-level (if (equal sender "you") "**" "***"))
        (emojified-sender (if (equal sender "you") "🤔 *You*" "🏮 *Khoj*"))
        (suffix-newlines (if (equal sender "khoj") "\n\n" ""))
        (received (or receive-date (format-time-string "%F %T"))))
    (format "%s %s: %s\n   :PROPERTIES:\n   :RECEIVED: [%s]\n   :END:\n%s\n%s"
            heading-level
            emojified-sender
            first-message-line
            received
            rest-message-lines
            suffix-newlines)))

(defun khoj--generate-reference (reference)
  "Create `org-mode' footnotes with REFERENCE."
  (setq khoj--reference-count (1+ khoj--reference-count))
  (cons
   (propertize (format "^{ [fn:%x]}" khoj--reference-count) 'help-echo reference)
   (thread-last
     reference
     ;; remove filename top heading line from reference
     ;; prevents actual reference heading in next line jumping out of references footnote section
     (replace-regexp-in-string "^\* .*\n" "")
     ;; remove multiple, consecutive empty lines from reference
     (replace-regexp-in-string "\n\n" "\n")
     (format "\n[fn:%x] %s" khoj--reference-count))))

(defun khoj--render-chat-response (json-response)
  "Render chat message using JSON-RESPONSE from Khoj Chat API."
  (let* ((message (cdr (or (assoc 'response json-response) (assoc 'message json-response))))
         (sender (cdr (assoc 'by json-response)))
         (receive-date (cdr (assoc 'created json-response)))
         (references (or (cdr (assoc 'context json-response)) '()))
         (footnotes (mapcar #'khoj--generate-reference references))
         (footnote-links (mapcar #'car footnotes))
         (footnote-defs (mapcar #'cdr footnotes)))
    (thread-first
      ;; concatenate khoj message and references from API
      (concat
       message
       ;; append reference links to khoj message
       (string-join footnote-links "")
       ;; append reference sub-section to khoj message and fold it
       (if footnote-defs "\n**** References\n:PROPERTIES:\n:VISIBILITY: folded\n:END:" "")
       ;; append reference definitions to references subsection
       (string-join footnote-defs " "))
      ;; Render chat message using data obtained from API
      (khoj--render-chat-message sender receive-date))))


;; ------------------
;; Incremental Search
;; ------------------

(defun khoj--incremental-search (&optional rerank)
  "Perform Incremental Search on Khoj. Allow optional RERANK of results."
  (let* ((rerank-str (cond (rerank "true") (t "false")))
         (khoj-buffer-name (get-buffer-create khoj--search-buffer-name))
         (query (minibuffer-contents-no-properties))
         (query-url (khoj--construct-search-api-query query khoj--content-type rerank-str)))
    ;; Query khoj API only when user in khoj minibuffer and non-empty query
    ;; Prevents querying if
    ;;   1. user hasn't started typing query
    ;;   2. during recursive edits
    ;;   3. with contents of other buffers user may jump to
    ;;   4. search not triggered right after rerank
    ;;      ignore to not overwrite reranked results before the user even sees them
    (if khoj--rerank
        (setq khoj--rerank nil)
      (when
          (and
           (not (equal query ""))
           (active-minibuffer-window)
           (equal (current-buffer) khoj--minibuffer-window))
      (progn
        (when rerank
          (setq khoj--rerank t)
          (message "khoj.el: Rerank Results"))
        (khoj--query-search-api-and-render-results
         query-url
         khoj--content-type
         query
         khoj-buffer-name))))))

(defun khoj--delete-open-network-connections-to-server ()
  "Delete all network connections to khoj server."
  (dolist (proc (process-list))
    (let ((proc-buf (buffer-name (process-buffer proc)))
          (khoj-network-proc-buf (string-join (split-string khoj-server-url "://") " ")))
      (when (string-match (format "%s" khoj-network-proc-buf) proc-buf)
        (ignore-errors (delete-process proc))))))

(defun khoj--teardown-incremental-search ()
  "Teardown hooks used for incremental search."
  (message "khoj.el: Teardown Incremental Search")
  ;; unset khoj minibuffer window
  (setq khoj--minibuffer-window nil)
  (when (and khoj--search-on-idle-timer
             (timerp khoj--search-on-idle-timer))
    (cancel-timer khoj--search-on-idle-timer))
  ;; delete open connections to khoj server
  (khoj--delete-open-network-connections-to-server)
  ;; remove hooks for khoj incremental query and self
  (remove-hook 'post-command-hook #'khoj--incremental-search)
  (remove-hook 'minibuffer-exit-hook #'khoj--teardown-incremental-search))

(defun khoj-incremental ()
  "Natural, Incremental Search for your personal notes and documents."
  (interactive)
  (let* ((khoj-buffer-name (get-buffer-create khoj--search-buffer-name)))
    ;; switch to khoj results buffer
    (switch-to-buffer khoj-buffer-name)
    ;; open and setup minibuffer for incremental search
    (minibuffer-with-setup-hook
        (lambda ()
          ;; Add khoj keybindings for configuring search to minibuffer keybindings
          (khoj--make-search-keymap minibuffer-local-map)
          ;; Display information on keybindings to customize khoj search
          (khoj--display-keybinding-info)
          ;; set current (mini-)buffer entered as khoj minibuffer
          ;; used to query khoj API only when user in khoj minibuffer
          (setq khoj--minibuffer-window (current-buffer))
          ; do khoj incremental search after idle time
          (setq khoj--search-on-idle-timer (run-with-idle-timer khoj-search-on-idle-time t #'khoj--incremental-search))
          ; teardown khoj incremental search on minibuffer exit
          (add-hook 'minibuffer-exit-hook #'khoj--teardown-incremental-search))
      (read-string khoj--query-prompt))))


;; --------------
;; Similar Search
;; --------------

(defun khoj--get-current-outline-entry-text ()
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

(defun khoj--get-current-paragraph-text ()
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


(defun khoj--find-similar (&optional content-type)
  "Find items of CONTENT-TYPE in khoj index similar to text surrounding point."
  (interactive)
  (let* ((rerank "true")
         ;; set content type to: specified > based on current buffer > default type
         (content-type (or content-type (khoj--buffer-name-to-content-type (buffer-name))))
         ;; get text surrounding current point based on the major mode context
         (query (cond
                 ;; get section outline derived mode like org or markdown
                 ((or (derived-mode-p 'outline-mode) (equal major-mode 'markdown-mode))
                  (khoj--get-current-outline-entry-text))
                 ;; get paragraph, if in text mode
                 (t
                  (khoj--get-current-paragraph-text))))
         (query-url (khoj--construct-search-api-query query content-type rerank))
         ;; extract heading to show in result buffer from query
         (query-title
          (format "Similar to: %s"
                  (replace-regexp-in-string "^[#\\*]* " "" (car (split-string query "\n")))))
         (buffer-name (get-buffer-create khoj--search-buffer-name)))
    (progn
      (khoj--query-search-api-and-render-results
       query-url
       content-type
       query-title
       buffer-name)
      (switch-to-buffer buffer-name)
      (goto-char (point-min)))))


;; ---------
;; Khoj Menu
;; ---------

(defun khoj--setup-and-show-menu ()
  "Create Transient menu for khoj and show it."
  ;; Create the Khoj Transient menu
  (transient-define-argument khoj--content-type-switch ()
    :class 'transient-switches
    :argument-format "--content-type=%s"
    :argument-regexp ".+"
    ;; set content type to: last used > based on current buffer > default type
    :init-value (lambda (obj) (oset obj value (format "--content-type=%s" (or khoj--content-type (khoj--buffer-name-to-content-type (buffer-name))))))
    ;; dynamically set choices to content types enabled on khoj backend
    :choices (or (ignore-errors (mapcar #'symbol-name (khoj--get-enabled-content-types))) '("all" "org" "markdown" "pdf" "image")))

  (transient-define-suffix khoj--search-command (&optional args)
    (interactive (list (transient-args transient-current-command)))
    (progn
      ;; set content type to: specified > last used > based on current buffer > default type
      (setq khoj--content-type (or (transient-arg-value "--content-type=" args) (khoj--buffer-name-to-content-type (buffer-name))))
      ;; set results count to: specified > last used > to default
      (setq khoj-results-count (or (transient-arg-value "--results-count=" args) khoj-results-count))
      ;; trigger incremental search
      (call-interactively #'khoj-incremental)))

  (transient-define-suffix khoj--find-similar-command (&optional args)
    "Find items similar to current item at point."
    (interactive (list (transient-args transient-current-command)))
    (progn
      ;; set content type to: specified > last used > based on current buffer > default type
      (setq khoj--content-type (or (transient-arg-value "--content-type=" args) (khoj--buffer-name-to-content-type (buffer-name))))
      ;; set results count to: specified > last used > to default
      (setq khoj-results-count (or (transient-arg-value "--results-count=" args) khoj-results-count))
      (khoj--find-similar khoj--content-type)))

  (transient-define-suffix khoj--update-command (&optional args)
    "Call khoj API to update index of specified content type."
    (interactive (list (transient-args transient-current-command)))
    (let* ((force-update (if (member "--force-update" args) "true" "false"))
           ;; set content type to: specified > last used > based on current buffer > default type
           (content-type (or (transient-arg-value "--content-type=" args) (khoj--buffer-name-to-content-type (buffer-name))))
           (url-request-method "GET"))
      (progn
        (setq khoj--content-type content-type)
        (khoj--server-index-files force-update content-type))))

  (transient-define-suffix khoj--chat-command (&optional _)
    "Command to Chat with Khoj."
    (interactive (list (transient-args transient-current-command)))
    (khoj--chat))

  (transient-define-prefix khoj--menu ()
    "Create Khoj Menu to Configure and Execute Commands."
    [["Configure Search"
      ("n" "Results Count" "--results-count=" :init-value (lambda (obj) (oset obj value (format "%s" khoj-results-count))))
      ("t" "Content Type" khoj--content-type-switch)]
     ["Configure Update"
      ("-f" "Force Update" "--force-update")]]
    [["Act"
      ("c" "Chat" khoj--chat-command)
      ("s" "Search" khoj--search-command)
      ("f" "Find Similar" khoj--find-similar-command)
      ("u" "Update" khoj--update-command)
      ("q" "Quit" transient-quit-one)]])

  ;; Show the Khoj Transient menu
  (khoj--menu))


;; ----------
;; Entrypoint
;; ----------

;;;###autoload
(defun khoj ()
  "Search and chat with your knowledge base using your personal AI copilot.

Collaborate with Khoj to search, understand, create, review and update your knowledge base.
Khoj can research across your org-mode, markdown notes, plaintext documents and the internet."
  (interactive)
  (when khoj-auto-setup
    (khoj-setup t))
  (khoj--setup-and-show-menu))

(provide 'khoj)

;;; khoj.el ends here
