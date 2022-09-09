;;; khoj.el --- Natural, Incremental Search for your Second Brain -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Debanjum Singh Solanky

;; Author: Debanjum Singh Solanky <debanjum@gmail.com>
;; Description: Natural, Incremental Search for your Second Brain
;; Keywords: search, org-mode, outlines, markdown, beancount, ledger, image
;; Version: 0.1.9
;; Package-Requires: ((emacs "27.1"))
;; URL: http://github.com/debanjum/khoj/interface/emacs

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

;; This package provides a natural, incremental search interface to your
;; `org-mode' notes, `markdown' files, `beancount' transactions and images.
;; It is a wrapper that interfaces with the Khoj server.
;; The server exposes an API for advanced search using transformer ML models.
;; The Khoj server needs to be running to use this package.
;; See the repository docs for detailed setup of the Khoj server.
;;
;; Quickstart
;; -------------
;; 1. Install Khoj Server
;;    pip install khoj-assistant
;; 2. Start, Configure Khoj Server
;;    khoj
;; 3. Install khoj.el
;;    (use-package khoj :bind ("C-c s" . 'khoj))

;;; Code:

(require 'url)
(require 'json)

(defcustom khoj-server-url "http://localhost:8000"
  "Location of Khoj API server."
  :group 'khoj
  :type 'string)

(defcustom khoj-image-width 156
  "Width of rendered images returned by Khoj."
  :group 'khoj
  :type 'integer)

(defcustom khoj-image-height 156
  "Height of rendered images returned by Khoj."
  :group 'khoj
  :type 'integer)

(defcustom khoj-results-count 5
  "Number of results to get from Khoj API for each query."
  :group 'khoj
  :type 'integer)

(defcustom khoj-default-search-type "org"
  "The default content type to perform search on."
  :group 'khoj
  :type '(choice (const "org")
                 (const "markdown")
                 (const "ledger")
                 (const "music")))

(defvar khoj--minibuffer-window nil
  "Minibuffer window being used by user to enter query.")

(defconst khoj--query-prompt "🦅Khoj: "
  "Query prompt shown to user in the minibuffer.")

(defconst khoj--buffer-name "*🦅Khoj*"
  "Name of buffer to show results from Khoj.")

(defvar khoj--search-type "org"
  "The type of content to perform search on.")

(defun khoj--keybindings-info-message ()
  "Show available khoj keybindings in-context, when user invokes Khoj."
  (let ((enabled-content-types (khoj--get-enabled-content-types)))
    (concat
     "
     Set Search Type
-------------------------\n"
     (when (member 'markdown enabled-content-types)
         "C-x m  | markdown\n")
     (when (member 'org enabled-content-types)
       "C-x o  | org-mode\n")
     (when (member 'ledger enabled-content-types)
       "C-x l  | ledger\n")
     (when (member 'image enabled-content-types)
       "C-x i  | images\n")
     (when (member 'music enabled-content-types)
       "C-x M  | music\n"))))

(defvar khoj--rerank nil "Track when re-rank of results triggered")
(defun khoj--search-markdown () "Set search-type to 'markdown'." (interactive) (setq khoj--search-type "markdown"))
(defun khoj--search-org () "Set search-type to 'org-mode'." (interactive) (setq khoj--search-type "org"))
(defun khoj--search-ledger () "Set search-type to 'ledger'." (interactive) (setq khoj--search-type "ledger"))
(defun khoj--search-images () "Set search-type to image." (interactive) (setq khoj--search-type "image"))
(defun khoj--search-music () "Set search-type to music." (interactive) (setq khoj--search-type "music"))
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
    (when (member 'ledger enabled-content-types)
      (define-key kmap (kbd "C-x l") #'khoj--search-ledger))
    (when (member 'image enabled-content-types)
      (define-key kmap (kbd "C-x i") #'khoj--search-images))
    (when (member 'music enabled-content-types)
      (define-key kmap (kbd "C-x M") #'khoj--search-music))
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

(defun khoj--extract-entries-as-markdown (json-response query)
  "Convert JSON-RESPONSE, QUERY from API to markdown entries."
  ;; remove leading (, ) or SPC from extracted entries string
  (replace-regexp-in-string
   "^[\(\) ]" ""
   ;; extract entries from response as single string and convert to entries
   (format "# %s\n%s"
           query
           (mapcar
            (lambda (args)
              (replace-regexp-in-string
               "^\#+" "##"
               (format "%s" (cdr (assoc 'entry args)))))
            json-response))))

(defun khoj--extract-entries-as-org (json-response query)
  "Convert JSON-RESPONSE, QUERY from API to 'org-mode' entries."
  ;; remove leading (, ) or SPC from extracted entries string
  (replace-regexp-in-string
   "^[\(\) ]" ""
   ;; extract entries from response as single string and convert to entries
   (format "* %s\n%s\n#+STARTUP: showall hidestars inlineimages"
           query
           (mapcar
            (lambda (args)
              (replace-regexp-in-string
               "^\*+" "**"
               (format "%s" (cdr (assoc 'entry args)))))
              json-response))))

(defun khoj--extract-entries-as-images (json-response query)
  "Convert JSON-RESPONSE, QUERY from API to html with images."
  ;; remove leading (, ) or SPC from extracted entries string
  (replace-regexp-in-string
   "[\(\) ]$" ""
   ;; remove leading (, ) or SPC from extracted entries string
   (replace-regexp-in-string
    "^[\(\) ]" ""
    ;; extract entries from response as single string and convert to entries
    (format "<html>\n<body>\n<h1>%s</h1>%s\n\n</body>\n</html>"
            query
            (mapcar
             (lambda (args) (format
                             "\n\n<h2>Score: %s Meta: %s Image: %s</h2>\n\n<a href=\"%s%s\">\n<img src=\"%s%s?%s\" width=%s height=%s>\n</a>"
                             (cdr (assoc 'score args))
                             (cdr (assoc 'metadata_score args))
                             (cdr (assoc 'image_score args))
                             khoj-server-url
                             (cdr (assoc 'entry args))
                             khoj-server-url
                             (cdr (assoc 'entry args))
                             (random 10000)
                             khoj-image-width
                             khoj-image-height))
             json-response)))))

(defun khoj--extract-entries-as-ledger (json-response query)
  "Convert JSON-RESPONSE, QUERY from API to ledger entries."
  ;; remove leading (, ) or SPC from extracted entries string
  (replace-regexp-in-string
   "[\(\) ]$" ""
   (replace-regexp-in-string
    "^[\(\) ]" ""
    ;; extract entries from response as single string and convert to entries
    (format ";; %s\n\n%s\n"
            query
            (mapcar
             (lambda (args)
               (format "%s\n\n" (cdr (assoc 'entry args))))
             json-response)))))

(defun khoj--buffer-name-to-search-type (buffer-name)
  "Infer search type based on BUFFER-NAME."
  (let ((enabled-content-types (khoj--get-enabled-content-types))
        (file-extension (file-name-extension buffer-name)))
    (cond
     ((and (member 'music enabled-content-types) (equal buffer-name "Music.org")) "music")
     ((and (member 'ledger enabled-content-types) (or (equal file-extension "bean") (equal file-extension "beancount"))) "ledger")
     ((and (member 'org enabled-content-types) (equal file-extension "org")) "org")
     ((and (member 'markdown enabled-content-types) (or (equal file-extension "markdown") (equal file-extension "md"))) "markdown")
     (t khoj-default-search-type))))

(defun khoj--get-enabled-content-types ()
  "Get content types enabled for search from API."
  (let ((config-url (format "%s/config/data" khoj-server-url)))
    (with-temp-buffer
      (erase-buffer)
      (url-insert-file-contents config-url)
      (let* ((json-response (json-parse-buffer :object-type 'alist))
            (content-type (cdr (assoc 'content-type json-response))))
        ;; return content-type items with configuration
        (mapcar
         'car
         (cl-remove-if-not
          '(lambda (a) (not (eq (cdr a) :null)))
          content-type))))))

(defun khoj--construct-api-query (query search-type &optional rerank)
  "Construct API Query from QUERY, SEARCH-TYPE and (optional) RERANK params."
  (let ((rerank (or rerank "false"))
        (encoded-query (url-hexify-string query)))
    (format "%s/search?q=%s&t=%s&r=%s&n=%s" khoj-server-url encoded-query search-type rerank khoj-results-count)))

(defun khoj--query-api-and-render-results (query search-type query-url buffer-name)
  "Query Khoj API using QUERY, SEARCH-TYPE, QUERY-URL.
Render results in BUFFER-NAME."
  ;; get json response from api
  (with-current-buffer buffer-name
    (let ((inhibit-read-only t))
      (erase-buffer)
      (url-insert-file-contents query-url)))
  ;; render json response into formatted entries
  (with-current-buffer buffer-name
    (let ((inhibit-read-only t)
          (json-response (json-parse-buffer :object-type 'alist)))
      (erase-buffer)
      (insert
       (cond ((or (equal search-type "org") (equal search-type "music")) (khoj--extract-entries-as-org json-response query))
             ((equal search-type "markdown") (khoj--extract-entries-as-markdown json-response query))
             ((equal search-type "ledger") (khoj--extract-entries-as-ledger json-response query))
             ((equal search-type "image") (khoj--extract-entries-as-images json-response query))
             (t (format "%s" json-response))))
      (cond ((equal search-type "org") (org-mode))
            ((equal search-type "markdown") (markdown-mode))
            ((equal search-type "ledger") (beancount-mode))
            ((equal search-type "music") (progn (org-mode)
                                                (org-music-mode)))
            ((equal search-type "image") (progn (shr-render-region (point-min) (point-max))
                                                (goto-char (point-min))))
            (t (fundamental-mode))))
    (read-only-mode t)))


(defun khoj--incremental-search (&optional rerank)
  "Perform Incremental Search on Khoj. Allow optional RERANK of results."
  (let* ((rerank-str (cond (rerank "true") (t "false")))
         (khoj-buffer-name (get-buffer-create khoj--buffer-name))
         (query (minibuffer-contents-no-properties))
         (query-url (khoj--construct-api-query query khoj--search-type rerank-str)))
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
          (message "Khoj: Rerank Results"))
        (khoj--query-api-and-render-results
         query
         khoj--search-type
         query-url
         khoj-buffer-name))))))

(defun khoj--delete-open-network-connections-to-server ()
  "Delete all network connections to khoj server."
  (dolist (proc (process-list))
    (let ((proc-buf (buffer-name (process-buffer proc)))
          (khoj-network-proc-buf (string-join (split-string khoj-server-url "://") " ")))
      (when (string-match (format "%s" khoj-network-proc-buf) proc-buf)
        (delete-process proc)))))

(defun khoj--teardown-incremental-search ()
  "Teardown hooks used for incremental search."
  (message "Khoj: Teardown Incremental Search")
  ;; unset khoj minibuffer window
  (setq khoj--minibuffer-window nil)
  ;; delete open connections to khoj server
  (khoj--delete-open-network-connections-to-server)
  ;; remove hooks for khoj incremental query and self
  (remove-hook 'post-command-hook #'khoj--incremental-search)
  (remove-hook 'minibuffer-exit-hook #'khoj--teardown-incremental-search))


;;;###autoload
(defun khoj ()
  "Natural, Incremental Search for your personal notes, transactions and music."
  (interactive)
  (let* ((khoj-buffer-name (get-buffer-create khoj--buffer-name)))
    ;; set khoj search type to last used or based on current buffer
    (setq khoj--search-type (or khoj--search-type (khoj--buffer-name-to-search-type (buffer-name))))
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
          (add-hook 'post-command-hook #'khoj--incremental-search) ; do khoj incremental search after every user action
          (add-hook 'minibuffer-exit-hook #'khoj--teardown-incremental-search)) ; teardown khoj incremental search on minibuffer exit
      (read-string khoj--query-prompt))))

;;;###autoload
(defun khoj-simple (query)
  "Natural Search for QUERY on your personal notes, transactions, music and images."
  (interactive "s🦅Khoj: ")
  (let* ((rerank "true")
         (default-type (khoj--buffer-name-to-search-type (buffer-name)))
         (search-type (completing-read "Type: " '("org" "markdown" "ledger" "music" "image") nil t default-type))
         (query-url (khoj--construct-api-query query search-type rerank))
         (buffer-name (get-buffer-create (format "*%s (q:%s t:%s)*" khoj--buffer-name query search-type))))
    (khoj--query-api-and-render-results
        query
        search-type
        query-url
        buffer-name)
    (switch-to-buffer buffer-name)))

(provide 'khoj)

;;; khoj.el ends here
