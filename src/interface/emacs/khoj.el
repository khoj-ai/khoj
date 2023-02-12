;;; khoj.el --- Natural, Incremental Search for your Second Brain -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Debanjum Singh Solanky

;; Author: Debanjum Singh Solanky <debanjum@gmail.com>
;; Description: Natural, Incremental Search for your Second Brain
;; Keywords: search, org-mode, outlines, markdown, beancount, ledger, image
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1") (transient "0.3.0"))
;; URL: https://github.com/debanjum/khoj/tree/master/src/interface/emacs

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
;; 3. Install khoj.el from MELPA Stable
;;    (use-package khoj :pin melpa-stable :bind ("C-c s" . 'khoj))

;;; Code:

(require 'url)
(require 'json)
(require 'transient)
(require 'outline)
(eval-when-compile (require 'subr-x)) ;; for string-trim before Emacs 28.2


;; -------------------------
;; Khoj Static Configuration
;; -------------------------

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

(defcustom khoj-default-content-type "org"
  "The default content type to perform search on."
  :group 'khoj
  :type '(choice (const "org")
                 (const "markdown")
                 (const "ledger")
                 (const "image")
                 (const "music")))


;; --------------------------
;; Khoj Dynamic Configuration
;; --------------------------

(defvar khoj--minibuffer-window nil
  "Minibuffer window used to enter query.")

(defconst khoj--query-prompt "🦅Khoj: "
  "Query prompt shown in the minibuffer.")

(defconst khoj--buffer-name "*🦅Khoj*"
  "Name of buffer to show results from Khoj.")

(defvar khoj--content-type "org"
  "The type of content to perform search on.")

(declare-function beancount-mode "beancount" ())
(declare-function markdown-mode "markdown-mode" ())
(declare-function org-music-mode "org-music" ())
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
     (when (member 'ledger enabled-content-types)
       "C-x l  | ledger\n")
     (when (member 'image enabled-content-types)
       "C-x i  | image\n")
     (when (member 'music enabled-content-types)
       "C-x M  | music\n"))))

(defvar khoj--rerank nil "Track when re-rank of results triggered.")
(defun khoj--search-markdown () "Set content-type to `markdown'." (interactive) (setq khoj--content-type "markdown"))
(defun khoj--search-org () "Set content-type to `org-mode'." (interactive) (setq khoj--content-type "org"))
(defun khoj--search-ledger () "Set content-type to `ledger'." (interactive) (setq khoj--content-type "ledger"))
(defun khoj--search-images () "Set content-type to image." (interactive) (setq khoj--content-type "image"))
(defun khoj--search-music () "Set content-type to music." (interactive) (setq khoj--content-type "music"))
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


;; -----------------------------------------------
;; Extract and Render Entries of each Content Type
;; -----------------------------------------------

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
  (let ((org-results-buffer-format-str "* %s\n%s\n#+STARTUP: showall hidestars inlineimages"))
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
      (format org-results-buffer-format-str query)
      ;; remove leading (, ) or SPC from extracted entries string
      (replace-regexp-in-string "^[\(\) ]" ""))))

(defun khoj--extract-entries-as-ledger (json-response query)
  "Convert JSON-RESPONSE, QUERY from API to ledger entries."
  (thread-last json-response
               ;; extract and render entries from API response
               (mapcar (lambda (args) (format "%s\n\n" (cdr (assoc 'entry args)))))
               ;; Set query as heading in rendered results buffer
               (format ";; %s\n\n%s\n" query)
               ;; remove leading (, ) or SPC from extracted entries string
               (replace-regexp-in-string "^[\(\) ]" "")
               ;; remove trailing (, ) or SPC from extracted entries string
               (replace-regexp-in-string "[\(\) ]$" "")))

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

(defun khoj--buffer-name-to-content-type (buffer-name)
  "Infer content type based on BUFFER-NAME."
  (let ((enabled-content-types (khoj--get-enabled-content-types))
        (file-extension (file-name-extension buffer-name)))
    (cond
     ((and (member 'music enabled-content-types) (equal buffer-name "Music.org")) "music")
     ((and (member 'ledger enabled-content-types) (or (equal file-extension "bean") (equal file-extension "beancount"))) "ledger")
     ((and (member 'org enabled-content-types) (equal file-extension "org")) "org")
     ((and (member 'markdown enabled-content-types) (or (equal file-extension "markdown") (equal file-extension "md"))) "markdown")
     (t khoj-default-content-type))))


;; --------------
;; Query Khoj API
;; --------------

(defun khoj--get-enabled-content-types ()
  "Get content types enabled for search from API."
  (let ((config-url (format "%s/api/config/data" khoj-server-url))
        (url-request-method "GET"))
    (with-temp-buffer
      (erase-buffer)
      (url-insert-file-contents config-url)
      (let* ((json-response (json-parse-buffer :object-type 'alist))
            (content-type (cdr (assoc 'content-type json-response))))
        ;; return content-type items with configuration
        (mapcar
         #'car
         (cl-remove-if-not
          (lambda (a) (not (eq (cdr a) :null)))
          content-type))))))

(defun khoj--construct-api-query (query content-type &optional rerank)
  "Construct API Query from QUERY, CONTENT-TYPE and (optional) RERANK params."
  (let ((rerank (or rerank "false"))
        (encoded-query (url-hexify-string query)))
    (format "%s/api/search?q=%s&t=%s&r=%s&n=%s" khoj-server-url encoded-query content-type rerank khoj-results-count)))

(defun khoj--query-api-and-render-results (query-url content-type query buffer-name)
  "Query Khoj QUERY-URL. Render results in BUFFER-NAME using QUERY, CONTENT-TYPE."
  ;; get json response from api
  (with-current-buffer buffer-name
    (let ((inhibit-read-only t)
          (url-request-method "GET"))
      (erase-buffer)
      (url-insert-file-contents query-url)))
  ;; render json response into formatted entries
  (with-current-buffer buffer-name
    (let ((inhibit-read-only t)
          (json-response (json-parse-buffer :object-type 'alist)))
      (erase-buffer)
      (insert
       (cond ((or (equal content-type "org") (equal content-type "music")) (khoj--extract-entries-as-org json-response query))
             ((equal content-type "markdown") (khoj--extract-entries-as-markdown json-response query))
             ((equal content-type "ledger") (khoj--extract-entries-as-ledger json-response query))
             ((equal content-type "image") (khoj--extract-entries-as-images json-response query))
             (t (format "%s" json-response))))
      (cond ((equal content-type "org") (progn (org-mode)
                                               (visual-line-mode)))
            ((equal content-type "markdown") (progn (markdown-mode)
                                                    (visual-line-mode)))
            ((equal content-type "ledger") (beancount-mode))
            ((equal content-type "music") (progn (org-mode)
                                                (org-music-mode)))
            ((equal content-type "image") (progn (shr-render-region (point-min) (point-max))
                                                (goto-char (point-min))))
            (t (fundamental-mode))))
    (read-only-mode t)))


;; ------------------
;; Incremental Search
;; ------------------

(defun khoj--incremental-search (&optional rerank)
  "Perform Incremental Search on Khoj. Allow optional RERANK of results."
  (let* ((rerank-str (cond (rerank "true") (t "false")))
         (khoj-buffer-name (get-buffer-create khoj--buffer-name))
         (query (minibuffer-contents-no-properties))
         (query-url (khoj--construct-api-query query khoj--content-type rerank-str)))
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

(defun khoj-incremental ()
  "Natural, Incremental Search for your personal notes, transactions and music."
  (interactive)
  (let* ((khoj-buffer-name (get-buffer-create khoj--buffer-name)))
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
         (query-url (khoj--construct-api-query query content-type rerank))
         ;; extract heading to show in result buffer from query
         (query-title
          (format "Similar to: %s"
                  (replace-regexp-in-string "^[#\\*]* " "" (car (split-string query "\n")))))
         (buffer-name (get-buffer-create khoj--buffer-name)))
    (progn
      (khoj--query-api-and-render-results
       query-url
       content-type
       query-title
       buffer-name)
      (switch-to-buffer buffer-name)
      (goto-char (point-min)))))


;; ---------
;; Khoj Menu
;; ---------

(transient-define-argument khoj--content-type-switch ()
  :class 'transient-switches
  :argument-format "--content-type=%s"
  :argument-regexp ".+"
  ;; set content type to: last used > based on current buffer > default type
  :init-value (lambda (obj) (oset obj value (format "--content-type=%s" (or khoj--content-type (khoj--buffer-name-to-content-type (buffer-name))))))
  ;; dynamically set choices to content types enabled on khoj backend
  :choices (or (ignore-errors (mapcar #'symbol-name (khoj--get-enabled-content-types))) '("org" "markdown" "ledger" "music" "image")))

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
         (update-url (format "%s/api/update?t=%s&force=%s" khoj-server-url content-type force-update))
         (url-request-method "GET"))
    (progn
      (setq khoj--content-type content-type)
      (url-retrieve update-url (lambda (_) (message "Khoj %s index %supdated!" content-type (if (member "--force-update" args) "force " "")))))))

(transient-define-prefix khoj-menu ()
  "Create Khoj Menu to Configure and Execute Commands."
  [["Configure General"
    ("t" "Content Type" khoj--content-type-switch)]
   ["Configure Search"
    ("n" "Results Count" "--results-count=" :init-value (lambda (obj) (oset obj value (format "%s" khoj-results-count))))]
   ["Configure Update"
    ("-f" "Force Update" "--force-update")]]
  [["Act"
    ("s" "Search" khoj--search-command)
    ("f" "Find Similar" khoj--find-similar-command)
    ("u" "Update" khoj--update-command)
    ("q" "Quit" transient-quit-one)]])


;; ----------
;; Entrypoint
;; ----------

;;;###autoload
(defun khoj ()
  "Natural, Incremental Search for your personal notes, transactions and images."
  (interactive)
  (khoj-menu))

(provide 'khoj)

;;; khoj.el ends here
