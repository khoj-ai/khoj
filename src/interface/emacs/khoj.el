;;; khoj.el --- Natural, Incremental Search via Emacs

;; Copyright (C) 2021-2022 Debanjum Singh Solanky

;; Author: Debanjum Singh Solanky <debanjum@gmail.com>
;; Version: 2.0
;; Keywords: search, org-mode, outlines, markdown, image
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
;; org-mode notes, markdown files, beancount transactions and images.
;; It is a wrapper that interfaces with transformer based ML models.
;; The models search capabilities are exposed via the Khoj HTTP API.

;;; Code:

(require 'url)
(require 'json)

(defcustom khoj--server-url "http://localhost:8000"
  "Location of Khoj API server."
  :group 'khoj
  :type 'string)

(defcustom khoj--image-width 156
  "Width of rendered images returned by Khoj."
  :group 'khoj
  :type 'integer)

(defcustom khoj--rerank-after-idle-time 1.0
  "Idle time (in seconds) to trigger cross-encoder to rerank incremental search results."
  :group 'khoj
  :type 'float)

(defcustom khoj--results-count 5
  "Number of results to get from Khoj API for each query."
  :group 'khoj
  :type 'integer)

(defvar khoj--rerank-timer nil
  "Idle timer to make cross-encoder re-rank incremental search results if user idle.")

(defvar khoj--minibuffer-window nil
  "Minibuffer window being used by user to enter query.")

(defconst khoj--query-prompt "🦅Khoj: "
  "Query prompt shown to user in the minibuffer.")

(defvar khoj--search-type "org"
  "The type of content to perform search on.")

(defun khoj--extract-entries-as-markdown (json-response query)
  "Convert json response from API to markdown entries"
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
  "Convert json response from API to org-mode entries"
  ;; remove leading (, ) or SPC from extracted entries string
  (replace-regexp-in-string
   "^[\(\) ]" ""
   ;; extract entries from response as single string and convert to entries
   (format "* %s\n%s"
           query
           (mapcar
            (lambda (args)
              (replace-regexp-in-string
               "^\*+" "**"
               (format "%s" (cdr (assoc 'entry args)))))
              json-response))))

(defun khoj--extract-entries-as-images (json-response query)
  "Convert json response from API to html with images"
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
                             "\n\n<h2>Score: %s Meta: %s Image: %s</h2>\n\n<a href=\"%s%s\">\n<img src=\"%s%s?%s\" width=100 height=100>\n</a>"
                             (cdr (assoc 'score args))
                             (cdr (assoc 'metadata_score args))
                             (cdr (assoc 'image_score args))
                             khoj--server-url
                             (cdr (assoc 'entry args))
                             khoj--server-url
                             (cdr (assoc 'entry args))
                             (random 10000)))
             json-response)))))

(defun khoj--extract-entries-as-ledger (json-response query)
  "Convert json response from API to ledger entries"
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
  (let ((file-extension (file-name-extension buffer-name)))
    (cond
     ((equal buffer-name "Music.org") "music")
     ((equal file-extension "bean") "ledger")
     ((equal file-extension "org") "org")
     ((or (equal file-extension "markdown") (equal file-extension "md")) "markdown")
     (t "org"))))

(defun khoj--construct-api-query (query search-type &optional rerank)
  (let ((rerank (or rerank "false"))
        (results-count (or khoj--results-count 5))
        (encoded-query (url-hexify-string query)))
    (format "%s/search?q=%s&t=%s&r=%s&n=%s" khoj--server-url encoded-query search-type rerank results-count)))

(defun khoj--query-api-and-render-results (query search-type query-url buffer-name)
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


;; Incremental Search on Khoj
(defun khoj--incremental-search (&optional rerank)
  (let* ((rerank-str (cond (rerank "true") (t "false")))
         (search-type khoj--search-type)
         (buffer-name (get-buffer-create (format "*Khoj (t:%s)*" search-type)))
         (query (minibuffer-contents-no-properties))
         (query-url (khoj--construct-api-query query search-type rerank-str)))
    ;; Query khoj API only when user in khoj minibuffer.
    ;; Prevents querying during recursive edits or with contents of other buffers user may jump to
    (when (and (active-minibuffer-window) (equal (current-buffer) khoj--minibuffer-window))
      (progn
        (when rerank
          (message "[Khoj]: Rerank Results"))
        (khoj--query-api-and-render-results
         query
         search-type
         query-url
         buffer-name)))))

(defun delete-open-network-connections-to-khoj ()
  "Delete all network connections to khoj server"
  (dolist (proc (process-list))
    (let ((proc-buf (buffer-name (process-buffer proc)))
          (khoj-network-proc-buf (string-join (split-string khoj--server-url "://") " ")))
      (when (string-match (format "%s" khoj-network-proc-buf) proc-buf)
        (delete-process proc)))))

(defun khoj--teardown-incremental-search ()
  (message "[Khoj]: Teardown Incremental Search")
  ;; remove advice to rerank results on normal exit from minibuffer
  (advice-remove 'exit-minibuffer #'khoj--minibuffer-exit-advice)
  ;; unset khoj minibuffer window
  (setq khoj--minibuffer-window nil)
  ;; cancel rerank timer
  (when (timerp khoj--rerank-timer)
    (cancel-timer khoj--rerank-timer))
  ;; delete open connections to khoj
  (delete-open-network-connections-to-khoj)
  ;; remove hooks for khoj incremental query and self
  (remove-hook 'post-command-hook #'khoj--incremental-search)
  (remove-hook 'minibuffer-exit-hook #'khoj--teardown-incremental-search))

(defun khoj--minibuffer-exit-advice (&rest _args)
  (khoj--incremental-search t))

;;;###autoload
(defun khoj ()
  "Natural, Incremental Search for your personal notes, transactions and music using Khoj"
  (interactive)
  (let* ((default-type (khoj--buffer-name-to-search-type (buffer-name)))
         (search-type (completing-read "Type: " '("org" "markdown" "ledger" "music") nil t default-type))
         (buffer-name (get-buffer-create (format "*Khoj (t:%s)*" search-type))))
    (setq khoj--search-type search-type)
    ;; setup rerank to improve results once user idle for KHOJ--RERANK-AFTER-IDLE-TIME seconds
    (setq khoj--rerank-timer (run-with-idle-timer khoj--rerank-after-idle-time t 'khoj--incremental-search t))
    ;; switch to khoj results buffer
    (switch-to-buffer buffer-name)
    ;; open and setup minibuffer for incremental search
    (minibuffer-with-setup-hook
        (lambda ()
          ;; set current (mini-)buffer entered as khoj minibuffer
          ;; used to query khoj API only when user in khoj minibuffer
          (setq khoj--minibuffer-window (current-buffer))
          ;; rerank results on normal exit from minibuffer
          (advice-add 'exit-minibuffer :before #'khoj--minibuffer-exit-advice)
          (add-hook 'post-command-hook #'khoj--incremental-search) ; do khoj incremental search after every user action
          (add-hook 'minibuffer-exit-hook #'khoj--teardown-incremental-search)) ; teardown khoj incremental search on minibuffer exit
      (read-string khoj--query-prompt))))

;;;###autoload
(defun khoj-simple (query)
  "Natural Search for QUERY in your personal notes, transactions, music and images using Khoj"
  (interactive "s🦅Khoj: ")
  (let* ((rerank "true")
         (default-type (khoj--buffer-name-to-search-type (buffer-name)))
         (search-type (completing-read "Type: " '("org" "markdown" "ledger" "music" "image") nil t default-type))
         (query-url (khoj--construct-api-query query search-type rerank))
         (buffer-name (get-buffer-create (format "*Khoj (q:%s t:%s)*" query search-type))))
    (khoj--query-api-and-render-results
        query
        search-type
        query-url
        buffer-name)
    (switch-to-buffer buffer-name)))

(provide 'khoj)

;;; khoj.el ends here
