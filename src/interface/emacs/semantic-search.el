;;; semantic-search.el --- Semantic search via Emacs

;; Copyright (C) 2021-2022 Debanjum Singh Solanky

;; Author: Debanjum Singh Solanky <debanjum@gmail.com>
;; Version: 0.1
;; Keywords: search, org-mode, outlines
;; URL: http://github.com/debanjum/semantic-search/interface/emacs

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

;; This package provides semantic search on org-mode files
;; It is a wrapper that interfaces with transformer based ML model
;; The models semantic search capabilities are exposed via an HTTP API

;;; Code:

(require 'url)
(require 'json)

(defcustom semantic-search--server-url "http://localhost:8000"
  "Location of semantic search API server."
  :group 'semantic-search
  :type 'string)

(defcustom semantic-search--image-width 156
  "Width of rendered images returned by semantic search"
  :group 'semantic-search
  :type 'integer)

(defun semantic-search--extract-entries-as-org (json-response query)
  "Convert json response from API to org-mode entries"
  ;; remove leading (, ) or SPC from extracted entries string
  (replace-regexp-in-string
   "^[\(\) ]" ""
   ;; extract entries from response as single string and convert to entries
   (format "* %s\n%s"
           query
           (mapcar
            (lambda (args) (format "%s" (cdr (assoc 'Entry args))))
            json-response))))

(defun semantic-search--extract-entries-as-images (json-response query)
  "Convert json response from API to org-mode entries with images"
  ;; remove leading (, ) or SPC from extracted entries string
  (replace-regexp-in-string
   "^[\(\) ]" ""
   ;; extract entries from response as single string and convert to entries
   (format "* %s \n%s"
           query
           (mapcar
            (lambda (args) (format "\n** \n  [[%s]]" (cdr (assoc 'Entry args))))
            json-response))))

(defun semantic-search--extract-entries-as-ledger (json-response)
  "Convert json response from API to ledger entries"
  ;; remove leading (, ) or SPC from extracted entries string
  (replace-regexp-in-string
   "^[\(\) ]" ""
   ;; extract entries from response as single string and convert to entries
   (format "%s"
           (mapcar
            (lambda (args) (format "* %s" (cdr (assoc 'Entry args))))
            json-response))))

(defun semantic-search--buffer-name-to-search-type (buffer-name)
  (let ((file-extension (file-name-extension buffer-name)))
    (cond
     ((equal buffer-name "Music.org") "music")
     ((equal file-extension "bean") "ledger")
     ((equal file-extension "org") "notes")
     (t "notes"))))

(defun semantic-search--construct-api-query (query search-type)
  (let ((encoded-query (url-hexify-string query)))
    (format "%s/search?q=%s&t=%s" semantic-search--server-url encoded-query search-type)))

;;;###autoload
(defun semantic-search (query)
  "Semantic search on org-mode content via semantic-search API"
  (interactive "sQuery: ")
  (let* ((default-type (semantic-search--buffer-name-to-search-type (buffer-name)))
         (search-type (completing-read "Type: " '("notes" "ledger" "music" "image") nil t default-type))
         (url (semantic-search--construct-api-query query search-type))
         (buff (get-buffer-create "*semantic-search*")))
    ;; get json response from api
    (with-current-buffer buff
      (let ((inhibit-read-only t))
        (erase-buffer)
        (url-insert-file-contents url)))
    ;; convert json response to org-mode entries
    (with-current-buffer buff
      (let ((inhibit-read-only t)
            (json-response (json-parse-buffer :object-type 'alist)))
        (erase-buffer)
        (insert
         (cond ((or (equal search-type "notes") (equal search-type "music")) (semantic-search--extract-entries-as-org json-response query))
               ((equal search-type "ledger") (semantic-search--extract-entries-as-ledger json-response))
               ((equal search-type "image") (semantic-search--extract-entries-as-images json-response query))
               (t (format "%s" json-response)))))
      (cond ((equal search-type "notes") (org-mode))
            ((equal search-type "music") (progn (org-mode)
                                                (org-music-mode)))
            ((equal search-type "image") (progn (org-mode)
                                                (setq org-image-actual-width semantic-search--image-width)
                                                (org-display-inline-images)))
            (t (fundamental-mode)))
      (read-only-mode t))
    (switch-to-buffer buff)))

(provide 'semantic-search)

;;; semantic-search.el ends here
