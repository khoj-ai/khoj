;;; khoj.el --- Natural Search via Emacs

;; Copyright (C) 2021-2022 Debanjum Singh Solanky

;; Author: Debanjum Singh Solanky <debanjum@gmail.com>
;; Version: 1.0
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

;; This package provides natural language search on org-mode notes,
;; markdown files, beancount transactions and images.
;; It is a wrapper that interfaces with transformer based ML models.
;; The models search capabilities are exposed via the Khoj HTTP API

;;; Code:

(require 'url)
(require 'json)

(defcustom khoj--server-url "http://localhost:8000"
  "Location of Khoj API server."
  :group 'khoj
  :type 'string)

(defcustom khoj--image-width 156
  "Width of rendered images returned by Khoj"
  :group 'khoj
  :type 'integer)

(defun khoj--extract-entries-as-markdown (json-response query)
  "Convert json response from API to markdown entries"
  ;; remove leading (, ) or SPC from extracted entries string
  (replace-regexp-in-string
   "^[\(\) ]" ""
   ;; extract entries from response as single string and convert to entries
   (format "# %s\n%s"
           query
           (mapcar
            (lambda (args) (format "%s" (cdr (assoc 'entry args))))
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
            (lambda (args) (format "%s" (cdr (assoc 'entry args))))
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

(defun khoj--construct-api-query (query search-type)
  (let ((encoded-query (url-hexify-string query)))
    (format "%s/search?q=%s&t=%s" khoj--server-url encoded-query search-type)))

;;;###autoload
(defun khoj (query)
  "Search your content naturally using the Khoj API"
  (interactive "sQuery: ")
  (let* ((default-type (khoj--buffer-name-to-search-type (buffer-name)))
         (search-type (completing-read "Type: " '("org" "markdown" "ledger" "music" "image") nil t default-type))
         (url (khoj--construct-api-query query search-type))
         (buff (get-buffer-create (format "*Khoj (q:%s t:%s)*" query search-type))))
    ;; get json response from api
    (with-current-buffer buff
      (let ((inhibit-read-only t))
        (erase-buffer)
        (url-insert-file-contents url)))
    ;; render json response into formatted entries
    (with-current-buffer buff
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
      (read-only-mode t))
    (switch-to-buffer buff)))

(provide 'khoj)

;;; khoj.el ends here
