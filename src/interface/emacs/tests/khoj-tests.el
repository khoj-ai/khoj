;;; khoj-tests.el --- Test suite for khoj.el  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Debanjum Singh Solanky

;; Author: Debanjum Singh Solanky <debanjum@gmail.com>
;; Version: 0.0.0
;; Package-Requires: ((emacs "27.1") (transient "0.3.0"))
;; URL: https://github.com/debanjum/khoj/tree/master/src/interface/emacs

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

;; This file contains the test suite for khoj.el.

;;; Code:

(require 'ert)
(require 'khoj)



;; ----------------------------------------------------
;; Test Extract and Render Entries of each Content Type
;; ----------------------------------------------------

(ert-deftest khoj-tests--extract-entries-as-markdown ()
  "Test `json-response', `query' from API formatted as markdown."
  (let ((user-query "Become God")
        (json-response-from-khoj-backend
         (json-read-from-string
          "[\
{\
  \"entry\": \"## Upgrade\\n\\n Penance to Immortality\",\
  \"score\": \"0.376\",\
  \"additional\": {\
    \"file\": \"/home/ravan/upgrade.md\",\
    \"compiled\": \"## Upgrade Penance to Immortality\"\
  }\
},\
{\
  \"entry\": \"## Act\\n\\n Rule everything\",\
  \"score\": \"0.153\",\
  \"additional\": {\
    \"file\": \"/home/ravan/act.md\",\
    \"compiled\": \"## Act Rule everything\"\
  }\
}]\
")))
    (should
     (equal
      (khoj--extract-entries-as-markdown json-response-from-khoj-backend user-query)
      "\
# Become God\n\
## Upgrade\n\
\n\
Penance to Immortality\n\n\
## Act\n\
\n\
Rule everything\n\n"))))


(ert-deftest khoj-tests--extract-entries-as-org ()
  "Test `json-response', `query' from API formatted as org."
  (let ((user-query "Become God")
        (json-response-from-khoj-backend
         (json-read-from-string
          "[\
{\
  \"entry\": \"** Upgrade\\n\\n Penance to Immortality\\n\",\
  \"score\": \"0.42\",\
  \"additional\": {\
    \"file\": \"/home/ravan/upgrade.md\",\
    \"compiled\": \"** Upgrade Penance to Immortality\"\
  }\
},\
{\
  \"entry\": \"** Act\\n\\n Rule everything\\n\",\
  \"score\": \"0.42\",\
  \"additional\": {\
    \"file\": \"/home/ravan/act.md\",\
    \"compiled\": \"** Act Rule everything\"\
  }\
}]\
")))
    (should
     (equal
      (khoj--extract-entries-as-org json-response-from-khoj-backend user-query)
      "\
* Become God\n\
** Upgrade\n\
\n\
Penance to Immortality\n\
** Act\n\
\n\
Rule everything\n\
\n\
#+STARTUP: showall hidestars inlineimages"))))


(ert-deftest khoj-tests--extract-entries-as-ledger ()
  "Test `json-response', `query' from API formatted as beancount ledger."
  (let ((user-query "Become God")
        (json-response-from-khoj-backend
         (json-read-from-string
          "[\
{\
  \"entry\": \"4242-04-01 * \\\"Penance Center\\\" \\\"Book Stay for 10,000 Years\\\"\\n  Expenses:Health:Mental  15 GOLD\\n  Assets:Commodities:Gold\",\
  \"score\": \"0.42\",\
  \"additional\": {\
    \"file\": \"/home/ravan/ledger.beancount\",\
    \"compiled\": \"4242-04-01 * \\\"Penance Center\\\" \\\"Book Stay for 10,000 Years\\\"  Expenses:Health:Mental  15 GOLD Assets:Commodities:Gold\"\
  }\
},\
{\
  \"entry\": \"14242-04-01 * \\\"Brahma\\\" \\\"Boon for Invincibility from Higher Beings\\\"\\n  Income:Health  -1,00,00,000 LIFE\\n  Assets:Commodities:Life\",\
  \"score\": \"0.42\",\
  \"additional\": {\
    \"file\": \"/home/ravan/ledger.beancount\",\
    \"compiled\": \"4242-04-01 * \\\"Brahma\\\" \\\"Boon for Invincibility from Higher Beings\\\"  Income:Health  -1,00,00,000 LIFE Assets:Commodities:Life\"\
  }\
}]\
")))
    (should
     (equal
      (khoj--extract-entries-as-ledger json-response-from-khoj-backend user-query)
      ";; Become God\n\
\n\
4242-04-01 * \"Penance Center\" \"Book Stay for 10,000 Years\"\n\
 Expenses:Health:Mental  15 GOLD\n\
 Assets:Commodities:Gold\n\
\n\
14242-04-01 * \"Brahma\" \"Boon for Invincibility from Higher Beings\"\n\
 Income:Health  -1,00,00,000 LIFE\n\
 Assets:Commodities:Life\n\
\n\
\n\
"))))



;; -------------------------------------
;; Test Helpers for Find Similar Feature
;; -------------------------------------

(ert-deftest khoj-tests--get-current-outline-entry-text ()
  "Test get current outline-mode entry text'."
  (with-temp-buffer
    (insert "\
* Become God\n\
** Upgrade\n\
\n\
Penance to Immortality\n\
** Act\n\
\n\
Rule everything\\n")
    (goto-char (point-min))

    ;; Test getting current entry text from cursor at start of outline heading
    (outline-next-visible-heading 1)
    (should
     (equal
      (khoj--get-current-outline-entry-text)
      "\
** Upgrade\n\
\n\
Penance to Immortality"))

    ;; Test getting current entry text from cursor within outline entry
    (forward-line)
    (should
     (equal
      (khoj--get-current-outline-entry-text)
      "\
** Upgrade\n\
\n\
Penance to Immortality"))
    ))


(ert-deftest khoj-tests--get-current-paragraph-text ()
  "Test get current paragraph text'."
  (with-temp-buffer
    (insert "\
* Become God\n\
** Upgrade\n\
\n\
Penance to Immortality\n\
** Act\n\
\n\
Rule everything\n")
    ;; Test getting current paragraph text from cursor at start of buffer
    (goto-char (point-min))
    (should
     (equal
      (khoj--get-current-paragraph-text)
      "* Become God\n\
** Upgrade"))

    ;; Test getting current paragraph text from cursor within paragraph
    (goto-char (point-min))
    (forward-line 1)
    (should
     (equal
      (khoj--get-current-paragraph-text)
      "* Become God\n\
** Upgrade"))

    ;; Test getting current paragraph text from cursor at paragraph end
    (goto-char (point-min))
    (forward-line 2)
    (should
     (equal
      (khoj--get-current-paragraph-text)
      "* Become God\n\
** Upgrade"))

    ;; Test getting current paragraph text from cursor at start of middle paragraph
    (goto-char (point-min))
    (forward-line 3)
    (should
     (equal
      (khoj--get-current-paragraph-text)
      "Penance to Immortality\n\
** Act"))

    ;; Test getting current paragraph text from cursor at end of buffer
    (goto-char (point-max))
    (should
     (equal
      (khoj--get-current-paragraph-text)
      "Rule everything"))
    ))


(provide 'khoj-tests)

;;; khoj-tests.el ends here
