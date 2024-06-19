;;; khoj-tests.el --- Test suite for khoj.el  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Debanjum Singh Solanky

;; Author: Debanjum Singh Solanky <debanjum@gmail.com>
;; Version: 0.0.0
;; Package-Requires: ((emacs "27.1") (transient "0.3.0") (dash "2.19.1") (org "9.0.0"))
;; URL: https://github.com/khoj-ai/khoj/tree/master/src/interface/emacs

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

(require 'dash)
(require 'ert)
(require 'khoj)
(require 'org)



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
\n"))))



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


;; -------------------------------------
;; Test Helpers to Index Content
;; -------------------------------------

(ert-deftest khoj-tests--render-files-to-add-request-body ()
  "Test files are formatted into a multi-part http request body"
  (let ((upgrade-file (make-temp-file "upgrade" nil ".org" "# Become God\n## Upgrade\n\nPenance to Immortality\n\n"))
        (act-file (make-temp-file "act" nil ".org" "## Act\n\nRule everything\n\n")))
    (unwind-protect
        (progn
          (should
           (equal
            (khoj--render-update-files-as-request-body (list upgrade-file act-file) "khoj")
            (format
             "\n--khoj\r\n\
Content-Disposition: form-data; name=\"files\"; filename=\"%s\"\r\n\
Content-Type: text/org\r\n\r\n\
# Become God\n\
## Upgrade\n\n\
Penance to Immortality\n\n\r
--khoj\r\n\
Content-Disposition: form-data; name=\"files\"; filename=\"%s\"\r\n\
Content-Type: text/org\r\n\r\n\
## Act\n\n\
Rule everything\n\n\r\n\
--khoj--\r\n" upgrade-file act-file))))
      (delete-file upgrade-file)
      (delete-file act-file))))

(ert-deftest khoj-tests--render-files-to-add-delete-in-request-body ()
  "Test files are formatted into a multi-part http request body"
  (let ((upgrade-file (make-temp-file "upgrade" nil ".org" "# Become God\n## Upgrade\n\nPenance to Immortality\n\n"))
        (act-file (make-temp-file "act" nil ".org" "## Act\n\nRule everything\n\n")))
    (unwind-protect
        (progn
          (should
           (equal
            (khoj--render-delete-files-as-request-body (list upgrade-file act-file "/tmp/deleted-file.org") "khoj")
            (format
             "\n--khoj\r\n\
Content-Disposition: form-data; name=\"files\"; filename=\"%s\"\r\n\
Content-Type: text/org\r\n\r\n\
\r
--khoj\r\n\
Content-Disposition: form-data; name=\"files\"; filename=\"%s\"\r\n\
Content-Type: text/org\r\n\r\n\
\r
--khoj\r\n\
Content-Disposition: form-data; name=\"files\"; filename=\"%s\"\r\n\
Content-Type: text/org\r\n\r\n\
\r
--khoj--\r\n" upgrade-file act-file "/tmp/deleted-file.org"))))
      (delete-file upgrade-file)
      (delete-file act-file))))

(ert-deftest khoj-tests--extract-online-references ()
  (let* (;; Arrange
         (onlineContext '((Albert\ Einstein\ wife
                           (organic . [((link . "https://en.wikipedia.org/wiki/Mileva_Mari%C4%87")
                                        (title . "Mileva Marić - Wikipedia")
                                        (snippet . "Marić married Einstein in 1903 and they had three ...")
                                        (position . 1)
                                        (sitelinks . [((link . "https://en.wikipedia.org/wiki/Mileva_Mari%C4%87#Bi...")
                                                       (title . "Biography"))
                                                      ((link . "https://en.wikipedia.org/wiki/Mileva_Mari%C4%87#De...")
                                                       (title . "Debate over collaboration with..."))]))
                                       ((link . "https://en.wikipedia.org/wiki/Elsa_Einstein")
                                        (title . "Elsa Einstein - Wikipedia")
                                        (snippet . "Elsa Einstein (18 January 1876 – 20 December 1936)...")
                                        (position . 2))
                                       ((link . "https://www.amazon.com/Einsteins-Wife-Story-Mileva...")
                                        (price . 25.86)
                                        (title . "Einstein's Wife: The Real Story of Mileva Einstein...")
                                        (rating . 3.8)
                                        (snippet . "Albert Einstein's first wife, Mileva Einstein-Mari...")
                                        (currency . "$")
                                        (position . 3)
                                        (ratingCount . 80))])
                           (peopleAlsoAsk . [((link . "https://en.wikipedia.org/wiki/Mileva_Mari%C4%87")
                                              (title . "Mileva Marić - Wikipedia")
                                              (snippet . "Death. Mileva Marić suffered a severe stroke and d...")
                                              (question . "What happened to Einstein's first wife?"))
                                             ((link . "https://www.redalyc.org/journal/5117/511767145014/...")
                                              (title . "The story of Mileva Marić: Did Einstein's first wi...")
                                              (snippet . "The couple were married in 1903. Some claim that M...")
                                              (question . "Did Albert Einstein's wife do all his work?"))
                                             ((link . "https://en.wikipedia.org/wiki/Hans_Albert_Einstein")
                                              (title . "Hans Albert Einstein - Wikipedia")
                                              (snippet . "Klaus Martin Einstein (1932–1939), died of diphthe...")
                                              (question . "What happened to Einstein's children?"))])
                           (knowledgeGraph (type . "Theoretical physicist")
                                           (title . "Albert Einstein")
                                           (attributes (Born . "March 14, 1879, Ulm, Germany")
                                                       (Died . "April 18, 1955 (age 76 years), Princeton, NJ")
                                                       (Spouse . "Elsa Einstein (m. 1919–1936) and Mileva Marić (m. ...")
                                                       (Children . "Eduard Einstein, Hans Albert Einstein, and Lieserl...")
                                                       (Education . "University of Zurich (1905), ETH Zürich (1897–1900...")
                                                       (Nationality . "American, German, Hungarian, and more")
                                                       (Grandchildren . "Evelyn Einstein, Bernhard Caesar Einstein, Klaus M..."))
                                           (description . "Albert Einstein was a German-born theoretical phys...")
                                           (descriptionLink . "https://en.wikipedia.org/wiki/Albert_Einstein")
                                           (descriptionSource . "Wikipedia")))
                          (Prince\ Albert\ spouse
                           (webpages (link . "https://en.wikipedia.org/wiki/Prince_Albert_of_Sax...") (snippet . "Prince Albert of Saxe-Coburg and Gotha was the hus..."))
                           (organic . [((date . "Feb 4, 2024")
                                        (link . "https://en.wikipedia.org/wiki/Prince_Albert_of_Sax...")
                                        (title . "Prince Albert of Saxe-Coburg and Gotha - Wikipedia")
                                        (snippet . "Prince Albert of Saxe-Coburg and Gotha was the hus...")
                                        (position . 1)
                                        (sitelinks . [((link . "https://en.wikipedia.org/wiki/Prince_consort")
                                                       (title . "Prince consort"))
                                                      ((link . "https://en.wikipedia.org/wiki/Nellie_Clifden")
                                                       (title . "Nellie Clifden"))]))])
                           (answerBox (title . "Prince Albert of Saxe-Coburg and Gotha / Spouse")
                                      (answer . "Queen Victoria"))
                           (peopleAlsoAsk . [((link . "https://en.wikipedia.org/wiki/Albert_II,_Prince_of...")
                                              (title . "Albert II, Prince of Monaco - Wikipedia")
                                              (snippet . "In July 2011, Prince Albert married South African ...")
                                              (question . "How many children does Prince Albert of Monaco hav..."))]))))
         ;; Act
         (result (khoj--extract-online-references
                  '(organic knowledgeGraph peopleAlsoAsk webpages)
                  onlineContext)))
    ;; Assert
    (progn
      (should (equal (length result) 10))
      result)))

(provide 'khoj-tests)

;;; khoj-tests.el ends here
