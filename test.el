;;; test.el --- Tests for copy-data-mode

;; Copyright (C) 2021 Felipe Santa Cruz Martínez Alcalá

;; THIS FILE IS SUBJECT TO CHANGE, AND NOT SUITABLE FOR DISTRIBUTION
;; BY PACKAGE MANAGERS SUCH AS APT, PKGSRC, MACPORTS, &C.

;; Copy Data Mode is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; Copy Data Mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Copy Data Mode. If not, see
;; <http://www.gnu.org/licenses/>.

(require 'ert)
(require 'copy-data-mode)
(message "Emacs version: %s" emacs-version)

;;; Test constants
;; Not grouped snipped
(defconst snippet-key-a "a")
(defconst snippet-description-a "A snippet description")
(defconst snippet-data-a "A snippet data")
(defconst snippet-a `(,snippet-key-a ,snippet-description-a ,snippet-data-a))

;; Group t
(defconst group-key-t "t")
(defconst group-description-t "T group description")
(defconst group-t `(,group-key-t ,group-description-t))

(defconst snippet-key-ta "ta")
(defconst snippet-description-ta "A snippet from T group")
(defconst snippet-group-t-data-a "just data")
(defconst snippet-ta `(,snippet-key-ta
		       ,snippet-description-ta
		       ,snippet-group-t-data-a))

;;; Test accesors and predicates
(ert-deftest copy-data-key-accesor ()
  "Calling `copy-data--key' on list should return its first
element"
  (should
   (string= (copy-data--key group-t) group-key-t))
  (should-not (copy-data--key nil)))

(ert-deftest copy-data-description-accesor ()
  "Calling `copy-data--description' on list should return its
second element"
  (should
   (string= (copy-data--description group-t)
	    group-description-t))
  (should-not (copy-data--description nil))
  (should-not (copy-data--description `(,group-key-t))))

(ert-deftest copy-data-group-predicate ()
  "`copy-data--group-p' returns t if list's length = 2"
  (should (copy-data--group-p group-t))
  (should-not (copy-data--group-p snippet-a))
  (should-not (copy-data--group-p '("just-key")))
  (should-not (copy-data--group-p `(,@snippet-a "something else"))))

(ert-deftest copy-data-snippet-predicate ()
  "`copy-data--snippet-p' returns t if list's length = 3"
  (should (copy-data--snippet-p snippet-a))
  (should-not (copy-data--snippet-p group-t))
  (should-not (copy-data--snippet-p '("just-key")))
  (should-not (copy-data--snippet-p `(,@snippet-a "something else"))))

(ert-deftest copy-data-create-query ()
  "`copy-data-create-query' should return query string"
  (defun last-key-char (el)
    (substring (copy-data--key el) -1))
  (let* ((abstract-query "%s [%s]: %s,  [%s]: %s,  [%s]: %s")
	 (propertized-group-key-t
	  (propertize group-key-t 'face 'copy-data-group-key))
	 (propertized-key-snippet-a
	  (propertize snippet-key-a 'face 'copy-data-snippet-key))
	 (propertized-key-snippet-ta
	  (propertize (last-key-char snippet-ta)
		      'face 'copy-data-snippet-key))
	 (formated-query
	  (format abstract-query
		  copy-data--query-head
		  propertized-group-key-t
		  group-description-t
		  propertized-key-snippet-a
		  snippet-description-a
		  propertized-key-snippet-ta
		  snippet-description-ta)))
    (let ((copy-data-query-sort #'copy-data-sort-no))
      (should
       (equal-including-properties
	(copy-data--create-query `(,group-t ,snippet-a ,snippet-ta))
	formated-query)))
    (let ((copy-data-query-sort #'copy-data-sort-by-groups))
      (should
       (equal-including-properties
	(copy-data--create-query `(,snippet-a ,group-t ,snippet-ta))
	formated-query)))
    (let ((copy-data-query-sort #'copy-data-sort-by-snippets))
      (should
       (equal-including-properties
	(copy-data--create-query `(,group-t ,snippet-a ,snippet-ta))
	(format abstract-query
		copy-data--query-head
		propertized-key-snippet-a
		snippet-description-a
		propertized-key-snippet-ta
		snippet-description-ta
		propertized-group-key-t
		group-description-t))))))

(ert-deftest copy-data-query-test ()
  "Copy `copy-data-query' interactively."
  (let ((copy-data-user-snippets `(,group-t ,snippet-a ,snippet-ta)))
    (let (kill-ring
	  (unread-command-events (listify-key-sequence (kbd "a"))))
      (copy-data-query)
      (should (string= (car kill-ring) snippet-data-a)))
    (let (kill-ring
	  (unread-command-events (listify-key-sequence (kbd "ta"))))
      (copy-data-query)
      (should (string= (car kill-ring) snippet-group-t-data-a)))
    (let (kill-ring
	  (unread-command-events (listify-key-sequence (kbd "o"))))
      (should
       (string= (copy-data-query)
		(format copy-data--not-found-msg "o"))))
    (let ((copy-data-user-snippets nil))
      (should-error (copy-data-query)))))
