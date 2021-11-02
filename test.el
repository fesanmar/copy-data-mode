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
(defconst snippet-description-a "ASnippetDescription")
(defconst snippet-data-a "ASnippetData")
(defconst snippet-a `(,snippet-key-a ,snippet-description-a ,snippet-data-a))

;; Group t
(defconst group-key-t "t")
(defconst group-description-t "TGroupDescription")
(defconst group-t `(,group-key-t ,group-description-t))

(defconst snippet-key-ta "ta")
(defconst snippet-description-ta "ASnippetFromTGroup")
(defconst snippet-group-t-data-a "JustData")
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
  (let* ((copy-data-hot-edit-enable nil)
	 (abstract-query "%s [%s]: %s,  [%s]: %s,  [%s]: %s")
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
  (let ((copy-data-hot-edit-enable nil)
	(copy-data-user-snippets `(,group-t ,snippet-a ,snippet-ta)))
    (let (kill-ring
	  (unread-command-events (listify-key-sequence (kbd "a"))))
      (copy-data-query)
      (should (string= (car kill-ring) snippet-data-a)))
    (let (kill-ring
	  (unread-command-events (listify-key-sequence (kbd "ta"))))
      (copy-data-query)
      (should (string= (car kill-ring) snippet-group-t-data-a)))
    (let (kill-ring
	  (copy-data-user-snippets `(,group-t ,snippet-a))
	  (unread-command-events (listify-key-sequence (kbd "t"))))
      (should-error (copy-data-query))
      (should-not kill-ring))
    (let* (kill-ring
	   (key-fail "o")
	   (unread-command-events (listify-key-sequence (kbd key-fail))))
      (should
       (string= (copy-data-query)
		(format copy-data--not-found-msg key-fail)))
      (should-not kill-ring))
    (let (kill-ring
	  (copy-data-user-snippets nil))
      (should-error (copy-data-query))
      (should-not kill-ring))))

(defmacro with-hot-edit-fixtures (&rest body)
  `(let ((test-custom-file (make-temp-file "hot-edit-custom" nil ".el")))
     (unwind-protect
	 (let ((copy-data-user-snippets nil)
	       (copy-data-hot-edit-enable t)
	       (copy-data-hot-edit-add-key ?+)
	       (custom-file test-custom-file))
	   ,@body)
       (delete-file test-custom-file))))

;;; Test hot-edit

(defun add-element-string (element &optional prefix)
  "Creates string of events for adding a new element.

Will create something like \"saTest\rATest\r\". This is the key
event that creates a new snippet key the key \"a\", the
description \"aTest\" and the data \"ATest\"."
  (let ((snippet-p (copy-data--snippet-p element))
	(return "\r"))
    (concat
     (or prefix "")
     (string copy-data-hot-edit-add-key)
     (if snippet-p "s" "g")
     (copy-data--key element)
     (copy-data--description element)
     return
     (if snippet-p
	 (concat (copy-data--data element) return)
       ""))))

(ert-deftest copy-data-hot-edit-add ()
  "If `copy-data-hot-edit-enable' is t should add an element."
  ;; Given hot edit disable When adding snippet Then will signal error
  (with-hot-edit-fixtures
   (let (kill-ring
	 (unread-command-events (listify-key-sequence (kbd "+")))
	 (copy-data-hot-edit-enable nil))
     (should-error (copy-data-query))
     (should-not kill-ring)))
  ;; Given hot edit enabled When adding snippet Then will be added
  (with-hot-edit-fixtures
   (let ((unread-command-events
	  (listify-key-sequence (kbd (add-element-string snippet-a)))))
     (copy-data-query)
     (should (equal `(,snippet-a) copy-data-user-snippets))))
  ;; Given hot edit enabled and an existing group When adding a
  ;; snippet inside it Then will be added
  (with-hot-edit-fixtures
   (let ((copy-data-user-snippets `(,group-t))
	 (unread-command-events
	  (listify-key-sequence (kbd (add-element-string snippet-a "t")))))
     (copy-data-query)
     (should (equal
	      `(,group-t ("ta" ,snippet-description-a ,snippet-data-a))
	      copy-data-user-snippets))))
  ;; Given hot edit disable When adding a group Then a snippet will be added
  (with-hot-edit-fixtures
   (let ((unread-command-events
	  (listify-key-sequence (kbd (add-element-string group-t)))))
     (copy-data-query)
     (should (equal `(,group-t) copy-data-user-snippets))))
  ;; Given hot edit enable and existing snippet When adding a snippet
  ;; with same key as existing element in the group Then will singal
  ;; error
  (with-hot-edit-fixtures
   (let ((copy-data-user-snippets `(snippet-a))
	 (unread-command-events
	  (listify-key-sequence (kbd (add-element-string snippet-a)))))
     (should-error (copy-data-query))
     (should (equal `(snippet-a) copy-data-user-snippets))))
  ;; Given hot edit enable and existing snippet When adding a group
  ;; with same key as existing element in the group Then will singal
  ;; error
  (with-hot-edit-fixtures
   (let ((copy-data-user-snippets `(snippet-a))
	 (unread-command-events
	  (listify-key-sequence (kbd (concat (string copy-data-hot-edit-add-key)
					     "gaTestGroup")))))
     (should-error (copy-data-query))
     (should (equal `(snippet-a)
		    copy-data-user-snippets))))
  ;; Given hot edit enabled and an existing group and a snippet insde
  ;; When adding a snippet inside that group with same key as the
  ;; exsiting one Then will signal error
  (with-hot-edit-fixtures
   (let ((copy-data-user-snippets `(,group-t ("ta" ,snippet-description-a ,snippet-data-a)))
	 (unread-command-events
	  (listify-key-sequence (kbd (add-element-string snippet-a "t")))))
     (should-error (copy-data-query))
     (should (equal
	      `(,group-t ("ta" ,snippet-description-a ,snippet-data-a))
	      copy-data-user-snippets)))))
