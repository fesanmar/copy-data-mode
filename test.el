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
(defconst key-group-t "t")
(defconst description-group-t "T group description")
(defconst group-t `(,key-group-t ,description-group-t))

;;; Test accesors and predicates
(ert-deftest copy-data-key-accesor ()
  "Calling `copy-data--key' on list should return its first
element"
  (should
   (string= (copy-data--key group-t) key-group-t))
  (should-not (copy-data--key nil)))

(ert-deftest copy-data-description-accesor ()
  "Calling `copy-data--description' on list should return its
second element"
  (should
   (string= (copy-data--description group-t)
	    description-group-t))
  (should-not (copy-data--description nil))
  (should-not (copy-data--description `(,key-group-t))))

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
