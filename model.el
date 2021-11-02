;;; model.l --- copy-data-mode model -*- lexical-binding: t -*-

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

(defun copy-data--key (snippet)
  "Returns the SNIPPET's key string."
  (car snippet))

(defun copy-data--description (snippet)
  "Returns the SNIPPET's description."
  (nth 1 snippet))

(defun copy-data--data (snippet)
  "Returns the SNIPPET's data string."
  (nth 2 snippet))

(defun copy-data--group-p (snippet)
  "Return t if SNIPPET is a group."
  (= (length snippet) 2))

(defun copy-data--snippet-p (snippet)
  "Return t if SNIPPET is a real snippet not a group."
  (= (length snippet) 3))

(defun copy-data--group-members (groups-key)
  "Returns the member of `copy-data-user-snippets' for a group.
GROUPS-KEY is the key for the group wanted to filter by. For
example, if GROUPS-KEY is \"tt\", `copy-data--group-members' will
return all the mebers with a \"tt\" starting key."
  (-filter
   (lambda (snippet)
     (let ((groups-key-length (length groups-key))
	   (snippet-key (copy-data--key snippet)))
       (and
	(string-prefix-p groups-key snippet-key)
	(= (length snippet-key) (1+ groups-key-length)))))
   copy-data-user-snippets))

(defun copy-data--key-exist-in-group-p (path key)
  "Returs t if KEY exist as key of the group PATH.

PATH mus be the group's path. For example, if there is a group
\"b\" inside the group \"b\", its PATH would be \"ab\"."
  (let ((used-keys
	 (seq-map
	  #'copy-data--key
	  (copy-data--group-members path))))
    (seq-contains-p used-keys key)))
