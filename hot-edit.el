;;; hot-edit.el --- Extension for copy-data-mode -*- lexical-binding: t -*-

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

(defgroup copy-data-hot-edit nil
  "Options concerning hot edit features.")

(defcustom copy-data-hot-edit-enable nil
  "A boolean value that tells copy-data to use hot-edit."
  :group 'copy-data-hot-edit
  :type 'boolean)

(defcustom copy-data-hot-edit-add-key ?+
  "The char used by hot edit to add new element."
  :group 'copy-data-hot-edit
  :type 'character)

(defun copy-data--user-want-group ()
  "Ask user if want to create a group.
If user wants a group, return t. If user wants snippets, returns
nil. Otherwise it will signal error."
  (let ((char (read-char "Create:\ng - Group\ns - Snippet")))
    (cond ((= char ?g) t)
	  ((= char ?s) nil)
	  (t (user-error "[%c] is not a valid option" char)))))

(defun copy-data--ask-for-not-empty-string (wanted-data)
  "Ask user for a string and signal error if is empty."
  (let ((str (string-trim
	      (read-string (concat wanted-data ": ")))))
    (if (string-empty-p str)
	(user-error "%s can't be empty" wanted-data)
      str)))

(defun copy-data--ask-for-key (prefix type)
  "Ask user for a key and signal error if key is in use.

PREFIX mus be the group path. For example, it can be \"tt\". The
function will check if the user enter a char that already existis
for \"tt\" group in `copy-data-user-snippets' list. If user
enters 'l', the function will check if already exist a path like
\"ttl\".

TYPE must be a string, typically \"group\" or
\"snippet\". This string will be used to display the accurate
promt."
  (let ((key (concat
	      prefix
	      (string
	       (read-char
		(format "Press any valid key as your %s's key..."
			type))))))
    (if (copy-data--key-exist-in-group-p prefix key)
	(user-error "[%s] key already exists" key)
      key)))

(defun copy-data--ask-for-element (path)
  "Ask for an element to insert into `copy-data-user-snippets'."
  (let* ((group (copy-data--user-want-group))
	 (el-type (if group "group" "snippet"))
	 (key (copy-data--ask-for-key path
				      el-type))
	 (description
	  (copy-data--ask-for-not-empty-string "Description"))
	 (data (unless group
		 (copy-data--ask-for-not-empty-string "Snippet"))))
    (if group
	`(,key ,description)
      `(,key ,description ,data))))

(defun copy-data--hot-edit-action (action path)
  "Runs the accurate hot-edit action.

ACTION should be a char. If it matches any of chars defined in
the variables `copy-data-hot-edit-add-key',
`copy-data-hot-edit-remove-key' or
`copy-data-hot-edit-modify-key' the accurate action will be
executed.

The PATH argument is used to give a context. It tells Emacs where
should run that command. Will be something like \"tt\", meaning
group t inside t. "
  (cond ((equal action copy-data-hot-edit-add-key)
	 (add-to-list 'copy-data-user-snippets
		      (copy-data--ask-for-element path)
		      t)
	 (customize-save-variable 'copy-data-user-snippets
				  copy-data-user-snippets))))
