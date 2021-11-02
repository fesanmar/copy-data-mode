;;; copy-data-mode.el --- Saves repeatedly used data to the kill ring -*- lexical-binding: t -*-

;; Copyright (C) 2021 Felipe Santa Cruz Martínez Alcalá

;; Author: Felipe Santa Cruz Martínez Alcalá <fesanmar@gmail.com>
;; Maintainer: Felipe Santa Cruz Martínez Alcalá <fesanmar@gmail.com>
;; URL: https://github.com/fesanmar/copy-data-mode
;; Version: 1.2.0-SNAPSHOT
;; Created: 2021-08-19
;; Keywords: kill-ring

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

;; Install copy-data-mode by placing `copy-data-mode.el' in
;; `/path/to/elisp', a directory of your choice, and adding to your
;; .emacs file:

;; (add-to-list 'load-path "/path/to/elisp")
;; (when (require 'copy-data-mode nil t)
;;   (copy-data-mode)
;;   (global-set-key (kbd "C-c d") 'copy-data-query)) ;; use whatever key bingding

;; Copy Data Mode is a tool created for saving time seraching for
;; some data and copying it just for pasting somewhere else. First of
;; all, you need to create your backend, your data pool. You can
;; create your snippets by customizing the `copy-data-user-snippets'
;; variable or creating a list yourself. Here is an example:

;; (setq copy-data-user-snippets
;;       '(("h" "Home snippets")
;; 	("hd" "Dog name" "Roger")
;; 	("ha" "Home Address" "That Creepy House 1")
;; 	("w" "Work snippets")
;; 	("wp" "My project")
;; 	("wpb" "This year branches prefix" "/wawa/wi/wa/US21")
;; 	("wpt" "My Team Leader" "Roger As Well")
;; 	("wu" "Work User" "165432")))

;; As you can see, you can create groups and place snippets inside
;; those groups. Of course, you can create snippet belonging to no
;; group as well. Now, if you want to copy your Work User, you just
;; have to press `M-x copy-data-query RET wu' and 165432 will be
;; ready to be pasted anywere you want. You don't have to memorize
;; thouse paths; the echo area will show you the aviable snippets or
;; groups for each level.

;;; Custom groups and variables
(defgroup copy-data-user-data nil
  "Options concerning user custom data copy.")

(defcustom copy-data-user-snippets nil
  "A list mapping options to user's data to push into the kill ring.

The first element musth be the snippet or group key string. This
will be used to prompt and select the data to push into the kill
ring. The second element should be another string with the data
definition. And the third one is the data. Must be a string as
well. If a snippet is member of a particular group, its key
should start wiht the group key.

For example, if we have a group like this (\"h\" \"home\") and we
want to add a snippet inside it, like telephone, we will add a
list like this: (\"ht\" \"Home Telephone\" \"938119238\"). And
the complete list will look like this:

((\"h\" \"home\")
 (\"ht\" \"Home Telephone\" \"938119238\"))"
  :group 'copy-data-user-data
  :type '(repeat (choice
		  (list :tag "Group"
		   (string :tag "Group key code")
		   (string :tag "Group description"))
		  (list :tag "Snippet"
		   (string :tag "Key code")
		   (string :tag "Description")
		   (string :tag "Data")))))

(defcustom copy-data-query-sort 'copy-data-sort-no
  "The function used to sort the elements in the echo area.

This variable is a function that takes two arguments and returns
non-`nil' if the first argument should sort before the second.

There are some functions already defined for this purpose:

- No sorting at all: `copy-data-sort-no'
- Place groups first `copy-data-sort-by-groups'
- Place groups first `copy-data-sort-by-groups'"
  :group 'copy-data-user-data
  :type '(choice
	  (function-item :tag "No sorting at all" copy-data-sort-no)
	  (function-item :tag "Place groups first" copy-data-sort-by-groups)
	  (function-item :tag "Place snippets first" copy-data-sort-by-snippets)))

;;; Custom faces
(defface copy-data-snippet-key
  '((t :foreground "red"
       :weight bold))
  "The face used by snippet's keys at the echo area.")

(defface copy-data-group-key
  '((t :foreground "peru"
       :weight bold))
  "The face used by group's keys at the echo area.")

;;; Constants
(defconst copy-data--query-head "Select snippet:"
  "Head of the query created by `copy-data--create-query'")

(defconst copy-data--empty-data "There is no snippet here yet..."
  "Message to display when `copy-data-user-data' is nil.")

(defconst copy-data--not-found-msg "There is no [%s] key"
  "Message to be displayed when user insert not existing key.")

(defconst copy-data--directory (file-name-directory load-file-name)
  "Copy data extension's current directory.")

;;; Load submodules
(load-file (expand-file-name "./model.el" copy-data--directory))
(load-file (expand-file-name "./hot-edit.el" copy-data--directory))

;;; Main functions
(defun copy-data-sort-by-groups (el1 el2)
  "Returns t if EL1 is a group and EL2 isn't."
  (and (copy-data--group-p el1)
       (copy-data--snippet-p el2)))

(defun copy-data-sort-by-snippets (el1 el2)
  "Returns t if EL1 is a snippet and EL2 isn't."
  (and (copy-data--snippet-p el1)
       (copy-data--group-p el2)))

(defun copy-data-sort-no (el1 el2)
  "Returs nil."
  nil)

(defun copy-data--create-query (snippets)
  "Creates accurate user data query string from SNIPPETS.

SNIPPETS should be a list of snippets, like
`copy-data-user-snippets'."
  (defun create-snippet-query-head ()
    (if (and (not snippets) copy-data-hot-edit-enable)
	(format "%s Press %s to create a new element."
		copy-data--empty-data
		(string copy-data-hot-edit-add-key))
      copy-data--query-head))
  (defun create-snippet-query-body (snippet)
    (let ((last-key-char (substring (copy-data--key snippet) -1))
	  (description (copy-data--description snippet))
	  (accurate-key (if (copy-data--snippet-p snippet)
			    'copy-data-snippet-key
			  'copy-data-group-key)))
      (concat " ["
	      (propertize last-key-char 'face accurate-key)
	      "]: " description)))
  (let ((sorted-snippets (seq-sort copy-data-query-sort snippets)))
    (concat (create-snippet-query-head)
	    (mapconcat 'create-snippet-query-body
		       sorted-snippets
		       ", "))))

(defun copy-data--read-char (snippets-list)
  "Read a char from a buffer, displaying snippets options.

 The character displayed is the key's last character. If
 SNIPPETS-LIST is nil and hot edit is disabled will signal
 error."
  (if (or snippets-list
	  (and (not snippets-list)
	       copy-data-hot-edit-enable))
      (read-char
       (copy-data--create-query snippets-list))
    (user-error copy-data--empty-data)))

(defun copy-data-query (&optional prefix)
  "Push accurate data into the kill ring.

Queries the user to insert the char binded to the data wanted,
and push the accurate snippet into the kill ring.

Snippets must be defined. `copy-data-user-snippets' can be
customized for that purpose. PREFIX is used to display and select
only the snippets or groups in a particular group. \"\" will
display first level options.

The faces used to display the snippets and groups keys at the
echo area are `copy-data-snippet-key' and `copy-data-group-key'.
Can be customized as well.

The order used to display the elements in the echo area can be
customized by the `copy-data-query-sort' variable."
  (interactive)
  (let* ((prefix (or prefix ""))
	 (filterd-snippets (copy-data--group-members prefix))
	 (user-char (copy-data--read-char filterd-snippets))
	 (wanted-key (concat prefix (char-to-string user-char)))
	 (found-snippet (seq-find (lambda (snippet)
				 (string-equal (copy-data--key snippet) wanted-key))
			       filterd-snippets)))
    (cond ((and copy-data-hot-edit-enable
		(seq-contains-p `(,copy-data-hot-edit-add-key) user-char))
	   (copy-data--hot-edit-action user-char prefix))
	  ((and found-snippet
		(copy-data--snippet-p found-snippet))
	   (kill-new (copy-data--data found-snippet)) ; TODO: don't like that nth 2. Use selector
	   (message "%s saved into kill ring."
		    (copy-data--description found-snippet)))
	  ((and found-snippet
		(copy-data--group-p found-snippet))
	   (copy-data-query wanted-key))
	  (t (message copy-data--not-found-msg wanted-key)))))

;;;###autoload
(define-minor-mode copy-data-mode
  "Save your snippets into the kill ring.
You can store your snippets into `copy-data-user-snippets' and
save it into your kill ring. That way, you can that snippet
either inside or outside Emacs."
  :lighter " copy-data"
  :global t
  :version "1.2.0-SNAPSHOT")

(provide 'copy-data-mode)
