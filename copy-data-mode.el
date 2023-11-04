;;; copy-data-mode.el --- Saves repeatedly used data to the kill ring -*- lexical-binding: t -*-

;; Copyright (C) 2021 Felipe Santa Cruz Martínez Alcalá

;; Author: Felipe Santa Cruz Martínez Alcalá <fesanmar@gmail.com>
;; Maintainer: Felipe Santa Cruz Martínez Alcalá <fesanmar@gmail.com>
;; URL: https://github.com/fesanmar/copy-data-mode
;; Version: 1.2.0
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
;; 	   ("hd" "Dog name" "Roger")
;; 	   ("ha" "Home Address" "That Creepy House 1")
;; 	   ("w" "Work snippets")
;; 	   ("wp" "My project")
;; 	   ("wpb" "This year branches prefix" "/wawa/wi/wa/US21")
;; 	   ("wpt" "My Team Leader" "Roger As Well")
;; 	   ("wu" "Work User" "165432")))

;; As you can see, you can create groups and place snippets inside
;; those groups. Of course, you can create snippet belonging to no
;; group as well. Now, if you want to copy your Work User, you just
;; have to press `M-x copy-data-query RET wu' and 165432 will be
;; ready to be pasted anywere you want. You don't have to memorize
;; thouse paths; the echo area will show you the aviable snippets or
;; groups for each level.

(require 'subr-x)
(require 'cl-macs)

;;; Custom groups and variables
;;;; Main copy data customizations
(defgroup copy-data nil
  "Copy Data customizations"
  :group 'applications)

(defgroup copy-data-user-data nil
  "Options concerning Copy Data snippets"
  :group 'copy-data)

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

;;;; Hot edit customizations
(defgroup copy-data-hot-edit nil
  "Options concerning hot edit features."
  :group 'copy-data)

(defcustom copy-data-hot-edit-enable nil
  "A boolean value that tells copy-data to use hot-edit."
  :group 'copy-data-hot-edit
  :type 'boolean)

(defcustom copy-data-hot-edit-add-key ?+
  "The char used by hot edit to add new element."
  :group 'copy-data-hot-edit
  :type 'character)

(defcustom copy-data-hot-edit-edit-key ?.
  "The char used by hot edit to edit an element."
  :group 'copy-data-hot-edit
  :type 'character)

(defcustom copy-data-hot-edit-remove-key ?-
  "The char used by hot edit to remove an element."
  :group 'copy-data-hot-edit
  :type 'character)

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

;; Copy Data General constants
(defconst copy-data--query-head "Select snippet:"
  "Head of the query created by `copy-data--create-query'")

(defconst copy-data--empty-data "There is no snippet here yet..."
  "Message to display when `copy-data-user-data' is nil.")

(defconst copy-data--not-found-msg "There is no [%s] key"
  "Message to be displayed when user insert not existing key.")

(defconst copy-data--directory (file-name-directory load-file-name)
  "Copy data extension's current directory.")

;; Hot Edit cosntants
(defconst copy-data--hot-edit-edition-msg "Select element to edit: "
  "Hot Edit message as head for edition action.")

;;; Copy data model for dealing with snippets and groups
(defun copy-data--make-element (key description &optional data)
  "Creates a snippet with KEY, DESCRIPTION and DATA.

KEY must be a string representing the element's path and key
inside the group. DESCRIPTION must be a string describing de
element. And DATA if probided, should be a string as well. In
that the function will return a snippet. Otherwise, if DATA is
nil, a group will be retrieved."
  (let ((element (list key description)))
    (if data
	(append element (list data))
      element)))

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

(defun copy-data--element (key group)
  "Finds KEY in GROUP and retunrs its element if exists o returns nil.

KEY musth be a string representing the element for example
\"tta\". GROUP, in the other hand must be the list of elements
used to find the KEY."
  (seq-find (lambda (snippet)
	      (string-equal (copy-data--key snippet) key))
	    group))

(defun copy-data--group-members (groups-key)
  "Returns the member of `copy-data-user-snippets' for a group.
GROUPS-KEY is the key for the group wanted to filter by. For
example, if GROUPS-KEY is \"tt\", `copy-data--group-members' will
return all the mebers with a \"tt\" starting key."
  (seq-filter
   (lambda (snippet)
     (let ((groups-key-length (length groups-key))
	   (snippet-key (copy-data--key snippet)))
       (and
	(string-prefix-p groups-key snippet-key)
	(= (length snippet-key) (1+ groups-key-length)))))
   copy-data-user-snippets))

(defun copy-data--group-empty-p (group)
  "Returns t if the GROUP is empty.

If GROUP is not a group or if it's a group but it's emtpy returns
nil."
  (not (or (not (copy-data--group-p group))
	   (copy-data--group-members (copy-data--key group)))))

(defun copy-data--key-exist-in-group-p (path key)
  "Returs t if KEY exist as key of the group PATH.

PATH mus be the group's path. For example, if there is a group
\"b\" inside the group \"b\", its PATH would be \"ab\"."
  (let ((used-keys
	 (seq-map
	  #'copy-data--key
	  (copy-data--group-members path))))
    (seq-contains-p used-keys key)))

;;; Sorting options functions
(defun copy-data-sort-by-groups (el1 el2)
  "Returns t if EL1 is a group and EL2 isn't."
  (and (copy-data--group-p el1)
       (copy-data--snippet-p el2)))

(defun copy-data-sort-by-snippets (el1 el2)
  "Returns t if EL1 is a snippet and EL2 isn't."
  (and (copy-data--snippet-p el1)
       (copy-data--group-p el2)))

(defun copy-data-sort-no (_ _)
  "Returs nil."
  nil)

;;; Helper functions

(defun copy-data--key-doesnt-exist-msg (wanted-key)
  "Displays in the echo area an error messages saying WANTED-KEY
doesn't exists."
  (message copy-data--not-found-msg wanted-key))

(defun copy-data--create-query (snippets &optional prompt)
  "Creates accurate user data query string from SNIPPETS.

SNIPPETS should be a list of snippets, like
`copy-data-user-snippets'."
  (cl-flet ((create-snippet-query-head ()
	      (if (and (not snippets) copy-data-hot-edit-enable)
		  (format "%s Press %s to create a new element."
			  copy-data--empty-data
			  (string copy-data-hot-edit-add-key))
		(or prompt copy-data--query-head)))
	    (create-snippet-query-body (snippet)
	      (let ((last-key-char (substring (copy-data--key snippet) -1))
		    (description (copy-data--description snippet))
		    (accurate-key (if (copy-data--snippet-p snippet)
				      'copy-data-snippet-key
				    'copy-data-group-key)))
		(concat " ["
			(propertize last-key-char 'face accurate-key)
			"]: " description))))
    (let ((sorted-snippets (seq-sort copy-data-query-sort snippets)))
      (concat (create-snippet-query-head)
	      (mapconcat #'create-snippet-query-body
			 sorted-snippets
			 ", ")))))

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

;;; Hot edit functions
(defun copy-data--user-want-group ()
  "Ask user if want to create a group.
If user wants a group, return t. If user wants snippets, returns
nil. Otherwise it will signal error."
  (let ((char (read-char "Create:\ng - Group\ns - Snippet")))
    (cond ((= char ?g) t)
	  ((= char ?s) nil)
	  (t (user-error "[%c] is not a valid option" char)))))

(defun copy-data--ask-for-not-empty-string (wanted-data &optional default)
  "Ask user for a string and signal error if is empty."
  (let* ((max-length 30)
	 (default-length (and default (length default)))
	 (formated-default
	  (and default (if (> default-length max-length)
			   (concat (substring default 0 max-length) "...")
			 default)))
	 (hint (or (and default (concat " (" formated-default ")")) ""))
	 (prompt (concat wanted-data hint ": "))
	 (str (string-trim
	       (read-string prompt nil nil default))))
    (if (string-empty-p str)
	(user-error "%s can't be empty" wanted-data)
      str)))

(defun copy-data--ask-for-key (prefix &optional prompt query should-exists)
  "Ask user for a key and returns the key's qualified path.

PREFIX mus be the group path. For example, it can be \"tt\". The
function will check if the user enter a char that already existis
for \"tt\" group in `copy-data-user-snippets' list. If user
enters 'l', the function will check if already exist a path like
\"ttl\".

PROMPT, if any, must be a string telling the user what to do. If
PROMPT is nil `copy-data--query-head' will be used instead.

If QUERY is non nil the function will display a string with the
differtens elements inside PREFIX's group, otherwise will display
only PROMPT.

If SHOULD-EXISTS is non nil, singals an error if key doesn't
exists inside the group represented by PREFIX.

This function returns the key selected by the user qualified.
Meaning that if the PATH is \"tt\" and the user select ?a, the
function will return \"tta\"."
  (concat
   prefix
   (string
    (let* ((key
	    (if query
		(read-char
		 (copy-data--create-query
		  (copy-data--group-members prefix)
		  prompt))
	      (read-char (or prompt copy-data--query-head))))
	   (key-exists (copy-data--key-exist-in-group-p prefix (concat prefix (string key)))))
      (if (or key-exists (and (not key-exists) (not should-exists)))
	  key
	(copy-data--key-doesnt-exist-msg (string key)))))))

(defun copy-data--ask-for-new-key (prefix type &optional default)
  "Ask user for a key and signal error if key is in use.

PREFIX mus be the group path. For example, it can be \"tt\". The
function will check if the user enter a char that already existis
for \"tt\" group in `copy-data-user-snippets' list. If user
enters 'l', the function will check if already exist a path like
\"ttl\".

TYPE must be a string, typically \"group\" or
\"snippet\". This string will be used to display the accurate
promt.

DEFAULT must be a string value, similar to prefix plus one char.
If so when the user press the return key (?\C-m char or ^M
string) the defualt value will be returned. The prompt will
display this option."
  (let* ((default-prompt
	   (or (and default
		    (format " or press RET to use [%s]"
			    (substring default (- (length default) 1))))
	       ""))
	 (prompt (format "Press any valid key as your %s's key%s..." type default-prompt))
	 (key (copy-data--ask-for-key prefix prompt)))
    (cond ((string-suffix-p "\15" key) default) 
	  ((copy-data--key-exist-in-group-p prefix key) (user-error "[%s] key already exists" key))
	  (t key))))

(defun copy-data--remove-element-by-key (el-key &optional remove-by-prefix)
  "Remove element with EL-KEY ky from `copy-data-user-snippets'.

If there is no element with EL-KEY key, this function won't do
anything.

If REMOVE-BY-PREFIX is non-nil value the function will remove
every element whose key starts with EL-KEY."
  (setq copy-data-user-snippets
	(seq-remove (lambda (el)
		      (let ((predicate (if remove-by-prefix
					   #'string-prefix-p
					 #'string=)))
			(funcall predicate el-key (copy-data--key el))))
		    copy-data-user-snippets)))

(defun copy-data--create-element-interactiviley (path)
  "Ask for an element to insert into `copy-data-user-snippets'.

This function creates a new element in PATH but doesn't save
updated `copy-data-user-snippets' variable for future sessions."
  (let* ((group (copy-data--user-want-group))
	 (el-type (if group "group" "snippet"))
	 (key (copy-data--ask-for-new-key path
					  el-type))
	 (description
	  (copy-data--ask-for-not-empty-string "Description"))
	 (data (unless group
		 (copy-data--ask-for-not-empty-string "Snippet")))
	 (element (if group
		      `(,key ,description)
		    `(,key ,description ,data))))
    (add-to-list 'copy-data-user-snippets element t)))

(defun copy-data--update-element-interactively (path)
  "Interactively guide user to update an element.

If any element is updated this function return t. Otherwise nil
will be returned.

This function updates the element in PATH. But doesn't save
updated `copy-data-user-snippets' variable for future sessions."
  (let* ((members (copy-data--group-members path))
	 (el-key
		 (copy-data--ask-for-key path copy-data--hot-edit-edition-msg t))
	 (element (copy-data--element el-key members))
	 (is-snippet (copy-data--snippet-p element))
	 (type (if is-snippet "snippet" "group"))
	 (description-default (copy-data--description element))
	 (new-key (copy-data--ask-for-new-key path type el-key))
	 (description
	  (copy-data--ask-for-not-empty-string
	   "Description"
	   description-default))
	 (data-default (and is-snippet (copy-data--data element)))
	 (data
	  (and is-snippet
	       (copy-data--ask-for-not-empty-string
		"Snippet"
		data-default)))
	 (new-element (copy-data--make-element new-key description data)))
    (if (equal element new-element)
	(user-error "Element hasn't change and no action need to be performed.")
      (copy-data--remove-element-by-key el-key))
    (add-to-list 'copy-data-user-snippets
		 new-element)))

(defun copy-data--remove-element-interactively (path)
  "Interactively guide user to delete an element.

This function removes the element whose key is PATH. If PATH is a
group and it's not empty it will ask the user if he sure about
removing the group recursively. If the user says yes, the group
will be remove entirely.

If any element is removed, this function return t. Otherwise nil
will be returned.

This function doesn't updated `copy-data-user-snippets' variable
for future sessions."
  (let* ((el-key (copy-data--ask-for-key path "Select element to remove:" t t))
	 (element (copy-data--element el-key (copy-data--group-members path)))
	 (description (copy-data--description element))
	 (group-p (copy-data--group-p element))
	 (emptyp (and group-p (copy-data--group-empty-p element)))
	 (wants-to-remove
	  (and
	   (and group-p (not emptyp))
	   (yes-or-no-p (format "\"%s\" is not empty. Remove it anyway?" description)))))
    (if (or (not group-p) emptyp (and (not emptyp) wants-to-remove))
	(copy-data--remove-element-by-key el-key group-p)
      (message "Action aborted")
      nil)))

(defun copy-data--hot-edit-action (action path)
  "Runs the accurate hot-edit action.

ACTION should be a char. If it matches any of chars defined in
the variables `copy-data-hot-edit-add-key',
`copy-data-hot-edit-remove-key' or
`copy-data-hot-edit-modify-key' the accurate action will be
executed.

The PATH argument is used to give a context. It tells Emacs where
should run that command. Will be something like \"tt\", meaning
group t inside t."
  (let ((modifications-p
	 (cond ((equal action copy-data-hot-edit-add-key)
		(copy-data--create-element-interactiviley path))
	       ((equal action copy-data-hot-edit-edit-key)
		(copy-data--update-element-interactively path))
	       ((equal action copy-data-hot-edit-remove-key)
		(copy-data--remove-element-interactively path)))))
    (when modifications-p
      (customize-save-variable 'copy-data-user-snippets
			       copy-data-user-snippets))))

;;; Main functions

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
	 (found-snippet (copy-data--element wanted-key filterd-snippets)))
    (cond ((and copy-data-hot-edit-enable
		(seq-contains-p
		 `(,copy-data-hot-edit-add-key
		   ,copy-data-hot-edit-edit-key
		   ,copy-data-hot-edit-remove-key)
		 user-char))
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
  :version "1.2.0")

(provide 'copy-data-mode)
