;;; copy-data-mode.el --- Saves repeatedly used data to the kill ring -*- lexical-binding: t -*-

;; Copyright (C) 2021 Felipe Santa Cruz Martínez Alcalá

;; Author: Felipe Santa Cruz Martínez Alcalá <fesanmar@gmail.com>
;; Maintainer: Felipe Santa Cruz Martínez Alcalá <fesanmar@gmail.com>
;; URL: https://github.com/fesanmar/copy-data-mode
;; Version: 0.2.0
;; Created: 2021-08-19
;; Keywords: kill-ring


(defgroup copy-data-user-data nil
  "Options concerning user custom data copy.")

(defcustom copy-data-user-snippets nil
  "A list mapping options to user's data to push into the kill ring.

The first element musth be a single character string. This will
be used to prompt and select the data to push into the kill ring.
The second element should be another string with the data
definition. And the third one is the data. Must be a string as
well."
  :group 'copy-data-user-data
  :type '(repeat (choice
		  (list :tag "Group"
		   (string :tag "Group key code")
		   (string :tag "Group description"))
		  (list :tag "Snippet"
		   (string :tag "Key code")
		   (string :tag "Description")
		   (string :tag "Data")))))

(defface copy-data-key
  '((t :foreground "red"
       :weight bold))
  "The face used by snippet's keys at the echo area.")

(defun copy-data-create-query (snippets)
  "Creates accurate user data query string from SNIPPETS.
SNIPPETS should be a list of snippets, like
`copy-data-create-query'."
  (defun create-snippet-query (snippet)
    (let ((key (substring (car snippet) -1))
	  (description (nth 1 snippet)))
      (concat " ["
	      (propertize key 'face 'copy-data-key)
	      "]: " description)))
  (concat "Select snippet:"
	  (mapconcat 'create-snippet-query snippets ", ")))

(defun copy-data-members (groups-key)
  "Returns the member of `copy-data-user-snippets' for a group.
GROUPS-KEY is the key for the group wanted to filter by. For
example, if GROUPS-KEY is \"tt\", `copy-data-members' will return
all the mebers with a \"tt\" starting key."
  (-filter
   (lambda (snippet)
     (let ((groups-key-length (length groups-key))
	   (snippet-key (car snippet)))
       (and
	(string-prefix-p groups-key snippet-key)
	(= (length snippet-key) (1+ groups-key-length)))))
   copy-data-user-snippets))

(defun copy-data-read-char (prefix snippets-list)
  "Read a char from a buffer, displaying snippets options.
Uses PREFIX to display only the snippets or groups starting with
that particular key in SNNIPPETS-LIST. The character displayed is
the key's last character."
  (read-char
   (copy-data-create-query snippets-list)))

(defun copy-data-query (&optional prefix)
  "Push accurate data into the kill ring.

Queries the user to insert the char binded to the data wanted,
and push the accurate snippet into the kill ring.

Snippets must be defined. `copy-data-user-snippets' can be
customized for that purpose. PREFIX is used to display and select
only the snippets or groups in a particular group. \"\" will
display first level options.

The face used to display the snippet's keys at the echo area is
`copy-data-key'. Can be customized as well."
  (interactive)
  (when (not copy-data-user-snippets)
    (error "There is no snippet yet..."))
  (let* ((prefix (or prefix ""))
	 (filterd-snippets (copy-data-members prefix))
	 (wanted-key (concat
		      prefix
		      (char-to-string
		       (copy-data-read-char prefix filterd-snippets))))
	 (found-snippet (-find (lambda (snippet)
				 (string-equal (car snippet) wanted-key))
			       filterd-snippets)))
    (cond ((and found-snippet
		(= (length found-snippet) 3))
	   (kill-new (nth 2 found-snippet))
	   (message "%s saved into kill ring."
		    (nth 1 found-snippet)))
	  ((and found-snippet
		(= (length found-snippet) 2))
	   (copy-data-query wanted-key))
	  (t (message "There is no [%s] key" wanted-key)))))

;;;###autoload
(define-minor-mode copy-data-mode
  "Save your snippets into the kill ring.
You can store your snippets into `copy-data-user-snippets' and
save it into your kill ring. That way, you can that snippet
either inside or outside Emacs."
  :lighter " copy-data"
  :global t
  :version "0.2.0")

(provide 'copy-data-mode)
