;;;; A convinient way to copy data into the kill ring and use it
;;; either inside or outside Emacs.

(defgroup copy-data-user-data nil
  "Options concerning user custom data copy.")

(defcustom copy-data-user-snippets nil
  "A list mapping options to user's data to push into the kill ring.

The first element musth be a single character string. This will
be used to prompt and select the data to push into the kill ring.
The second element should be another string with the data
definition. And the third one is the data. Must be a string as well"
  :group 'copy-data-user-data
  :type '(repeat (list
		  (string :tag "Key code")
		  (string :tag "Description")
		  (string :tag "Data"))))

(defun copy-data-create-query ()
  "Creates accurate user data query string.
Uses the `copy-data-user-snippets' variable."
  (defun create-snippet-query (snippet)
    (let ((key (car snippet))
	  (description (nth 1 snippet)))
      (concat " ["
	      (propertize key
			  'face '(:foreground "red" :weight bold))
	      "]: " description)))
  (concat "Select snippet:"
	  (mapconcat 'create-snippet-query copy-data-user-snippets ", ")))

(defun copy-data-query (option)
  "Push accurate data into the kill ring.

Queries the user to insert the char binded to the data wanted,
and push the accurate snippet into the kill ring. OPTION
represents the snippet's key character.

Snippets must be defined. For that `copy-data-user-snippets' can
be customized.
"
  (interactive (if copy-data-user-snippets
		   (list
		    (read-char (copy-data-create-query)))
		 (error "There is no snippet yet...")))
  (let* ((option-chr (char-to-string option))
	 (found-snippet (-find (lambda (snippet)
				 (string-equal (car snippet) option-chr))
			       copy-data-user-snippets)))
    (if found-snippet
	(progn
	  (kill-new (nth 2 found-snippet))
	  (message "%s saved into kill ring."
		   (nth 1 found-snippet)))
      (progn
	(message "There is no [%s] key" option-chr)
	(sit-for 2.0)
	(message nil)))))

;;;###autoload
(define-minor-mode copy-data-mode
  "Save your snippets into the kill ring.
You can store your snippets into `copy-data-user-snippets' and
save it into your kill ring. That way, you can that snippet
either inside or outside Emacs."
  :lighter " copy-data"
  :global t)

(provide 'copy-data-mode)
