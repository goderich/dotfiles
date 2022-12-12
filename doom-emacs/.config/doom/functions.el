;;; functions.el --- Various functions for my personal use -*- lexical-binding: t; -*-

(defun prepend-numbers (numlines)
  "Prepend numbers to the beginning of lines.
Numbers start with 1 and increase by one
each time. The argument specifies the total
amount of lines to prepend the numerals to.
The numbers are followed by a dot and whitespace."
  (interactive "nPrepend how many times? ")
  (save-excursion
    (beginning-of-line)
    (dotimes (i numlines)
      (insert (format "%d. " (1+ i)))
      (forward-line)
      (beginning-of-line))))

(defun insert-numbers (numlines)
  "Insert new lines with incrementing numbers.
Numbers start with 1 and increase by one
each time. The argument specifies the total
amount of lines to create."
  (interactive "nInsert how many lines? ")
  (save-excursion
    (beginning-of-line)
    (dotimes (i numlines)
      (insert (format "%d\n" (1+ i))))))

(after! ebib
  ; These functions need to be loaded after ebib,
  ; which itself doesn't get loaded until it is called manually.

  (defun gd/ebib-get-author-names (key)
    (-->
     (ebib-get-field-value "author" key ebib--cur-db "default" 'unbraced)
     (s-split " and " it)
     (--map (car (s-split "," it)) it)
     (if (< 2 (length it))
         (concat (car it) " et al")
       (s-join " and " it))))

  (defun gd/ebib-get-year (key)
    (->>
     (or
      (ebib-get-field-value "year" key ebib--cur-db 'noerror 'unbraced)
      (ebib-get-field-value "date" key ebib--cur-db 'noerror 'unbraced))
     (s-split "-")
     (-first-item)))

  (defun gd/ebib-get-title (key)
    (-->
     (ebib-get-field-value "title" key ebib--cur-db "default" 'unbraced)
     (s-split ":" it)
     (car it)
     (replace-regexp-in-string "[{}]" "" it)
     (s-trim it)
     (s-truncate 100 it "")))

  (defun gd/ebib-generate-filename (key)
    (let ((names (gd/ebib-get-author-names key))
          (year (gd/ebib-get-year key))
          (title (gd/ebib-get-title key)))
      (->> (list names year title)
       (-filter #'identity) ; remove nil values
       (s-join " ")
       (replace-regexp-in-string "/" "")
       (replace-regexp-in-string " " "_"))))

  (defun gd/ebib-edit-as-string ()
    "Edit the current field as a string.
This is a function for `ebib-entry-mode'. Since `ebib-edit-field'
has to take a numeric prefix /= 1 in order to begin string
editing, it seems easier to abstract this into a function and
give it its own name and keybinding."
    (interactive)
    (ebib-edit-field 2))

  (defun gd/ebib-import-file-from-index ()
    "Import a file and add to the entry at point.
Used from the ebib index."
    (interactive)
    (ebib-edit-entry)
    (ebib-import-file nil)
    (ebib-quit-entry-buffer))
) ; end `after!' block

(defun gd/send-confirm-has-recipient ()
  "Confirm that the recipient field is not empty before sending."
  (interactive)
  (if (not (message-field-value "To"))
      (message "Empty recipient field!")
    (org-msg-ctrl-c-ctrl-c)))

(defun gd/org-up-heading ()
  "Go up to the nearest heading, or to a higher level heading.
If not on a heading, finds the next heading backwards.
If already on a heading, goes higher up in the tree. This
makes sense to me to combine into a single keybinding."
  (interactive)
  (if (org-at-heading-p)
      (org-up-element)
    (org-back-to-heading)))

(defun advice-add! (symbols where functions)
  "Like `advice-add', except SYMBOLS and FUNCTIONS can be lists."
  (unless (listp symbols) (setq symbols (list symbols)))
  (unless (and (not (functionp functions))
               (listp functions))
    (setq functions (list functions)))
  (dolist (sym symbols)
    (dolist (fn functions)
      (advice-add sym where fn))))

(defun turn-off-visual-line-mode ()
  (visual-line-mode -1))

(defun org-move-subtree-all-the-way-down ()
  "Move the current subtree down past all siblings.
Only moves past headings of the same level, but not past
headings of a higher level. Use instead of mashing Alt+down."
  (interactive)
  (while (org-get-next-sibling)
    (org-get-last-sibling)
    (org-move-subtree-down))
  (outline-previous-heading))

(defun org-move-subtree-all-the-way-up ()
  "Move the current subtree up past all siblings.
Only moves past headings of the same level, but not past
headings of a higher level. Use instead of mashing Alt+up."
  (interactive)
  (while (org-get-last-sibling)
    (org-get-next-sibling)
    (org-move-subtree-up))
  (outline-next-heading))

(defun gd/org-next-heading ()
  "Go to the next sibling, or next heading."
  (interactive)
  (org-get-next-sibling))

(defun gd/org-previous-heading ()
  "Go to the previous sibling, or previous heading."
  (interactive)
  (evil-beginning-of-line)
  (org-get-last-sibling))

(defun save-and-kill-this-buffer ()
  "Save the current buffer and close it."
  (interactive)
  (save-buffer)
  (kill-this-buffer))

(defun gd/reply-received ()
  "Reply to an email with a simple acknowledgement."
  (interactive)
  (mu4e-compose-reply)
  (sleep-for 1) ; wait for the buffer to load
  (let ((buff (car (buffer-list))))
    (with-current-buffer buff
      (insert "Received, thank you.\n")
      (message-send-and-exit))))

(defun gd/org-copy-this-link ()
  "Copy the link under cursor."
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1) ; in a link
      (progn
        ;; Copy the first parenthesized group in the regexp
        (kill-new (match-string-no-properties 1))
        (message "Copied link!"))
    (message "Not on a valid link!")))

(defun gd/set-racket-do-indent ()
  "Set up indentation for `do' in Racket."
  (put 'do 'racket-indent-function 0))

(defun gd/beginning-or-first-non-blank ()
  "Move the point to the beginning of the line.
If point is already at the beginning, move to the first
non-blank character instead."
  (interactive)
  (if (bolp)
      (evil-first-non-blank)
    (evil-beginning-of-line)))

(defun gd/insert-link (address &optional name)
  "Insert an Org link to ADDRESS.
Prompts for a link name (the string that will be visible
as the hyperlink text). If the prompt is left blank,
uses NAME if it's provided, and ADDRESS otherwise."
  (let* ((link-name (read-string "Link name: " "" nil (or name address)))
         (link-string (org-link-make-string address link-name)))
    (insert link-string)))

(defun gd/consult-org-get-heading-text ()
  "Get the text of an Org heading with completion, using `consult'."
  (save-excursion
    (consult-org-heading)
    (org-get-heading 'no-tags 'no-todo 'no-priority 'no-comment)))

(defun gd/org-insert-link ()
  "Insert link to org-mode heading with completion."
  (interactive)
  (gd/insert-link (gd/consult-org-get-heading-text)))

(defun gd/org-insert-link-from-clipboard ()
  "Insert org link from clipboard.
Prompts for link name."
  (interactive)
  (let ((address (substring-no-properties (current-kill 0))))
    (gd/insert-link address)))

(defun gd/org--get-heading-id-pair ()
  "Get the text and ID of an org heading.
Creates the ID if one isn't already present."
  (save-excursion
    (consult-org-heading)
    (let ((heading (org-get-heading 'no-tags 'no-todo 'no-priority 'no-comment))
          (id (concat "id:" (org-id-get nil 'create))))
      (cons heading id))))

(defun gd/org-insert-link-with-id ()
  "Insert a link to a heading with completion, using a unique ID."
  (interactive)
  (let* ((heading-id-pair (gd/org--get-heading-id-pair))
         (heading (car heading-id-pair))
         (id (cdr heading-id-pair)))
    (gd/insert-link id heading)))

(defun gd/org-set-custom-id ()
  "Create a new custom ID property at the current org heading.
Prompts for user input, converts it to lisp-case, and
sets that as the new CUSTOM_ID. If the input is left
blank, uses the heading text itself.

Returns the new CUSTOM_ID value as a string."
  (let ((custom-id
         (->> (org-get-heading)
              (read-string "Create new custom ID: " "" nil)
              (s-dashed-words)
              (concat "sec:"))))
    (org-set-property "CUSTOM_ID" custom-id)
    custom-id))

(defun gd/org-get-custom-id ()
  "Retrieve the custom_id of a heading.
If one does not exist, create it.

The function prompts the user for a new custom ID. By default,
the heading name is used. The user input or heading is then
transformed into a lisp-case string."
  (save-excursion
    (consult-org-heading)
    (let* ((props (org-entry-properties))
           (custom-id (or (map-elt props "CUSTOM_ID")
                          (gd/org-set-custom-id))))
      custom-id)))

(defun gd/org-insert-reference-heading (&optional capitalize?)
  "Insert a pandoc reference to a heading, with completion.
We use narrowing to find the required heading, and then insert a link
using its CUSTOM_ID property. If the property isn't set, it is
created."
  (interactive)
  (let* ((custom-id (gd/org-get-custom-id))
         (cite-str (if capitalize?
                       (s-capitalize custom-id)
                     custom-id)))
    (insert "[cite: @" cite-str "]")))

(defun gd/org-insert-capitalized-reference-heading ()
  (interactive)
  (gd/org-insert-reference-heading 'capitalize))

(defun gd/browse-org-directory ()
  (interactive)
  (doom-project-browse (f-slash my/org-directory)))

(defun gd/org-msg-attach-file ()
  "Attach a file to an org-msg buffer and remember the directory.
A wrapper around `org-msg-attach-attach' that remembers the location
of the last attachment of the current email. Helpful when attaching
several files from a deeply nested directory without using dired."
  (interactive)
  (let* ((file (read-file-name "Attachment: "))
         (dir (f-dirname file)))
    (setq-local default-directory dir)
    (org-msg-attach-attach file)))

(defun map-put-many! (map &rest rest)
  "Like `map-put!', but with many key-value pairs.
Key-value pairs should be supplied without any syntax
(so not as cons cells), just as a bare list."
  (when (cl-oddp (length rest))
    (user-error "Odd number of arguments!"))
  (cl-loop for (k v) on rest by #'cddr
           do (map-put! map k v)))

(defun gd/org-books-tag-short (&optional pagenum)
  "Tag a book as short.
Tags books as short if they are shorter than PAGENUM,
or 300 pages by default. Books are only tagged as short
if they don't already have this tag, including inherited
tags."
  (let* ((props (org-entry-properties))
         (pages (map-elt props "PAGES"))
         (tags (org-get-tags))
         (local-tags (org-get-tags nil 'local)))
    (when (and pages
               (< (string-to-number pages) (or pagenum 300))
               (not (member "short" tags)))
      (org-set-tags (-snoc local-tags "short")))))

(defun gd/exercism-submit ()
  "Submit this file to exercism."
  (interactive)
  (call-process "exercism" nil nil nil "submit" (f-this-file)))

(defun gd/consult-goto-org-heading ()
  "Find an org heading in the current buffer, and open it.
`consult-org-heading' doesn't do this automatically for some reason."
  (interactive)
  (consult-org-heading)
  (org-fold-show-siblings)
  (org-fold-show-context)
  (org-fold-show-entry))

(defcustom empty-line-regex (rx bol (0+ space) eol)
  "Regex for an empty line.")

(defun gd/delete-empty-lines ()
  "Delete empty lines in visually selected region."
  (interactive)
  (if (evil-visual-state-p)
      (delete-matching-lines empty-line-regex (region-beginning) (region-end))
    (message "Select a region with visual mode first!")))

(defun gd/org-link-dwim ()
  "If on a link, open it. If not, insert last stored link.
This is a convenience function to bind it to a single keystroke."
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1)
      (org-open-at-point)
    (org-insert-last-stored-link 1)))

(defun gd/mu4e-link-dwim ()
  "If on a link, open it. If not, store the current message as link.
This is a convenience function to bind it to a single keystroke,
to be used within mu4e's view mode."
  (interactive)
  (if (thing-at-point-url-at-point)
    (mu4e~view-browse-url-from-binding)
    (org-store-link nil 1)))

(defun gd/evil-paste-no-kill ()
  "Paste over visual selection without copying it.
This is how shift+p functions in visual mode in vim,
but apparently evil mode does not do this."
  ;; TODO: add issue?
  (interactive)
  (let ((evil-kill-on-visual-paste nil))
    (evil-visual-paste 1)))
