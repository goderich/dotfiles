;;; functions.el --- Various functions for my personal use -*- lexical-binding: t; -*-

(require 'dash)
(require 's)
(require 'transient)
(require 'map)

(defun gd/prepend-numbers-normal (numlines)
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

(defun gd/prepend-numbers-visual ()
  "Prepend numbers to the beginning of visually selected lines."
  (interactive)
  (let* ((begin (line-number-at-pos evil-visual-beginning))
         (end (line-number-at-pos evil-visual-end))
         (times (- end begin)))
    (goto-char evil-visual-beginning)
    (prepend-numbers times)))

(defun gd/prepend-numbers-dispatch ()
  "Decide how to prepend numbers based on the current mode."
  (interactive)
  (pcase evil-state
    ('normal (call-interactively #'gd/prepend-numbers-normal))
    ('visual (funcall #'gd/prepend-numbers-visual))
    (_ (error "Can only be called in normal or visual state."))))

(defun gd/insert-numbers (numlines)
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
    (let ((names
           (->>
            (ebib-get-field-value "author" key ebib--cur-db "default" 'unbraced)
            (s-split " and ")
            (--map (car (s-split "," it))))))
      (if (< 2 (length names))
          (concat (car names) " et al")
        (s-join " and " names))))

  (defun gd/ebib-get-year (key)
    (let ((date
           (or
            (ebib-get-field-value "year" key ebib--cur-db 'noerror 'unbraced)
            (ebib-get-field-value "date" key ebib--cur-db 'noerror 'unbraced))))
      (->> date
           (s-split "-")
           (-first-item))))

  (defun gd/ebib-get-title (key)
    (let ((title
           (->> (ebib-get-field-value "title" key ebib--cur-db "default" 'unbraced)
                (s-split ":")
                (car)
                (replace-regexp-in-string "[{}]" "")
                (s-trim))))
      (s-truncate 100 title "")))

  (defun gd/ebib-generate-filename (key)
    (let ((names (gd/ebib-get-author-names key))
          (year (gd/ebib-get-year key))
          (title (gd/ebib-get-title key)))
      (->> (list names year title)
       (-non-nil)
       (s-join " ")
       (replace-regexp-in-string "/" "")
       (replace-regexp-in-string "," "")
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
    (org-get-previous-sibling)
    (org-move-subtree-down))
  (outline-previous-heading))

(defun org-move-subtree-all-the-way-up ()
  "Move the current subtree up past all siblings.
Only moves past headings of the same level, but not past
headings of a higher level. Use instead of mashing Alt+up."
  (interactive)
  (while (org-get-previous-sibling)
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
  (org-get-previous-sibling))

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

(defun gd/org-copy-link-dwim ()
  "Copy link under cursor, or by selecting one on the screen."
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1) ; in a link
      (progn
        ;; Copy the first parenthesized group in the regexp
        (kill-new (match-string-no-properties 1))
        (let ((link (car kill-ring)))
          (message "Copied %s" link)))
    (link-hint-copy-link)))

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
         (dir (f-slash (f-dirname file))))
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
  (save-buffer)
  (call-process "exercism" nil nil nil "submit" (f-this-file)))

(defun gd/consult-goto-org-heading ()
  "Find an org heading in the current buffer, and open it.
`consult-org-heading' doesn't do this automatically for some reason."
  (interactive)
  (consult-org-heading)
  (org-fold-show-context)
  (org-fold-show-entry)
  (org-fold-show-children))

(defconst empty-line-regex (rx bol (0+ space) eol)
  "Regex for an empty line.")

(defun gd/delete-empty-lines ()
  "Delete empty lines in visually selected region."
  (interactive)
  (if (evil-visual-state-p)
      (delete-matching-lines empty-line-regex (region-beginning) (region-end))
    (message "Select a region with visual mode first!")))

;; mu4e

(defun gd/mu4e-link-dwim ()
  "If on a link, open it. If not, store the current message as link.
This is a convenience function to bind it to a single keystroke,
to be used within mu4e's view mode."
  (interactive)
  (if (thing-at-point-url-at-point)
    (mu4e--view-browse-url-from-binding)
    (org-store-link nil 1)))

(defun gd/mu4e-open-file (file)
  "Open FILE in a specified way.
This function is written to be used as `mu4e-view-open-program',
which can be either a string or a function.
The default value was xdg-open, which I am keeping here as
the fallback."
  (let ((opener
         (pcase (f-ext file)
           ((or "doc" "docx") "zaread")
           ((or "jpg" "png") "sxiv")
           ((or "xls" "xlsx") "gnumeric")
           (_ "xdg-open"))))
    (call-process opener nil 0 nil file)))
