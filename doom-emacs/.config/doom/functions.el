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

(defun md-table->latex ()
  "Convert a Markdown table preceding the point into LaTeX."
  (interactive)
  (defun convert-row ()
    (let ((md-asterisk (rx "\\" "\*"))
          (md-bold (rx (repeat 2 "\*")
                       word-start
                       (group (minimal-match (zero-or-more (not (any "\&")))))
                       word-end
                       (repeat 2 "\*")))
          (md-opening-quote (rx " '" word-start)))
      (delete-char 2)
      (while (re-search-forward "|" (1- (line-end-position)) t)
        (replace-match "&"))
      (re-search-forward "|")
      (replace-match "\\\\ \\addlinespace" t t)
      (beginning-of-line)
      (while (re-search-forward md-asterisk (1- (line-end-position)) t)
        (replace-match "\\\\text{\*}"))
      (beginning-of-line)
      (while (re-search-forward md-bold (1- (line-end-position)) t)
        (replace-match "\\\\textbf{\\1}"))
      (beginning-of-line)
      (while (re-search-forward md-opening-quote (1- (line-end-position)) t)
        (replace-match " \`"))
      (forward-line)))
  (progn
    (re-search-backward "^:")
    (skip-chars-forward "[: ]")
    (setq table-caption
          (replace-regexp-in-string
           "\\\\\\*"  "\\\\text{\*}"
           (delete-and-extract-region
            (point)
            (- (search-forward " {#") 3))))
    (setq table-label
          (delete-and-extract-region
           (point)
           (1- (line-end-position))))
    (beginning-of-line)
    (delete-region (point) (progn (forward-line 1) (point)))
    (insert
     "\\begin{table}[hbt!]\n"
     "\\centering\n"
     "\\caption{" table-caption "}\n"
     "\\label{" table-label "}\n")
    (setq table-top (point))
    (forward-line)
    (setq table-columns
          (1- (cl-count ?| (thing-at-point 'line))))
    (convert-row)
    (re-search-backward "addlinespace")
    (replace-match "midrule \\\\addlinespace")
    (forward-line)
    (delete-region (point) (progn (forward-line 1) (point)))
    (while (< 0 (cl-count ?| (thing-at-point 'line)))
      (convert-row))
    (setq table-bottom (point))
    (re-search-backward "addlinespace")
    (replace-match "bottomrule")
    (forward-line)
    (insert
     "\\end{tabular}\n"
     "\\end{table}\n")
    (goto-char table-top)
    (insert
     "\\begin{tabular}{ "
     (apply 'concat (make-list table-columns "l "))
     "} \\toprule")
    (re-search-backward "]")))

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

;; This function is adapted from
;; https://www.reddit.com/r/emacs/comments/nvhb82/can_i_use_racket_like_i_use_elisp/h13ug8i/
(defun vz/racket-eros-eval-last-sexp ()
  "Eval the previous sexp asynchronously and create an eros overlay."
  (interactive)
  (unless (racket--repl-live-p)
    (user-error "No repl session available."))
  (racket--cmd/async
   (racket--repl-session-id)
   `(eval ,(buffer-substring-no-properties (racket--repl-last-sexp-start) (point)))
   (lambda (result)
     (let ((point (point))
           ;; Using :duration 'command (which is my current value of
           ;; `eros-eval-result-duration') didn't work without setting
           ;; `this-command'. The variable got nulled somewhere during
           ;; the call, and eros would delete the overlay before even
           ;; displaying it.
           (this-command 'vz/racket-eros-eval-last-sexp))
       (message "%s" result)
       (eros--make-result-overlay result
         :where point
         :duration eros-eval-result-duration)))))

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

(defun gd/scroll-line-to-top-minus-2 ()
  "Scroll line almost to the very top, but not quite."
  (interactive)
  (save-excursion
    (-> (line-number-at-pos)
        (- 2)
        (evil-scroll-line-to-top))))

(defun gd/insert-link (address)
  (let* ((link-name (read-string "Link name: " "" nil address))
         (link-string (org-link-make-string address link-name)))
    (insert link-string)))

(defun gd/org-insert-link-helper (candidate)
  (let ((heading (-last-item (s-split "/" (car candidate)))))
    (gd/insert-link heading)))

(defun gd/org-insert-link ()
  "Insert link to org-mode heading with completion."
  (interactive)
  (let ((settings (cdr (assq 'org-mode counsel-outline-settings))))
    (ivy-read "Outline: " (counsel-outline-candidates settings)
              :action #'gd/org-insert-link-helper
              :history 'counsel-org-goto-history
              :preselect 0
              :caller 'counsel-org-goto)))

(defun gd/org-insert-link-from-clipboard ()
  "Insert org link from clipboard.
Prompts for link name."
  (interactive)
  (let ((address (substring-no-properties (current-kill 0))))
    (gd/insert-link address)))

(defun gd/org-insert-link-with-id ()
  "Insert a link to a heading with completion, using a unique ID."
  (interactive)
  (let ((heading)
        (id))
    (save-excursion
      (counsel-org-goto)
      (setq heading (org-get-heading))
      (setq id (concat "id:" (org-id-get nil 'create))))
    (let* ((link-name (read-string "Link name: " "" nil heading))
           (link-string (org-link-make-string id link-name)))
      (insert link-string))))

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
  "Retrieve a custom_id of a heading.
If one does not exist, create it.

The function prompts the user for a new custom ID. By default,
the heading name is used. The user input or heading is then
transformed into a lisp-case string."
  (save-excursion
    (counsel-org-goto)
    (let* ((props (org-entry-properties))
           (custom-id (or (map-elt props "CUSTOM_ID")
                          (gd/org-set-custom-id))))
      custom-id)))

(defun gd/org-insert-reference-heading (&optional capitalize?)
  "Insert a pandoc reference to a heading, with completion.
We use ivy to find the required heading, and then insert a link
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
  (doom-project-browse my/org-directory))

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

(defmacro map-put-many! (map &rest rest)
  "Like `map-put!', but with many key-value pairs.
Key-value pairs should be supplied without any syntax
(so not as cons cells), just as a bare list."
  (when (oddp (length rest))
    (user-error "Odd number of arguments!"))
  (cl-loop for (k v) on '(k1 v1 k2 v2) by #'cddr
           collect `(map-put! map ,k ,v) into expressions
           finally (return (cons 'progn expressions))))
