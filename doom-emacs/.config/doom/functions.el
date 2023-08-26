;;; functions.el --- Various functions for my personal use -*- lexical-binding: t; -*-

(require 'dash)
(require 's)

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
       (-filter #'identity) ; remove nil values
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

(defun gd/mu4e-link-dwim ()
  "If on a link, open it. If not, store the current message as link.
This is a convenience function to bind it to a single keystroke,
to be used within mu4e's view mode."
  (interactive)
  (if (thing-at-point-url-at-point)
    (mu4e~view-browse-url-from-binding)
    (org-store-link nil 1)))

(defun gd/pandoc--find-csl (dir)
  "Find a single CSL file in DIR or supply a default."
  (let ((fs (f-glob "*.csl" dir))
        (default (f-full "~/dotfiles/pandoc/.local/share/pandoc/defaults/linguistics.csl")))
    (cond
     ((length> fs 1) (error "Error: more than one CSL file in current directory!"))
     ((null fs) default)
     (t (-first-item fs)))))

(cl-defun gd/pandoc-org--convert (&key extension defaults)
  "Convert the current file using pandoc.
The format and the defaults file need to be supplied by the caller."
  (save-buffer)
  (let* ((input (f-this-file))
         (dir (f-dirname input))
         (output (f-swap-ext input extension))
         (metadata (f-join dir "metadata.yaml"))
         (style (f-join dir "style.css"))
         (csl (gd/pandoc--find-csl dir))
         (args `("pandoc" "-i" ,input ,defaults
                 ,@(when (f-exists? metadata) `("--metadata-file" ,metadata))
                 ,@(when (and (string= extension "html") (f-exists? style))
                     `("--css" ,style))
                 "--csl" ,csl
                 "-o" ,output)))
    (message "Calling: %s" args)
    (apply #'start-process "pandoc" "*pandoc*" args)))

(defvar gd/pandoc-org->pdf-hook nil
  "Hook to run before converting from org-mode to PDF.")

(defun gd/pandoc-org->pdf ()
  "Convert the current file to pdf using pandoc.
Works only on org files using my pdf template."
  (interactive)
  (save-buffer)
  (run-hooks 'gd/pandoc-org->pdf-hook)
  (gd/pandoc-org--convert :extension "pdf" :defaults "-dpdf"))

(defun gd/pandoc-org->revealjs ()
  "Convert the current file to revealjs using pandoc.
Works only on org files using my revealjs template."
  (interactive)
  (gd/pandoc-org--convert :extension "html" :defaults "-drev"))

(defun gd/pandoc-org->docx ()
  "Convert the current file to pdf using pandoc.
Works only on org files using my docx template."
  (interactive)
  (gd/pandoc-org--convert :extension "docx" :defaults "-ddoc"))

;; Org-mode links

(defun gd/insert-link (address &optional name)
  "Insert an Org link to ADDRESS.
Prompts for a link name (the string that will be visible
as the hyperlink text). If the prompt is left blank,
uses NAME if it's provided, and ADDRESS otherwise."
  (let* ((default (or name address))
         (prompt (concat "Link name (default \"" default "\"): "))
         (link-name (read-string prompt "" nil default))
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

(defun gd/org--get-id-heading ()
  "Get the text and ID of an org heading.
Creates the ID if one isn't already present."
  (save-excursion
    (consult-org-heading)
    (let ((heading (org-get-heading 'no-tags 'no-todo 'no-priority 'no-comment))
          (id (concat "id:" (org-id-get-create))))
      (list id heading))))

(defun gd/org-insert-link-with-id ()
  "Insert a link to a heading with completion, using a unique ID."
  (interactive)
  (apply #'gd/insert-link (gd/org--get-id-heading)))

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

(defun gd/org-insert-last-stored-link ()
  "Assumes that `org-stored-links' is non-nil."
  (let ((link (pop org-stored-links)))
    (gd/insert-link (car link) (cadr link))))

(defun gd/org-link-dwim ()
  "Do-what-I-mean for linking.
If on a heading, link it. If on a link, open it.
Elsewhere, insert last stored link:
either from `org-stored-links', or from the clipboard.
This is a convenience function to bind it to a single keystroke."
  (interactive)
  (cond
   ((org-at-heading-p) (org-store-link nil 1))
   ((org-in-regexp org-link-bracket-re 1) (org-open-at-point))
   ((and org-stored-links) (gd/org-insert-last-stored-link))
   (t (gd/org-insert-link-from-clipboard))))

;; References in org-mode

(defun gd/org-insert-reference (str &optional capitalize?)
  (let ((str (if capitalize? (s-capitalize str) str)))
    (insert "[cite: @" str "]")))

(defun gd/org-insert-ref-heading (&optional capitalize?)
  "Insert a pandoc reference to a heading, with completion.
We use narrowing to find the required heading, and then insert a link
using its CUSTOM_ID property. If the property isn't set, it is
created."
  (interactive)
  (let ((custom-id (gd/org-get-custom-id)))
    (gd/org-insert-reference custom-id capitalize?)))

(defun gd/org-insert-ref-heading-capitalized ()
  (interactive)
  (gd/org-insert-ref-heading 'capitalize))

(defun gd/org--extract-latex-label (str)
  "Extract the text from inside a LaTeX label STR."
  (let ((regex (rx "\\label{" (group (1+ (or alpha ?: ?- ?_))) "}")))
    (string-match regex str)
    (match-string 1 str)))

(defun gd/org--get-labels-latex (type)
  "Get a list of all LaTeX labels in this file beginning with the string TYPE."
  (->> (f-this-file)
       (shell-quote-argument)
       (concat "grep \"\\\\\\label{\"" type " ")
       (shell-command-to-string)
       (s-trim-right)
       (s-split "\n")
       (-map #'gd/org--extract-latex-label)))

(defun gd/org--get-labels-org (type)
  "Get a list of all org labels in this file beginning with the string TYPE."
  (->> (f-this-file)
       (shell-quote-argument)
       (concat "awk \'/^#\\+label: " type "/ {print $2}\' ")
       (shell-command-to-string)
       (s-trim-right)
       (s-split "\n")))

(defun gd/org-get-labels (type)
  "Get a list of all labels in this file beginning with the string TYPE.
Searches for both org-mode and LaTeX style labels."
  (-remove #'s-blank? ;; removes nil as well
   (-concat
    (gd/org--get-labels-org type)
    (gd/org--get-labels-latex type))))

(defun gd/org-insert-crossref (type &optional capitalize?)
  "Insert an org-cite reference of a given TYPE.
Optionally capitalize it."
  (let ((choice (->> (gd/org-get-labels type)
                     (completing-read "Choose candidate:"))))
    (gd/org-insert-reference choice capitalize?)))

(defun gd/org-insert-ref-table ()
  (interactive)
  (gd/org-insert-crossref "tbl"))

(defun gd/org-insert-ref-table-capitalized ()
  (interactive)
  (gd/org-insert-crossref "tbl" 'capitalize))

(defun gd/org-insert-ref-figure ()
  (interactive)
  (gd/org-insert-crossref "fig"))

(defun gd/org-insert-ref-figure-capitalized ()
  (interactive)
  (gd/org-insert-crossref "fig" 'capitalize))

;; Temporary shadowing of Doom's mu4e functions.
;; Necessary because Doom does not yet fully support mu 1.10 fn names.
;; [[file:~/.doom/modules/email/mu4e/config.el::defun +mu4e-view-select-attachment (][source]]

(after! mu4e
  (defun +mu4e-view-select-attachment ()
    "Use completing-read to select a single attachment.
Acts like a singular `mu4e-view-save-attachments', without the saving."
    (if-let ((parts (delq nil (mapcar
                               (lambda (part)
                                 (when (assoc "attachment" (cdr part))
                                   part))
                               (mu4e--view-gather-mime-parts))))
             (files (+mu4e-part-selectors parts)))
        (cdr (assoc (completing-read "Select attachment: " (mapcar #'car files)) files))
      (user-error (mu4e-format "No attached files found"))))

  (defun +mu4e-view-open-attachment ()
    "Select an attachment, and open it."
    (interactive)
    (mu4e--view-open-file
     (mu4e--view-mime-part-to-temp-file (cdr (+mu4e-view-select-attachment))))))

;; Duplicate reference checks for papers written in org-mode
;; (make into a separate package?)
;; (name idea: orca (org ref checker assistance))

(defun gd/find-rx-all (regexp &optional group)
  "Return a list of matches of GROUP in REGEXP in current buffer.
If GROUP is not provided, default to matching the whole REGEXP."
  (let ((matches))
    (save-match-data
      (save-excursion
        (save-restriction
          (widen)
          (goto-char 1)
          (while (search-forward-regexp regexp nil t)
            (push (match-string-no-properties (or group 0)) matches)))))
    matches))

(defun gd/org-check-duplicate (lst)
  "Check LST for any duplicates and throw an error when one is found."
  (let ((hash (make-hash-table :test #'equal)))
    (--each lst
      (if (map-elt hash it)
          (error "Error! Duplicate name: %s." it)
        (map-put! hash it t)))
    (message "Success!")))

(defun gd/org-check-orphan-refs (labels refs)
  "Check whether any members of LABELS are absent from REFS.
Means an in-text reference that does not point to anything."
  (let ((orphan-refs (-difference (-map #'s-downcase refs) labels)))
    (when orphan-refs
      (error
       "Error! The following references do not have a parent:\n%s"
       orphan-refs))))

;; TODO: check table syntax (starts with "tbl:")

(defun gd/org-check-tbl ()
  (interactive)
  (let* ((regex (rx bol "#+label: " (group "tbl:" (+ (or alnum "-" "_")))))
         (labels (gd/find-rx-all regex 1))
         (ref-rx (rx "[cite" (opt "/t") ":" (* space) "@"
                     (group "tbl:" (+ (or alnum "-" "_"))) "]"))
         (refs (gd/find-rx-all ref-rx 1)))
    (gd/org-check-duplicate labels)
    (gd/org-check-orphan-refs labels refs)))

(defun gd/org-check-section-ids (ids)
  "Check that section IDS conform to pandoc-crossref requirements."
  (let ((incorrect-ids
         (--remove (s-starts-with? "sec:" it) ids)))
    (when incorrect-ids
        (error
         (concat
          "Error! All section IDs need to start with `sec:'\n"
          "The following sections violate this:\n%s")
         incorrect-ids))))

(defun gd/org-check-sec ()
  (interactive)
  (let* ((get-custom-id (lambda () (org-entry-properties (point) "CUSTOM_ID")))
         (ids (->>
               (org-map-entries get-custom-id)
               (-filter #'identity)
               (--map (map-elt it "CUSTOM_ID"))))
         (ref-rx (rx "[cite" (* (or "/" alpha)) ":" (* space) "@"
                     (group "sec:" (+ (or alnum "-" "_"))) "]"))
         (refs (gd/find-rx-all ref-rx 1)))
    (gd/org-check-section-ids ids)
    (gd/org-check-duplicate ids)
    (gd/org-check-orphan-refs ids refs)))

(defun gd/org-check-duplicate-fn ()
  (interactive)
  (let* ((fn (rx "[fn:" (group (+ (or alnum "-" "_"))) "]"))
         (matches (gd/find-rx-all fn 1)))
    (dolist (item (-frequencies matches))
      (unless (= (cdr item) 2)
        (error "Error! Footnote %s occurs %d times." (car item) (cdr item))))
    (message "Success!")))

(defun gd/org-check-duplicates-all ()
  (interactive)
  (gd/org-check-tbl)
  (gd/org-check-sec)
  (gd/org-check-duplicate-fn)
  (message "No errors found, compiling..."))
