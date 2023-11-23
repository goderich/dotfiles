;;; ~/dotfiles/doom-emacs/.config/doom/refs.el -*- lexical-binding: t; -*-

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
       (concat "awk \'/^#\\+label:\\s+" type "/ {print $2}\' ")
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
