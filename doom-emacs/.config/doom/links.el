;;; ~/dotfiles/doom-emacs/.config/doom/links.el -*- lexical-binding: t; -*-

;; Org-mode links

(defun gd/insert-link (address &optional name)
  "Insert an Org link to ADDRESS.
Prompts for a link name (the string that will be visible
as the hyperlink text). If the prompt is left blank,
uses NAME if it's provided, and ADDRESS otherwise.

If on a whitespace at the end of the line,insert an extra
space before the link. This is useful when inserting links
in normal mode."
  (let* ((default (substring-no-properties (or name address)))
         (prompt (concat "Link name (default \"" default "\"): "))
         (link-name (read-string prompt "" nil default))
         (link-string (org-link-make-string address link-name)))
    (if (and (evil-eolp) (= (char-after) 32))
        (insert " " link-string)
      (insert link-string))))

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
If on a heading, link it. If on a link, edit it.
Elsewhere, insert last stored link:
either from `org-stored-links', or from the clipboard.
This is a convenience function to bind it to a single keystroke."
  (interactive)
  (cond
   ((org-at-heading-p) (org-store-link nil 1))
   ((org-in-regexp org-link-bracket-re 1) (org-insert-link))
   ((and org-stored-links) (gd/org-insert-last-stored-link))
   (t (gd/org-insert-link-from-clipboard))))
