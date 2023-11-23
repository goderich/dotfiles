;;; ~/dotfiles/doom-emacs/.config/doom/orca.el -*- lexical-binding: t; -*-

;; orca -- Org Reference Check Assistant
;; Duplicate reference checks for papers written in org-mode

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
               (-non-nil)
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
