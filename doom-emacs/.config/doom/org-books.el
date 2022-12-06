;;; ../../dotfiles/doom-emacs/.config/doom/org-books.el -*- lexical-binding: t; -*-

;; org-books setup and configuration

(setq org-books-file (f-join org-directory "books.org"))

;; Derive a separate mode for org-books-specific keybinds
(define-derived-mode org-books-mode org-mode "Org books mode"
  "Major mode for managing prose using org.")

;; Autostart this mode when opening the org-books file
(add-to-list 'auto-mode-alist `(,org-books-file . org-books-mode))
;; Add new books at the bottom of the chosen subtree
(setq org-books-add-to-top nil)
;; Allow adding books under level 3 headings
(setq org-books-file-depth 3)

(defun org-books-setup+ ()
  (make-local-variable 'org-cycle-hook)
  (remove-hook 'org-cycle-hook #'org-cycle-hide-drawers 'local))

(add-hook 'org-books-mode-hook #'org-books-setup+)

(add-hook 'org-books-after-insert-hook #'gd/org-books-tag-short)

;;; org-books automatic genre tags settings
(after! org-books
  ;; Here I have tags that need to be manually mapped to genres:
  (map-put-many! org-books-genre-tag-associations
                 "Humor" "funny"
                 "Dark Fantasy" "dark")

  ;; And here is a whitelist of tags that will simply be
  ;; transformed via `s-snake-case'.
  (let ((org-books-genre-tag-whitelist
         '("Mystery" "Urban Fantasy" "Vampires"
           "Crime" "Dark" "Horror")))
    (cl-loop for genre in org-books-genre-tag-whitelist
             do (map-put! org-books-genre-tag-associations
                          genre
                          (s-snake-case genre)))))
