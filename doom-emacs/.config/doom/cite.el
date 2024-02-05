;;; ~/dotfiles/doom-emacs/.config/doom/+referencing.el -*- lexical-binding: t; -*-

; Activate Yasnippets in BibTeX files
(add-hook 'bibtex-mode-hook #'yas-minor-mode-on)

; Ebib settings
(after! ebib
  (setq ebib-preload-bib-files (list my/default-bibliography))
  (setq ebib-notes-directory my/ebib-notes)
  (setq ebib-file-search-dirs (list my/ebib-file-search-dir))
  (setq ebib-import-directory my/ebib-import-directory)
  (setq ebib-file-associations '(("pdf" . "zathura") ("ps" . "gv")))
  (setq ebib-bibtex-dialect 'biblatex)
  (setq ebib-citation-insert-multiple t)
  (setq ebib-index-columns '(("Author/Editor" 20 t)
                             ("Year" 6 t)
                             ("Title" 40 t)))
  (map-put-many! ebib-reference-templates
                 "Article"
                 "{Author}. {Date|Year}. {\"Title\".} {Journaltitle|Journal} {Volume}{(Issue)}{:Pages}.{ Doi.}"
                 "Book"
                 "{Author|Editor}. {Date|Year}. {\"Title\".} {Address: }{Publisher.}")
  (setq ebib-notes-name-transform-function #'identity)
  (setq ebib-name-transform-function #'gd/ebib-generate-filename)
  ;; Set auto-generated citation key options
  (setq bibtex-autokey-year-length 4
        bibtex-autokey-titleword-length 0
        bibtex-autokey-name-separator "-"
        bibtex-autokey-year-title-separator ""
        bibtex-autokey-edit-before-use t)
  (map-put! ebib-citation-commands
            'org-mode
            '((("text" "[cite/t: %(@%K%< %A%>%; )]")
               ("paren" "[cite: %(@%K%< %A%>%; )]")
               ("bare" "@%K")
               ("no-name" "[cite/na: %(@%K%< %A%>%; )]")))))

;; Visual line mode makes entries occupy 2 or even more lines in the index.
;; This is unnecessary, because I can see the full list of authors and title
;; right below in the entry window. It's better to see more entries at the
;; same time in the index window.
;; I also turn off `evil-snipe-mode' so that I can use the s and S keys
;; in my own keybindings.
(add-hook! 'ebib-index-mode-hook #'turn-off-visual-line-mode
                                 #'turn-off-evil-snipe-mode)
(add-hook! 'ebib-entry-mode-hook #'turn-off-evil-snipe-mode)

; Org-cite settings
(after! oc

  (defun gd/ebib-open-on-citation (citation _)
    (let ((key (map-elt (cadr citation) :key)))
      (ebib)
      (ebib-db-set-current-entry-key key ebib--cur-db)
      (ebib--update-buffers 'no-refresh)))

  (org-cite-register-processor 'gd/org-cite-follow-processor
    ;; Note that the citation is passed as an object, not a string.
    ;; The follow function must take two arguments.
    ;; See `org-cite-register-processor' documentation for details.
    :follow #'gd/ebib-open-on-citation)

  (setq org-cite-global-bibliography (list my/default-bibliography))
  (setq org-cite-follow-processor 'gd/org-cite-follow-processor))
