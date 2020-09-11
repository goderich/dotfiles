;;; ~/dotfiles/doom-emacs/.config/doom/+referencing.el -*- lexical-binding: t; -*-

; Activate Yasnippets in BibTeX files
(add-hook 'bibtex-mode-hook #'yas-minor-mode-on)

; Ebib settings
(after! ebib
  (setq ebib-preload-bib-files `(,my/default-bibliography))
  (setq ebib-file-associations '(("pdf" . "zathura") ("ps" . "gv")))
  (setq ebib-notes-directory my/ebib-notes)
  (setq ebib-bibtex-dialect 'biblatex)
  (setq ebib-index-columns '(("Author/Editor" 20 t)
                             ("Year" 6 t)
                             ("Title" 40 t)))
  (let-alist ebib-citation-commands ; requires let-alist package
    (setf (car .org-mode)
          '(("text" "@%K%< [%A]%>")
            ("paren" "[%(%<%A %>@%K%<, %A%>%; )]")))))

; Visual line mode makes entries occupy 2 or even more lines in the index.
; This is unnecessary, because I can see the full list of authors and title
; right below in the entry window. It's better to see more entries at the
; same time in the index window.
(add-hook 'ebib-index-mode-hook (lambda () (visual-line-mode -1)))
