;;; ~/dotfiles/doom-emacs/.config/doom/+referencing.el -*- lexical-binding: t; -*-

(setq reftex-default-bibliography my/default-bibliography)
(setq org-ref-default-bibliography reftex-default-bibliography)
(setq bibtex-completion-bibliography reftex-default-bibliography)

; Activate Yasnippets in BibTeX files
(add-hook 'bibtex-mode-hook #'yas-minor-mode-on)

; Formatting of helm-bibtex results
(after! org-ref-helm-bibtex
  (setq bibtex-completion-display-formats
        '((t . "${author:18} ${year:5} ${title:*}")))

  (setq bibtex-completion-format-citation-functions
        '((org-mode . my/bibtex-completion-format-citation-pandoc-citeproc)
          (latex-mode . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default . bibtex-completion-format-citation-default))))

; Ebib settings
(after! ebib
  (setq ebib-preload-bib-files `(,my/default-bibliography))
  (setq ebib-file-associations '(("pdf" . "zathura") ("ps" . "gv")))
  (setq ebib-notes-directory my/ebib-notes)
  (setq ebib-bibtex-dialect 'biblatex)
  (setq ebib-index-columns '(("Author/Editor" 20 t)
                             ("Year" 6 t)
                             ("Title" 40 t))))

; Visual line mode makes entries occupy 2 or even more lines in the index.
; This is unnecessary, because I can see the full list of authors and title
; right below in the entry window. It's better to see more entries at the
; same time in the index window.
(add-hook 'ebib-index-mode-hook (lambda () (visual-line-mode -1)))
