;; -*- no-byte-compile: t; -*-
;;; private/iwaka/packages.el

;; No-distraction writing
(package! writeroom-mode)

;; Citations
(package! org-ref)

(package! helm-org-rifle)

(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))
;; When using org-roam via the `+roam` flag
(unpin! org-roam company-org-roam)
;; When using bibtex-completion via the `biblio` module
(unpin! bibtex-completion helm-bibtex ivy-bibtex)

;; Bibliography management
(package! ebib)
