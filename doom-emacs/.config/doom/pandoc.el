;;; ~/dotfiles/doom-emacs/.config/doom/pandoc.el -*- lexical-binding: t; -*-

;; Elisp functions to call Pandoc from Emacs

(defun gd/pandoc--defaults-option (extension)
  (pcase extension
    ("pdf" "-dpdf")
    ("html" "-drev")
    ("docx" "-ddoc")))

(defun gd/pandoc--find-csl (dir)
  "Find a single CSL file in DIR or supply a default."
  (let ((fs (f-glob "*.csl" dir))
        (default (f-full "~/dotfiles/pandoc/.local/share/pandoc/defaults/linguistics.csl")))
    (when (length> fs 1)
      (error "Error: more than one CSL file in current directory!"))
    (or (-first-item fs) default)))

(cl-defun gd/pandoc-org--convert (&key extension incremental numbered)
  "Convert the current file using pandoc.
The format and the defaults file need to be supplied by the caller."
  (save-buffer)
  (let* ((input (f-this-file))
         (dir (f-dirname input))
         (output (f-swap-ext input extension))
         (defaults (gd/pandoc--defaults-option extension))
         (metadata (f-join dir "metadata.yaml"))
         (style? (f-exists? (f-join dir "style.css")))
         (csl (gd/pandoc--find-csl dir))
         (args `("pandoc" ,input ,defaults
                 ,@(when (f-exists? metadata) `("--metadata-file" ,metadata))
                 ,@(when (and (string= extension "html") style?)
                     '("--css" "./style.css"))
                 ,@(when (and (string= extension "html") incremental)
                     '("--incremental=true"))
                 ,@(when (and (member extension '("pdf" "docx")) numbered)
                     '("--number-sections"))
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
  (run-hooks 'gd/pandoc-org->pdf-hook)
  (let ((num? (transient-arg-value "--number-sections" (transient-args 'gd/pandoc-transient))))
    (gd/pandoc-org--convert :extension "pdf" :numbered num?)))

(defun gd/pandoc-org->revealjs ()
  "Convert the current file to revealjs using pandoc.
Works only on org files using my revealjs template."
  (interactive)
  (let ((inc? (transient-arg-value "--incremental=true" (transient-args 'gd/pandoc-transient))))
    (gd/pandoc-org--convert :extension "html" :incremental inc?)))

(defun gd/pandoc-org->docx ()
  "Convert the current file to pdf using pandoc.
Works only on org files using my docx template."
  (interactive)
  (gd/pandoc-org--convert :extension "docx"))

(transient-define-infix gd/pandoc--incremental? ()
  :argument "--incremental=true"
  :shortarg "i"
  :class 'transient-switch
  :description "Toggle incremental lists (reveal.js only)."
  :init-value (lambda (obj)
                (oset obj value "--incremental=true")))

(transient-define-infix gd/pandoc--number-sections? ()
  :argument "--number-sections"
  :shortarg "n"
  :class 'transient-switch
  :description "Toggle section numbering (pdf/doc)."
  :init-value (lambda (obj)
                (oset obj value "--number-sections")))

(transient-define-prefix gd/pandoc-transient ()
  ["Convert this file with pandoc..."
   [("p" "to pdf" gd/pandoc-org->pdf)
    ("r" "to revealjs" gd/pandoc-org->revealjs)
    ("d" "to docx" gd/pandoc-org->docx)]
   [("q" "quit" transient-quit-all)]]
  ["Options"
   [(gd/pandoc--incremental?)
    (gd/pandoc--number-sections?)]])
