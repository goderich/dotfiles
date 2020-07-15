;;; ~/dotfiles/doom-emacs/.config/doom/+keybindings.el -*- lexical-binding: t; -*-


;; comma as localleader is baws
(setq doom-localleader-key ",")

;; Global keybindings
(map!
 (:leader
  (:prefix "i"
   (:prefix ("n" . "Insert numbers")
    :desc "Insert on new lines" :n "n" 'insert-numbers
    :desc "Prepend to existing lines" :n "p" 'prepend-numbers))
  (:prefix "t"
   :desc "Toggle writeroom mode" :n "w" 'writeroom-mode)
  :desc "Make a new Emacs frame" :nv "F" 'make-frame))

;; evil-ex keybindings
;; I haven't found a way to map these with the `map!' macro,
;; so I'm assuming it can't be used that way.
(evil-ex-define-cmd "q" #'kill-this-buffer)

;; org-mode keybindings
(after! org
  ;; Unbind the already defined keys first.
  ;; general.el does this automatically for most things,
  ;; but not for newly defined prefix keys, so I need to
  ;; unbind "T" manually
  (map! :map org-mode-map
      :localleader "T" nil)
  (map! :map org-mode-map
    (:prefix "g"
      :nv "h" #'org-up-element
      :nv "k" #'org-previous-visible-heading)
    :localleader
    :desc "C-c C-c"           ","   #'org-ctrl-c-ctrl-c
    :desc "Open link"         "l"   #'org-open-at-point
    :desc "Archive subtree"   "A"   #'org-archive-subtree
    :desc "Open agenda"       "a"   #'org-agenda
    :desc "org-todo"          "t"   #'org-todo
    :desc "org-rifle"         "r"   #'helm-org-rifle-current-buffer
    (:prefix ("T" . "tags")
     :desc "Toggle tag groups" "g" #'org-toggle-tags-groups)))

;; org agenda keybindings
;; These complement the keybinds already set in evil-collection
(after! org-agenda
  (map! :map evil-org-agenda-mode-map
        :m "b" #'org-agenda-earlier
        :m "f" #'org-agenda-later))

;; lisp editing keybindings
(map! :map (emacs-lisp-mode-map dune-mode-map)
      (:localleader
        :desc "Forward slurp"    ")"   #'sp-forward-slurp-sexp
        :desc "Backward slurp"   "("   #'sp-backward-slurp-sexp
        :desc "Forward barf"     ">"   #'sp-forward-barf-sexp
        :desc "Backward barf"    "<"   #'sp-backward-barf-sexp))

;; markdown-mode keybindings
(map! :map markdown-mode-map
      :nvi "<tab>" #'markdown-cycle
      :nv "k" #'evil-previous-visual-line
      :nv "j" #'evil-next-visual-line
      (:prefix "g"
        :nv "h" #'markdown-up-heading)
      (:localleader
        :desc "Insert citation"   "c"  #'org-ref-helm-insert-cite-link))

(map! :map pdf-view-mode-map
      (:localleader
      :desc "Add text annotation" "t" 'pdf-annot-add-text-annotation))

(map! (:leader
        (:prefix "g"
          :nv "n" #'git-gutter:next-hunk)))
