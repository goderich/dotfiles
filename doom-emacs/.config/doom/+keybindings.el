;;; ~/dotfiles/doom-emacs/.config/doom/+keybindings.el -*- lexical-binding: t; -*-


;; comma as localleader is baws
(setq doom-localleader-key ",")

;; Global keybindings
(map!
 (:leader
   (:prefix "t"
    :desc "Toggle writeroom mode" :n "w" 'writeroom-mode)
    :desc "Make a new Emacs frame" :nv "F" 'make-frame))

;; org-mode keybindings
(after! org
  ;; Unbind the already defined keys first.
  ;; general.el does this automatically for most things,
  ;; but not for newly defined prefix keys, so I need to
  ;; unbind "T" manually
  (map! :map org-mode-map
        :localleader "T" nil)
  (map! :map org-mode-map
      :localleader
      :desc "C-c C-c"     ","   #'org-ctrl-c-ctrl-c
      :desc "Open link"   "l"   #'org-open-at-point
      :desc "org-todo"    "t"   #'org-todo
      (:prefix ("T" . "tags")
        :desc "Toggle tag groups" "g" #'org-toggle-tags-groups)))

;; markdown-mode keybindings
(map! :map markdown-mode-map
      :nvi "<tab>" 'markdown-cycle
      (:prefix "g"
        :nv "h" 'markdown-up-heading))

(map! :map pdf-view-mode-map
      (:localleader
      :desc "Add text annotation" "t" 'pdf-annot-add-text-annotation))
