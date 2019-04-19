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
  (map! :map evil-org-mode-map
        (:localleader
        :desc "Open link" :nv "l" 'org-open-at-point
        :desc "org-todo" :nv "t" 'org-todo)
        ))

;; markdown-mode keybindings
(map! :map markdown-mode-map
      :nv "<tab>" 'markdown-cycle)

