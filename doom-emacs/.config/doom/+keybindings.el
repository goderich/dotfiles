;;; ~/dotfiles/doom-emacs/.config/doom/+keybindings.el -*- lexical-binding: t; -*-

;; Global keybindings

;; comma as localleader is baws
(setq doom-localleader-key ",")

(map!
 (:leader
   (:prefix "t"
    :desc "Toggle writeroom mode" :n "w" 'writeroom-mode)))

;; org-mode keybindings

(after! org
  (map! :map evil-org-mode-map
        :localleader
        :desc "Open link" :nv "l" 'org-open-at-point
        :desc "org-todo" :nv "t" 'org-todo))


;; mu4e keybindings

(after! mu4e-compose
  (map! :map mu4e-compose-mode-map
        :localleader
        :desc "Cancel and delete" :nv "k" 'mu4e-message-kill-buffer
        :desc "Add attachment" :nv "a" 'mail-add-attachment
        :desc "Send" :nv "s" 'message-send
        ))
