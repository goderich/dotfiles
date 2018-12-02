;;; ~/dotfiles/doom-emacs/.config/doom/+keybindings.el -*- lexical-binding: t; -*-


;; comma as localleader is baws
(setq doom-localleader-key ",")

;; Global keybindings
(map!
 (:leader
   (:prefix "t"
    :desc "Toggle writeroom mode" :n "w" 'writeroom-mode)))

;; org-mode keybindings
(after! org
  (map! :map evil-org-mode-map
        (:localleader
        :desc "Open link" :nv "l" 'org-open-at-point
        :desc "org-todo" :nv "t" 'org-todo)
        ))


;;;; mu4e keybindings
;; headers-mode
;; (after! mu4e-headers
;;  (add-hook 'mu4e-headers-mode-hook #'evil-normalize-keymaps))
;; (after! mu4e-headers
;;   (evil-define-key 'normal mu4e-headers-mode-map "r" 'mu4e-compose-reply))
(map! :after mu4e-headers
      :map mu4e-headers-mode-map
      :n "r" 'mu4e-compose-reply)

;; compose-mode
(map! :after mu4e-compose
      :map mu4e-compose-mode-map
      :localleader
      :desc "Cancel and delete" :nv "k" 'mu4e-message-kill-buffer
      :desc "Add attachment" :nv "a" 'mail-add-attachment
      :desc "Send" :nv "s" 'message-send
        )
