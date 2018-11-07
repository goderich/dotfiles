;;; ~/dotfiles/doom-emacs/.config/doom/+keybindings.el -*- lexical-binding: t; -*-

;; Set my keybindings
(map!
 (:leader
   (:prefix "t"
   :desc "Toggle writeroom mode" :n "w" 'writeroom-mode)))
