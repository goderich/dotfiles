(in-package :nyxt-user)

(defvar *my-keymap* (make-keymap "my-map"))
(define-key *my-keymap*
  "down" 'nyxt/web-mode:scroll-down
  "up" 'nyxt/web-mode:scroll-up
  "h" 'nyxt/web-mode:follow-hint
  "H" 'nyxt/web-mode:follow-hint-new-buffer
  "f" 'nyxt/web-mode:history-backwards)

(define-mode my-mode ()
  "Dummy mode for the custom key bindings in `*my-keymap*'."
  ((keymap-scheme (keymap:make-scheme
                   scheme:cua *my-keymap*
                   scheme:emacs *my-keymap*
                   scheme:vi-normal *my-keymap*))))

(define-configuration (buffer web-buffer)
  ((default-modes (append '(my-mode) %slot-default))))
