(in-package :nyxt-user)

(defvar *my-keymap* (make-keymap "my-map"))
(define-key *my-keymap*
  "down" 'nyxt/web-mode:scroll-down
  "up" 'nyxt/web-mode:scroll-up
  "h" 'nyxt/web-mode:follow-hint
  "H" 'nyxt/web-mode:follow-hint-new-buffer
  "f" 'nyxt/web-mode:history-backwards
  "?" 'nyxt/web-mode:search-buffers
  "b b" 'switch-buffer
  "r" 'reload-current-buffer
  "R" 'reload-buffer
  "; d" 'nyxt/web-mode:download-hint-url
  "; y" 'nyxt/web-mode:copy-hint-url
  "escape" 'nyxt/web-mode:remove-search-hints)

(define-mode my-mode ()
  "Dummy mode for the custom key bindings in `*my-keymap*'."
  ((keymap-scheme (keymap:make-scheme
                   scheme:cua *my-keymap*
                   scheme:emacs *my-keymap*
                   scheme:vi-normal *my-keymap*))))

(define-configuration (buffer web-buffer)
  ((default-modes (append '(my-mode vi-normal-mode) %slot-default))))
