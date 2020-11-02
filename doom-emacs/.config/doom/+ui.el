;;; ~/dotfiles/doom-emacs/.config/doom/+ui.el -*- lexical-binding: t; -*-

;; Soft wrap long lines
(global-visual-line-mode 1)

;; Disable auto-fill-mode
(add-hook! '(markdown-mode-hook
             org-mode-hook
             mu4e-compose-mode-hook)
           #'turn-off-auto-fill)

;; Do not display line numbers
(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
              #'display-line-numbers-mode)

(after! evil-vars
  (setq evil-ex-substitute-global t))

;; Show diff when committing in Magit
(after! magit-commit
  (setq magit-commit-show-diff t))

;; Do not automatically fullscreen a frame on entering writeroom mode
(after! writeroom-mode
  (setq writeroom-fullscreen-effect 'maximized))

;; Banish the pointer to the upper-right corner on any keypress
(mouse-avoidance-mode 'banish)

(defun center-screen-after-fn (fn)
  "Return an advice that centers the screen after using FN.

This function is written specifically for the `center-screen-after'
macro."
  `(advice-add ,fn :after
               (lambda (&rest _)
                 (evil-scroll-line-to-center (line-number-at-pos)))))

(defmacro center-screen-after (fns)
  "Center screen after using any of the functions in FNS.

This is a convenience macro that takes an unquoted list
of function symbols (i.e. each function should be quoted
individually, this makes more sense with Emacs's autocomplete).
It generates an advice for each function that centers the
screen after the function is used. This is helpful with
various functions that move the screen during searching.
The advice is generated using the `center-screen-after-fn'
function.

Demo:

(center-screen-after ('evil-ex-search-next
                      'evil-ex-search-previous))
"
  (macroexp-progn (mapcar #'center-screen-after-fn fns)))

;; Center screen on various search functions
(center-screen-after ('evil-ex-search-next
                      'evil-ex-search-previous
                      'git-gutter:next-hunk
                      'git-gutter:previous-hunk))

(set-evil-initial-state!
  '(sly-repl-mode racket-repl-mode) 'emacs)

;; Disable popup windows by default
;; (make things open in new buffers instead).
;; I have also added a keybinding to toggle this mode (SPC t p).
(after! popup
  (+popup-mode -1))
