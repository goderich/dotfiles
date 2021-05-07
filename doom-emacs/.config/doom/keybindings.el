;;; ~/dotfiles/doom-emacs/.config/doom/+keybindings.el -*- lexical-binding: t; -*-

;; comma as localleader is baws
(setq doom-localleader-key ",")

;; Global keybindings
(map!
 (:leader
  (:prefix "i"
   (:prefix ("n" . "Insert numbers")
    :desc "Insert on new lines" :n "n" #'insert-numbers
    :desc "Prepend to existing lines" :n "p" #'prepend-numbers))
  (:prefix ("t" . "Toggle...")
   :desc "Toggle writeroom mode" :n "w" #'writeroom-mode
   :desc "Toggle flyspell mode" :nv "s" #'flyspell-mode
   :desc "Toggle pop-up windows" :n "p" #'+popup-mode)
  (:prefix "o"
   :desc "Ebib" :nv "e" #'ebib)
  :desc "Make a new Emacs frame" :nv "F" #'make-frame
  (:prefix "g"
   :nv "p" #'git-gutter:previous-hunk
   :nv "n" #'git-gutter:next-hunk)))

;; TAB always indents
;; (to insert an actual TAB, use "C-v TAB")
(setq tab-always-indent t)

;; evil keybindings
(after! evil
  (map!
   :nv "k" #'evil-previous-visual-line
   :nv "j" #'evil-next-visual-line))

;; evil-snipe keybindings
(after! evil-snipe
  (map! :map evil-snipe-parent-transient-map
;; After a successful snipe, evil-snipe switches to a
;; transient mode, where f/t/s (whichever one was used
;; for the search), search for the next occurrence instead
;; of the usual behaviour, and F/T/S search in the other
;; direction. This mode also grabs the ";" and "," keys
;; for the same purpose. I'm okay with the ";" key being
;; used this way, but I prefer if "," wasn't touched.
        "," nil))

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
      :nv "h" #'gd/org-up-heading
      :nv "k" #'gd/org-previous-heading
      :nv "j" #'gd/org-next-heading
      :nv "l" #'org-next-visible-heading)
    :localleader
    :desc "C-c C-c"           ","   #'org-ctrl-c-ctrl-c
    :desc "Open link"         "l"   #'org-open-at-point
    :desc "Archive subtree"   "A"   #'org-archive-subtree
    :desc "Open agenda"       "a"   #'org-agenda
    :desc "Edit source block" "s"   #'org-edit-src-code
    :desc "org-todo"          "t"   #'org-todo
    :desc "org-rifle"         "r"   #'helm-org-rifle-current-buffer
    :desc "Insert citation"   "c"   #'ebib-insert-citation
    (:prefix ("T" . "tags")
     :desc "Toggle tag groups" "g" #'org-toggle-tags-groups)))

;; org-src keybindings
;; These are active when editing a source code block in a separate window.
(after! org-src
  (map! :map org-src-mode-map
        :nv "Z Z" #'org-edit-src-exit
        :nv "Z Q" #'org-edit-src-abort))

;; org agenda keybindings
;; These complement the keybinds already set in evil-collection
(after! org-agenda
  (map! :map evil-org-agenda-mode-map
        :m "b" #'org-agenda-earlier
        :m "f" #'org-agenda-later))

;; org-msg keybindings
;; These are used when composing emails in mu4e
(after! org-msg
  (map! :map org-msg-edit-mode-map
        :localleader
        :desc "Send message and exit" :nv "s" #'gd/send-confirm-has-recipient
        :desc "Attach file"           :nv "a" #'org-msg-attach-attach))

;; org-books keybindings
;; These are used with my own derived org-books-mode within the books.org file
(map! :map org-books-mode-map
      :localleader
      :desc "Add book from URL"       "u" #'org-books-add-url
      :desc "Add book from clipboard" "c" #'org-books-cliplink
      :desc "Start reading a book"    "r" #'gd/org-books-start-reading
      :desc "Finish and rate book"    "f" #'org-books-rate-book)

;; lisp editing keybindings
(map! :map (emacs-lisp-mode-map
            racket-mode-map
            sly-mode-map
            dune-mode-map)
      (:localleader
        :desc "Forward slurp"    ")"   #'sp-forward-slurp-sexp
        :desc "Backward slurp"   "("   #'sp-backward-slurp-sexp
        :desc "Forward barf"     ">"   #'sp-forward-barf-sexp
        :desc "Backward barf"    "<"   #'sp-backward-barf-sexp))

;; markdown-mode keybindings
(map! :map markdown-mode-map
      :nvi "<tab>" #'markdown-cycle
      (:prefix "g"
        :nv "h" #'markdown-up-heading)
      (:localleader
        :desc "Insert citation"   "c"  #'ebib-insert-citation))

(map! :map pdf-view-mode-map
      (:localleader
      :desc "Add text annotation" "t" #'pdf-annot-add-text-annotation))

;; ebib keybindings
;; ebib has several windows, which use different maps
(map! :map ebib-index-mode-map
      :n "s"   #'ebib-save-current-database
      :n "S"   #'ebib-save-all-databases
      :n "J"   #'ebib-jump-to-entry
      :n [tab] #'ebib-edit-entry)

(map! :map ebib-entry-mode-map
      :n [tab] #'ebib-quit-entry-buffer
      :n "z"   #'ebib-leave-ebib-windows
      :n "E"   #'gd/ebib-edit-as-string)

(map! :map racket-mode-map
      (:localleader
       :n "e e" #'racket-eval-last-sexp
       :nv "r" #'racket-run-and-switch-to-repl
       :nv "R" #'racket-run))
