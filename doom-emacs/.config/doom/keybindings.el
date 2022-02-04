;;; ~/dotfiles/doom-emacs/.config/doom/+keybindings.el -*- lexical-binding: t; -*-

;; comma as localleader is baws
(setq doom-localleader-key ",")

;; Global keybindings
(map!
 (:leader
  (:prefix "i"
   (:prefix ("n" . "Insert numbers")
    :desc "Insert on new lines"       :n "n" #'insert-numbers
    :desc "Prepend to existing lines" :n "p" #'prepend-numbers))
  (:prefix ("t" . "Toggle...")
   :desc "Toggle writeroom mode"   :n "w" #'writeroom-mode
   :desc "Toggle flyspell mode"    :n "s" #'flyspell-mode
   :desc "Toggle mixed pitch mode" :n "m" #'mixed-pitch-mode
   :desc "Toggle pop-up windows"   :n "p" #'+popup-mode)
  (:prefix "o"
   :desc "Ebib" :nv "e" #'ebib)
  :desc "Make a new Emacs frame" :nv "F" #'make-frame
  (:prefix "g"
   :nv "p" #'git-gutter:previous-hunk
   :nv "n" #'git-gutter:next-hunk)
  (:prefix ("B" . "org-books")
   :desc "Jump to reading" :nv "j" #'org-books-jump-to-reading)
  (:prefix ("m" . "org-moar")
   :desc "Find note"   :nv "f" #'org-moar-open-note
   :desc "Insert link" :nv "l" #'org-moar-link-note)))

;; TAB always indents (to insert an actual TAB, use "C-v TAB")
(setq tab-always-indent t)

;; evil keybindings
(map!
 :after evil
 :nv "k"  #'evil-previous-visual-line
 :nv "j"  #'evil-next-visual-line
 :nv "gj" #'evil-next-line
 :nv "gk" #'evil-previous-line
 :nv "zt" #'gd/scroll-line-to-top-minus-2
 :nv "zT" #'evil-scroll-line-to-top
 :nv "0"  #'gd/beginning-or-first-non-blank)

(map! :map evil-snipe-parent-transient-map
      :after evil-snipe
;; After a successful snipe, evil-snipe switches to a
;; transient mode, where f/t/s (whichever one was used
;; for the search), search for the next occurrence instead
;; of the usual behaviour, and F/T/S search in the other
;; direction. This mode also grabs the ";" and "," keys
;; for the same purpose. I'm okay with the ";" key being
;; used this way, but I prefer if "," wasn't touched.
      "," nil)

;; evil-ex keybindings (: commands)
;; I haven't found a way to map these with the `map!' macro,
;; so I'm assuming it can't be used that way.
(evil-ex-define-cmd "q"  #'kill-this-buffer)
(evil-ex-define-cmd "wq" #'save-and-kill-this-buffer)
(evil-ex-define-cmd "x"  #'save-and-kill-this-buffer)

(map! :map org-mode-map
      :after org
      (:prefix "g"
       :nv "h" #'gd/org-up-heading
       :nv "k" #'gd/org-previous-heading
       :nv "j" #'gd/org-next-heading
       :nv "l" #'org-next-visible-heading)
      :localleader
      :desc "C-c C-c"           ","   #'org-ctrl-c-ctrl-c
      :desc "Open link"         "l"   #'org-open-at-point
      :desc "Copy link"         "y"   #'gd/org-copy-this-link
      :desc "Archive subtree"   "A"   #'org-archive-subtree
      :desc "Open agenda"       "a"   #'org-agenda
      :desc "Edit source block" "s"   #'org-edit-src-code
      :desc "org-todo"          "t"   #'org-todo
      :desc "Insert citation"   "c"   #'ebib-insert-citation
      :desc "Insert link"       "L"   #'org-insert-last-stored-link
      :desc "Sparse tree"       "S"   #'org-sparse-tree
      ;; Unbind the already defined keys first.
      ;; general.el does this automatically for most things,
      ;; but not for prefix keys, so I need to
      ;; unbind "T" manually
      "T" nil
      (:prefix ("T" . "toggle")
       :desc "Toggle tag groups"  "g" #'org-toggle-tags-groups
       :desc "Toggle valign mode" "v" #'valign-mode)
      "i" nil
      (:prefix ("i" . "insert")
       :nv "l" #'gd/insert-org-link
       :nv "c" #'gd/insert-org-link-from-clipboard
       :nv "i" #'gd/insert-link-with-id)
      ;; Bindings for pandoc-mode
      :after pandoc-mode
      ;; =, p= is a prefix in Doom's org keybindings,
      ;; so I need to unbind it first.
      "p" nil
      :desc "Pandoc export" "p" #'pandoc-main-hydra/body)

;; These are active when editing a source code block in a separate window.
(map! :map org-src-mode-map
      :after org-src
      :nv "Z Z" #'org-edit-src-exit
      :nv "Z Q" #'org-edit-src-abort)

;; These complement the keybinds already set in evil-collection
(map! :map evil-org-agenda-mode-map
      :after org-agenda
      :m "b" #'org-agenda-earlier
      :m "f" #'org-agenda-later)

;; These are used when composing emails in mu4e
(map! :map org-msg-edit-mode-map
      :after org-msg
      :localleader
      :desc "Send message and exit" :nv "s" #'gd/send-confirm-has-recipient
      :desc "Attach file"           :nv "a" #'org-msg-attach-attach)

;; These are used with my own derived org-books-mode within the books.org file
(map! :map org-books-mode-map
      :localleader
      :desc "Add book from URL"       "u" #'org-books-add-url
      :desc "Add book from clipboard" "c" #'org-books-cliplink
      :desc "Start reading a book"    "r" #'org-books-start-reading
      :desc "Finish and rate book"    "f" #'org-books-rate-book
      :desc "DNF a book"              "n" #'org-books-dnf
      :desc "Jump to a current book"  "j" #'org-books-jump-to-reading)

;; lisp editing keybindings
(map! :map (emacs-lisp-mode-map ielm-map
            racket-mode-map racket-repl-mode-map
            sly-mode-map dune-mode-map)
      (:localleader
       :desc "Forward slurp"  :n ")" #'sp-forward-slurp-sexp
       :desc "Backward slurp" :n "(" #'sp-backward-slurp-sexp
       :desc "Forward barf"   :n ">" #'sp-forward-barf-sexp
       :desc "Backward barf"  :n "<" #'sp-backward-barf-sexp))

(map! :map markdown-mode-map
      :nvi "<tab>"      #'markdown-cycle
      :nvi "M-<left>"   #'markdown-promote
      :nvi "M-<right>"  #'markdown-demote
      :nvi "M-<up>"     #'markdown-move-up
      :nvi "M-<down>"   #'markdown-move-down
      :nvi "C-<return>" #'markdown-insert-list-item
      (:prefix "g"
       :nv "h" #'markdown-up-heading)
      (:localleader
       :desc "Insert citation"    "c" #'ebib-insert-citation
       :desc "Toggle valign mode" "v" #'valign-mode
       :after pandoc-mode
       :desc "Pandoc export" "p" #'pandoc-main-hydra/body))

(map! :map pdf-view-mode-map
      (:localleader
      :desc "Add text annotation" "t" #'pdf-annot-add-text-annotation
      :desc "Add highlight"       "h" #'pdf-annot-add-highlight-markup-annotation))

(map! :map pdf-annot-edit-contents-minor-mode-map
      :desc "Commit annotation" :nv "Z Z" #'pdf-annot-edit-contents-commit
      :desc "Abort annotation"  :nv "Z Q" #'pdf-annot-edit-contents-abort)

;; ebib keybindings
;; ebib has several windows, which use different maps
(map! :map ebib-index-mode-map
      :n "s"   #'ebib-save-current-database
      :n "S"   #'ebib-save-all-databases
      :n "J"   #'ebib-jump-to-entry
      :n [tab] #'ebib-edit-entry
      (:localleader
       :n "f" #'gd/ebib-import-file-from-index))

(map! :map ebib-entry-mode-map
      :n [tab] #'ebib-quit-entry-buffer
      :n "z"   #'ebib-leave-ebib-windows
      :n "E"   #'gd/ebib-edit-as-string
      :n "s"   #'ebib-save-current-database
      (:localleader
       :n "f" #'ebib-import-file))

(map! :map racket-mode-map
      (:localleader
       :n  "e e" #'vz/racket-eros-eval-last-sexp
       :nv "r"   #'racket-run-and-switch-to-repl
       :nv "R"   #'racket-run))

;; mu4e keybindings
;; When viewing email headers as a list
(map! :map mu4e-headers-mode-map
      :after mu4e
      :nv "r" #'mu4e-compose-reply)

;; When opening individual emails
(map! :map mu4e-view-mode-map
      :after mu4e
      :nv "r" #'mu4e-compose-reply
      :nv "R" #'gd/reply-received
      :nv "p" #'mu4e-view-mime-part-action
      :nv [RET] #'mu4e~view-browse-url-from-binding
      :localleader
      :nv "l" #'mu4e~view-browse-url-from-binding)

(map! :map tuareg-mode-map
      :after tuareg
      :localleader
      (:prefix ("e" . "Eval")
       :n  "e" #'tuareg-eval-phrase
       :v  "e" #'tuareg-eval-region
       :nv "b" #'tuareg-eval-buffer))

(map! :map helpful-mode-map
      :after helpful
      :nvm "q" #'evil-delete-buffer)
