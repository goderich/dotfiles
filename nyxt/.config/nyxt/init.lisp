(in-package :nyxt-user)

(defvar *my-keymap* (make-keymap "my-map"))
(define-key *my-keymap*
  "down"   'nyxt/web-mode:scroll-down
  "up"     'nyxt/web-mode:scroll-up
  "h"      'nyxt/web-mode:follow-hint
  "H"      'nyxt/web-mode:follow-hint-new-buffer
  "f"      'nyxt/web-mode:history-backwards
  "?"      'nyxt/web-mode:search-buffers
  "b b"    'switch-buffer
  "r"      'reload-current-buffer
  "R"      'reload-buffers
  "; d"    'nyxt/web-mode:download-hint-url
  "; y"    'nyxt/web-mode:copy-hint-url
  "escape" 'nyxt/web-mode:remove-search-hints)

(define-mode my-mode ()
  "Dummy mode for the custom key bindings in `*my-keymap*'."
  ((keymap-scheme (keymap:make-scheme
                   scheme:cua *my-keymap*
                   scheme:emacs *my-keymap*
                   scheme:vi-normal *my-keymap*))))

(define-configuration (buffer web-buffer)
  ((default-modes (append '(my-mode) %slot-default))))

(define-configuration browser
  ((session-restore-prompt :always-restore)))

(define-configuration prompt-buffer
  ((style
"* { font-family: monospace,monospace; font-size: 18px; line-height: 18px; }
body { overflow: hidden; margin: 0; padding: 0; }
#prompt-area { background-color: dimgray; display: grid; grid-template-columns: auto auto 1fr; width: 100%; color: white; }
#prompt { padding-left: 10px; line-height: 26px; }
#prompt-extra { line-height: 26px; padding-right: 7px; }
#input { border: none; outline: none; padding: 3px; background-color: #E8E8E8; width: 100%; autofocus: true; }
.source { margin-left: 10px; margin-top: 15px; }
.source-glyph { margin-right: 3px; }
.source-name { color: white; padding-left: 5px; line-height: 24px; background-color: gray; }
#suggestions { overflow-y: hidden; overflow-x: hidden; height: 100%; width: 100%; }
.source-content { margin-left: 10px; background-color: #F7F7F7; width: 100%; table-layout: fixed; }
.source-content th:first-child { width: 20%; }
.source-content th:nth-child(2) { width: 20%; }
.source-content td { white-space: nowrap; height: 20px; overflow: auto; }
.source-content th { font-weight: normal; padding-left: 3px; text-align: left; background-color: #E8E8E8; }
.source-content td::-webkit-scrollbar { display: none; }
#selection { background-color: 575757; color: white; }
.marked { background-color: darkgray; font-weight: bold; color: white; }
.selected { background-color: gray; color: white; }")))
