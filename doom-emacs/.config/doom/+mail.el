;;; ~/dotfiles/doom-emacs/.config/doom/+mail.el -*- lexical-binding: t; -*-
(require 'mu4e)

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

(setq mu4e-maildir "~/.mail")
(setq mu4e-drafts-folder "/bak.drafts")
(setq mu4e-sent-folder   "/bak.sent")
(setq mu4e-trash-folder  "/bak.trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; additional non-Gmail addresses and want assign them different
;; behavior.)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
    '( ("/inbox"      . ?i)
       ("/bak.sent"   . ?s)
       ("/bak.trash"  . ?t)))

;; allow for updating mail using 'u' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; something about ourselves
(setq
   user-mail-address "yuhsien77@gmail.com"
   user-full-name  "Andre Goderich 郭育賢")

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
    smtpmail-stream-type 'starttls
    smtpmail-default-smtp-server "smtp.gmail.com"
    smtpmail-smtp-server "smtp.gmail.com"
    smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; don't use org-mode to compose email
(setq mu4e-compose-mode-hook
      '(flyspell-mode))

;; enable automatic wrapping of emails
(add-hook 'message-mode-hook 'auto-fill-mode)
(setq fill-column 72)
(setq mu4e-compose-format-flowed t)
;; optional:
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; use messages-are-flowing package
(add-hook 'message-mode-hook 'messages-are-flowing-use-and-mark-hard-newlines)
