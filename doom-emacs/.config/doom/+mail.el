;;; ~/dotfiles/doom-emacs/.config/doom/+mail.el -*- lexical-binding: t; -*-
(require 'mu4e)

;; Use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

(setq mu4e-drafts-folder "/bak.drafts")
(setq mu4e-sent-folder   "/bak.sent")
(setq mu4e-trash-folder  "/bak.trash")

;; Don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)
;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; additional non-Gmail addresses and want assign them different
;; behavior.)

;; Setup some handy shortcuts
;; You can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.
(setq mu4e-maildir-shortcuts
    '(("/inbox"      . ?i)
      ("/bak.sent"   . ?s)
      ("/bak.trash"  . ?t)))

;; Allow for updating mail using 'u' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; My credentials
(setq user-mail-address "yuhsien77@gmail.com"
      user-full-name  "郭育賢 Andre Goderich")

;; Sending mail
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      mu4e-compose-format-flowed nil
      mu4e-compose-dont-reply-to-self t)

;; Use a normal date format
(setq mu4e-headers-date-format "%F")

;; Default location for saving attachments
(setq mu4e-attachment-dir "~/Downloads")

;; Don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; Changing filenames when moving works better with mbsync,
;; but not with offlineimap, which is what I'm using now.
;; Having it as `t' gives me errors that mu4e can't find
;; the file.
(setq mu4e-change-filenames-when-moving nil)

;; Use org-mode to compose email
(add-hook 'mu4e-compose-mode-hook #'org-msg-post-setup)

;; Visually wrap lines at 80 colums when composing.
;; I have tried format=flowing before, but any shenanigans
;; with line wrapping break in Gmail. The only way to have
;; emails display decently on different devices is to send
;; them in long lines. Editing these is a pain though, hence
;; this setting.
(add-hook 'mu4e-compose-mode-hook (lambda () (setq fill-column 80)))
(add-hook 'mu4e-compose-mode-hook #'visual-fill-column-mode)

;; optional:
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(after! org-msg
  (setq org-msg-text-plain-alternative nil))
