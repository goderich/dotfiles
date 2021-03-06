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
    '(("/gmail"      . ?g)
      ("/ncue"       . ?n)
      ("/bak.sent"   . ?s)
      ("/bak.trash"  . ?t)))

;; Allow for updating mail using 'u' in the main view.
;; Warning: fdm needs a -k flag or a "keep" action in the .fdm.conf file,
;; otherwise it will nuke mail on the server.
(setq mu4e-get-mail-command "fdm fetch")

(setq mu4e-bookmarks
      '((:name "Unread messages" :query "flag:unread" :key ?u)
        (:name "Today's messages" :query "date:today..now" :key ?t)
        (:name "Last 7 days" :query "date:7d..now" :key ?w)))

;; Disable visual-line-mode in the headers view for better alignment.
(add-hook 'mu4e-headers-mode-hook #'turn-off-visual-line-mode)

;; Sending mail
(require 'smtpmail)
(setq message-send-mail-function #'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      mu4e-compose-format-flowed nil
      mu4e-compose-dont-reply-to-self t)

(setq mu4e-context-policy 'pick-first)
(setq mu4e-compose-context-policy 'ask-if-none)

(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "Private"
           :enter-func (lambda () (mu4e-message "Entering Private context"))
           :leave-func (lambda () (mu4e-message "Leaving Private context"))
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg :to "yuhsien77@gmail.com")))
           :vars '((user-mail-address . "yuhsien77@gmail.com")
                   (user-full-name . "郭育賢 Andre Goderich")
                   (smtpmail-smtp-server . "smtp.gmail.com")
                   (smtpmail-smtp-service . 587)))

         ;; Right now I can't send emails from the school address without a school IP.
         ;; (see here: https://olis.ncue.edu.tw/ct.asp?xItem=7767&ctNode=1137&mp=1)
         ;; It might be possible to choose an SMTP server that lets me send email anywhere.
         ;; ,(make-mu4e-context
         ;;   :name "NCUE"
         ;;   :enter-func (lambda () (mu4e-message "Entering NCUE context"))
         ;;   :leave-func (lambda () (mu4e-message "Leaving NCUE context"))
         ;;   :match-func (lambda (msg)
         ;;                 (when msg
         ;;                   (mu4e-message-contact-field-matches msg :to "goderich@cc.ncue.edu.tw")))
         ;;   :vars '((user-mail-address . "goderich@cc.ncue.edu.tw")
         ;;           (user-full-name . "郭育賢 Andre Goderich")
         ;;           (smtpmail-smtp-server . "cc.ncue.edu.tw")
         ;;           (smtpmail-smtp-service . 25)))
         ))

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

(after! org-msg
  (setq org-msg-text-plain-alternative nil))
