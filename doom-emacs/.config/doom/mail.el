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
(setq mu4e-get-mail-command "mbsync -a")

(setq mu4e-index-cleanup t
      mu4e-index-lazy-check nil)

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
      mu4e-compose-dont-reply-to-self t
      mu4e-compose-in-new-frame t)

(setq mu4e-context-policy 'ask-if-none)
(setq mu4e-compose-context-policy 'ask-if-none)

(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "Private"
           :enter-func (lambda () (mu4e-message "Entering Private context"))
           :leave-func (lambda () (mu4e-message "Leaving Private context"))
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg :to "yuhsien77")))
           :vars '((user-mail-address . "yuhsien77@gmail.com")
                   (user-full-name . "郭育賢 Andre Goderich")
                   (smtpmail-smtp-server . "smtp.gmail.com")
                   (smtpmail-smtp-service . 587)))

         ,(make-mu4e-context
           :name "NCUE"
           :enter-func (lambda () (mu4e-message "Entering NCUE context"))
           :leave-func (lambda () (mu4e-message "Leaving NCUE context"))
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg :to "goderich.*ncue.edu.tw")))
           :vars '((user-mail-address . "goderich@gm.ncue.edu.tw")
                   (user-full-name . "郭育賢 Andre Goderich")
                   (smtpmail-smtp-server . "smtp.gmail.com")
                   (smtpmail-smtp-service . 587)))))

;; Use a normal date format
(setq mu4e-headers-date-format "%F")

;; Default location for saving attachments
(setq mu4e-attachment-dir "~/Downloads")

;; Don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; Changing filenames when moving works better with mbsync.
(setq mu4e-change-filenames-when-moving t)

;; Use org-mode to compose email
(add-hook 'mu4e-compose-mode-hook #'org-msg-post-setup)

(after! org-msg
  (setq org-msg-text-plain-alternative nil))
