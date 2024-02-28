;;; ~/dotfiles/doom-emacs/.config/doom/+mail.el -*- lexical-binding: t; -*-

;; Use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

;; Allow for updating mail using 'u' in the main view.
(setq mu4e-get-mail-command "mbsync -a")

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
      smtpmail-servers-requiring-authorization "smtp.fastmail.com"
      smtpmail-default-smtp-server "smtp.fastmail.com"
      smtpmail-smtp-server "smtp.fastmail.com"
      mu4e-drafts-folder "/fastmail/Drafts"
      mu4e-sent-folder   "/fastmail/Sent"
      mu4e-trash-folder  "/fastmail/Trash"
      ;; Save sent messages to `mu4e-sent-folder'
      mu4e-sent-messages-behavior 'sent
      smtpmail-smtp-service 587
      ;; Do not wait for errors when sending mail
      mail-interactive nil)

;; Composing mail
(setq mu4e-compose-format-flowed nil
      mu4e-compose-dont-reply-to-self t
      mu4e-compose-in-new-frame t
      mu4e-context-policy 'ask-if-none
      mu4e-compose-context-policy 'ask-if-none)

;; Indexing and other behaviour
(setq mu4e-index-cleanup t
      mu4e-index-lazy-check nil)

;; Use a normal date format
(setq mu4e-headers-date-format "%F")

;; Default location for saving attachments
(setq mu4e-attachment-dir "~/Downloads")

;; Don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; Changing filenames when moving works better with mbsync.
(setq mu4e-change-filenames-when-moving t)

;; Use a function to decide how to open attachments
(setq mu4e-view-open-program #'gd/mu4e-open-file)

;; Use org-mode to compose email
(add-hook 'mu4e-compose-mode-hook #'org-msg-post-setup)
(after! org-msg
  (setq org-msg-text-plain-alternative nil))

;; Setup directory shortcuts
(setq mu4e-maildir-shortcuts
      '((:maildir "/fastmail/Archive"
         :key ?a
         :name "Archive")
        (:maildir "/fastmail/Sent"
         :key ?s
         :name "Sent")
        (:maildir "/fastmail/Trash"
         :key ?t
         :name "Trash")))

;; User information
(setq user-mail-address "goderich@fastmail.com"
      user-full-name "郭育賢 Andre Goderich"
      smtpmail-smtp-user "goderich@fastmail.com")

(setq mu4e-contexts
      `(
        ,(make-mu4e-context
          :name "Gmail"
          :enter-func (lambda () (mu4e-message "Entering Gmail context"))
          :leave-func (lambda () (mu4e-message "Leaving Gmail context"))
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches msg :to "yuhsien77")))
          :vars '((user-mail-address . "yuhsien77@gmail.com")))

        ,(make-mu4e-context
          :name "NCUE"
          :enter-func (lambda () (mu4e-message "Entering NCUE context"))
          :leave-func (lambda () (mu4e-message "Leaving NCUE context"))
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches msg :to "ncue.edu.tw")))
          :vars '((user-mail-address . "goderich@gm.ncue.edu.tw")))

        ,(make-mu4e-context
          :name "Default"
          :enter-func (lambda () (mu4e-message "Entering Private context"))
          :leave-func (lambda () (mu4e-message "Leaving Private context"))
          :match-func (lambda (_) t)
          :vars '((user-mail-address . "goderich@fastmail.com")))))
