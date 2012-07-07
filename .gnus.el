;; mail comes from imap
(setq gnus-select-method '(nnimap "tuffmail"
                                  (nnimap-address "mail.mxes.net")
                                  (nnimap-port 993)
                                  (nnimap-stream ssl)))

;; and is sent via smtp
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.mxes.net"
      smtpmail-smtp-service 587)

;; store sent mail on the server
(setq gnus-message-archive-group "nnimap+tuffmail:INBOX/Sent")

;; add messages I read to the agent cache?
;(add-hook 'gnus-agent-fetch-selected-article 'gnus-select-article-hook)

;; I am me
(setq gnus-posting-styles '((".*"
                             (address "db48x@db48x.net")
                             (name "Daniel Brooks")
                             (face (gnus-convert-png-to-face "~/Deus Ex Guy 48×48 6color.png")))))

;; display email correctly
; show ticking elapsed time "header"
(setq gnus-article-update-date-headers 1)

; show face and x-face headers
(setq gnus-treat-display-x-face 'head
      gnus-treat-display-face 'head)

(setq gnus-group-line-format "%p%M%s  %B%(%-40G %7uy/%-7ut %L%)\n"
      gnus-group-line-format "%p%M%s  %B%(%-40g %7N/%-7t  %L%)\n"
      gnus-summary-line-format "%U%R%z %16&user-date; %4k: %* %-32,32f %B %s\n")

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(setq gnus-summary-same-subject "...")
(setq gnus-sum-thread-tree-root "> "
      gnus-sum-thread-tree-false-root "> "
      gnus-sum-thread-tree-indent "  "
      gnus-sum-thread-tree-single-indent ""
      gnus-sum-thread-tree-vertical "| "
      gnus-sum-thread-tree-single-leaf "\\-> "
      gnus-sum-thread-tree-leaf-with-other "+-> ")

(setq gnus-sum-thread-tree-root "┬─"
      gnus-sum-thread-tree-false-root "└┬"
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-single-indent "─"
      gnus-sum-thread-tree-vertical "│"
      gnus-sum-thread-tree-single-leaf "└─"
      gnus-sum-thread-tree-leaf-with-other "├─")

; the total number of messages in the group
(defun gnus-user-format-function-t (dummy)
  (case (car gnus-tmp-method)
    (nnimap
     (let ((count (nnimap-request-message-count gnus-tmp-qualified-group gnus-tmp-news-server)))
       (if count (format "%d" (car count))
         "?")))
    (t gnus-tmp-number-total)))

; the number of unseen messages in the group
(defun gnus-user-format-function-y (dummy)
  (case (car gnus-tmp-method)
    (nnimap
     (let ((count (nnimap-request-message-count gnus-tmp-qualified-group gnus-tmp-news-server)))
       (if count (format "%d" (cadr count))
         "?")))
    (t gnus-tmp-number-of-unread)))

; unseen/total
(defun gnus-user-format-function-x (dummy)
  (case (car gnus-tmp-method)
    (nnimap
     (let ((count (nnimap-request-message-count gnus-tmp-qualified-group gnus-tmp-news-server)))
       (if count (format "%d/%d" (cadr count) (car count))
         "?")))
    (t (format "%s/%s" gnus-tmp-number-of-unread gnus-tmp-number-total))))

;; helper functions for getting message counts from imap
(defvar nnimap-message-count-cache-alist nil)

(defun nnimap-message-count-cache-clear nil
  (setq nnimap-message-count-cache-alist nil))

(defun nnimap-message-count-cache-get (mbox &optional server)
  (when (nnimap-possibly-change-server server)
    (cadr (assoc (concat nnimap-currnet-server ":" mbox)
                 nnimap-message-count-cache-alist))))

(defun nnimap-message-count-cache-set (mbox count &optional server)
  (when (nnimap-possibly-change-server server)
    (push (list (concat nnimap-current-server ":" mbox)
                count)
          nnimap-message-count-cache-alist)))

(defun nnimap-request-message-count (mbox &optional server)
  (let ((count (or (nnimap-message-count-cache-get mbox server)
                   (and (nnimap-possibly-change-server server)
                        (progn (message "Requesting message count for %s..." mbox)
                               (prog1 (imap-mailbox-status mbox '(message unseen) nnimap-server-buffer)
                                 (message "Requesting message count for %s...done" mbox)))))))))

(add-hook 'gnus-after-getting-new-news-hook 'nnimap-message-count-cache-clear)

;; integrate with NetworkManager to detect offline state
(require 'dbus)
(setq gnus-nm-dbus-registration nil)
(defvar gnus-nm-connected-hook nil
  "Functions to run when network is connected.")
(defvar gnus-nm-disconnected-hook nil
  "Functions to run when network is disconnected.")

(defun imap-nuke-server-processes()
 "Brutally kill running IMAP server background processes. Useful
when Gnus hangs on network outs or changes."
  (interactive)
  (let ((sm (if gnus-select-method
                (cons gnus-select-method gnus-secondary-select-methods)
              gnus-secondary-select-methods)))
    (while sm
      (let ((method (car (car sm)))
            (vserv (nth 1 (car sm))))
        (when (and (eq 'nnimap method)
                   (buffer-local-value 'imap-process (get-buffer (nnimap-get-server-buffer vserv))))
          (gnus-message 6 "Killing IMAP process for server %s" vserv)
          (delete-process (buffer-local-value 'imap-process (get-buffer (nnimap-get-server-buffer vserv))))))
      (setq sm (cdr sm)))))

(defun gnus-nm-agent-unplug()
  "Kill IMAP server processes and unplug Gnus agent."
  (gnus-message 6 "Network is disconnected, unplugging Gnus agent.")
  (with-current-buffer gnus-group-buffer
;    (imap-nuke-server-processes) ; optional, help prevent hangs in IMAP processes when network has gone down.
    (gnus-agent-toggle-plugged nil)))

(defun gnus-nm-agent-plug() 
  "Plug Gnus agent."
  (gnus-message 6 "Network is connected, plugging Gnus agent.")
  (with-current-buffer gnus-group-buffer
    (gnus-agent-toggle-plugged t)))

(defun gnus-nm-state-dbus-signal-handler (nmstate)
  "Handles NetworkManager signals and runs appropriate hooks."
  (when (and (fboundp 'gnus-alive-p) (gnus-alive-p))
    (cond
     ((or (= 20 nmstate) (= 10 nmstate) (= 50 nmstate) (= 60 nmstate))
      (run-hooks 'gnus-nm-disconnected-hook))
     ((= 70 nmstate)
      (run-hooks 'gnus-nm-connected-hook)))))

(defun gnus-nm-enable()
  "Enable integration with NetworkManager."
  (interactive)
  (when (not gnus-nm-dbus-registration)
    (progn (setq gnus-nm-dbus-registration
                 (dbus-register-signal :system
                  "org.freedesktop.NetworkManager" "/org/freedesktop/NetworkManager"
                  "org.freedesktop.NetworkManager" "StateChanged"
                  'gnus-nm-state-dbus-signal-handler))
           (gnus-message 6 "Enabled integration with NetworkManager"))))

(defun gnus-nm-disable()
  "Disable integration with NetworkManager."
  (interactive)
  (when gnus-nm-dbus-registration
      (progn (dbus-unregister-object gnus-nm-dbus-registration)
             (setq gnus-nm-dbus-registration nil)
             (gnus-message 6 "Disabled integration with NetworkManager"))))

;; Add hooks for plugging/unplugging on network state change:
(add-hook 'gnus-nm-connected-hook 'gnus-nm-agent-plug)
(add-hook 'gnus-nm-disconnected-hook 'gnus-nm-agent-unplug)
;; Add hooks for enabling/disabling integration on startup/shutdown:
(add-hook 'gnus-started-hook 'gnus-nm-enable)
(add-hook 'gnus-exit-gnus-hook 'gnus-nm-disable)
