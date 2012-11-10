;;;; themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/zenburn-emacs")
;(load-theme 'zenburn)
;(add-to-list 'load-path (concat dotfiles-dir "/zenburn-emacs"))
;(require 'color-theme-zenburn)
;(color-theme-zenburn)

;;;; maximize all new windows
(add-to-list 'load-path (concat dotfiles-dir "maxframe.el"))
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

;;;; elnode server
(add-to-list 'load-path (concat dotfiles-dir "/elnode"))
(load-library "elnode")

;;;; start in server mode
(server-mode t)

;;;; handy functions for deleting whitespace
(defun db48x/delete-ws-backward ()
  (interactive)
  (delete-region (point) (save-excursion (skip-syntax-backward " ") (point))))

(defun db48x/delete-ws-forward ()
  (interactive)
  (delete-region (point) (save-excursion (skip-syntax-forward " ") (point))))

(global-set-key (kbd "C-c C-d") 'db48x/delete-ws-forward)
(global-set-key (kbd "C-c M-d") 'db48x/delete-ws-backward)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;(require 'js-beautify)
;(global-set-key (kbd "C-c C-t") 'js-beautify)

(defun func-region (start end func)
  "run a function over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defun hex-region (start end)
  "urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-hexify-string))

(defun unhex-region (start end)
  "de-urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-unhex-string))

(fset 'jsdoc-add-for-this-function
   (lambda (&optional arg) "very simple macro that grabs the arg list for the function after point and creates a skelton javadoc/jsdoc for it. no error checking. put the point on the first line of the function name, before the arg list." (interactive "p") (kmacro-exec-ring-item (quote ([19 40 13 67108896 19 41 left 134217847 1 47 42 42 44 32 25 return 32 42 47 return up 67108896 up 134217765 44 32 return 17 10 32 42 32 64 112 97 114 97 109 32 return 33 18 47 42 127 92 42 5 32 backspace return 32 42 32] 0 "%d")) arg)))
(global-set-key [24 11 49] 'jsdoc-add-for-this-function)

(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

;;;; set up js2-mode
(add-to-list 'load-path (concat dotfiles-dir "/js2-mode"))
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;(require 'js2-highlight-vars)

(defun my-indent-sexp ()
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (syntax-ppss (point)))
             (beg (nth 1 parse-status))
             (end-marker (make-marker))
             (end (progn (goto-char beg) (forward-list) (point)))
             (ovl (make-overlay beg end)))
        (set-marker end-marker end)
        (overlay-put ovl 'face 'highlight)
        (goto-char beg)
        (while (< (point) (marker-position end-marker))
          ;; don't reindent blank lines so we don't set the "buffer
          ;; modified" property for nothing
          (beginning-of-line)
          (unless (looking-at "\\s-*$")
            (indent-according-to-mode))
          (forward-line))
        (run-with-timer 0.5 nil '(lambda(ovl)
                                   (delete-overlay ovl)) ovl)))))

(defun my-js2-mode-hook ()
  (define-key js2-mode-map [(control meta q)] 'my-indent-sexp)
  (define-key js2-mode-map [(meta control |)] 'cperl-lineup)
  (if (featurep 'js2-highlight-vars)
      (js2-highlight-vars-mode)))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)

;;;; set up org-mode
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;;;; set up epresent for presenting org files
(add-to-list 'load-path (concat dotfiles-dir "/epresent"))
(require 'epresent)

;;;; set up BBDB
(setq bbdb-file "~/.emacs.d/.bbdb")
(require 'bbdb-loaddefs "~/.emacs.d/bbdb/lisp/bbdb-loaddefs.el")
(setq bbdb-update-records-p 'create
      bbdb-message-pop-up 'horiz)
(bbdb-initialize 'gnus 'message)
(bbdb-mua-auto-update-init 'gnus 'message)
(setq bbdb-user-mail-address-re (regexp-opt '("db48x@yahoo.com"
                                              "db48x@db48x.net"
                                              "stone.of.erech@gmail.com"
                                              "daniel.brooks@ask.com"
                                              "dbrooks@cleanpowerfinance.com")))
;(add-to-list 'Info-directory-list "~/.emacs.d/bbdb/doc")

(require 'dbus)
(defun nm-is-connected()
  (equal 70 (dbus-get-property
             :system "org.freedesktop.NetworkManager" "/org/freedesktop/NetworkManager"
             "org.freedesktop.NetworkManager" "State")))

(defun switch-to-or-startup-gnus ()
  "Switch to Gnus group buffer if it exists, otherwise start Gnus in plugged or unplugged state, 
depending on network status."
  (interactive)
  (if (or (not (fboundp 'gnus-alive-p))
          (not (gnus-alive-p)))
      (if (nm-is-connected)
          (gnus)
        (gnus-unplugged))
    (switch-to-buffer gnus-group-buffer)
    (delete-other-windows)))

(global-set-key (kbd "<f12>") 'switch-to-or-startup-gnus)

(require 'gnus)
(setq gnus-init-file "~/.emacs.d/.gnus.el"
      gnus-startup-file "~/.emacs.d/.newsrc")

;;;; Magit for git integration
(add-to-list 'load-path (concat dotfiles-dir "magit"))
(require 'magit)

;;;; set up js2-refactor and dependencies
(add-to-list 'load-path (concat dotfiles-dir "expand-region.el"))
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(add-to-list 'load-path (concat dotfiles-dir "mark-multiple.el"))
(require 'inline-string-rectangle)
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)
(require 'mark-more-like-this)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
(global-set-key (kbd "C-*") 'mark-all-like-this)
;(require 'rename-sgml-tag)
;(define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)
;(require 'js2-rename-var)
;(define-key js2-mode-map (kbd "C-c C-r") 'js2-rename-var)

(add-to-list 'load-path (concat dotfiles-dir "js2-refactor.el"))
(require 'js2-refactor)

(load "init-erc.el")

(add-to-list 'load-path (concat dotfiles-dir "zencoding"))
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)

;;;; word count mode
(require 'wc-mode)
