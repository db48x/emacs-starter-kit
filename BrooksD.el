;;;; maximize all new windows
(add-to-list 'load-path (concat dotfiles-dir "maxframe.el"))
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

;;;; start in server mode
(server-mode)

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
