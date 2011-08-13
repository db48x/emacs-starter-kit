;;;; maximize all new windows
(add-to-list 'load-path (concat dotfiles-dir "maxframe.el"))
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

;;;; elnode server
(add-to-list 'load-path (concat dotfiles-dir "/elnode"))
(load-library "elnode")

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

(require 'js-beautify)
(global-set-key (kbd "C-c C-t") 'js-beautify)


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
