(add-to-list 'load-path (concat dotfiles-dir "/maxframe.el"))
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

;;;; elnode server
(add-to-list 'load-path (concat dotfiles-dir "/elnode"))
(load-library "elnode")



;;;; emacs server
(server-mode)

