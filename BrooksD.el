(add-to-list 'load-path (concat dotfiles-dir "/maxframe.el"))
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

(server-mode)

