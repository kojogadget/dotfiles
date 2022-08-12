(defun kg/load-system-settings ()
  (interactive)
  (load-file "~/dotfiles/.emacs.d/per-system-settings.el"))

(defun kg/system-settings-get (setting)
  (alist-get setting kg/system-settings))

(provide 'kg-settings)
