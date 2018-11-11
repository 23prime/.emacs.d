;; For Saiten
(defun insert-name ()
  (interactive)
  (insert "--[Hikawa] ")
  )

(global-set-key (kbd "C-c C-n") 'insert-name)
