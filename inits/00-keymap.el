;;;;;;;;;;;;;;;;;;;;;;;;
;; global key binding ;;
;;;;;;;;;;;;;;;;;;;;;;;;
;; typing
(global-set-key (kbd "C-z")     'undo)
(global-set-key (kbd "C-;")     'undo)
(global-set-key (kbd "C-v")     'yank)
(global-set-key (kbd "C-y")     'kill-line)
(global-set-key (kbd "C-b")     'delete-backward-char)
(global-set-key (kbd "C-SPC")   'nil) ;; kill to select input method
(global-set-key (kbd "C-q")     'query-replace)
(global-set-key (kbd "C-r")     'replace-string)
(global-set-key (kbd "RET")     'newline)
(global-set-key (kbd "C-o")     'newline)
(global-set-key (kbd "C-u")     'kill-whole-line)

(global-set-key (kbd "C-c C-m") 'set-mark-command)
(global-set-key (kbd "C-c C-j") 'set-mark-command)

;; cursor
(global-set-key (kbd "C-h") 'backward-char)
(global-set-key (kbd "C-l") 'forward-char)

(global-set-key (kbd "C-j") 'next-line)
(global-set-key (kbd "C-k") 'previous-line)
(global-set-key (kbd "M-h") 'backward-word)
(global-set-key (kbd "M-l") 'forward-word)

;; scroll
(global-set-key (kbd "C-p") 'scroll-down-command)
(global-set-key (kbd "C-n") 'scroll-up-command)
(global-set-key (kbd "C-f") 'recenter-top-bottom)

;; buffer
(global-set-key (kbd "M-k")     'previous-buffer)
(global-set-key (kbd "M-j")     'next-buffer)
(global-set-key (kbd "C-c C-r") 'eval-buffer)

;; move window
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c l") 'windmove-right)
;; select window size
(global-set-key (kbd "C-c w") 'window-resizer)

;; system
;; quickrun
(global-set-key (kbd "C-c C-q") 'quickrun)
(global-set-key (kbd "C-c q") 'quickrun)
;; shell
(global-set-key (kbd "C-c C-s") 'shell)
;;(global-set-key (kbd "C-c C-s") 'ansi-term)

;; git
(global-set-key (kbd "C-x g") 'magit-status)
