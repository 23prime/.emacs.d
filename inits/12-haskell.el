;;;;;;;;;;;;;
;; Haskell ;;
;;;;;;;;;;;;;
(add-hook 'haskell-mode-hook 'intero-mode)

(require 'intero)
(require 'flycheck)
(flycheck-add-next-checker 'intero '(warning . haskell-hlint))

(defun intero-repl-and-flycheck ()
  (interactive)
  (delete-other-windows)
  (flycheck-list-errors)
  (intero-repl-load)
  (split-window-below)
  (other-window 1)
  (switch-to-buffer flycheck-error-list-buffer)
  (other-window 1)
  (windmove-right)
  )

(add-hook 'haskell-mode-hook
          '(lambda ()
             (define-key intero-mode-map (kbd "C-c C-p") 'intero-repl-and-flycheck)
             (define-key intero-mode-map (kbd "C-c C-h") 'haskell-hoogle)
             (define-key intero-mode-map (kbd "C-c C-r") 'intero-restart)
             (define-key intero-mode-map (kbd "C-c C-a") 'hlint-refactor-refactor-at-point)
             (define-key intero-mode-map (kbd "C-c C-b") 'hlint-refactor-refactor-buffer)
             ))

(custom-set-variables '(haskell-stylish-on-save t))


(require 'smartparens)

;;(sp-local-pair 'haskell-mode "{-# L" "ANGUAGE  #-}")
