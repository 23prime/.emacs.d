;;;;;;;;;;;;;
;; Haskell ;;
;;;;;;;;;;;;;
(package-install 'intero)
(add-hook 'haskell-mode-hook 'intero-mode)


(require 'intero)
(require 'flycheck)
(flycheck-add-next-checker 'intero '(warning . haskell-hlint))

;;(require 'hs-lint)
;;(defun my-haskell-mode-hook ()
;;   (local-set-key "\C-cl" 'hs-lint))
;;(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
