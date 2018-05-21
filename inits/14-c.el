;;;;;;;
;; C ;;
;;;;;;;
(require 'cc-mode)

(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-default-style "k&r") ;; カーニハン・リッチースタイル
            (setq indent-tabs-mode nil)  ;; タブは利用しない
            (setq c-basic-offset 2)      ;; indent は 2 スペース
            ))

(add-hook 'c-mode-common-hook 'flycheck-mode)

(defmacro flycheck-define-clike-checker (name command modes)
  `(flycheck-define-checker ,(intern (format "%s" name))
     ,(format "A %s checker using %s" name (car command))
     :command (,@command source-inplace)
     :error-patterns
     ((warning line-start (file-name) ":" line ":" column ": 警告:" (message) line-end)
      (error line-start (file-name) ":" line ":" column ": エラー:" (message) line-end))
     :modes ',modes))
(flycheck-define-clike-checker c-gcc-ja
			       ("gcc" "-fsyntax-only" "-Wall" "-Wextra")
			       c-mode)
(add-to-list 'flycheck-checkers 'c-gcc-ja)
(flycheck-define-clike-checker c++-g++-ja
			       ("g++" "-fsyntax-only" "-Wall" "-Wextra" "-std=c++11")
			       c++-mode)
(add-to-list 'flycheck-checkers 'c++-g++-ja)
