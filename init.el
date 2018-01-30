;; key binding
;; type
(define-key global-map (kbd "C-z") 'undo)
(define-key global-map (kbd "C-v") 'yank)
(define-key global-map (kbd "C-y") 'yank)
(define-key global-map (kbd "C-c r") 'replace-string)


;; cursor
(define-key global-map (kbd "C-h") 'backward-char)
(define-key global-map (kbd "C-l") 'forward-char)
(define-key global-map (kbd "C-j") 'next-line)
(define-key global-map (kbd "C-k") 'previous-line)
(define-key global-map (kbd "M-h") 'backward-word)
(define-key global-map (kbd "M-l") 'forward-word)

;; scroll
(define-key global-map (kbd "C-p") 'scroll-down-command)
(define-key global-map (kbd "C-n") 'scroll-up-command)
(define-key global-map (kbd "C-f") 'recenter-top-bottom)

;; buffer && window
;; select buffer
(define-key global-map (kbd "M-k") 'previous-buffer)
(define-key global-map (kbd "M-j") 'next-buffer)
(define-key global-map (kbd "C-r") 'eval-buffer)
;; move window
(define-key global-map (kbd "C-c h") 'windmove-left)
(define-key global-map (kbd "C-c j") 'windmove-down)
(define-key global-map (kbd "C-c k") 'windmove-up)
(define-key global-map (kbd "C-c l") 'windmove-right)
;; select window size
(define-key global-map (kbd "C-c w") 'window-resizer)
;; quickrun
(define-key global-map (kbd "C-c C-q") 'quickrun)
;; shell
(define-key global-map (kbd "C-c C-s") 'shell)
;;(define-key global-map (kbd "C-c C-s") 'ansi-term)


;; others
(define-key global-map (kbd "C-b") 'describe-bindings)
(define-key global-map (kbd "C-SPC") 'nil) ;; kill to select input method
(define-key global-map (kbd "C-q") 'query-replace)
(define-key global-map (kbd "RET") 'newline)
(define-key global-map (kbd "C-o") 'newline)


;; Mozc
(global-set-key (kbd "C-|") 'mozc-mode)

(require 'mozc)
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")

;; character code
(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)

;; copy && paste
(setq x-select-enable-clipboard t)


;; design
;; kill menu bar
(menu-bar-mode -1)
;; kill menu bar
(tool-bar-mode -1)

;; column && line num
(column-number-mode t)
(global-linum-mode t)

;; highlight in cursor line
(global-hl-line-mode t)

;; theme setting
(custom-set-variables '(custom-enabled-themes (quote (tsdh-dark))))
(custom-set-faces )

;; kill start 
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; font
(when window-system
  (set-face-attribute 'default nil
              :family "Migu 1M"
              :height 102
              ))

;; action
;; kill backup
(setq make-backup-files nil)

;; kill auto seve
(setq delete-auto-save-files t)

;; tab setting
(setq-default tab-width 2 indent-tabs-mode nil)

;; only frame
(setq ns-pop-up-frames nil)

;; scroll line
(setq scroll-conservatively 1)

;; parenthesis
(show-paren-mode 1)

;; clear && active
(add-to-list 'default-frame-alist '(alpha . (1.0 1.0)))

;; return-tab kill
(setq electric-indent-mode nil)



;; extended packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)


;; YaTeX
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons (cons "\\.sty$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq YaTeX-open-lines 0)
(setq YaTeX-kanji-code nil)
(setq tex-command "platex")
(setq dviprint-from-format "-p %b")
(setq dviprint-to-format "-l %e")
(setq dviprint-command-format "dvips %f %t %s | lpr")
(add-hook 'yatex-mode-hook'(lambda ()(setq auto-fill-function nil)))
(add-hook 'latex-mode-hook 'turn-on-reftex)
(add-hook 'yatex-mode-hook 'turn-on-reftex)


;; web-mode setting
(require 'web-mode)
;; extensions
(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp\\'"     . web-mode))
;; indents
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-html-offset   4)
  (setq web-mode-css-offset    4)
  (setq web-mode-script-offset 4)
  (setq web-mode-php-offset    4)
  (setq web-mode-java-offset   4)
  (setq web-mode-asp-offset    4)
  (setq indent-tabs-mode t)
  (setq tab-width 4)
;; auto tag closing
  ;0=no auto-closing
  ;1=auto-close with </
  ;2=auto-close with > and </
  (setq web-mode-tag-auto-close-style 2))

  
(add-hook 'web-mode-hook 'web-mode-hook)

;; 

(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        c)
    (catch 'end-flag
      (while t
        (message "size[%dx%d]"
                 (window-width) (window-height))
        (setq c (read-char))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (message "Quit")
               (throw 'end-flag t)))))))



;; Haskell
(add-hook 'haskell-mode-hook 'intero-mode)

;; JS
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Rust
;; racerやrustfmt，コンパイラにパスを通す
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
;; rust-modeでrust-format-on-saveをtにすると自動でrustfmtが走る
(eval-after-load "rust-mode"
  '(setq-default rust-format-on-save t))
;; rustのファイルを編集するときにracerとflycheckを起動する
(add-hook 'rust-mode-hook (lambda ()
                            (racer-mode)
                            (flycheck-rust-setup)))
;; racerのeldocサポートを使う
(add-hook 'racer-mode-hook #'eldoc-mode)
;; racerの補完サポートを使う
(add-hook 'racer-mode-hook (lambda ()
                             (company-mode)
                             ;; この辺の設定はお好みで
                             (set (make-variable-buffer-local 'company-idle-delay) 0.1)
                             (set (make-variable-buffer-local 'company-minimum-prefix-length) 0)))


;; ansi-term
;;(defun skt:shell ()
;;  (or
;;   (executable-find "zsh")
;;   (executable-find "bash")
;;   (executable-find "cmdproxy")
;;   (error "can't find 'shell' command in PATH!!"))

;; Shell 名の設定
;;(setq shell-file-name (skt:shell))
;;(setenv "SHELL" shell-file-name)
;;(setq explicit-shell-file-name shell-file-name)
