;; key binding
;; type
(define-key global-map (kbd "C-z") 'undo)
(define-key global-map (kbd "C-v") 'yank)
;; cursor
(define-key global-map (kbd "C-h") 'backward-char)
(define-key global-map (kbd "C-l") 'forward-char)
(define-key global-map (kbd "C-j") 'next-line)
(define-key global-map (kbd "C-k") 'previous-line)
;; scroll
(define-key global-map (kbd "C-n") 'scroll-up-command)
(define-key global-map (kbd "C-p") 'scroll-down-command)
(define-key global-map (kbd "C-f") 'recenter-top-bottom)
;; buffer
(define-key global-map (kbd "M-n") 'next-buffer)
(define-key global-map (kbd "M-p") 'previous-buffer)
(define-key global-map (kbd "C-r") 'eval-buffer)
;; others
(define-key global-map (kbd "C-b") 'describe-bindings)
(define-key global-map (kbd "C-x C-m") 'magit-status)
(define-key global-map (kbd "C-SPC") 'nil)
(define-key global-map (kbd "C-q") ' query-replace)
(define-key global-map (kbd "C-y") ' kill-line)
(define-key global-map (kbd "RET") ' newline)


;; Mozc
(global-set-key (kbd "C-|") 'mozc-mode)

(require 'mozc)
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")

;; character code
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
              :height 120))

;; action
;; kill backup
(setq make-backup-files nil)

;; kill auto seve
(setq delete-auto-save-files t)

;; tab setting
(setq-default tab-width 4 indent-tabs-mode nil)

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
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)


;; haskell
(add-hook 'haskell-mode-hook 'intero-mode)

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
