;;;;;;;;;;;;
;; design ;;
;;;;;;;;;;;;
;; kill menu bar

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(menu-bar-mode -1)
;; kill menu bar
(tool-bar-mode -1)

;; column && line num
(column-number-mode t)

;; highlight in cursor line
(global-hl-line-mode t)

;; theme setting
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq custom-theme-directory "~/.emacs.d/themes")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tsdh-dark2)))
 '(custom-safe-themes
   (quote
    ("1ef8434cebfdf3505930f3a64a99f980704fdaa37741f47e0d3dab7cb57b283d" "cecd25a1b18009fd052cf7c4dfb1ace13d21d69da5ed0fbfda65df2e083c76e5" default)))
 '(haskell-stylish-on-save t)
 '(package-selected-packages
   (quote
    (multiple-cursors intero color-theme-modern smartparens flymake-easy flymake-cursor flycheck-yamllint yaml-mode magit markdown-mode markdown-mode+ yatex web-mode ruby-electric racer quickrun python-mode py-autopep8 nodejs-repl js2-mode jedi inf-ruby hlint-refactor flymake-python-pyflakes flymake-jslint flycheck-rust exec-path-from-shell cargo auto-highlight-symbol))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; kill start
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; font
;;(when window-system)
(set-face-attribute 'default nil
                    :family "Ricty Diminished"
                    ;;:family "Takenocoding"
                    :height 96
                    )
(set-fontset-font t 'japanese-jisx0208
                  (font-spec
                   :family "Ricty Diminished"
                   ;;:family "Takenocoding"
                  ))


;;;;;;;;;
;; env ;;
;;;;;;;;;
(setq load-path (cons "~/.emacs.d/elisp" load-path))

(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

(require 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load "~/.emacs.d/inits")


;;;;;;;;;;;;
;; action ;;
;;;;;;;;;;;;
;; kill backup
(setq make-backup-files nil)

;; kill auto seve
(setq auto-save-default nil)
(setq delete-auto-save-files t)

;; tab setting
(setq-default tab-width 2 indent-tabs-mode nil)

;; only frame
(setq ns-pop-up-frames nil)

;; scroll line
(setq scroll-conservatively 1)

;; clear && active
(add-to-list 'default-frame-alist '(alpha . (1.0 1.0)))

;; return-tab kill
(setq electric-indent-mode nil)


;; split into 4 windows
(defun window-spliter ()
  (interactive)
  (message "split!")
  (split-window-horizontally)
  (split-window-vertically)
  (other-window 2)
  (split-window-vertically)
  (other-window 1))
(global-set-key (kbd "C-c C-w") 'window-spliter)

;; resize window
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


;;;;;;;;;;;
;; input ;;
;;;;;;;;;;;
;;(global-set-key (kbd "C-|") 'mozc-mode)
;;(require 'mozc)
;;(set-language-environment "Japanese")
;;(setq default-input-method "japanese-mozc")

;; character code
(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)

;; copy && paste
(setq x-select-enable-clipboard t)


;; parenthesis
(show-paren-mode 1)
