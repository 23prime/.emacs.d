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
(global-linum-mode t)

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
                    ;; :family "Ricty Diminished"
                    ;; :family "Takenocoding"
                    :family "noco"
                    :height 96
                    )
(set-fontset-font t 'japanese-jisx0208
                  (font-spec
                   ;; :family "Ricty Diminished"
                   ;; :family "Takenocoding"
                   :family "noco"
                  ))


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

(when (>= emacs-major-version 27)
  (require 'smartparens-config)
  (smartparens-global-mode t)
  ;; (show-smartparens-global-mode t)
)


;;;;;;;;;;;;;;;;;;;;;;;;
;; global key binding ;;
;;;;;;;;;;;;;;;;;;;;;;;;
;; kill binding
(global-set-key (kbd "C-SPC")   'nil) ;; kill for select input method

;; typing
(global-set-key (kbd "C-z")     'undo)
(global-set-key (kbd "C-;")     'undo)
(global-set-key (kbd "C-v")     'yank)
(global-set-key (kbd "C-y")     'kill-line)
(global-set-key (kbd "C-u")     'kill-whole-line)
(global-set-key (kbd "RET")     'newline)
(global-set-key (kbd "C-o")     'newline)
(global-set-key (kbd "C-b")     'delete-backward-char)
(global-set-key (kbd "C-q")     'query-replace)
(global-set-key (kbd "C-r")     'replace-string)
;;(global-set-key (kbd "C-c C-m") 'set-mark-command)
(global-set-key (kbd "C-c C-j") 'set-mark-command)

;; cursor
(global-set-key (kbd "C-h") 'backward-char)
(global-set-key (kbd "C-l") 'forward-char)
(global-set-key (kbd "C-k") 'previous-line)
(global-set-key (kbd "C-j") 'next-line)
(global-set-key (kbd "M-h") 'backward-word)
(global-set-key (kbd "M-l") 'forward-word)
(global-set-key (kbd "C-p") 'backward-paragraph)
(global-set-key (kbd "C-n") 'forward-paragraph)
;;(global-set-key (kbd "C-p") '(lambda () (interactive) (previous-line 5)))
;;(global-set-key (kbd "C-n") '(lambda () (interactive) (next-line 5)))

(defun beginning-of-line-or-indentation ()
  (interactive)
  (if (and (smie-rule-bolp) (not (bolp)))
      (beginning-of-line)
    (back-to-indentation)))

(if (< emacs-major-version 27)
    (global-set-key (kbd "C-a") 'beginning-of-line)
  (global-set-key (kbd "C-a") 'beginning-of-line-or-indentation)
)

;;(require 'multiple-cursors)
;;(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;;(global-set-key (kbd "C-M-j")       'mc/mark-next-like-this)
;;(global-set-key (kbd "C-M-k")       'mc/mark-previous-like-this)
;;(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)

;; scroll
(global-set-key (kbd "M-p") 'scroll-down-command)
(global-set-key (kbd "M-n") 'scroll-up-command)
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
;;(global-set-key (kbd "C-c C-s") 'shell)
;;(global-set-key (kbd "C-c C-s") 'ansi-term)

;; git
(global-set-key (kbd "C-x g") 'magit-status)
