;;;;;;;;;;;;
;; design ;;
;;;;;;;;;;;;

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


;;;;;;;;;;;;
;; action ;;
;;;;;;;;;;;;

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


;;;;;;;;;
;; env ;;
;;;;;;;;;
;;(setq exec-path (parse-colon-path (getenv "PATH")))

(setq load-path (cons "~/.emacs.d/elisp" load-path))

(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

(require 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load "~/.emacs.d/inits")


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


;;;;;;;;;
;; TeX ;;
;;;;;;;;;
;; TeXrm
(defun texrm ()
  (interactive)
  (message "TeXrm -- Done!")
  (shell-command-to-string "texrm -y"))
;;(async-shell-command "texrm -y"))
(add-hook 'yatex-mode-hook
          '(lambda ()
             (define-key YaTeX-mode-map (kbd "C-c d") 'texrm)))

;; YaTeX + RefTeX
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons (cons "\\.sty$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq YaTeX-open-lines 0)
(setq YaTeX-kanji-code nil)
;;(setq tex-command "platex")
(setq tex-command "latexmk -pvc")
(setq dviprint-from-format "-p %b")
(setq dviprint-to-format "-l %e")
(setq dviprint-command-format "dvips %f %t %s | lpr")
(add-hook 'yatex-mode-hook'(lambda ()(setq auto-fill-function nil)))
(add-hook 'latex-mode-hook 'turn-on-reftex)
(add-hook 'yatex-mode-hook 'turn-on-reftex)

;; SyncTeX
(defun evince-forward-search ()
  (interactive)
  (let* ((ctf (buffer-name))
         (mtf)
         (pf)
         (ln (format "%d" (line-number-at-pos)))
         (cmd "fwdevince")
         (args))
    (if (YaTeX-main-file-p)
        (setq mtf (buffer-name))
      (progn
        (if (equal YaTeX-parent-file nil)
            (save-excursion
              (YaTeX-visit-main t)))
        (setq mtf YaTeX-parent-file)))
    (setq pf (concat (car (split-string mtf "\\.")) ".pdf"))
    (setq args (concat pf " " ln " " ctf))
    (message (concat cmd " " args))
    (process-kill-without-query
     (start-process-shell-command "fwdevince" nil cmd args))))

(add-hook 'yatex-mode-hook
          '(lambda ()
             (define-key YaTeX-mode-map (kbd "C-c e") 'evince-forward-search)))

(require 'dbus)

(defun un-urlify (fname-or-url)
  "A trivial function that replaces a prefix of file:/// with just /."
  (if (string= (substring fname-or-url 0 8) "file:///")
      (substring fname-or-url 7)
    fname-or-url))

(defun th-evince-sync (file linecol &rest ignored)
  (let* ((fname (un-urlify file))
 ;        (buf (find-buffer-visiting fname))
         (buf (find-file fname))
         (line (car linecol))
         (col (cadr linecol)))
    (if (null buf)
        (message "[Synctex]: %s is not opened..." fname)
      (switch-to-buffer buf)
      (goto-line (car linecol))
      (unless (= col -1)
        (move-to-column col)))))

(defvar *dbus-evince-signal* nil)

(defun enable-evince-sync ()
  (require 'dbus)
  (when (and
         (eq window-system 'x)
         (fboundp 'dbus-register-signal))
    (unless *dbus-evince-signal*
      (setf *dbus-evince-signal*
            (dbus-register-signal
             :session nil "/org/gnome/evince/Window/0"
             "org.gnome.evince.Window" "SyncSource"
             'th-evince-sync)))))

(add-hook 'yatex-mode-hook 'enable-evince-sync)


;;;;;;;;;
;; Web ;;
;;;;;;;;;

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

 
;;;;;;;;;;;;;
;; Haskell ;;
;;;;;;;;;;;;;
(package-install 'intero)
(add-hook 'haskell-mode-hook 'intero-mode)
(custom-set-variables '(haskell-stylish-on-save t))

(require 'intero)
(require 'flycheck)
(flycheck-add-next-checker 'intero '(warning . haskell-hlint))

;;(require 'hs-lint)
;;(defun my-haskell-mode-hook ()
;;   (local-set-key "\C-cl" 'hs-lint))
;;(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)


;;;;;;;;
;; JS ;;
;;;;;;;;
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


;;;;;;;;;;
;; Rust ;;
;;;;;;;;;;
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
(eval-after-load "rust-mode"
  '(setq-default rust-format-on-save t))
(add-hook 'rust-mode-hook (lambda ()
                            (racer-mode)
                            (flycheck-rust-setup)))
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook (lambda ()
                             (company-mode)
                             (set (make-variable-buffer-local 'company-idle-delay) 0.1)
                             (set (make-variable-buffer-local 'company-minimum-prefix-length) 0)))

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

;;;;;;;;;;
;; Ruby ;;
;;;;;;;;;;
(autoload 'ruby-mode "ruby-mode"
   "Mode for editing ruby source files" t)
  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("[Rr]akefile$" . ruby-mode))

(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
(setq ruby-electric-expand-delimiters-list nil)

(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)

(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

;; repl
(add-to-list 'inf-ruby-implementations '("pry" . "pry"))
(setq inf-ruby-default-implementation "pry")
(setq inf-ruby-first-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)> *")
(setq inf-ruby-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)[>*\"'] *")

(add-hook 'ruby-mode-hook '(lambda () (define-key ruby-mode-map (kbd "C-c C-r") 'ruby-send-region)))
