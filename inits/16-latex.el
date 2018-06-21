;;;;;;;;;
;; TeX ;;
;;;;;;;;;
;; TeXrm
(defun texrm ()
  (interactive)
  (message "TeXrm -- Done!")
  (shell-command-to-string "texrm -y")
  )

(add-hook 'yatex-mode-hook
          '(lambda ()
             (define-key YaTeX-mode-map (kbd "C-c d") 'texrm)))

;; YaTeX + RefTeX
(setq auto-mode-alist
      (cons (cons "\\.tex\\'" 'yatex-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons (cons "\\.sty\\'" 'yatex-mode) auto-mode-alist))
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
