(eval-when-compile (require 'cl))
(require 'benchmark)

;;; customize-variables
(defgroup init-loader nil
  "Loader of configuration files."
  :prefix "init-loader-"
  :group 'initialization)

(defcustom init-loader-directory
  (expand-file-name (concat (if (boundp 'user-emacs-directory)
                                (file-name-as-directory user-emacs-directory)
                              "~/.emacs.d/")
                            "inits"))
  "Default directory of configuration files."
  :type 'directory)

(defcustom init-loader-show-log-after-init t
  "Show loading log message if this value is t. If this value is `error-only',
log message is shown only errors occured."
  :type 'boolean)

(defcustom init-loader-byte-compile nil
  "Byte-compile configuration files if this value is non-nil."
  :type 'boolean)

(defcustom init-loader-default-regexp "\\(?:\\`[[:digit:]]\\{2\\}\\)"
  "Regular expression determining valid configuration file names.

The default value matches files that start with two digits.  For
example, 00_foo.el, 01_bar.el ... 99_keybinds.el."
  :type 'regexp)

(defcustom init-loader-meadow-regexp "\\`meadow-"
  "Regular expression of Meadow specific configuration file names."
  :type 'regexp)

(defcustom init-loader-windows-regexp "\\`windows-"
  "Regular expression of Windows specific configuration file names."
  :type 'regexp)

(defcustom init-loader-carbon-emacs-regexp "\\`carbon-emacs-"
  "Regular expression of Carbon Emacs specific configuration file names."
  :type 'regexp)

(defcustom init-loader-cocoa-emacs-regexp "\\`cocoa-emacs-"
  "Regular expression of Cocoa Emacs specific configuration file names."
  :type 'regexp)

(defcustom init-loader-nw-regexp "\\`nw-"
  "Regular expression of no-window Emacs configuration file names."
  :type 'regexp)

(defcustom init-loader-linux-regexp "\\`linux-"
  "Regular expression of GNU/Linux specific configuration file names."
  :type 'regexp)

;;;###autoload
(defun* init-loader-load (&optional (init-dir init-loader-directory))
  "Load configuration files in INIT-DIR."
  (let ((init-dir (init-loader-follow-symlink init-dir))
        (is-carbon-emacs nil))
    (assert (and (stringp init-dir) (file-directory-p init-dir)))
    (init-loader-re-load init-loader-default-regexp init-dir t)

    ;; Windows
    (when (featurep 'dos-w32)
      (init-loader-re-load init-loader-windows-regexp init-dir))
    ;; meadow
    (when (featurep 'meadow)
      (init-loader-re-load init-loader-meadow-regexp init-dir))

    ;; Carbon Emacs
    (when (featurep 'carbon-emacs-package)
      (init-loader-re-load init-loader-carbon-emacs-regexp init-dir)
      (setq is-carbon-emacs t))
    ;; Cocoa Emacs
    (when (or (memq window-system '(ns mac))
              (and (not is-carbon-emacs) ;; for daemon mode
                   (not window-system)
                   (eq system-type 'darwin)))
      (init-loader-re-load init-loader-cocoa-emacs-regexp init-dir))

    ;; GNU Linux
    (when (eq system-type 'gnu/linux)
      (init-loader-re-load init-loader-linux-regexp init-dir))

    ;; no-window
    (when (not window-system)
      (init-loader-re-load init-loader-nw-regexp init-dir))

    (case init-loader-show-log-after-init
      (error-only (add-hook 'after-init-hook 'init-loader--show-log-error-only))
      ('t (add-hook 'after-init-hook 'init-loader-show-log)))))

(defun init-loader-follow-symlink (dir)
  (cond ((file-symlink-p dir)
         (expand-file-name (file-symlink-p dir)))
        (t (expand-file-name dir))))

(defvar init-loader--log-buffer nil)
(defun init-loader-log (&optional msg)
  (if msg
      (when (stringp msg)
        (push msg init-loader--log-buffer))
    (mapconcat 'identity (reverse init-loader--log-buffer) "\n")))

(defvar init-loader--error-log-buffer nil)
(defun init-loader-error-log (&optional msg)
  (if msg
      (when (stringp msg)
        (push msg init-loader--error-log-buffer))
    (mapconcat 'identity (reverse init-loader--error-log-buffer) "\n")))

(defvar init-loader-before-compile-hook nil)
(defun init-loader-load-file (file)
  (when init-loader-byte-compile
    (let* ((path (file-name-sans-extension (locate-library file)))
           (el (concat path ".el")) (elc (concat path ".elc")))
      (when (and (not (file-exists-p el)) (file-exists-p elc))
        (error "There is only byte-compiled file."))
      (when (or (not (file-exists-p elc))
                (file-newer-than-file-p el elc))
        (when (file-exists-p elc) (delete-file elc))
        (run-hook-with-args 'init-loader-before-compile-hook file)
        (byte-compile-file el))))
  (load file))

(defun init-loader-re-load (re dir &optional sort)
  ;; 2011/JUN/12 zqwell: Don't localize `load-path' and use it as global
  (add-to-list 'load-path dir)
  (dolist (el (init-loader--re-load-files re dir sort))
    (condition-case e
        (let ((time (car (benchmark-run (init-loader-load-file (file-name-sans-extension el))))))
          (init-loader-log (format "loaded %s. %s" (locate-library el) time)))
      (error
       ;; 2011/JUN/12 zqwell: Improve error message
       ;; See. http://d.hatena.ne.jp/kitokitoki/20101205/p1
       (init-loader-error-log (format "%s. %s" (locate-library el) (error-message-string e)))))))

;; 2011/JUN/12 zqwell Read first byte-compiled file if it exist.
;; See. http://twitter.com/#!/fkmn/statuses/21411277599
(defun init-loader--re-load-files (re dir &optional sort)
  (loop for el in (directory-files dir t)
        when (and (string-match re (file-name-nondirectory el))
                  (or (string-match "elc\\'" el)
                      (and (string-match "el\\'" el)
                           (not (locate-library (concat el "c"))))))
        collect (file-name-nondirectory el) into ret
        finally return (if sort (sort ret 'string<) ret)))

(defun init-loader--show-log-error-only ()
  (let ((err (init-loader-error-log)))
    (when (and err (not (string= err "")))
      (init-loader-show-log))))

;;;###autoload
(defun init-loader-show-log ()
  "Show init-loader log buffer."
  (interactive)
  (let ((b (get-buffer-create "*init log*")))
    (with-current-buffer b
      (view-mode -1)
      (erase-buffer)
      (insert "------- error log -------\n\n"
              (init-loader-error-log)
              "\n\n")
      (insert "------- init log -------\n\n"
              (init-loader-log)
              "\n\n")
      ;; load-path
      (insert "------- load path -------\n\n"
              (mapconcat 'identity load-path "\n"))
      (goto-char (point-min)))
    (switch-to-buffer b)
    (view-mode +1)))

(provide 'init-loader)
