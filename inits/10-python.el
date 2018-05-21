(require 'python)
(defun python-shell-parse-command ()
  "Return the string used to execute the inferior Python process."
  "python3 -i"
  )

(defun my-jedi-mode-setup ()
  (require 'jedi-core)
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-to-list 'company-backends 'company-jedi))

(defun my-python-mode-setup ()
  (require 'py-autopep8)
  (setq py-autopep8-options '("--max-line-length=200" "--ignore=E731"))
  (py-autopep8-enable-on-save)

  (flymake-mode t)
  (require 'flymake-python-pyflakes)
  (flymake-python-pyflakes-load)
  )

(add-hook
 'python-mode-hook
 '(lambda ()
    (my-jedi-mode-setup)
    (my-python-mode-setup)
    ))

(setq python-shell-interpreter "python3"
      python-shell-interpreter-args "-i")
