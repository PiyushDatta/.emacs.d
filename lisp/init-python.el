;;; init-python.el

;;; Code:

;;============================================================================
;;============================================================================
;;==================********* PYTHON PACKAGES *********=======================
;;============================================================================
;;============================================================================

;; python
(use-package python
  :ensure t)

;; autocompletion for python
;; to install:
;; M-x jedi:install-server
(use-package jedi
  :ensure t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; to turn emacs into python ide
(use-package elpy
  :ensure t
  :config
  ;; Use Flycheck instead of Flymake
  (when (require 'flycheck nil t)
    (remove-hook 'elpy-modules 'elpy-module-flymake)
    (remove-hook 'elpy-modules 'elpy-module-yasnippet)
    (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation)
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  (elpy-enable)
  ;; Enable elpy in a Python mode
  (add-hook 'python-mode-hook 'elpy-mode)
  ; (setq py-python-command "/usr/bin/python3")
  (setq elpy-rpc-python-command "python3")
  (setq elpy-rpc-backend "jedi")
  (setq jedi:complete-on-dot t)
  ;; turn off warning for E111 (indentation of four)
  (setq elpy-syntax-check-command "flake8 --max-line-length 80 --ignore=E111, E114 ")
  ;; Open the Python shell in a buffer after sending code to it
  (add-hook 'inferior-python-mode-hook 'python-shell-switch-to-shell)
  ;; Use IPython as the default shell, with a workaround to accommodate IPython 5
  ;; https://emacs.stackexchange.com/questions/24453/weird-shell-output-when-using-ipython-5
  (setq python-shell-interpreter "ipython3")
  (setq python-shell-interpreter-args "--simple-prompt -i")
  ;; Enable pyvenv, which manages Python virtual environments
  (pyvenv-mode 1)
  ;; Tell Python debugger (pdb) to use the current virtual environment
  ;; https://emacs.stackexchange.com/questions/17808/enable-python-pdb-on-emacs-with-virtualenv
  (setq gud-pdb-command-name "python3 -m pdb "))

;; turn of indenation mode for elpy
(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))

;; python flycheck
(use-package flycheck-pyflakes
  :ensure t
  :defer t
  :config (add-hook 'python-mode-hook 'flycheck-mode))

;; ;; python pep8 style standards
(use-package py-autopep8
  :ensure t
  :config
  (setq py-autopep8-options '("--max-line-length=80" "--indent-size=2")))
  ;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

;; set flycheck custom settings
(setq flycheck-checker 'python-flake8)
(setq flycheck-flake8-maximum-line-length 80)
(add-to-list 'flycheck-disabled-checkers 'python-pylint)
;;; init-python.el ends here
