;;; init.el --- Emacs init file
;;  Author: Piyush Datta

;; set default directory to config folder
(getenv "HOME")
(setq default-directory "~/")
(global-set-key (kbd "C-x C-f")  (lambda () (interactive)
                                     (cd "~/.emacs.d")
                                     (call-interactively 'find-file)))

;; create themes directory
(let ((themes-dir (concat default-directory "/.emacs.d/themes")))
  (unless (file-exists-p themes-dir)
    (make-directory themes-dir)))
		
;; set some keybinds
;; copy
(global-set-key (kbd "C-c") 'kill-ring-save)		
;; cut
(global-set-key (kbd "C-x x") 'kill-region)
;; paste
(global-set-key (kbd "C-x v") 'yank)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(setq package-enable-at-startup nil)
(package-initialize)

;; Setting up the package manager. Install if missing.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t))

;; workaround bug in Emacs 26.2
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Load theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'monokai t)
