;;; init.el --- Emacs init file
;;  Author: Piyush Datta

;; set default directory to config folder
(getenv "HOME")
(setq default-directory "~/")
(setq ring-bell-function 'ignore)
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

;; packages
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-enable-at-startup nil)

(package-initialize)
(package-refresh-contents)

;; Setting up the package manager. Install if missing.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t))

;; workaround bug in Emacs 26.2
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Load theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'monokai t)

;; Take off tool bar/menu bar/scroll bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; replace the active region just by typing text, just like modern editors
(delete-selection-mode +1)

;; show column number in mode-line
(column-number-mode +1)

;; better scrolling experience
(setq scroll-margin 0
      scroll-conservatively 10000
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

;; increase line space for better readability
(setq-default line-spacing 3)

;; Always use spaces for indentation (default to 4 spaces).
(setq-default tab-width 4)

;; split package
(use-package "window"
  :ensure nil
  :config
  (defun ian/split-and-follow-horizontally ()
    "Split window below."
    (interactive)
    (split-window-below)
    (other-window 1))
  (defun ian/split-and-follow-vertically ()
    "Split window right."
    (interactive)
    (split-window-right)
    (other-window 1))
  (global-set-key (kbd "C-x 2") 'ian/split-and-follow-horizontally)
  (global-set-key (kbd "C-x 3") 'ian/split-and-follow-vertically))

;; no confirm kill process
(use-package files
  :ensure nil
  :config
  (setq confirm-kill-processes nil))

;; auto refresh buffer
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t
        auto-revert-verbose nil))

;; performance, only use eldoc in prog-mode
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :config
  (global-eldoc-mode -1)
  (add-hook 'prog-mode-hook 'eldoc-mode)
  (setq eldoc-idle-delay 0.4))

;; indentation
(use-package cc-vars
  :ensure nil
  :config
  (setq-default c-basic-offset 4)
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "k&r"))))

(use-package js
  :ensure nil
  :config (setq js-indent-level 2))

;; slow down mouse wheel
(use-package mwheel
  :ensure nil
  :config (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
                mouse-wheel-progressive-speed nil))

;; show matching parentheses, reduce the highlight delay instantly
;; (use-package paren
;;   :ensure nil
;;   :config
;;   (setq show-paren-delay 0)
;;   (show-paren-mode +1))

;; Maximize the emacs frame on start up and make font size to 13
(use-package frame
  :ensure nil
  :config
  (setq initial-frame-alist (quote ((fullscreen . maximized)))))

;; check spelling in strings and comments
(use-package flyspell
  :ensure nil
  :hook (prog-mode . flyspell-prog-mode))

;; auto pairing quotes and parantheses
(use-package elec-pair
  :ensure nil
  :config (add-hook 'prog-mode-hook 'electric-pair-mode))

;; code auto completion
(use-package company
  :diminish company-mode
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                            company-echo-metadata-frontend))
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)))

;; on the fly syntax checking
(use-package flycheck
  :hook (after-init . global-flycheck-mode))

;; make ido mode display vertically
(use-package ido-vertical-mode
  :hook ((after-init . ido-mode)
         (after-init . ido-vertical-mode))
  :config
  (setq ido-everywhere t
        ido-enable-flex-matching t
        ido-vertical-define-keys 'C-n-C-p-up-and-down))

(use-package flx-ido :config (flx-ido-mode +1))

;; interface for git
(use-package magit :bind ("C-x g" . magit-status))

;; lightweight syntax highlighting improvement for numbers, operators, and escape sequences
(use-package highlight-numbers :hook (prog-mode . highlight-numbers-mode))
(use-package highlight-operators :hook (prog-mode . highlight-operators-mode))
(use-package highlight-escape-sequences :hook (prog-mode . hes-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(highlight-escape-sequences highlight-operators highlight-numbers magit flx-ido ido-vertical-mode flycheck company use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
