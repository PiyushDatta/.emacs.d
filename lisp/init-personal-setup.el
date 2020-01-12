;; init-personal-setup.el

;;; Code:

;;============================================================================
;;============================================================================
;;==================********* INSTALL PACKAGES *********======================
;;============================================================================
;;============================================================================
;;; remove SC if you are not using sunrise commander and org if you like outdated packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)


;;; Bootstrapping use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t))

;;============================================================================
;;============================================================================
;;==================********* NON INSTALL PACKAGES *********==================
;;============================================================================
;;============================================================================

(use-package emacs
  :config
  (setq user-full-name "Piyush Datta"
        frame-title-format '("Emacs")
        ring-bell-function 'ignore
        default-directory "~/"
        frame-resize-pixelwise t
        ;; better scrolling experience
        scroll-conservatively 10000
        scroll-preserve-screen-position t
        auto-window-vscroll nil
        load-prefer-newer t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  ;; increase line space for better readability
  (setq-default line-spacing 3
                indent-tabs-mode nil
                ;; Always use spaces for indentation (default to 4 spaces).
                tab-width 4))

;; startup page
(use-package "startup"
  :ensure nil
  :config (setq inhibit-startup-screen t))

;; window
;; (use-package "window"
;;  :ensure nil
;;  :config
;;  (defun split-and-follow-horizontally ()
;;  "Split window below."
;;  (interactive)
;;  (split-window-below)
;;  (other-window 1))
;;  (defun split-and-follow-vertically ()
;;  "Split window right."
;;  (interactive)
;;  (split-window-right)
;;  (other-window 1))
;;  (global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)
;;  (global-set-key (kbd "C-x 3") 'split-and-follow-vertically))

;; delete selection, replace the active region just by typing text, just like modern editors
(use-package delsel
  :ensure nil
  :config (delete-selection-mode +1))

;; no confirm kill process
(use-package files
  :ensure nil
  :config
  (setq confirm-kill-processes nil))

;; set default directory to config folder
(getenv "HOME")

;; Windows performance tweaks
(when *win64*
  (when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0))
  ;; Set the buffer size to 64K on Windows (from the original 4K)
  (when (boundp 'w32-pipe-buffer-size)
    (setq irony-server-w32-pipe-buffer-size (* 64 1024))))

;; create themes directory
(let ((themes-dir (concat default-directory "/.emacs.d/themes")))
  (unless (file-exists-p themes-dir)
    (make-directory themes-dir)))

;; Load theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'monokai t)

;; workaround bug in Emacs 26.2
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Set default font
(set-face-attribute 'default nil
                    :family "consolas"
                                        ; :family "Source Code Pro"
                    :height 170
                    :weight 'normal
                    :width 'normal)

;; show column number in mode-line
(column-number-mode +1)

;; Open last buffer on load up
(desktop-save-mode 1)

;; disable auto formatting
(setq web-mode-enable-auto-indentation nil)

;; auto revert mode (refresh buffer)
(global-auto-revert-mode 1)

;; auto refresh dired when file changes
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Make fringe half-width
(fringe-mode '(4 . 4))

;;; init-personal-setup.el ends here
