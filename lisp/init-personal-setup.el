;;; init-personal-setup.el 

;;; Code:

;; set default directory to config folder
(getenv "HOME")
(setq default-directory "~/")
(setq ring-bell-function 'ignore)
(global-set-key (kbd "C-x C-f")  (lambda () (interactive)
                   (cd "~/.emacs.d")
                   (call-interactively 'find-file)))

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
          :height 170
          :weight 'normal
          :width 'normal)

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

;; Open last buffer on load up
(desktop-save-mode 1)

;; disable auto formatting
(setq web-mode-enable-auto-indentation nil)

;; auto revert mode (refresh buffer)
(global-auto-revert-mode 1)
;; auto refresh dired when file changes
(add-hook 'dired-mode-hook 'auto-revert-mode)

;;; init-personal-setup.el ends here