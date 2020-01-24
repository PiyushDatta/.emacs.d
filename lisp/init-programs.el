;;; init-programs.el

;;; Code:

;;============================================================================
;;============================================================================
;;==================********* CUSTOM PACKAGES *********=======================
;;============================================================================
;;============================================================================

;; performance, eldoc
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :config
  (global-eldoc-mode -1)
  (add-hook 'prog-mode-hook 'eldoc-mode)
  (setq eldoc-idle-delay 0.4))

;; helm - incremental completions and narrowing selections for emacs commands
(use-package helm
  :ensure t
  :config
  (setq helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
        helm-quick-update t ; do not display invisible candidates
        helm-idle-delay 0.01 ; be idle for this many seconds, before updating in delayed sources.
        helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer
        helm-show-completion-display-function #'helm-show-completion-default-display-function
        helm-split-window-default-side 'below ;; open helm buffer in another window
        helm-split-window-inside-p t ;; open helm buffer inside current window, not occupy whole other window
        helm-candidate-number-limit 200 ; limit the number of displayed canidates
        helm-move-to-line-cycle-in-source nil ; move to end or beginning of source when reaching top or bottom of source.
        )
  )
;; projectile with helm
(use-package helm-projectile
  :ensure t
  :init
  (helm-projectile-on)
  (setq projectile-switch-project-action 'helm-projectile))

;; swiper with helm backend
(use-package swiper-helm
  :ensure t
  :config
  (setq swiper-helm-display-function 'helm-default-display-buffer))

;; to search (instead of grep or ag)
(use-package ripgrep
  :if (executable-find "rg")
  :ensure t)

(use-package helm-rg
  :if (executable-find "rg")
  :ensure t)

;; projectile, search for files in directory
(use-package projectile
  :ensure t
  :config
  (setq projectile-enable-caching t)
  (setq projectile-require-project-root nil)
  (setq projectile-generic-command "find -L . -type f -print0")
  (projectile-global-mode)
  (setq projectile-current-project-on-switch 'keep)
  (setq projectile-completion-system 'helm)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "A-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (helm-projectile-on)
  :custom
  (projectile-current-project-on-switch 'keep))

;; matching parenthesis
(use-package paren
  :ensure nil
  :config
  (setq show-paren-delay 0)
  (show-paren-mode +1))

;; frame maximized
(use-package frame
  :ensure nil
  :config
  (setq initial-frame-alist (quote ((fullscreen . maximized))))
  (blink-cursor-mode -1))

(use-package whitespace
  :ensure nil
  :config (add-hook 'before-save-hook 'whitespace-cleanup))

;; display line numbers
(use-package display-line-numbers
  :ensure nil
  :bind ("s-j" . global-display-line-numbers-mode))

;; so that magit can work properly
(use-package magit-popup
  :ensure t
  :demand t
  )

(use-package magit-section
  :ensure t
  :demand t
  )

;; using git inside of emacs
(use-package magit
  :ensure t ; make sure it is installed
  :demand t ; make sure it is loaded
  )

;; highlighting changes through version control (git)
(use-package diff-hl
  :ensure t
  :init
  ;; change colors
  (custom-set-faces
   ;; change colours
   '(diff-hl-change ((t (:background "#6897BB"))))
   '(diff-hl-insert ((t (:background "#0A7700"))))
   '(diff-hl-delete ((t (:background "#6C6C6C")))))
  ;; On-the-fly diff updates
  (diff-hl-flydiff-mode)
  ;; Enable diff-hl globally
  (global-diff-hl-mode 1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  ;; to have symbols, like '+'
  ;; (setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)
  (setq diff-hl-margin-side 'left))

;; code autocompletion
(use-package auto-complete
  :ensure t)
(ac-config-default)

;; language servers
(use-package lsp-mode
  :ensure t
  :config
  ;; turn off for better performance
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-prefer-flymake nil) ;; Prefer using flycheck over flymake.
  (add-to-list 'lsp-file-watch-ignored "build$")
  (add-to-list 'lsp-file-watch-ignored "__pycache__$")
  (add-to-list 'lsp-file-watch-ignored "lib/python3.6$")
  (add-to-list 'lsp-file-watch-ignored "include/python3.6m$")
  (add-to-list 'lsp-file-watch-ignored "bin$")
  (add-to-list 'lsp-file-watch-ignored ".ccls-cache$")
  :hook (prog-mode . lsp))

;; error/warning checking
(use-package flycheck
  :ensure t)
;; Disable the error indicator on the fringe
(setq flycheck-indication-mode nil)
(set-face-attribute 'flycheck-fringe-warning nil :foreground (face-attribute 'fringe :background ))
;; add to programming mode
(add-hook 'prog-mode-hook 'flycheck-mode)
(set-face-attribute 'flycheck-error nil :underline '(:color "red2" :style wave))
(set-face-attribute 'flycheck-error nil :underline '(:color "yellow2" :style wave))

;; flycheck error/warning by hovering over it
(use-package flycheck-pos-tip
  :ensure t
  :after flycheck)
(add-hook 'prog-mode-hook 'flycheck-pos-tip-mode)

;;; init-programs.el ends here
