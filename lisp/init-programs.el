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

;; projectile, search for files in directory
(use-package projectile
  :ensure t
  :config
  (setq projectile-enable-caching t)
  (setq projectile-require-project-root nil)
  (setq projectile-generic-command "find -L . -type f -print0")
  (setq projectile-current-project-on-switch 'keep)
  (setq projectile-completion-system 'helm)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "A-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
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

;; (use-package whitespace
;;   :ensure nil
;;   :config (add-hook 'before-save-hook 'whitespace-cleanup))

;; display line numbers
(use-package display-line-numbers
  :ensure nil
  :bind ("s-j" . global-display-line-numbers-mode))

;; using git inside of emacs
(use-package magit)

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

;; language servers
(use-package lsp-mode
  :ensure t
  :hook ((c-mode ; clangd
          c++-mode  ; clangd
          c-or-c++-mode ; clangd
          java-mode ; eclipse-jdtls
          js-mode ; typescript-language-server
          python-mode ; pyls
          dart-mode
          web-mode) . lsp)
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-symbol-highlighting nil)
  (use-package lsp-java :after lsp))

;; for code auto-completion
(use-package company
  :ensure t
  :config
  ;; Global
  (setq company-idle-delay 1
        company-minimum-prefix-length 1
        company-show-numbers t
        company-tooltip-limit 20)
  ;; Default backends
  (setq company-backends '((company-files)))
  ;; Activating globally
(global-company-mode t))

(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (company-quickhelp-mode 1))

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :config (setq company-lsp-cache-candidates 'auto))

;; yasnippet
(use-package yasnippet
  :ensure t
  :config
  ;; Adding yasnippet support to company
  (add-to-list 'company-backends '(company-yasnippet))
  ;; Activate global
  (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t
  )

;; flymake error/warning by hovering over it
(use-package flymake-cursor
  :ensure t)

;;; init-programs.el ends here
