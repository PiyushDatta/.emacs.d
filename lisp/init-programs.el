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
  (setq helm-M-x-fuzzy-match t)
  (helm-mode 1))

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
  :diminish company-mode
  :hook (prog-mode . company-mode)
  :config
  ;; for c++ header files
  (use-package company-c-headers
    :ensure t
    :init
    (add-to-list 'company-backends 'company-c-headers))
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                            company-echo-metadata-frontend))
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)))

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :config (setq company-lsp-cache-candidates 'auto))

;; yasnippet
(use-package yasnippet-snippets
  :config
  (yas-global-mode +1)
  (advice-add 'company-complete-common
              :before
              (lambda ()
                (setq my-company-point (point))))
  (advice-add 'company-complete-common
              :after
              (lambda ()
                (when (equal my-company-point (point))
                  (yas-expand)))))

;; go-to definitions
(use-package dumb-jump
  :config (setq dumb-jump-selector 'helm)
  :ensure)

;; flymake error/warning by hovering over it
(use-package flymake-cursor
  :ensure t)

;;; init-programs.el ends here
