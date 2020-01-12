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

;; ivy, completion
(use-package ivy
  :diminish
  :hook (after-init . ivy-mode)
  :config
  (setq ivy-display-style nil)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "<escape>") #'minibuffer-keyboard-quit)
  (setq ivy-re-builders-alist
        '((counsel-rg . ivy--regex-plus)
          (counsel-projectile-rg . ivy--regex-plus)
          (counsel-ag . ivy--regex-plus)
          (counsel-projectile-ag . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil))

(use-package ivy-posframe
  :after ivy
  :diminish
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))
        ivy-posframe-height-alist '((t . 20))
        ivy-posframe-parameters '((internal-border-width . 10)))
  (setq ivy-posframe-width 70)
  (ivy-posframe-mode +1))

(use-package ivy-rich
  :preface
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
        (get-buffer candidate)
      (all-the-icons-icon-for-mode major-mode)))
  :init
  (setq ivy-rich-display-transformers-list ; max column width sum = (ivy-poframe-width - 1)
        '(ivy-switch-buffer
          (:columns
           (
            (ivy-rich-switch-buffer-icon (:width 2))
            (ivy-rich-candidate (:width 35))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-major-mode (:width 13 :face warning)))
           :predicate
           #'(lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 35))
            (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 35))
            (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 35))
            (ivy-rich-counsel-variable-docstring (:width 34 :face font-lock-doc-face))))
          package-install
          (:columns
           ((ivy-rich-candidate (:width 25))
            (ivy-rich-package-version (:width 12 :face font-lock-comment-face))
            (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
            (ivy-rich-package-install-summary (:width 23 :face font-lock-doc-face))))))
  :config
  (ivy-rich-mode +1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; projectile, search for files in directory
(use-package projectile
  :ensure t
  :config
  (setq projectile-enable-caching t)
  (setq projectile-require-project-root nil)
  (setq projectile-generic-command "find -L . -type f -print0")
  (setq projectile-current-project-on-switch 'keep)    
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "A-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  :custom
  (projectile-current-project-on-switch 'keep))

;; mouse wheel speed
(use-package mwheel
  :ensure nil
  :config (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil))

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

(use-package elec-pair
  :ensure nil
  :config (add-hook 'prog-mode-hook 'electric-pair-mode))

;; (use-package whitespace
;;   :ensure nil
;;   :config (add-hook 'before-save-hook 'whitespace-cleanup))

(use-package display-line-numbers
  :ensure nil
  :bind ("s-j" . global-display-line-numbers-mode))

(use-package solaire-mode
  :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
     (minibuffer-setup . solaire-mode-in-minibuffer))
  :config
  (solaire-global-mode)
  (solaire-mode-swap-bg))

(use-package ido-vertical-mode
  :hook ((after-init . ido-mode)
     (after-init . ido-vertical-mode))
  :config
  (setq ido-everywhere t
    ido-enable-flex-matching t
    ido-vertical-define-keys 'C-n-C-p-up-and-down))

(use-package flx-ido :config (flx-ido-mode +1))

(use-package magit)

;; (use-package format-all
;;   :config
;;   (defun ian/format-code ()
;;  "Auto-format whole buffer"
;;  (interactive)
;;  (if (derived-mode-p 'prolog-mode)
;;    (prolog-indent-buffer)
;;    (format-all-buffer))))

(use-package lsp-mode
  :hook ((c-mode ; clangd
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

(use-package company-lsp
  :commands company-lsp
  :config (setq company-lsp-cache-candidates 'auto))

(use-package web-mode
  :mode (("\\.tsx?\\'" . web-mode)
     ("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
      web-mode-code-indent-offset 2
      web-mode-css-indent-offset
     2))

(use-package company
  :diminish company-mode
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
    company-idle-delay 0.1
    company-selection-wrap-around t
    company-tooltip-align-annotations t
    company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
              company-echo-metadata-frontend))
  (with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)))

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

;; centaur tabs
(use-package centaur-tabs
  :demand
  :init (setq centaur-tabs-set-bar 'under)
  :config
  (centaur-tabs-mode +1)
  (centaur-tabs-headline-match)
  ;; Note: If you're not using Spacemacs, in order for the underline to display
  ;; correctly you must add the following line:
  (setq x-underline-at-descent-line t)
  (setq centaur-tabs-set-modified-marker t
    centaur-tabs-modified-marker " ● "
    centaur-tabs-cycle-scope 'tabs
    centaur-tabs-height 30
    centaur-tabs-close-button " × ")
  (centaur-tabs-change-fonts "consolas" 130)
  ;; (centaur-tabs-group-by-projectile-project)
  (defun centaur-tabs-buffer-groups
    ()
      "`centaur-tabs-buffer-groups' control buffers' group rules.

    Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    Other buffer group by `centaur-tabs-get-group-name' with project name."
      (list
  (cond
    ; (string-equal "*" (substring (buffer-name) 0 1))
   ((or (memq major-mode '(magit-process-mode
           magit-status-mode
         magit-diff-mode
         magit-log-mode
         magit-file-mode
         magit-blob-mode
         magit-blame-mode
         ))
        (derived-mode-p 'prog-mode 'dired-mode)
      )
    "Emacs")
   ;; ((derived-mode-p 'prog-mode)
   ;;  "Editing")
   ;; ((derived-mode-p 'dired-mode)
   ;;  "Dired")
   ((memq major-mode '(helpful-mode
           help-mode))
    "Help")
   ((memq major-mode '(org-mode
           org-agenda-clockreport-mode
           org-src-mode
           org-agenda-mode
           org-beamer-mode
           org-indent-mode
           org-bullets-mode
           org-cdlatex-mode
           org-agenda-log-mode
           diary-mode))
    "OrgMode")
   (t
    (centaur-tabs-get-group-name (current-buffer))
    )))))

;; treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
  (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
  (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
      treemacs-deferred-git-apply-delay      0.5
      treemacs-display-in-side-window        t
      treemacs-eldoc-display                 t
      treemacs-file-event-delay              5000
      treemacs-file-extension-regex          treemacs-last-period-regex-value
      treemacs-file-follow-delay             0.2
      treemacs-follow-after-init             t
      treemacs-git-command-pipe              ""
      treemacs-goto-tag-strategy             'refetch-index
      treemacs-indentation                   2
      treemacs-indentation-string            " "
      treemacs-is-never-other-window         nil
      treemacs-max-git-entries               5000
      treemacs-missing-project-action        'ask
      treemacs-no-png-images                 t
      treemacs-no-delete-other-windows       t
      treemacs-project-follow-cleanup        nil
      treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
      treemacs-position                      'left
      treemacs-recenter-distance             0.1
      treemacs-recenter-after-file-follow    nil
      treemacs-recenter-after-tag-follow     nil
      treemacs-recenter-after-project-jump   'always
      treemacs-recenter-after-project-expand 'on-distance
      treemacs-show-cursor                   nil
      treemacs-show-hidden-files             t
      treemacs-silent-refresh                nil
      treemacs-sorting                       'alphabetic-desc
      treemacs-space-between-root-nodes      t
      treemacs-tag-follow-cleanup            t
      treemacs-tag-follow-delay              1.5
      treemacs-width                         16)

  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;;(treemacs-resize-icons 44)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
         (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
    ("M-0"       . treemacs-select-window)
    ("C-x t 1"   . treemacs-delete-other-windows)
    ("C-x t t"   . treemacs)
    ("C-x t r"   . treemacs-toggle)
    ("C-x t p"   . treemacs-projectile-toggle)
    ("C-x t B"   . treemacs-bookmark)
    ("C-x t C-t" . treemacs-find-file)
    ("C-x t M-t" . treemacs-find-tag))
  )
  
  (use-package treemacs-projectile
     :defer t
     :ensure t
     :config
     (setq treemacs-header-function #'treemacs-projectile-create-header)
  )

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

; (use-package treemacs-icons-dired
;   :after treemacs dired
;   :ensure t
;   :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;; Swiper, alternative to isearch (ctrl-f find/search)
(use-package swiper
  :after ivy)
  ;; :bind (("\s-f" . swiper)
     ;; ("\C-r" . swiper)))

;; go-to definitions
(use-package dumb-jump
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
  :ensure)

;; irony is for auto-complete, syntax checking and documentation for c++/c
;; You will need to install irony-server first time use
;; to install irony-server, your system need to install clang, cmake and clang-devel in advance
;; To do so, type M-x irony-install-server RET.
(use-package irony
  :ensure t
  :hook ((c++-mode . irony-mode)
         (c-mode . irony-mode))
  :config
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (use-package company-irony-c-headers
    :ensure t)
  (use-package company-irony
    :ensure t
    :config
    (add-to-list (make-local-variable 'company-backends)
                 '(company-irony company-irony-c-headers)))
  (use-package flycheck-irony
    :ensure t
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
    )
  (use-package irony-eldoc
    :ensure t
    :config
    (add-hook 'irony-mode-hook #'irony-eldoc)
    ))

;; flymake  with google for cpp
(use-package flymake-cursor
  :ensure t)
(use-package flymake-google-cpplint
  :ensure t
  :init
  :config
  (add-hook 'c-mode-hook 'flymake-google-cpplint-load)
  (add-hook 'c++-mode-hook 'flymake-google-cpplint-load))

(use-package google-c-style
  :ensure t
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

;; clang-format can be triggered using C-M-tab
(use-package clang-format
  :ensure t
  :init
  (require 'clang-format)
  ;; (global-set-key [C-M-tab] 'clang-format-region)
  ;; Create clang-format file using google style
  ;; clang-format -style=google -dump-config > .clang-format
  (setq clang-format-style-option "google"))

;; Format c++ code everytime we save
(defun cpp-save-hook()
    "Save cpp files with format"
    (setq file-ext-name (file-name-extension buffer-file-name))
    (when (equal file-ext-name "cpp")
        (clang-format-buffer)))
(add-hook 'before-save-hook 'cpp-save-hook)

;; An extensible emacs startup screen showing you what’s most important
;; (use-package dashboard
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-startup-banner 'logo
;;         dashboard-banner-logo-title "Dangerously powerful"
;;         dashboard-items nil
;;         dashboard-set-footer nil))

;; mode lines customization (the toolbar at the bottom)
; (use-package smart-mode-line-atom-one-dark-theme)
; (use-package smart-mode-line
;   :config
;   (when (member "Menlo" (font-family-list))
;     (progn
;       (set-face-attribute 'mode-line nil :height 120 :font "Menlo")
;       (set-face-attribute 'mode-line-inactive nil :height 120 :font "Menlo")))
;   (setq sml/no-confirm-load-theme t
;         sml/theme 'atom-one-dark)
;   (sml/setup))

(use-package counsel
  :diminish
  :hook (ivy-mode . counsel-mode)
  :config
  (setq counsel-rg-base-command "rg --vimgrep %s"))

(use-package counsel-projectile
  :config (counsel-projectile-mode +1))

;; Simple but effective sorting and filtering for Emacs.
(use-package prescient
  :config
  (setq prescient-filter-method '(literal regexp initialism fuzzy))
  (prescient-persist-mode +1))

(use-package ivy-prescient
  :after (prescient ivy)
  :config
  (setq ivy-prescient-sort-commands
        '(:not swiper counsel-grep ivy-switch-buffer))
  (setq ivy-prescient-retain-classic-highlighting t)
  (ivy-prescient-mode +1))

(use-package company-prescient
  :after (prescient company)
  :config (company-prescient-mode +1))

;; for .md files
(use-package markdown-mode
  :mode (("\\.md" . markdown-mode))
  :config
  (setq markdown-css-paths '("~/.emacs.d/extra-files/markdown.css"))
  (add-hook 'markdown-mode-hook
            '(lambda ()
               (cond ((eq *win64* t) "perl ~/.emacs.d/extra-files/Markdown.pl")
                            (t markdown-command "markdown")))))

;; Configuring visual-line-mode for markdown mode
(add-hook 'markdown-mode-hook 'visual-line-mode)

;; lightweight syntax highlighting improvement for numbers, operators, and escape sequences
;; (use-package highlight-numbers :hook (prog-mode . highlight-numbers-mode))
;; (use-package highlight-escape-sequences :hook (prog-mode . hes-mode))
;; (use-package highlight-operators :hook (prog-mode . highlight-operators-mode))
;; (use-package all-the-icons :config (setq all-the-icons-scale-factor 1.0))

;;; init-programs.el ends here
