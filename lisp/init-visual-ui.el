;;; init-visual-ui.el

;;; Code:

;;============================================================================
;;============================================================================
;;==================********* VISUAL/UI TYPE PACKAGES *********===================
;;============================================================================
;;============================================================================

;; aesthetic plugin that helps visually distinguish file-visiting windows from other types of windows (like popups or sidebars) by giving
;; them a slightly different -- often brighter -- background
(use-package solaire-mode
  :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
         (minibuffer-setup . solaire-mode-in-minibuffer))
  :config
  (solaire-global-mode)
  (solaire-mode-swap-bg))

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
          treemacs-deferred-git-apply-delay      0
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              50
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
          treemacs-show-cursor                   t
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

;; posframe, shows the frame at the middle when you do M-x
(use-package posframe
  :ensure t
  :config
  (use-package helm-posframe
    :init
    (add-hook 'helm-org-rifle-after-command-hook 'helm-posframe-cleanup)
    :config
    (setq helm-posframe-poshandler 'posframe-poshandler-frame-center
      helm-posframe-width (round (* (frame-width) 0.80))
      ; helm-posframe-parameters '((internal-border-width . 10))
      helm-posframe-height 30)
  (helm-posframe-enable))
  (use-package company-posframe
    :ensure t
    :config (company-posframe-mode 1)))

;;; init-visual-ui.el ends here
