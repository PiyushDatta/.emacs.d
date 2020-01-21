;;; init-c-cpp.el

;;; Code:

;;============================================================================
;;============================================================================
;;==================********* SETUP FOR C/C++ PACKAGES *********==================
;;============================================================================
;;============================================================================

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

;; flymake with google for cpp
(use-package flymake-google-cpplint
  :ensure t
  :init
  :config
  (add-hook 'c-mode-hook 'flymake-google-cpplint-load)
  (add-hook 'c++-mode-hook 'flymake-google-cpplint-load))

;; c/c++ style from google
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

;;; init-c-cpp.el ends here
