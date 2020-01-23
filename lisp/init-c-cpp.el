;;; init-c-cpp.el

;;; Code:

;;============================================================================
;;============================================================================
;;==================********* SETUP FOR C/C++ PACKAGES *********==================
;;============================================================================
;;============================================================================

;; c/c++ mode
(use-package cc-mode
  :ensure nil
  :mode ("\\.h\\'" . c++-mode))

;; the backend for lsp
(use-package ccls
  :ensure t
  :init
  :custom
  (projectile-project-root-files-top-down-recurring
   (append '("compile_commands.json" ".ccls")
           projectile-project-root-files-top-down-recurring))
  :config
  (setq ccls-executable "ccls")
  (push ".ccls-cache" projectile-globally-ignored-directories))

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

;; flycheck with google for cpp
(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-google-cpplint)
     ;; Add Google C++ Style checker.
     ;; In default, syntax checked by Clang and Cppcheck.
     (flycheck-add-next-checker 'c/c++-cppcheck
                                '(warning . c/c++-googlelint))))
(custom-set-variables
 '(flycheck-c/c++-googlelint-executable "cpplint")
 '(flycheck-googlelint-verbose "3")
 '(flycheck-googlelint-filter "-whitespace,+whitespace/braces")
 '(flycheck-googlelint-linelength "120"))

;;; init-c-cpp.el ends here
