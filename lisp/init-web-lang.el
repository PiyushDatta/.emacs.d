;;; init-web-lang.el 

;;; Code:

;;===============================================================================================
;;===============================================================================================
;;==================********* WEB (HTML/CSS/JS/MD) LANG PACKAGES *********=======================
;;===============================================================================================
;;===============================================================================================

(use-package web-mode
  :mode (("\\.tsx?\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset
        2))

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

;;; init-web-lang.el ends here

