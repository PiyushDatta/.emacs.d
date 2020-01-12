;;; init-personal-keybinds.el

;;; Code:

;; copy C-c, cut C-x, paste C-v, undo C-z
(cua-mode t)

;; Map Alt key to Alt
(setq w32-alt-is-meta nil)

(global-set-key (kbd "C-x C-f")  (lambda () (interactive)
                                   (cd "~/.emacs.d")
                                   (call-interactively 'find-file)))

;; Set cua-mode for alt key as well
(global-set-key (kbd "A-c") 'kill-ring-save)
(global-set-key (kbd "A-x") 'kill-region)
(global-set-key (kbd "A-v") 'yank)
(global-set-key (kbd "A-z") 'undo)

(global-set-key (kbd "C-r") 'select-current-line)
(global-set-key (kbd "s-r") 'select-current-line)
(global-set-key (kbd "A-r") 'select-current-line)

;; Go to end of line
(global-set-key (kbd "s-e") 'end-of-line)
(global-set-key (kbd "A-e") 'end-of-line)

;; Go to beginning of line
(global-set-key (kbd "s-d") 'beginning-of-line)
(global-set-key (kbd "A-d") 'beginning-of-line)

;; incremental search (ctrl-find)
;; (global-set-key (kbd "C-S-f") 'isearch-forward)
;; (define-key isearch-mode-map "\C-S-f" 'isearch-repeat-forward)

;; save
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "A-s") 'save-buffer)

;; scroll up
(global-set-key (kbd "C-x z") 'scroll-up-command)

;; swiper find text within all buffers, same as pycharm/intelij keybind
(global-set-key (kbd "C-f") 'swiper-all)
(global-set-key (kbd "s-f") 'swiper)
(global-set-key (kbd "A-f") 'swiper)

;; turn off shortcut to create a new frame, so it doesn't collide with keybind below
(global-set-key (kbd "s-n") nil)
(global-set-key (kbd "A-n") nil)

;; find a file in directory using projectile, same as pycharm/intelij keybind
(global-set-key (kbd "C-S-n") 'projectile-find-file)
(global-set-key (kbd "s-S-n") 'projectile-find-file)
(global-set-key (kbd "s-n") 'projectile-find-file)
(global-set-key (kbd "A-S-n") 'projectile-find-file)
(global-set-key (kbd "A-n") 'projectile-find-file)

;;switch projects with projectile
(global-set-key (kbd "C-S-p") 'projectile-switch-project)

;; find text in all files in project, same as pycharm/intelij keybind
;; (global-set-key (kbd "C-S-f") 'projectile-multi-occur)
(global-set-key (kbd "C-S-f") 'projectile-ripgrep)

;; to go definition, dumb jump
(global-set-key (kbd "<C-return>") 'dumb-jump-go)
(global-set-key (kbd "<s-return>") 'dumb-jump-go)
(global-set-key (kbd "<A-return>") 'dumb-jump-go)

;; show line numbers for the current buffer
(global-set-key (kbd "C-n") 'display-line-numbers-mode)

;; other and prev windows
(define-key global-map (kbd "s-1") 'prev-window)
(define-key global-map (kbd "s-2") 'other-window)
(define-key global-map (kbd "s-3") 'other-window)
(define-key global-map (kbd "A-1") 'prev-window)
(define-key global-map (kbd "A-2") 'other-window)
(define-key global-map (kbd "A-3") 'other-window)

;; turn of alt/cmd-w
(global-set-key (kbd "s-w") nil)
(global-set-key (kbd "A-w") nil)

;; turn off close window
(global-set-key (kbd "s-q") nil)
(global-set-key (kbd "A-q") nil)

;; switch buffers
(define-key global-map (kbd "s-w <right>") 'next-buffer)
(define-key global-map (kbd "s-w <s-right>") 'next-buffer)
(define-key global-map (kbd "s-w <left>") 'previous-buffer)
(define-key global-map (kbd "s-w <s-left>") 'previous-buffer)
(define-key global-map (kbd "A-w <right>") 'next-buffer)
(define-key global-map (kbd "A-w <A-right>") 'next-buffer)
(define-key global-map (kbd "A-w <left>") 'previous-buffer)
(define-key global-map (kbd "A-w <A-left>") 'previous-buffer)

;; go up and back paragraphs
(define-key global-map (kbd "<C-up>") 'backward-paragraph)
(define-key global-map (kbd "<C-down>") 'forward-paragraph)
(define-key global-map (kbd "<s-up>") 'backward-paragraph)
(define-key global-map (kbd "<s-down>") 'forward-paragraph)
(define-key global-map (kbd "<A-up>") 'backward-paragraph)
(define-key global-map (kbd "<A-down>") 'forward-paragraph)

;; word right and left
(define-key global-map (kbd "<C-right>") 'right-word)
(define-key global-map (kbd "<C-left>") 'left-word)
(define-key global-map (kbd "<s-right>") 'right-word)
(define-key global-map (kbd "<s-left>") 'left-word)
(define-key global-map (kbd "<A-right>") 'right-word)
(define-key global-map (kbd "<A-left>") 'left-word)

;; for centaur tabs, scroll through the tabs
(define-key global-map (kbd "C-<tab>") 'centaur-tabs-forward)
(define-key global-map (kbd "C-S-<tab>") 'centaur-tabs-backward)
(define-key global-map (kbd "s-q <right>") 'centaur-tabs-forward)
(define-key global-map (kbd "s-q <left>") 'centaur-tabs-backward)
(define-key global-map (kbd "A-q <right>") 'centaur-tabs-forward)
(define-key global-map (kbd "A-q <left>") 'centaur-tabs-backward)

;; Open/add a new projectile through treemacs
(define-key global-map (kbd "C-x p") 'treemacs-add-project-to-workspace)

;; Keyboard escape quit, just cancel everything, exits M-x as well, bound to C-g
(define-key global-map (kbd "s-g") 'keyboard-escape-quit)
(define-key global-map (kbd "A-g") 'keyboard-escape-quit)

;; Make tab, always insert tab and untab with shift+tab as well, set to 4-spaces right now
(global-set-key (kbd "<S-tab>") 'untab-region)
(global-set-key (kbd "<backtab>") 'untab-region)
(global-set-key (kbd "<tab>") 'tab-region)

;; comment or uncomment region/line
(global-set-key (kbd "s-/") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "A-/") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)

;; Compile and run any file depending on the file extension.
(global-set-key (kbd "C-b") 'compile-and-run-file)
(global-set-key (kbd "s-b") 'compile-and-run-file)
(global-set-key (kbd "A-b") 'compile-and-run-file)

;; counsel
(global-set-key (kbd "C-x A-x") 'counsel-M-x)
(global-set-key (kbd "C-x A-z") 'counsel-grep-or-swiper)

;; magit
(global-set-key (kbd "C-x C-g") 'magit-status)

;; select all
(global-set-key (kbd "A-a") 'mark-whole-buffer)

;;; init-personal-keybinds.el ends here
