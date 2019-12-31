;;; init.el --- Emacs init file
;;  Author: Piyush Datta

;;============================================================================
;;============================================================================
;;==================********* PERFORMANCE *********===========================
;;============================================================================
;;============================================================================
(push (expand-file-name "~/.emacs.d/lisp") load-path)

(let* ((minver "24.4"))
  (when (version< emacs-version minver)
	(error "Emacs v%s or higher is required." minver)))

(defvar best-gc-cons-threshold
  4000000
  "Best default gc threshold value.  Should NOT be too big!")

;; don't GC during startup to save time
(setq gc-cons-threshold most-positive-fixnum)

(setq emacs-load-start-time (current-time))

;; {{ emergency security fix
;; https://bugs.debian.org/766397
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
	 (list start end)))
;; }}
;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
(setq *is-a-mac* (eq system-type 'darwin))
(setq *win64* (eq system-type 'windows-nt))
(setq *cygwin* (eq system-type 'cygwin) )
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )
(setq *emacs24* (>= emacs-major-version 24))
(setq *emacs25* (>= emacs-major-version 25))
(setq *emacs26* (>= emacs-major-version 26))
(setq *no-memory* (cond
				   (*is-a-mac*
					(< (string-to-number (nth 1 (split-string (shell-command-to-string "sysctl hw.physmem")))) 4000000000))
				   (*linux* nil)
				   (t nil)))

;; @see https://www.reddit.com/r/emacs/comments/55ork0/is_emacs_251_noticeably_slower_than_245_on_windows/
;; Emacs 25 does gc too frequently
(when *emacs25*
  ;; (setq garbage-collection-messages t) ; for debug
  (setq best-gc-cons-threshold (* 64 1024 1024))
  (setq gc-cons-percentage 0.5)
  (run-with-idle-timer 5 t #'garbage-collect))

(defun require-init (pkg &optional maybe-disabled)
  "Load PKG if MAYBE-DISABLED is nil or it's nil but start up in normal slowly."
  (when (or (not maybe-disabled) (not (boundp 'startup-now)))
	(load (file-truename (format "~/.emacs.d/lisp/%s" pkg)) t t)))

(defun local-require (pkg)
  (unless (featurep pkg)
	(load (expand-file-name
		   (cond
			((eq pkg 'go-mode-load)
			 (format "~/.emacs.d/site-lisp/go-mode/%s" pkg))
			(t
			 (format "~/.emacs.d/site-lisp/%s/%s" pkg pkg))))
		  t t)))

;; *Message* buffer should be writable in 24.4+
(defadvice switch-to-buffer (after switch-to-buffer-after-hack activate)
  (if (string= "*Messages*" (buffer-name))
	  (read-only-mode -1)))

;; @see https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; Normally file-name-handler-alist is set to
;; (("\\`/[^/]*\\'" . tramp-completion-file-name-handler)
;; ("\\`/[^/|:][^/|]*:" . tramp-file-name-handler)
;; ("\\`/:" . file-name-non-special))
;; Which means on every .el and .elc file loaded during start up, it has to runs those regexps against the filename.
(let* ((file-name-handler-alist nil))

  ;; ;; {{
  ;; (require 'benchmark-init-modes)
  ;; (require 'benchmark-init)
  ;; (benchmark-init/activate)
  ;; ;; `benchmark-init/show-durations-tree' to show benchmark result
  ;; ;; }}

  ;; (require-init 'init-autoload)
  ;; `package-initialize' takes 35% of startup time
  ;; need check https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast for solution
  ;; (require-init 'init-exec-path t) ;; Set up $PATH
  ;; Any file use flyspell should be initialized after init-spelling.el
  
  ;; Custom highlight numbers and highlight operators 
  (require-init 'init-parent-mode t)
  ;; inherits off of font-lock-keyword-face
  (require-init 'init-highlight-operators-custom t)
  ;; inherits off of font-lock-regexp-grouping-backslash
  (require-init 'init-highlight-numbers-custom t)
  ;; turn both highlights for all programming buffers
  (add-hook 'prog-mode-hook 'highlight-numbers-mode)
  (add-hook 'prog-mode-hook 'highlight-operators-mode)

  ;; @see https://github.com/hlissner/doom-emacs/wiki/FAQ
  ;; Adding directories under "site-lisp/" to `load-path' slows
  ;; down all `require' statement. So we do this at the end of startup
  ;; NO ELPA package is dependent on "site-lisp/".
  (setq load-path (cdr load-path))

  ;; create site-lisp directory
  (let ((sitelisp-dir "~/.emacs.d/site-lisp/"))
	(unless (file-exists-p sitelisp-dir)
	  (make-directory sitelisp-dir)))

  (unless (boundp 'startup-now)
	;; my personal setup, other major-mode specific setup need it.
	;; It's dependent on "~/.emacs.d/site-lisp/*.el"
	(load (expand-file-name "~/.custom.el") t nil)

	;; @see https://www.reddit.com/r/emacs/comments/4q4ixw/how_to_forbid_emacs_to_touch_configuration_files/
	;; See `custom-file' for details.
	(load (setq custom-file (expand-file-name "~/.emacs.d/custom-set-variables.el")) t t)))

(setq gc-cons-threshold best-gc-cons-threshold)

(when (require 'time-date nil t)
  (message "Emacs startup time: %d seconds."
		   (time-to-seconds (time-since emacs-load-start-time))))

;;; Local Variables:
;;; no-byte-compile: t
(put 'erase-buffer 'disabled nil)

;;============================================================================
;;============================================================================
;;==================********* SETUP *********=================================
;;============================================================================
;;============================================================================
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
;;============================================================================
;;============================================================================
;;==================********* KEYBINDS *********==============================
;;============================================================================
;;============================================================================
;; copy C-c, cut C-x, paste C-v, undo C-z
(cua-mode t)

;; Map Alt key to Alt
(setq w32-alt-is-meta nil)
;; Set cua-mode for alt key as well
(global-set-key (kbd "A-c") 'kill-ring-save)
(global-set-key (kbd "A-x") 'kill-region)
(global-set-key (kbd "A-v") 'yank)
(global-set-key (kbd "A-z") 'undo)

;; copy entire line
;; (defun copy-line (arg)
;;       "Copy lines (as many as prefix argument) in the kill ring"
;;       (interactive "p")
;;       (kill-ring-save (line-beginning-position)
;;                       (line-beginning-position (+ 1 arg)))
;;       (message "%d line%s copied" arg (if (= 1 arg) "" "s")))
;; (global-set-key (kbd "C-c l") 'copy-line)

;; Select entire line
(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

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
(global-set-key (kbd "C-S-f") 'projectile-multi-occur)

;; to go definition, dumb jump
(global-set-key (kbd "<C-return>") 'dumb-jump-go)
(global-set-key (kbd "<s-return>") 'dumb-jump-go)
(global-set-key (kbd "<A-return>") 'dumb-jump-go)

;; show line numbers for the current buffer
(global-set-key (kbd "C-n") 'display-line-numbers-mode)

;; other and prev windows
 (defun prev-window ()
   (interactive)
   (other-window -1))
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
(defun indent-region-custom(numSpaces)
    (progn
        ; default to start and end of current line
        (setq regionStart (line-beginning-position))
        (setq regionEnd (line-end-position))

        ; if there's a selection, use that instead of the current line
        (when (use-region-p)
            (setq regionStart (region-beginning))
            (setq regionEnd (region-end))
        )

        (save-excursion ; restore the position afterwards
            (goto-char regionStart) ; go to the start of region
            (setq start (line-beginning-position)) ; save the start of the line
            (goto-char regionEnd) ; go to the end of region
            (setq end (line-end-position)) ; save the end of the line

            (indent-rigidly start end numSpaces) ; indent between start and end
            (setq deactivate-mark nil) ; restore the selected region
        )
    )
)

(defun untab-region (N)
    (interactive "p")
    (indent-region-custom -4)
)

(defun tab-region (N)
    (interactive "p")
    (if (active-minibuffer-window)
        (minibuffer-complete)    ; tab is pressed in minibuffer window -> do completion
    ; else
    (if (string= (buffer-name) "*shell*")
        (comint-dynamic-complete) ; in a shell, use tab completion
    ; else
    (if (use-region-p)    ; tab is pressed is any other buffer -> execute with space insertion
        (indent-region-custom 4) ; region was selected, call indent-region
        (insert "    ") ; else insert four spaces as expected
    )))
)
(global-set-key (kbd "<S-tab>") 'untab-region)
(global-set-key (kbd "<tab>") 'tab-region)

;; comment or uncomment region/line
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))
(global-set-key (kbd "s-/") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "A-/") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)

;; Get formatted time to float seconds
(defun format-time-to-seconds (curr-time-formatted)
  "Get formatted time to float seconds"
  (interactive)
  (format "%.2f" (float-time curr-time-formatted)))

;; Compile and run any file depending on the file extension.
(defun compile-and-run-file ()
  "Compile and run any file depending on the file extension."
  (interactive)
  ;; get the extension name
  (setq file-ext-name (file-name-extension buffer-file-name))
  ;; run command based on the extension name
  (cond ((equal file-ext-name "el") (eval-buffer))
		((equal file-ext-name "cpp") (compile-c-cpp-file buffer-file-name))
		((equal file-ext-name "py") (compile-python-file buffer-file-name))
		(t (message "Sorry, this file extention is not supported."))))
(global-set-key (kbd "C-b") 'compile-and-run-file)
(global-set-key (kbd "s-b") 'compile-and-run-file)
(global-set-key (kbd "A-b") 'compile-and-run-file)

;; Compile and run c/c++ code.
(defun compile-c-cpp-file (curr-file-full-name)
  "Compile and run c/c++ code."
  (interactive)
  (setq current-function-time (current-time))
  (setq curr-file-name (file-name-sans-extension (file-name-nondirectory curr-file-full-name)))
  (setq curr-file-dir (file-name-directory curr-file-full-name))
  (setq curr-file-out-dir (concat curr-file-dir "out"))
  (setq curr-file-out-name (concat curr-file-name ".out"))
  (setq curr-file-out-full-name
		(format "%s/%s" (concat curr-file-dir "out") (concat curr-file-name ".out")))

  ;; create out directory
  (unless (file-exists-p curr-file-out-dir)
	(make-directory curr-file-out-dir))

  (setq compile-shell-command (format "clang++ -Wall -std=c++14 -o %s %s" curr-file-out-full-name curr-file-full-name))

  ;; Compile and execute the file
  (message (format "Saving file: %s" curr-file-full-name))
  (save-buffer curr-file-full-name)
  (message (format "Compiling...%s" curr-file-full-name))
  (setq compiled-file-err (shell-command-to-string compile-shell-command))

  (when (equal "" compiled-file-err)
	(message (format "Compiled! Output file at %s" curr-file-out-full-name))
	(message (format "%s (%s seconds)"(shell-command-to-string curr-file-out-full-name)
					 (format-time-to-seconds (time-subtract (current-time) current-function-time)))))

  (unless (equal "" compiled-file-err)
	(message (format "ERROR (%s seconds): %s"
					 (format-time-to-seconds (time-subtract (current-time) current-function-time)) compiled-file-err))))

;; Compile and run python code.
(defun compile-python-file (curr-file-full-name)
  "Compile and run python code."
  (interactive)
  (setq current-function-time (current-time))
  (setq compile-shell-command (format "python %s" curr-file-full-name))

  ;; Compile and execute the file
  (message (format "Compiling and running...%s" curr-file-full-name))
  (message (format "%s (%s seconds)" (shell-command-to-string compile-shell-command)
				   (format-time-to-seconds (time-subtract (current-time) current-function-time)))))

;;============================================================================
;;============================================================================
;;==================********* INSTALL PACKAGES *********======================
;;============================================================================
;;============================================================================
;;; remove SC if you are not using sunrise commander and org if you like outdated packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)


;;; Bootstrapping use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t))


;;============================================================================
;;============================================================================
;;==================********* CUSTOM PACKAGES *********=======================
;;============================================================================
;;============================================================================

;; startup page
(use-package "startup"
  :ensure nil
  :config (setq inhibit-startup-screen t))

;; window
(use-package "window"
  :ensure nil
  :config
  (defun ian/split-and-follow-horizontally ()
	"Split window below."
	(interactive)
	(split-window-below)
	(other-window 1))
  (defun ian/split-and-follow-vertically ()
	"Split window right."
	(interactive)
	(split-window-right)
	(other-window 1))
  (global-set-key (kbd "C-x 2") 'ian/split-and-follow-horizontally)
  (global-set-key (kbd "C-x 3") 'ian/split-and-follow-vertically))

;; delete selection
(use-package delsel
  :ensure nil
  :config (delete-selection-mode +1))

;; no confirm kill process
(use-package files
  :ensure nil
  :config
  (setq confirm-kill-processes nil))

;; performance, eldoc
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :config
  (global-eldoc-mode -1)
  (add-hook 'prog-mode-hook 'eldoc-mode)
  (setq eldoc-idle-delay 0.4))

;; projectile, search for files in directory
(use-package projectile
  :ensure t
  :config
  (setq projectile-enable-caching t)
  (setq projectile-require-project-root nil)
  (setq projectile-generic-command "find -L . -type f -print0")
 (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "A-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;; python
(use-package python
  :ensure nil
  :config (setq python-indent-offset 4))

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

(use-package magit :bind ("C-x g" . magit-status))

;; (use-package format-all
;;   :config
;;   (defun ian/format-code ()
;; 	"Auto-format whole buffer"
;; 	(interactive)
;; 	(if (derived-mode-p 'prolog-mode)
;; 		(prolog-indent-buffer)
;; 	  (format-all-buffer))))

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
		centaur-tabs-set-icons t
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
	  ))))  
  :bind
  ("C-S-<tab>" . centaur-tabs-backward)
  ("C-<tab>" . centaur-tabs-forward)
  )

;; function so that treemacs toggle works
(defun assoc-delete-all (key alist &optional test)
  "Delete from ALIST all elements whose car is KEY.
Compare keys with TEST.  Defaults to `equal'.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
  (unless test (setq test #'equal))
  (while (and (consp (car alist))
	      (funcall test (caar alist) key))
    (setq alist (cdr alist)))
  (let ((tail alist) tail-cdr)
    (while (setq tail-cdr (cdr tail))
      (if (and (consp (car tail-cdr))
	       (funcall test (caar tail-cdr) key))
	  (setcdr tail (cdr tail-cdr))
	(setq tail tail-cdr))))
  alist)

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
		("C-x t r"        . treemacs-toggle)
        ("C-x t p"  . treemacs-projectile-toggle)
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
  (add-hook 'c++-mode-hook 'flymake-google-cpplint-load)
  (custom-set-variables
   '(flymake-google-cpplint-command "C:/Users/PD/AppData/Local/Programs/Python/Python36/Scripts/cpplint")
   '(flymake-google-cpplint-verbose "--verbose=0")
   '(flymake-google-cpplint-filter "--filter=-whitespace/line_length,-build")))
(use-package google-c-style
  :ensure t
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

;; lightweight syntax highlighting improvement for numbers, operators, and escape sequences
;; (use-package highlight-numbers :hook (prog-mode . highlight-numbers-mode))
;; (use-package highlight-escape-sequences :hook (prog-mode . hes-mode))
;; (use-package highlight-operators :hook (prog-mode . highlight-operators-mode))
;; (use-package all-the-icons :config (setq all-the-icons-scale-factor 1.0))
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (kill-new (buffer-file-name))
  (message (buffer-file-name)))

(provide 'init)

;;; init.el ends here
