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

  (require-init 'init-autoload)
  ;; `package-initialize' takes 35% of startup time
  ;; need check https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast for solution
  (require-init 'init-modeline)
  (require-init 'init-utils)
  (require-init 'init-elpa)
  (require-init 'init-exec-path t) ;; Set up $PATH
  ;; Any file use flyspell should be initialized after init-spelling.el
  (require-init 'init-spelling t)
  (require-init 'init-gui-frames t)
  (require-init 'init-uniquify t)
  (require-init 'init-ibuffer t)
  (require-init 'init-ivy)
  (require-init 'init-hippie-expand)
  (require-init 'init-windows)
  (require-init 'init-markdown t)
  (require-init 'init-javascript t)
  (require-init 'init-org t)
  (require-init 'init-css t)
  (require-init 'init-python t)
  (require-init 'init-ruby-mode t)
  (require-init 'init-lisp t)
  (require-init 'init-elisp t)
  (require-init 'init-yasnippet t)
  (require-init 'init-cc-mode t)
  (require-init 'init-gud t)
  (require-init 'init-linum-mode)
  (require-init 'init-git t)
  ;; (require-init 'init-gist)
  (require-init 'init-gtags t)
  (require-init 'init-clipboard)
  (require-init 'init-ctags t)
  (require-init 'init-bbdb t)
  (require-init 'init-gnus t)
  (require-init 'init-lua-mode t)
  (require-init 'init-workgroups2 t) ; use native API in lightweight mode
  (require-init 'init-term-mode t)
  (require-init 'init-web-mode t)
  (require-init 'init-company t)
  (require-init 'init-chinese t) ;; cannot be idle-required
  ;; need statistics of keyfreq asap
  (require-init 'init-keyfreq t)
  (require-init 'init-httpd t)

  ;; projectile costs 7% startup time

  ;; misc has some crucial tools I need immediately
  (require-init 'init-essential)
  (require-init 'init-misc t)

  (require-init 'init-emacs-w3m t)
  (require-init 'init-shackle t)
  (require-init 'init-dired t)
  (require-init 'init-writting t)
  (require-init 'init-hydra) ; hotkey is required everywhere
  ;; use evil mode (vi key binding)
  (require-init 'init-evil) ; init-evil dependent on init-clipboard

  ;; ediff configuration should be last so it can override
  ;; the key bindings in previous configuration
  (require-init 'init-ediff)

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

;; create themes directory
(let ((themes-dir (concat default-directory "/.emacs.d/themes")))
  (unless (file-exists-p themes-dir)
	(make-directory themes-dir)))

;; workaround bug in Emacs 26.2
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Load theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'monokai t)

;; Set default font
(set-face-attribute 'default nil
					:family "consolas"
					:height 160
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

;;============================================================================
;;============================================================================
;;==================********* KEYBINDS *********==============================
;;============================================================================
;;============================================================================
;; copy C-c, cut C-x, paste C-v, undo C-z
(cua-mode t)
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

(global-set-key (kbd "C-e") 'select-current-line)
(global-set-key (kbd "s-e") 'select-current-line)

;; incremental search (ctrl-find)
;; (global-set-key (kbd "C-S-f") 'isearch-forward)
;; (define-key isearch-mode-map "\C-S-f" 'isearch-repeat-forward)

;; save
(global-set-key (kbd "C-s") 'save-buffer)

;; scroll up
(global-set-key (kbd "C-x z") 'scroll-up-command)

;; swiper find text within all buffers, same as pycharm/intelij keybind
(global-set-key (kbd "C-f") 'swiper-all)
(global-set-key (kbd "s-f") 'swiper-all)

;; turn off shortcut to create a new frame, so it doesn't collide with keybind below 
(global-set-key (kbd "s-n") nil)

;; find a file in directory using projectile, same as pycharm/intelij keybind
(global-set-key (kbd "C-S-n") 'projectile-find-file)
(global-set-key (kbd "s-S-n") 'projectile-find-file)
(global-set-key (kbd "s-n") 'projectile-find-file)

;; find text in all files in project, same as pycharm/intelij keybind
(global-set-key (kbd "C-S-f") 'projectile-multi-occur)

;; to go definition, dumb jump
(global-set-key (kbd "<C-return>") 'dumb-jump-go)
(global-set-key (kbd "<s-return>") 'dumb-jump-go)

;; show line numbers for the current buffer
(global-set-key (kbd "C-n") 'display-line-numbers-mode)

;; other and prev windows
 (defun prev-window ()
   (interactive)
   (other-window -1))
(define-key global-map (kbd "s-1") 'prev-window)
(define-key global-map (kbd "s-2") 'other-window)
(define-key global-map (kbd "s-3") 'other-window)

;; turn of alt/cmd-w 
(global-set-key (kbd "s-w") nil)

;; turn off close window
(global-set-key (kbd "s-q") nil)

;; switch buffers
(define-key global-map (kbd "s-w <right>") 'next-buffer)
(define-key global-map (kbd "s-w <s-right>") 'next-buffer)
(define-key global-map (kbd "s-w <left>") 'previous-buffer)
(define-key global-map (kbd "s-w <s-left>") 'previous-buffer)

;; go up and back paragraphs
(define-key global-map (kbd "<C-up>") 'backward-paragraph)
(define-key global-map (kbd "<s-up>") 'backward-paragraph)
(define-key global-map (kbd "<C-down>") 'forward-paragraph)
(define-key global-map (kbd "<s-down>") 'forward-paragraph)

;; word right and left
(define-key global-map (kbd "<C-right>") 'right-word)
(define-key global-map (kbd "<s-right>") 'right-word)
(define-key global-map (kbd "<C-left>") 'left-word)
(define-key global-map (kbd "<s-left>") 'left-word)

;; for centaur tabs, scroll through the tabs
(define-key global-map (kbd "s-q <right>") 'centaur-tabs-forward)
(define-key global-map (kbd "s-q <left>") 'centaur-tabs-backward)

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
(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)

;; compile c/cpp code in one key stroke
(defun compile_cpp_project ()
  "Build cpp project"
  (interactive)
  ;; create out directory
  (let ((working-dir (concat default-directory "/out")))
	(unless (file-exists-p working-dir)
	  (make-directory working-dir)))
  (let ((buf-name '"*compile_cpp_project*")
		(working-dir '~/out))
	(save-excursion
	  (with-current-buffer (get-buffer-create buf-name)
		(barf-if-buffer-read-only)
		(erase-buffer))
	  (cd working-dir)
	  (call-process-shell-command "pwd" nil buf-name 't)
	  (call-process-shell-command "gcc -c hello.c" nil buf-name 't)
	  (call-process-shell-command "gcc hello.o" nil buf-name 't)
	  (call-process-shell-command "./a.out" nil buf-name 't)
	  (message "compile project 1 done")
	  )))
;;(global-set-key (kbd "C-b") 'jea-compile-project-1)

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
  (setq projectile-generic-command "find -L . -type f -print0")
 (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
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
;;	"Auto-format whole buffer"
;;	(interactive)
;;	(if (derived-mode-p 'prolog-mode)
;;		(prolog-indent-buffer)
;;	  (format-all-buffer))))

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
  :init (setq centaur-tabs-set-bar 'over)
  :config
  (centaur-tabs-mode +1)
  (centaur-tabs-headline-match)
  (setq centaur-tabs-set-modified-marker t
		centaur-tabs-modified-marker " ● "
		centaur-tabs-cycle-scope 'tabs
		centaur-tabs-height 30
		centaur-tabs-set-icons t
		centaur-tabs-close-button " × ")
  (centaur-tabs-change-fonts "consolas" 130)
 ; (centaur-tabs-group-by-projectile-project)
  :bind
  ("C-S-<tab>" . centaur-tabs-backward)
  ("C-<tab>" . centaur-tabs-forward))

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
		  treemacs-width                         25)

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

;; (use-package treemacs-projectile
;;   :after treemacs projectile
;;   :ensure t)

; (use-package treemacs-icons-dired
;   :after treemacs dired
;   :ensure t
;   :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;; Swiper, alternative to isearch (ctrl-f find/search)
(use-package swiper
  :after ivy
  :bind (("\C-f" . swiper)
		 ("\C-r" . swiper)))

;; go-to definitions
(use-package dumb-jump
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
  :ensure)

;; lightweight syntax highlighting improvement for numbers, operators, and escape sequences
; (use-package highlight-numbers :hook (prog-mode . highlight-numbers-mode))
; (use-package highlight-escape-sequences :hook (prog-mode . hes-mode))
; (use-package highlight-operators :hook (prog-mode . highlight-operators-mode))
;(use-package all-the-icons :config (setq all-the-icons-scale-factor 1.0))

(provide 'init)

;;; init.el ends here
