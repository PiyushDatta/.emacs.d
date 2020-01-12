;;; init.el --- Emacs init file
;;  Author: Piyush Datta
;;; Commentary:
;;  This is my personal Emacs configuration
;;; Code:

;;=======================================================================================
;;=======================================================================================
;;==================********* PERFORMANCE *********======================================
;;=======================================================================================
;;=======================================================================================
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

  ;;=======================================================================================
  ;;=======================================================================================
  ;;==================********* INITIALIZE OTHER FILES *********===========================
  ;;=======================================================================================
  ;;=======================================================================================

  ;; Custom highlight numbers, highlight operators, and highlight indent guides (the vertical lines shown for indents)
  (require-init 'init-parent-mode t)
  ;; inherits off of font-lock-keyword-face
  (require-init 'init-highlight-operators-custom t)
  ;; inherits off of font-lock-regexp-grouping-backslash
  (require-init 'init-highlight-numbers-custom t)
  ;; inherits off of font-lock-keyword-face
  (require-init 'highlight-indent-guides t)

  ;; turn highlights for all programming buffers
  (add-hook 'prog-mode-hook 'highlight-numbers-mode)
  (add-hook 'prog-mode-hook 'highlight-operators-mode)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

  (setq highlight-indent-guides-method 'character)
  ;; (setq highlight-indent-guides-responsive 'top)
  (setq highlight-indent-guides-delay 0)
  ;; Don't display first level of indentation
  (defun +indent-guides-for-all-but-first-column (level responsive display)
    (unless (< level 1)
      (highlight-indent-guides--highlighter-default level responsive display)))
  (setq highlight-indent-guides-highlighter-function #'+indent-guides-for-all-but-first-column)

  ;; personal setup/ui
  (require-init 'init-personal-setup t)
  ;; personal functions
  (require-init 'init-personal-functions t)
  ;; personal custom (melpa and other) programs
  ;; (require-init 'init-programs-test t)
  (require-init 'init-programs t)
  ;; python programs
  (require-init 'init-python t)
  ;; personal keybinds
  (require-init 'init-personal-keybinds t)


  ;;=======================================================================================
  ;;=======================================================================================
  ;;==========================********* OTHER *********====================================
  ;;=======================================================================================
  ;;=======================================================================================

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

(provide 'init)

;;; init.el ends here
