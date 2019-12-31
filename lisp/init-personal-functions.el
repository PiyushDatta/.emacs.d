;;; init-personal-functions.el 

;;; Code:

;; Select entire line
(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

;; copy entire line
;; (defun copy-line (arg)
;;       "Copy lines (as many as prefix argument) in the kill ring"
;;       (interactive "p")
;;       (kill-ring-save (line-beginning-position)
;;                       (line-beginning-position (+ 1 arg)))
;;       (message "%d line%s copied" arg (if (= 1 arg) "" "s")))
;; (global-set-key (kbd "C-c l") 'copy-line)

;; other and prev window
 (defun prev-window ()
   (interactive)
   (other-window -1))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (kill-new (buffer-file-name))
  (message (buffer-file-name)))

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

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

;; Get formatted time to float seconds
(defun format-time-to-seconds (curr-time-formatted)
  "Get formatted time to float seconds"
  (interactive)
  (format "%.2f" (float-time curr-time-formatted)))

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

;;; init-personal-functions.el ends here