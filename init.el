;;; init.el --- Emacs init file
;;  Author: Piyush Datta

;; Set default directory to config folder
(getenv "HOME")
(setq default-directory "~/")
(global-set-key (kbd "C-x C-f")  (lambda () (interactive)
                                     (cd "~/emacs.d/init.el")
                                     (call-interactively 'find-file)))

;; Create themes directory
(let ((themes-dir (concat default-directory "/.emacs.d/themes")))
  (unless (file-exists-p themes-dir)
    (make-directory themes-dir)))
		

		
