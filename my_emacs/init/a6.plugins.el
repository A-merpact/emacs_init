(defun require-extensions (action lst)
  (mapcar (lambda(ext) "" (funcall action ext)) lst))

(require-extensions 'require
		    (list
		     'hl-line
		     'ido
		     'windmove
		     'cc-mode
		     ))


;hl-line-mode
(global-hl-line-mode t)

;windmove
(when (featurep 'windmove)
  (windmove-default-keybindings 'meta))

;; Use C-f during file selection to switch to regular find-file
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length 0)
(setq ido-use-virtual-buffers t)

(require-package 'ido-ubiquitous)
(ido-ubiquitous-mode t)

(require-package 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; Allow the same buffer to be open in different frames
(setq ido-default-buffer-method 'selected-window)

(provide 'init-ido)


