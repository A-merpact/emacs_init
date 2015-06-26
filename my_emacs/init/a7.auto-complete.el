(require-package 'auto-complete)
(require-package 'auto-complete-clang)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-expand-on-auto-complete nil)
(setq ac-auto-start nil)
(setq ac-dwim nil) ; To get pop-ups with docs even if a word is uniquely completed
(ac-set-trigger-key "TAB");
;; (setq tab-always-indent 'complete)  ;; use 't when auto-complete is disabled
;; (add-to-list 'completion-styles 'initials t)

;; ;; hook AC into completion-at-point
;; (defun sanityinc/auto-complete-at-point ()
;;   (when (and (not (minibufferp)) 
;; 	     (fboundp 'auto-complete-mode)
;; 	     auto-complete-mode)
;;     (auto-complete)))

;; (defun set-auto-complete-as-completion-at-point-function ()
;;   (add-to-list 'completion-at-point-functions 'sanityinc/auto-complete-at-point))

;; (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)


(setq ac-use-menu-map t)
;; Default settings
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
;; extra modes auto-complete must support
(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
				    sass-mode yaml-mode csv-mode espresso-mode haskell-mode
				    html-mode nxml-mode sh-mode smarty-mode clojure-mode
				    lisp-mode textile-mode markdown-mode tuareg-mode
				    js2-mode css-mode less-css-mode))
  (add-to-list 'ac-modes mode))

;; Exclude very large buffers from dabbrev
(defun sanityinc/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))

(setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)

;; clang stuff
;; @see https://github.com/brianjcj/auto-complete-clang
(defun my-ac-cc-mode-setup ()
  (require 'auto-complete-clang)
  (setq ac-sources (append '(ac-source-clang) ac-sources))
  (setq clang-include-dir-str
        (cond
         (t "
 /usr/lib/gcc/x86_64-redhat-linux/4.4.7/../../../../include/c++/4.4.7
 /usr/lib/gcc/x86_64-redhat-linux/4.4.7/../../../../include/c++/4.4.7/x86_64-redhat-linux
 /usr/lib/gcc/x86_64-redhat-linux/4.4.7/../../../../include/c++/4.4.7/backward
 /usr/local/include
 /usr/lib/gcc/x86_64-redhat-linux/4.4.7/include
 /usr/include
")
	 (t "") ; other platforms
        )
	)
  (setq ac-clang-flags
        (mapcar (lambda (item) (concat "-I" item))
                (split-string clang-include-dir-str)))

  ;(cppcm-reload-all)
					; fixed rinari's bug
  (remove-hook 'find-file-hook 'rinari-launch)

  (setq ac-clang-auto-save t)
  )
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)

(ac-config-default)

(provide 'init-auto-complete)

