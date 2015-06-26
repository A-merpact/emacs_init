(require 'org-install)
(require 'org-publish)

;;Turn on indent mode, not supported very well by emacs
;;(setq org-startup-indented t)

(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'mass-org-mode)

(defun mass-org-mode()
  (define-key org-mode-map "\C-ca" 'org-agenda)
  (define-key org-mode-map "\C-cb" 'org-iswitchb)
  (setq truncate-lines nil)
  ;; (setq note-root-dir
  ;; 	(file-name-directory (or load-file-name (buffer-file-name))))
  ;; (setq note-publish-dir
  ;; 	(expand-file-name "public_html" (directory-file-name
  ;; 					 (file-name-directory
  ;; 					  (directory-file-name note-root-dir)))))
  (setq org-log-done t)
  (setq org-publish-project-alist
	`(("note-org"
	   :base-directory "..\\org\\org"
	   :publishing-directory "..\\org\\publish"
	   :base-extension "org"
	   :recursive t
	   :publishing-function org-publish-org-to-html
	   :auto-index t
	   :index-filename "index.org"
	   :index-title "index"
	   :link-home "index.html")
	  ("note-static"
	   :base-directory "..\\org\\org"
	   :publishing-directory "..\\org\\publish"
	   :recursive t
	   :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|swf\\|zip\\|gz\\|txt\\|el"
	   :publishing-function org-publish-attachment)
	  ("note" 
	   :components ("note-org" "note-static")
	   :authoer "kai.zhang1984@gmail.com")))
  )