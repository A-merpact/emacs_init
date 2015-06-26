(setq mass-root "/home/admin/my_emacs")

(add-to-list 'load-path (concat mass-root "/init"))
 
(mapc 'load (directory-files (concat mass-root "/init") t "\\.el$"))