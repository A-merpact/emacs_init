(defun set-key-bindings (action bindingList)
  ""
  (mapcar (lambda(lst)
	    ""
	    (let ((x (car lst))
		  (y (car (last lst))))
	      (funcall action x y))) bindingList ))


(set-key-bindings 'global-set-key
		  (list
		    (list (kbd "C-z")  'undo)
		    (list (kbd "M-w")  'mass-smart-copy)
		    (list (kbd "C-x C-b") 'ibuffer);ibuffer
		    (list (kbd "<f2> 1") 'mass-tango);color
		    (list (kbd "<f2> 2") 'mass-no-tango);color
		    (list (kbd "<f2> 3") 'org-store-link);org link store
		    (list (kbd "<f2> 4") 'kill-other-buffers)
                    (list (kbd "RET") 'newline-and-indent)
		    ))

(global-set-key [C-f4] 'delete-frame)
;; ;; setting the PC keyboard's various keys to
;; ;; Super or Hyper, for emacs running on Windows.
;; (setq w32-pass-lwindow-to-system nil 
;;       w32-pass-rwindow-to-system nil 
;;       w32-pass-apps-to-system nil 
;;       w32-lwindow-modifier 'super ; Left Windows key 
;;       w32-rwindow-modifier 'super ; Right Windows key 
;;       w32-apps-modifier 'hyper) ; Menu key
(global-set-key [f3] 'indent-buffer)
