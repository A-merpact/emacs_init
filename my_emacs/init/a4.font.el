(defun my-default-font ()
  (interactive)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(font . "Dejavu Sans Mono-14"))
  ;; (set-fontset-font (frame-parameter nil 'font)
  ;;                   'han '("AR PL UMing CN"."unicode-bmp"))
  (if (eq system-type 'windows-nt)
      (progn
	;;(dolist (charset '(kana han symbol cjk-misc bopomofo))
          ;; (add-to-list 'default-frame-alist '
	  ;; (set-fontset-font (frame-parameter nil 'font)
	  ;;       	    charset
	  ;;       	    (font-spec :family "Microsoft Yahei" :size 15)))
	)
    )
  (if (eq system-type 'gnu/linux)
      (progn
        ;;(add-to-list 'default-frame-alist '(font . "AR PL UMing CN"))
	
	)
    )
  )

(my-default-font)
