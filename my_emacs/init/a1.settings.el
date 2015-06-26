;set user name
(setq user-full-name "Kai.Zhang")
(setq user-mail-address "kai.zhang@accelops.net")

(set-language-environment 'UTF-8) 
(set-locale-environment "UTF-8") 

(if (eq system-type 'windows-nt)
    (progn
      (setenv "HOME" mass-root)
      (setenv "PATH" (concat mass-root ";"
			     (getenv "PATH")))
      (setq default-directory "~/")
      )
  )

(if (eq system-type 'gnu/linux)
    (progn
      )
  )

(setq user-emacs-directory (concat mass-root "/extension/.emacs.d"))

;set load path
;(add-to-list 'load-path (concat mass-root "/extension/color-theme"))
;(add-to-list 'load-path (concat mass-root "/extension/org-mode"))
;(add-to-list 'load-path (concat mass-root "/extension/python-mode"))
;(add-to-list 'load-path (concat mass-root "/extension/others"))


(setq inhibit-splash-screen t)

(setq mouse-yank-at-point t)

(global-linum-mode t)
(set-scroll-bar-mode 'right)

(tool-bar-mode -1)
(menu-bar-mode -1)

(setq x-select-enable-clipboard t)
(blink-cursor-mode -1)

(setq backup-inhibited t);;disable ~filename
(setq auto-save-default nil);;disable#filename#

(show-paren-mode t)
(setq show-paren-style 'parentheses)

(transient-mark-mode t)

;replace 'yes' with 'y', 'no' with 'n'
(fset 'yes-or-no-p 'y-or-n-p)

;;dispaly time
;(display-time-mode t)

;
(setq-default kill-whole-line t)

;stay at end
(setq track-eol t)
(setq scroll-step 1
      scroll-margin 3
      scroll-conservatively 10000)

(add-to-list 'custom-theme-load-path (concat mass-root "/extension/theme"))

;When quite emacsclient window, delete buffers for it
(add-hook 'delete-frame-functions
          (lambda (frame)
            (let* ((window (frame-selected-window frame))
                   (buffer (and window (window-buffer window))))
              (when (and buffer (buffer-file-name buffer))
                (kill-buffer buffer)))))
