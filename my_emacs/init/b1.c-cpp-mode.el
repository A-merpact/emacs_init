;;c and cpp
;;;;
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)

;;;;C/C++
(add-hook 'c-mode-common-hook 'mass-c-cpp-mode)
(defun mass-c-cpp-mode()
  ;; 
  (c-set-style "K&R")
  ;;
  (which-function-mode)
  ;; {
;;;(c-toggle-auto-state);;
  ;; Backspace
  (c-toggle-hungry-state)
  ;; 4
  (setq c-basic-offset 2)
  ;;tab
  (setq-default indent-tabs-mode nil)
  ;;CC-mode http://cc-mode.sourceforge.net/
  (c-set-offset 'inline-open 0)
  (c-set-offset 'friend '-)
  (c-set-offset 'substatement-open 0)
  )

(setq auto-mode-alist
      (cons '("\\.h$" . c++-mode)
	    auto-mode-alist))

