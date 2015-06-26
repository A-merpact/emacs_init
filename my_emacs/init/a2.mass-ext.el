;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;max window functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;max window function for linux
;;copy from internet, many people use it, 
;;don't know the source
(defun mass-maximized ()
  (interactive)
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(1 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (interactive)
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(1 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;nxml mode functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;pretty print for nxml mode
(defun mass-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this. The function inserts linebreaks to separate tags that have
nothing but whitespace between them. It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
      (backward-char) (insert "\n") (setq end (1+ end)))
    (indent-region begin end))
  (message "Ah, much better!"))

;;many codes from lmb:http://www.stackedboxes.org/~lmb/files/dotEmacs
(defun mass-enable-nxml-folding()
  "Enables extra features on nxml-mode. All code related to
folding came from EmacsWiki, and seems to be by Peter Heslin."
  (make-local-variable 'outline-regexp)
  (setq outline-regexp "\\s *<\\([h][1-6]\\|html\\|body\\|head\\)\\b")
  (make-local-variable 'outline-level)
  (setq outline-level 'mass-xhtml-outline-level)
  ;(outline-minor-mode 1)
)

(defun mass-xhtml-outline-level()
  (save-excursion (re-search-forward html-outline-level))
  (let ((tag (buffer-substring (match-beginning 1) (match-end 1))))
    (if (eq (length tag) 2)
        (- (aref tag 1) ?0)
      0)))

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
               ""
               "<!--" ;; won't work on its own; uses syntax table
               (lambda (arg) (mass-nxml-forward-element))
               nil))

(defun mass-nxml-forward-element()
  (let ((nxml-sexp-element-flag))
    (setq nxml-sexp-element-flag (not (looking-at "<!--")))
    (unless (looking-at outline-regexp)
      (condition-case nil
          (nxml-forward-balanced-item 1)
        (error nil)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;From DEA www.emacer.com

;; (progn
;;   (setq hl-line-face-delta #X30FF)
;;   (defun color-theme-adjust-hl-line-face()
;;     "Auto adjust hl-line-face by background color."
;;     (interactive)
;;     (let* ((color (x-color-values (face-attribute 'default :background))))
;;       (if (null color)
;;  	  (error (format "Not support on system %s" system-type))
;;  	(let ((mass-color
;; 	       (mapcar
;;  		(lambda (x)
;;  		  (let ((y (/ #XFFFF 4))
;;  			(delta hl-line-face-delta))
;;  		    (cond
;;  		     ((< x (* y 1))
;;  		      (+ x delta))
;;  		     ((< x (* y 2))
;;  		      (+ x delta))
;;  		     ((< x (* y 3))
;;  		      (- x delta))
;;  		     (t
;;  		      (- x delta)))))
;;  		color)))
;;  	  (set-face-attribute
;;  	   hl-line-face
;;  	   nil
;;  	   :background
;;  	   (concat "#" (mapconcat (lambda (c) (format "%04X" c)) mass-color ""))))))))
;; (defun color-theme-adjust-hl-line-face())

(defun mass-copy-cur-line ()
  "拷贝当前行"
  (interactive)
  (let ((end (min (point-max) (1+ (line-end-position)))))
    (copy-region-as-kill (line-beginning-position) end)))

;; (defun mass-copy-lines (&optional number)
;;   "从当前行开始拷贝NUMBER行"
;;   (interactive "p")
;;   (if (null number)
;;       (mass-copy-cur-line)
;;     (let ((lineNo))
;;       (save-excursion
;;         (if (< number 0)
;;             (next-line))
;;         (setq lineNo (line-number-at-pos nil))
;;         (move-beginning-of-line nil)
;;         (set-mark-command nil)
;;         (goto-line (+ number lineNo))
;;         (call-interactively 'copy-region-as-kill-nomark)))))

(defun mass-smart-copy ()
  "智能拷贝, 如果'mark-active'的话, 则`copy-region', 否则`copy-lines'"
  (interactive)
  ;;(if mark-active (copy-region-as-kill (region-beginning) (region-end))
  (if mark-active (call-interactively 'copy-region-as-kill)
    (call-interactively 'mass-copy-cur-line)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defun mass-tango ()
  (interactive)
  (load-theme 'tangotango t))

(defun mass-no-tango ()
  (interactive)
  (disable-theme 'tangotango))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
