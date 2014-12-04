;;; rxvt.el --- support 24-bit color modes
; in rxvt-unicode-24bit
; with 24-bit color extensions from github.com/benizi/rxvt-unicode

;; Author: benizi

(load "term/rxvt")

(defun rxvt-register-color-cube (start stop n)
  "Register an NxNxN color cube starting at color START"
  (let ((r 0) (g 0) (b 0) (c start))
    (while (< c stop)
      (tty-color-define (format "color-%d" c)
			c
			(mapcar 'rxvt-rgb-convert-to-16bit
				(list (round (* r 42.5))
				      (round (* g 42.5))
				      (round (* b 42.5)))))
      (setq b (1+ b))
      (if (= b n)
	  (setq g (1+ g)
		b 0))
      (if (= g n)
	  (setq r (1+ r)
		g 0))
      (setq c (1+ c)))))

(defun rxvt-register-grays (start stop)
  "Register grayscale ramp from START to STOP"
  (let ((n 0)
	(c start))
    (while (< c stop)
      (let ((g (rxvt-rgb-convert-to-16bit (+ 8 (* n 10)))))
	(tty-color-define (format "color-%d" c)
			  c
			  (list g g g)))
      (setq n (1+ n)
	    c (1+ c)))))

(defun rxvt-register-standard-colors ()
  "Register the standard 16 lowest colors"
  (dolist (color rxvt-standard-colors)
    (tty-color-define (car color)
		      (cadr color)
		      (mapcar 'rxvt-rgb-convert-to-16bit
			      (car (cddr color))))))

(defun rxvt-register-default-colors ()
  "Register 24-bit colors"
  (tty-color-clear)
  (rxvt-register-standard-colors)
  (rxvt-register-color-cube 16 240 6)
  (rxvt-register-grays 240 256))

(defun rxvt-tty-color-approximate (rgb &optional frame)
  "*TODO*"
  (let* ((r (ash (car rgb) -8))
	 (g (ash (cadr rgb) -8))
	 (b (ash (nth 2 rgb) -8))
	 (name (format "#%02x%02x%02x" r g b))
	 (index (+ (ash r 16) (ash g 8) b)))
    (list name index (car rgb) (cadr rgb) (nth 2 rgb))))

(defun rxvt-overwrite-standard-tty-stuff ()
  "Overwrites standard tty function(s) with rxvt 24-bit versions"
  (defun tty-color-approximate (rgb &optional frame)
    (rxvt-tty-color-approximate rgb frame)))

(defun rxvt-set-background-mode ()
  "Set background mode to dark"
  (set-terminal-parameter nil 'background-mode 'dark))

;; ;; TODO - don't rely on `st' equivalents
(load "term/st")
(defun rxvt-tttt ()
  (st-overwrite-standard-tty-stuff)
  (st-remove-ansi-colors))

(defun terminal-init-rxvt ()
  "Terminal initialization function for rxvt."

  (let ((map (copy-keymap rxvt-alternatives-map)))
    (set-keymap-parent map (keymap-parent local-function-key-map))
    (set-keymap-parent local-function-key-map map))

  ;; Use inheritance to let the main keymap override those defaults.
  ;; This way we don't override terminfo-derived settings or settings
  ;; made in the init file.
  (let ((m (copy-keymap rxvt-function-map)))
    (set-keymap-parent m (keymap-parent input-decode-map))
    (set-keymap-parent input-decode-map m))

  ;; Initialize colors and background mode.
  (if (string-match "-super$" (getenv "TERM"))
      ; extra stuff for "super" mode
      (progn
	(rxvt-tttt) ; TODO
	(tty-set-up-initial-frame-faces))

    ; normal color initialization
    (progn
      (rxvt-register-default-colors)
      (rxvt-set-background-mode)))

  ;; This recomputes all the default faces given the colors we've just set up.
  (tty-set-up-initial-frame-faces))
