;;; st.el --- support 24-bit color modes
; in "st" (simpleterm) from suckless.org
; with 24-bit color extensions from github.com/benizi/st

;; Author: benizi

(load "term/xterm")

(defgroup st nil
  "st support."
  :version "24.1"
  :group 'emacs)

(defcustom st-24bit 'check
  "Whether the terminal supports 24-bit color sequences.
If nil, assume it doesn't.
Otherwise assume it does."
  :version "24.1"
  :group 'st
  :type '(choice (const :tag "No" nil)
		 (const :tag "Yes" check)))

(defun st-register-default-colors ()
  "Call xterm-register-default-colors defaulting to 256-color mode."
  (flet ((display-color-cells (frame) 256))
    (xterm-register-default-colors)
    (tty-set-up-initial-frame-faces)))

(defun st-tty-color-approximate (rgb &optional frame)
  "Convert RGB to a single integer. Returns a list of the same format as returned by tty-color-approximate: \(NAME INDEX R G B\)"
  (let* ((r (ash (car rgb) -8))
	 (g (ash (cadr rgb) -8))
	 (b (ash (nth 2 rgb) -8))
	 (name (format "#%02x%02x%02x" r g b))
	 (index (+ (ash r 16) (ash g 8) b)))
    (list name index (car rgb) (cadr rgb) (nth 2 rgb))))

(defun st-overwrite-standard-tty-stuff ()
  "Overwrites standard tty function(s) with st versions"
  (defun tty-color-approximate (rgb &optional frame)
    (st-tty-color-approximate rgb frame)))

(defun st-24bit-setup ()
  "Set things up for \"high\" mode (256-color mode)"
  (st-register-default-colors))

(defun st-24bit-super-setup ()
  "Set things up for \"super\" mode (24-bit color via terminfo)"
  (st-overwrite-standard-tty-stuff))

(defun st-super-p ()
  "Whether the st currently running supports \"super\" mode"
  (string-match "-super$" (getenv "TERM")))

(defun terminal-init-st ()
  "Settings for st terminal"
  (progn
    (push (getenv "TERM") mode-line-format)
    (if (st-super-p)
        (st-24bit-super-setup)
      (st-24bit-setup))))
