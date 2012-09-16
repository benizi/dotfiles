(global-font-lock-mode 1)
(add-to-list 'load-path "~/.emacs.d")

;; Keep backup {file}~ and autosave #{file}# files in central directories
(defvar autosave-dir "~/.emacs-autosave")
(make-directory autosave-dir t)
(setq auto-save-list-file-prefix (concat autosave-dir))
(setq auto-save-file-name-transforms `(("." ,autosave-dir t)))
(setq backup-directory-alist `(("." . "~/.emacs-backups")))

;; Keep VIPER settings in the emacs dir
(setq viper-custom-file-name "~/.emacs.d/viper")

;; Set up package management
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; Set color-theme if running in X
(when (display-graphic-p)
  (require 'color-theme-railscasts))

;; popup menu for autocomplete
(require 'popup)

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;; Lispy stuff
(require 'paredit)
(require 'clojure-mode)
(require 'ac-nrepl)
(defun turn-on-paredit () (paredit-mode 1))
(defun turn-off-paredit () (paredit-mode 0))
(add-hook 'clojure-mode-hook 'turn-on-paredit)
(add-hook 'emacs-lisp-mode-hook 'turn-on-paredit)
(add-hook 'lisp-interaction-mode-hook 'turn-off-paredit)

(defun my-paredit-mode-hook ()
  (progn
    (define-key paredit-mode-map (kbd "C-]") 'paredit-forward-slurp-sexp) ; c-] rather than c-)
    (define-key paredit-mode-map (kbd "C-[") 'paredit-backward-slurp-sexp) ; c-[ rather than c-(
    (define-key paredit-mode-map (kbd "ESC") nil))) ; kill <esc> map (interferes w/ st)
(add-hook 'paredit-mode-hook 'my-paredit-mode-hook)
