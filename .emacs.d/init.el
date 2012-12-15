(global-font-lock-mode 1)
(add-to-list 'load-path "~/.emacs.d")

;; Set up package management
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
      ("marmalade" . "http://marmalade-repo.org/packages/")
      ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(load "early-packages")

;; no menu or tool bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)

;; highlight whitespace errors
(setq-default whitespace-style '(face tabs trailing lines space-before-tab space-after-tab empty line))
(whitespace-mode)
(setq require-final-newline t)

;; set up tabstop functions
(setq-default indent-tabs-mode nil)

(defun setup-tabstop (sw et)
  "Setup tabstop like vim's &sw and &et"
  (progn
    (setq tab-width sw)
    (setq c-basic-offset sw)
    (setq cperl-indent-level sw)
    (setq indent-tabs-mode et)))

(column-number-mode)

;; Keep backup {file}~ and autosave #{file}# files in central directories
(defvar autosave-dir "~/.emacs.d/autosaves")
(make-directory autosave-dir t)
(defvar backup-dir "~/.emacs.d/backups")
(make-directory backup-dir t)

(setq auto-save-list-file-prefix (concat autosave-dir))
(setq auto-save-file-name-transforms `((".*" ,(concat autosave-dir "/\\1") t)))
(setq backup-directory-alist `(("." . ,backup-dir)))

;; Keep VIPER settings in the emacs dir
(setq viper-custom-file-name "~/.emacs.d/viper")

;; EMacs VIm compatibility Layer
(require 'evil)
(evil-mode)
(setq evil-default-state 'normal)
(evil-set-initial-state 'nrepl-mode 'insert)
(evil-set-initial-state 'fundamental-mode 'insert)

; clear default insert mode mappings
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map
  [escape] 'evil-normal-state)
(define-key evil-insert-state-map
  (read-kbd-macro evil-toggle-key) 'evil-emacs-state)

;; I DO things
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

;; set font in X11
(if (display-graphic-p)
    (set-default-font "DejaVu Sans Mono-14"))

;; Set color-theme if running in X or a high-color terminal
(defun setup-color-theme-p ()
  "Returns true if it looks like the display can handle 24-bit colors"
  (or (display-graphic-p)
      (< 256 (display-color-cells))
      (string-match "^st-24bit" (getenv "TERM"))
      (getenv "KONSOLE_DBUS_SERVICE")))

(defun setup-color-theme ()
  "Set up my color theme"
  (when (setup-color-theme-p)
    (require 'color-theme-railscasts)))

(add-hook 'window-setup-hook 'setup-color-theme)

;; popup menu for autocomplete
(require 'popup)

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

(require 'anything)

;; Lispy stuff
(require 'paredit)
(require 'clojure-mode)
(require 'ac-nrepl)
(defun turn-on-paredit () (paredit-mode 1))
(defun turn-off-paredit () (paredit-mode 0))
(add-hook 'clojure-mode-hook 'turn-on-paredit)
(add-hook 'emacs-lisp-mode-hook 'turn-on-paredit)
(add-hook 'nrepl-mode-hook 'turn-on-paredit)
(add-hook 'lisp-interaction-mode-hook 'turn-off-paredit)

(defun my-paredit-mode-hook ()
  (progn
    (define-key paredit-mode-map (kbd "C-]") 'paredit-forward-slurp-sexp) ; c-] rather than c-)
    (define-key paredit-mode-map (kbd "C-[") 'paredit-backward-slurp-sexp) ; c-[ rather than c-(
    (define-key paredit-mode-map (kbd "ESC") nil))) ; kill <esc> map (interferes w/ st)
(add-hook 'paredit-mode-hook 'my-paredit-mode-hook)

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)
(defun conditionally-enable-paredit-mode ()
  "enable paredit-mode during eval-expression"
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

(setq auto-mode-alist (cons '("\\.cljs$" . clojure-mode) auto-mode-alist))

; Haskell stuff
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(setup-tabstop 2 t)
