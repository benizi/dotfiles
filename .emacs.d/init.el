(global-font-lock-mode 1)
(add-to-list 'load-path "~/.emacs.d")

;; Many libs require the CL-compatibility library
(require 'cl)

;; Set up package management
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
      ("marmalade" . "http://marmalade-repo.org/packages/")
      ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(load "early-packages")

;; defaults from emacs-live
(setq initial-major-mode 'lisp-interaction-mode
      redisplay-dont-pause t
      column-number-mode t
      echo-keystrokes 0.02
      inhibit-startup-message t
      transient-mark-mode t
      ;; shift-select-mode nil
      require-final-newline t
      truncate-partial-width-windows nil
      delete-by-moving-to-trash nil
      confirm-nonexistent-file-or-buffer nil
      query-replace-highlight t
      next-error-highlight t
      next-error-highlight-no-select t)

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
(if nil
    (progn
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
        (read-kbd-macro evil-toggle-key) 'evil-emacs-state)))

;; El-Screen
(require 'elscreen)
(global-set-key (kbd "M-{") 'elscreen-previous)
(global-set-key (kbd "M-}") 'elscreen-next)
(global-set-key (kbd "M-t") 'elscreen-create)

;; I DO things
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

;; better M-x (emacs-live)
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ; old M-x

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
    (require 'color-theme-railscasts)
    (set-face-attribute 'default nil :background "#000000")))

(add-hook 'window-setup-hook 'setup-color-theme)

;; set up xterm (necessary evil?)
(custom-set-variables
 '(xterm-extra-capabilities (quote modifyOtherKeys)))

;; popup menu for autocomplete
(require 'popup)

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

(require 'anything)

;; Lispy stuff
(eval-after-load 'paredit
  '(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp))
(require 'paredit)
(require 'clojure-mode)
(require 'cider)
(defun turn-on-paredit () (paredit-mode 1))
(defun turn-off-paredit () (paredit-mode 0))
(add-hook 'clojure-mode-hook 'turn-on-paredit)
(add-hook 'emacs-lisp-mode-hook 'turn-on-paredit)
(add-hook 'cider-repl-mode-hook 'turn-on-paredit)
(add-hook 'lisp-interaction-mode-hook 'turn-off-paredit)

(require 'mic-paren) ;; show matching paren even when off-screen
(paren-activate)

;; highlight expr on eval
(require 'highlight)
(require 'eval-sexp-fu)
(setq eval-sexp-fu-flash-duration 0.5)

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
