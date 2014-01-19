(defvar early-packages
  '(
    auto-complete
    cider
    clojure-mode
    color-theme-railscasts
    elscreen
    evil
    expand-region
    haml-mode
    haskell-mode
    inf-ruby
    markdown-mode
    paredit
    projectile
    python
    rainbow-mode
    sass-mode
    scss-mode
    smex
    smooth-scrolling
    volatile-highlights
    yaml-mode
    yari
    )
  "Packages that should be installed immediately")

(defun early-packages-installed-p ()
  "Return true if all early-packages are installed"
  (loop for p in early-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (early-packages-installed-p)
  (message "%s" "Updating package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  (dolist (p early-packages)
    (when (not (package-installed-p p))
      (package-install p))))
