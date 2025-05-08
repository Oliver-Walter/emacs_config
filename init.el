;; ~/.emacs.d/init.el
;; Written by Oliver Walter

;; Set up package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Install and configure use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Require use-package
(require 'use-package)

;; Install and configure required packages
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

(use-package magit
  :ensure t)

;; Set the default font
(set-face-attribute 'default nil :font "Cascadia Code 14")
