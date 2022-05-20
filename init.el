;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0xmetal's emacs config ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; melpa
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; xray vision
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

;; need
(use-package undo-fu
  :ensure t
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

;; davinci
(use-package golden-ratio
  :ensure t
  :config
  (golden-ratio-mode 1))

;; nice 
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; highlight numbers support
(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; sexy theme
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-tomorrow-night t))

;; javascript mode
(use-package js2-mode
  :ensure t
  :mode
  ("\\.js\\'" . js2-mode)
  :config
  (setq js2-basic-offset 2)
  (setq js2-mode-show-strict-warnings nil))

;; git support
(use-package magit
  :ensure t)

;; better defaults
(prefer-coding-system 'utf-8)
(setq initial-scratch-message nil)
(setq inhibit-splash-screen t)
(setq make-backup-files nil)

;; ui
(setq ring-bell-function 'ignore)
(global-hl-line-mode 1)
(add-hook 'prog-mode-hook 'global-prettify-symbols-mode t)
(unless (eq window-system 'ns)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; basic modes
(recentf-mode 1)
(save-place-mode 1)
(show-paren-mode 1)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode 1)

;; recentf
(if (fboundp 'fido-mode)
    (progn
      (fido-mode 1)
      (when (fboundp 'fido-vertical-mode)
        (fido-vertical-mode 1))

      (defun fido-recentf-open ()
        "Use `completing-read' to find a recent file."
        (interactive)
        (if (find-file (completing-read "Find recent file: " recentf-list))
            (message "Opening file...")
          (message "Aborting")))
      (global-set-key (kbd "C-x C-r") 'fido-recentf-open))
  (progn
    (ido-mode 1)
    (ido-everywhere 1)

    (setq ido-use-virtual-buffers t
          ido-use-filename-at-point 'guess
          ido-create-new-buffer 'always
          ido-enable-flex-matching t)))

;; bindings for elisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-x") #'ielm)
            (local-set-key (kbd "C-c C-c") #'eval-defun)
            (local-set-key (kbd "C-c C-b") #'eval-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
