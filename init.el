(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Use Package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; rainbow delimiters
(use-package rainbow-mode
  :ensure t
  :init (rainbow-mode 1))
(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Download Evil && Enable Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))
(require 'evil)
(evil-mode 1) 

;; Which-key
(use-package which-key
  :ensure t
  :init (which-key-mode))

;; DOOM
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)) 

;; xnxx lol
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
(setq dashboard-center-content t)
(setq dashboard-startup-banner 'logo) 
(setq dashboard-items '((recents . 3)))
(setq dashboard-banner-logo-title "")
(setq dashboard-set-footer nil)
(setq dashboard-set-navigator t)

;; smartparens
(require 'smartparens-config)
(add-hook 'prog-mode-hook #'smartparens-mode)
(show-paren-mode 1) 

;; clean her up
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; zsh config
(defvar my-term-shell "/bin/zsh")  
  (defadvice ansi-term (before force-bash)
    (interactive (list my-term-shell)))
(ad-activate 'ansi-term)  

;; quick
(defalias 'yes-or-no-p 'y-or-n-p) 

;; quick term
(global-set-key (kbd "<s-return>") 'ansi-term) 

;; test
(global-set-key (kbd "C-x C-p") 'previous-buffer) 
(global-set-key (kbd "C-x C-n") 'next-buffer) 
;; chill
(setq ring-bell-function 'ignore) 

;; relac
(setq make-backup-files nil)
(setq auto-save-default nil) 

;; beacon
(use-package beacon
  :ensure t
  :init (beacon-mode 1)) 

;; symbols
(when window-system (global-prettify-symbols-mode t)) 

;; ido
(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t) (ido-mode 1) 
(use-package ido-vertical-mode
  :ensure t
  :init (ido-vertical-mode 1))
(setq ido-vertical-define-keys 'C-n-and-C-p-only) 

;; smex
(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind
  ("M-x" . smex)) 

;; avy
(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char)) 

;; modeline colum numbers
(column-number-mode 1) 

;; fuck yes fuck yes fuck yes

;;(nyan-mode 1)
(add-hook 'prog-mode-hook #'nyan-mode 1)
(add-hook 'prog-mode-hook #'nyan-start-animation 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(auth-source-save-behavior nil)
 '(custom-enabled-themes '(spacemacs-dark))
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" default))
 '(doom-modeline-mode t)
 '(linum-format " %7i ")
 '(package-selected-packages
   '(magit spacemacs-theme go-mode smartparens nyan-mode ccls company-lsp lsp-ui lsp-mode sublime-themes doom-themes rainbow-mode rainbow-delimiters avy smex ido-vertical-mode beacon evil dashboard which-key use-package doom-modeline)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
