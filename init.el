;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
;;(package-refresh-contents)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; evil
(unless (package-installed-p 'evil)
  (package-refresh-contents)
  (package-install 'evil))
(require 'evil)
(evil-mode 1)

;; smartparens
(unless (package-installed-p 'smartparens)
  (package-refresh-contents)
  (package-install 'smartparens))
(require 'smartparens-config)
(add-hook 'prog-mode-hook #'smartparens-mode)
(show-paren-mode 1)

;; dash 
(unless (package-installed-p 'dashboard)
  (package-refresh-contents)
  (package-install 'dashboard))
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
(setq dashboard-startup-banner 'logo)

;; which-key
(unless (package-installed-p 'which-key)
  (package-refresh-contents)
  (package-install 'which-key))
(use-package which-key
  :ensure t
  :init
  (which-key-mode))

;; snippets
(unless (package-installed-p 'yasnippet)
  (package-refresh-contents)
  (package-install 'yasnippet))
(unless (package-installed-p 'yasnippet-classic-snippets)
  (package-refresh-contents)
  (package-install 'yasnippet-classic-snippets))
(unless (package-installed-p 'yasnippet-snippets)
  (package-refresh-contents)
  (package-install 'yasnippet-snippets))
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;; golang
(unless (package-installed-p 'go-mode)
  (package-refresh-contents)
  (package-install 'go-mode))
(unless (package-installed-p 'go)
  (package-refresh-contents)
  (package-install 'go-mode))
(unless (package-installed-p 'go-snippets)
  (package-refresh-contents)
  (package-install 'go-mode))

;; beacon
(unless (package-installed-p 'beacon)
  (package-refresh-contents)
  (package-install 'beacon))
(use-package beacon
  :ensure t
  :init
  (beacon-mode 1)) 

;; smex
(unless (package-installed-p 'smex)
  (package-refresh-contents)
  (package-install 'smex))
(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind
  ("M-x" . smex))

;; avy
(unless (package-installed-p 'avy)
  (package-refresh-contents)
  (package-install 'avy))
(use-package avy
  :ensure t
  :bind
  ("M-s" . avy-goto-char))

;; ido
(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t) (ido-mode 1) 
(use-package ido-vertical-mode
  :ensure t
  :init (ido-vertical-mode 1))
(setq ido-vertical-define-keys
      'C-n-and-C-p-only) 
;; zsh config
(defvar my-term-shell "/bin/zsh")  
  (defadvice ansi-term (before force-bash)
    (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;; quick
(defalias 'yes-or-no-p
  'y-or-n-p)

;; quick term
(global-set-key (kbd "<s-return>")
		'ansi-term) 
;; fast buf
(global-set-key (kbd "C-x C-p") 'previous-buffer) 
(global-set-key (kbd "C-x C-n") 'next-buffer) 

;; chill
(setq ring-bell-function 'ignore) 

;; relac
(setq make-backup-files nil)
(setq auto-save-default nil)

;; scrolling
(setq scroll-conservatively 10000)

;; modeline colum numbers
(column-number-mode 1) 

;; clean up
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(avy smex go go-snippets yasnippet-classic-snippets go-mode yasnippet-snippets yasnippet which-key dashboard smartparens use-package evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
