;; Optimizations
;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Set garbage collection threshold
;; From https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))

;; Set file-name-handler-alist
;; Also from https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Set deferred timer to reset them
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold gc-cons-threshold-original)
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'gc-cons-threshold-original)
   (makunbound 'file-name-handler-alist-original)
   (message "gc-cons-threshold and file-name-handler-alist restored")))

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
(use-package evil
  :ensure t
  :init
  (evil-mode 1))

;; smartparens
(use-package smartparens
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'smartparens-mode)
  :init
  (show-paren-mode 1))

;; indent guides
(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))
(setq highlight-indent-guides-method 'character) 

;; dash 
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
(setq dashboard-startup-banner 'logo)

;; which-key
(use-package which-key
  :ensure t
  :init
  (which-key-mode))

;; snippets
(use-package yasnippet-snippets
  :ensure t)
(use-package yasnippet
  :ensure t
  :init
  (add-to-list 'load-path
	       "~/.emacs.d/plugins/yasnippet")
  :config
  (yas-global-mode))

;; golang
(use-package go-mode
  :ensure t)
(use-package go
  :ensure t)
(use-package go-snippets
  :ensure t)

;; highlight-parentheses
(use-package highlight-parentheses
  :ensure t)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; beacon
(use-package beacon
  :ensure t
  :init
  (beacon-mode 1)) 

;; smex
(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind
  ("M-x" . smex))

;; avy
(use-package avy
  :ensure t
  :bind ("M-s" . 'avy-goto-char))

;; color delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; mor colorz
(use-package color-identifiers-mode 
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'color-identifiers-mode))

;; symbols
(add-hook 'prog-mode-hook 'global-prettify-symbols-mode t)

;; hl-line
(use-package hl-line
  :config (add-hook 'term-mode-hook (lambda ()
                                    (setq-local global-hl-line-mode
                                                nil)))
  :init (global-hl-line-mode 1))

;; ido
(use-package ido-vertical-mode
  :ensure t
  :init (ido-vertical-mode 1))
(setq ido-vertical-define-keys
      'C-n-and-C-p-only) 
(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t) (ido-mode 1) 

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
;; treemacs
(global-set-key (kbd "<s-backspace>")
		'treemacs)
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
