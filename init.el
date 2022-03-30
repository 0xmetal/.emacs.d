(setq inhibit-startup-message t)
(global-linum-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package magit
  :ensure t
  :pin melpa-stable)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-1337 t))

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  :init (rainbow-delimiters-mode 1))

(add-hook 'prog-mode-hook 'global-prettify-symbols-mode t)

(setq ring-bell-function 'ignore) 
(setq make-backup-files nil)
(setq auto-save-default nil)

(global-set-key (kbd "C-x C-p") 'previous-buffer)
(global-set-key (kbd "C-x C-n") 'next-buffer)

(progn
  (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark )))
  (setq whitespace-display-mappings
        '(
          (newline-mark 10 [8629 10]))))
(add-hook 'prog-mode-hook 'whitespace-newline-mode)
