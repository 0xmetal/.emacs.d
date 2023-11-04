;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; metlx's emacs config ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Do not allow loading from the package cache (same reason).
(setq package-quickstart nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Prevent unwanted runtime builds in gccemacs (native-comp); packages are
;; compiled ahead-of-time when they are installed and site files are compiled
;; when gccemacs is installed.
(setq comp-deferred-compilation nil)

;; manually load theme
;; (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
(load-theme 'wheatgrass t)

;; config
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(add-hook 'prog-mode-hook 'whitespace-mode t) ;; tsoding
(setq use-file-dialog nil)
(prefer-coding-system 'utf-8)
(setq make-backup-files nil)
(setq ring-bell-function 'ignore)
(setq auto-save-default nil)
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(save-place-mode 1)
(show-paren-mode 1)
(setq scroll-step            1
      scroll-conservatively  10000)

;; melpa
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; magic hack
(use-package gcmh
  :ensure t
  :demand
  :config
  (gcmh-mode 1))

;; dashboard
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-startup-banner 'official)
  (setq dashboard-center-content t)
  (setq dashboard-items nil)
  (dashboard-setup-startup-hook)
  )

;; need
(use-package undo-fu
  :ensure t
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

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

;; is nice
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; auto completez
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0
	company-minimum-prefix-length 3
	company-tooltip-limit nil
	company-tooltip-align-annotations nil
	company-tooltip-flip-when-above nil)
  (global-company-mode t))

;; python bro
(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  )

;; python bro
(use-package company-anaconda
  :ensure t
  :init (require 'rx)
  :after (company)
  :config
  (add-to-list 'company-backends 'company-anaconda)
  )

;; recentd
(if (fboundp 'fido-mode)
    (progn
      (fido-mode 1)
      (when (fboundp 'fido-vertical-mode)
        (fido-vertical-mode 1))

      (defun fido-recentf-open-directory ()
        "Use `completing-read' to find a recent directory."
        (interactive)
        (let* ((recent-dirs (delete-dups (mapcar 'file-name-directory recentf-list)))
               (chosen-dir (completing-read "Find recent directory: " recent-dirs)))
          (when (file-directory-p chosen-dir)
            (dired chosen-dir)
            (message "Opening directory...")
            (revert-buffer)))
        (message "Aborting"))
      (global-set-key (kbd "C-x C-d") 'fido-recentf-open-directory))
  (progn
    (ido-mode 1)
    (ido-everywhere 1)

    (setq ido-use-virtual-buffers t
          ido-use-filename-at-point 'guess
          ido-create-new-buffer 'always
          ido-enable-flex-matching t)))

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
