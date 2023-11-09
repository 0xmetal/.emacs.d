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

;;; config
(setq use-file-dialog nil)
(setq make-backup-files nil)                         ;; relax
(setq ring-bell-function 'ignore)                    ;; relax
(setq auto-save-default nil)                         ;; no auto save
(setq recentf-max-saved-items 50)                    ;; increase recentf files
(setq scroll-step            1
      scroll-conservatively  10000)                  ;; smooth scrollin
(add-to-list 'display-buffer-alist
             '("\\*compilation\\*"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (window-height . 15)))                ;; compilation buffer at the bottom w/ adjustable height
;; (load-theme 'wheatgrass t)                           ;; set theme
(recentf-mode 1)                                     ;; lets you C-x C-r for recent files
(save-place-mode 1)
(show-paren-mode 1)
(prefer-coding-system 'utf-8)
(menu-bar-mode -1)                                   ;; swag
(tool-bar-mode -1)                                   ;; swag
(scroll-bar-mode -1)                                 ;; swag
(electric-pair-mode 1)                               ;; auto-closes parens ext...
(ido-mode 1)                                         ;; populates buffers w/ options
(set-face-attribute 'default nil :font "UbuntuMono Nerd Font" :height 160)


;; repos & use-package
(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;; doom swag
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-solarized-dark t))

;; colors
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; cool highlighting
(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; 👁️
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; auto-completion
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 99)
  (global-company-mode t)
  (global-set-key (kbd "C-c C-y") 'company-complete))

;; python support
(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))

;; python support
(use-package company-anaconda
  :ensure t
  :init (require 'rx)
  :after (company)
  :config
  (add-to-list 'company-backends 'company-anaconda))

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

;; kll comp buffer while retainin C-g functionallity
(defun my/kill-compilation-buffer-and-window ()
  "Kill the compilation buffer and its window."
  (interactive)
  (let ((compilation-buffer (get-buffer "*compilation*")))
    (when compilation-buffer
      (kill-buffer compilation-buffer)
      (delete-window)))
  (keyboard-quit))

(global-set-key (kbd "C-g") 'my/kill-compilation-buffer-and-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
