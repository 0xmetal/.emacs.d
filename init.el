;; metlx's config

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
(setq inhibit-startup-message t)
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
;; (set-face-attribute 'default nil :font "UbuntuMono Nerd Font" :height 160)

;; ELPA and MELPA are the package repositories from which we can install
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("elpa" . "https://elpa.gnu.org/packages/")))

;; Make sure `use-package' is available and install it if it isn't already.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; python
(use-package python
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil))

;; nice auto completion
(use-package company
  :ensure t
  :defer t
  :custom
  ;; Search other buffers with the same modes for completion instead of
  ;; searching all other buffers.
  (company-dabbrev-other-buffers t)
  (company-dabbrev-code-other-buffers t)
  ;; M-<num> to select an option according to its number.
  (company-show-numbers t)
  ;; Only 2 letters required for completion to activate.
  (company-minimum-prefix-length 2)
  ;; Do not downcase completions by default.
  (company-dabbrev-downcase nil)
  ;; Even if I write something with the wrong case,
  ;; provide the correct casing.
  (company-dabbrev-ignore-case t)
  ;; company completion wait
  (company-idle-delay 0.2)
  ;; No company-mode in shell & eshell
  (company-global-modes '(not eshell-mode shell-mode))
  ;; Use company with text and programming modes.
    :hook ((text-mode . company-mode)
           (prog-mode . company-mode)))

;;; <EGLOT> configuration, pick this or the LSP configuration but not both.
;; Using Eglot with Pyright, a language server for Python.
;; See: https://github.com/joaotavora/eglot.
(use-package eglot
  :ensure t
  :defer t
  :hook (python-mode . eglot-ensure))

;; nice syntax highlighting
(use-package tree-sitter
  :ensure t
  :config (global-tree-sitter-mode)
  :init (add-hook 'python-mode-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t)

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

;; swag
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-solarized-dark t))

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

(defun my/kill-compilation-buffer-and-window ()
  "Kill the compilation buffer, its window, and the Buffer List window."
  (interactive)
  (let ((compilation-buffer (get-buffer "*compilation*"))
        (buffer-list-buffer (get-buffer "*Buffer List*")))
    (when compilation-buffer
      (kill-buffer compilation-buffer)
      (delete-window))
    (when buffer-list-buffer
      (kill-buffer buffer-list-buffer)
      (delete-window)))
  (keyboard-quit))
(global-set-key (kbd "C-g") 'my/kill-compilation-buffer-and-window)

(defun my-switch-to-buffer-list ()
  "Show the buffer list and switch to the buffer list buffer."
  (interactive)
  (list-buffers)
  (other-window 1))
(global-set-key (kbd "C-x C-b") 'my-switch-to-buffer-list)

(defun custom/kill-this-buffer ()
  "buffer quickscopin"
  (interactive) (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'custom/kill-this-buffer)
