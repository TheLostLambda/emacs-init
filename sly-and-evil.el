;;; Setup Package Management ===================================================

;; MELPA Setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Pull the package list if it's missing
(unless package-archive-contents
  (package-refresh-contents))

;; Install and enable use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;;; Basic Visual Tweaks ========================================================

;; Set Emacs theme
(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-soft t))

;; Get rid of the big GUI toolbar
(tool-bar-mode -1)

;; Set default font
(set-face-attribute 'default nil :font "Berkeley Mono")

;;; Setup Common Lisp IDE ======================================================

;; Enable code completion
(use-package company
  :ensure t
  :hook (prog-mode . company-mode))

;; Sly (SLIME) setup + REPL
(use-package sly
  :ensure t
  :custom
  (inferior-lisp-program "ros run -- --dynamic-space-size 2048"))

;; Edit parentheses in a structured way
(use-package paredit
  :ensure t
  :hook
  (emacs-lisp-mode . paredit-mode)
  (lisp-mode . paredit-mode))

;; Colour paired parentheses
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Add Evil Support ===========================================================

;; Enable Evil mode (Vim keybindings)
(use-package evil
  :ensure t
  :custom
  (evil-undo-system 'undo-redo)
  (evil-want-keybinding nil)
  :config
  (evil-mode 1))

;; Add Evil bindings to more modes
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;;; General Editor Niceties ====================================================

;; Command and minibuffer completion
(use-package ivy
  :ensure t
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  :config
  (ivy-mode))

;; Spell checking with Hunspell
(use-package ispell
  :ensure t
  :custom
  (ispell-program-name (executable-find "hunspell"))
  (ispell-really-hunspell t)
  (ispell-dictionary "en_GB")
  :hook
  ((text-mode . flyspell-mode)
   (prog-mode . flyspell-prog-mode)))

;;; Emacs Setting Tweaks =======================================================

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Overwrite highlighted text when typing
(delete-selection-mode t)

;; Keep buffers synced with files
(global-auto-revert-mode)

;; Copy and paste from the system clipboard
(setq select-enable-clipboard t)

;; Allow spaces in the minibuffer
(define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)

;; Make scrolling a bit smoother
(setq scroll-conservatively 10000)
(setq scroll-margin 5)

;; Show column number
(setq column-number-mode t)

;; Set up relative line numbers
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Highlight the current line
(add-hook 'prog-mode-hook 'hl-line-mode)

;; Inhibit the Emacs startup screen
(setq inhibit-startup-screen t)
