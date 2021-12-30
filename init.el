;; MELPA Setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Enable use-package
(require 'use-package)

;; Set Emacs Theme
(load-theme 'gruvbox-dark-soft t)

;; Get rid of the big GUI toolbar
(tool-bar-mode -1)

;; Set Default Font
(set-face-attribute 'default nil :font "IBM Plex Mono Text")
(add-to-list 'default-frame-alist '(font . "IBM Plex Mono Text"))

;; Sly (SLIME) Setup + Company REPL
(setq inferior-lisp-program "sbcl --dynamic-space-size 2048")
(add-hook 'sly-mode-hook 'company-mode)

;; Sly C-return evals and adds result in a comment
(defun save-lisp-result ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively 'sly-eval-last-expression)))
(eval-after-load 'sly
  '(define-key sly-mode-map (kbd "<C-return>") 'save-lisp-result))

;; Set up automatic pairing of quotes and parentheses
(electric-pair-mode)

;; Tab Characters Are Evil
(setq-default indent-tabs-mode nil)

;; Enable and Setup Recent Files
(recentf-mode 1)
(add-to-list 'recentf-exclude "/recentf")
(run-at-time nil (* 5 60) 'recentf-save-list)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Spell Checking with Hunspell
(require 'ispell)
(setq ispell-program-name (executable-find "hunspell"))
(setq ispell-really-hunspell t)
(setq ispell-dictionary "en_GB")
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Changing Spell Check Language
;; Clean this up a little at some point...
;; Make it work like buffer switching. Take input but default to last.
(defun switch-dictionary-de-en ()
  "Switch german and english dictionaries."
  (interactive)
  (let* ((dict ispell-current-dictionary)
         (new (if (string= dict "de_DE") "en_GB" "de_DE")))
    (ispell-change-dictionary new)
    (message "Switched dictionary from %s to %s" dict new)))
(global-set-key (kbd "C-c d") 'switch-dictionary-de-en)

;; Set 80 Column Wrapping + Show Column
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq column-number-mode t)

;; Copy and Paste from GUI Support
(setq select-enable-clipboard t)

;; Enable some more niche commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Some Org-Mode Bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cr" 'org-latex-preview)

;; Org-Mode Setting Tweaks
(require 'org)
(setq org-log-done 'time)
(setq org-pretty-entities t)
(setq org-image-actual-width nil)
(setq org-cycle-separator-lines 1)
(setq org-startup-folded t)
(setq org-startup-with-inline-images t)
(setq org-startup-with-latex-preview t)
(setq org-preview-latex-image-directory ".ltximg/")
(setcar (nthcdr 4 org-emphasis-regexp-components) 3)
(setq org-agenda-files '("~/Documents/Notebook/Trackers/Development.org"))
;(setq org-latex-create-formula-image-program 'imagemagick)
(setq image-scaling (if (> (display-pixel-width) 2560) 3 1.5))
(setq org-format-latex-options (plist-put org-format-latex-options :scale image-scaling))
(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
(add-to-list 'org-modules 'habits)

;; Org-Download Setup
(require 'org-download)
(when (executable-find "spectacle")
  (setq org-download-screenshot-method "spectacle -rbno %s"))
(setq-default org-download-image-dir ".orgimg/")
(setq org-download-annotate-function (lambda (_) ""))
(global-set-key "\C-cs" 'org-download-screenshot)

;; Org-Roam Setup
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Documents/Zettelkasten/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
          ("C-c n f" . org-roam-node-find)
          ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup)
  :init
  (setq org-roam-v2-ack t))

;; Ivy Autocompletion Setup
(use-package ivy
  :ensure t
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))

;; Dependency For Evil (Until Emacs 28)
(use-package undo-fu
  :ensure t)

;; Evil Mode (Vim Keybindings)
(use-package evil
  :ensure t
  :custom
  (evil-undo-system 'undo-fu)
  :config
  (evil-mode 0))

;; My Zettel Template
(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
        :if-new (file+head "%<%Y-%m-%d>-${slug}.org"
                           "#+TITLE: ${title}\n#+DATE: %<%F (%R)>\n")
        :unnarrowed t
        :immediate-finish t)))

;; Prefer opening PDFs in an external program
(add-to-list 'org-file-apps '("pdf" . "evince %s"))

;; Fix xdg-open being killed (THIS BREAKS GNUPLOT)
(setq process-connection-type nil)

;; Allow spaces in the minibuffer
(define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)

;; Some AUCTeX Setting Tweaks
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; Overwrite Highlighted Text When Typing
(delete-selection-mode 1)

;; Keep buffers synced with files
(global-auto-revert-mode)

;; Make scrolling a bit smoother
(setq scroll-conservatively 101) ; Never recentre the cursor
(setq auto-window-vscroll nil) ; Don't jump on images
(setq mouse-wheel-progressive-speed nil) ; Mouse wheel has constant sensitivity

;; Wrap on whole words
(global-visual-line-mode 1)

;; Insert Custom Timestamps
(defun insert-datetime (f)
  "Inserts the current time in the specified format"
  (interactive "cInsert: [H]uman Readable, [S]hort, [D]ate Only, [T]ime Only")
  (insert (format-time-string (pcase f
    (?h "%A %B %-e, %Y (%R)")
    (?s "%F (%R)")
    (?d "%F")
    (?t "(%R)")))))
(global-set-key "\C-ct" 'insert-datetime)

;; Drag a duplicate of a line down
(defun duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank))

;; Bind duplicate to Super-
(global-set-key (kbd "<s-return>") 'duplicate-line)

;; Bind multi-term
;; FIXME: Make this launch asyncronously?
(global-set-key "\C-xt" 'multi-term)

;; Set up line numbers and line highlighting
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)

;; Open a couple of files by default and set default frame size
(setq inhibit-startup-screen t)
(setq left-file "~/Documents/Notebook/Index.org")
(setq right-file "~/Documents/University/Classes.org")
(defun init-frame (frame)
  (with-selected-frame frame
    (set-frame-size frame 164 48)
    (find-file left-file)
    (split-window-horizontally)
    (next-window)
    (find-file right-file)
    (setq initial-buffer-choice right-file)))

;; Run init-frame when a new frame is created
(add-hook 'after-make-frame-functions 'init-frame)

;; Don't init-frame if running with a file argument
(unless (> (length command-line-args) 1)
  (init-frame (selected-frame)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(use-package sly org-roam org-download multi-term ivy gruvbox-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
