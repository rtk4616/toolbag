;;;; MELPA
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;;;; My helper functions
(load "~/.emacs.d/mike-stuff/mike-functions.el")

;;;; Keybindings
(global-set-key "\C-\\" (lambda () (interactive) (window-configuration-to-register 'a)))
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\C-x\C-\\" 'kill-emacs)
(global-set-key "\C-x\C-k" 'kill-buffer)
(global-set-key "\C-y" 'clipboard-yank)
(global-set-key "\M-U" 'upcase-word)
(global-set-key "\M-\\" (lambda () (interactive) (jump-to-register 'a)))
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-k" 'mark-paragraph)
(global-set-key "\M-l" 'recenter-top-bottom)
(global-set-key "\M-n" 'MikeDownSomeLines)
(global-set-key "\M-o" 'other-window)
(global-set-key "\M-p" 'MikeUpSomeLines)
(global-set-key "\M-u" 'downcase-word)
(global-set-key "\M-w" 'clipboard-kill-ring-save)

(global-set-key (kbd "<C-return>") 'newline)
(global-set-key (kbd "C-M-n") 'MikeScrollDownOneLine)
(global-set-key (kbd "C-M-p") 'MikeScrollUpOneLine)
(global-set-key (kbd "M-SPC") 'hippie-expand)
;; (global-set-key (kbd "M-SPC") 'company-complete)


;;;; Emacs options
(fset 'yes-or-no-p 'y-or-n-p)
(global-font-lock-mode 1)
(load-theme 'monokai t)
(set-default-font "Menlo-12")

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(setq auto-save-default nil)
(setq c-default-style "bsd" c-basic-offset 4)
(setq case-fold-search t)
(setq css-indent-offset 4)
(setq dabbrev-case-fold-search nil)
(setq ediff-split-window-function 'split-window-horizontally)
(setq frame-title-format "%b")
(setq indent-line-function 'insert-tab)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq make-backup-files nil)
(setq python-indent 4)
(setq ring-bell-function 'ignore)
(setq ruby-indent-level 2)
(setq scroll-conservatively 10000)
(setq scroll-step 1)

(setq-default column-number-mode 1)
(setq-default fill-column 79)
(setq-default indent-tabs-mode nil)
(setq-default standard-indent 4)
(setq-default tab-width 4)


;;;; Hooks

;; go-mode hooks
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (setq company-idle-delay 0)
                          (company-mode)
                          (add-hook 'before-save-hook 'gofmt-before-save)))

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Add highlighting of TODOs
(add-hook 'prog-mode-hook
	  (lambda ()
	    (font-lock-add-keywords nil
				    '(("\\<\\(FIXME:\\|TODO:\\|BUG:\\|NOTE:\\)" 1 font-lock-warning-face t)))))
