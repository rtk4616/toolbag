;;;; Mike Wilkerson's Emacs configuration file.
;;
;; Sections:
;; - Setting variables by platform
;; - Mode-specific stuff
;; - My helper functions
;; - Keybindings
;; - Emacs options
;; - Hooks


;;;; ---------------------------------------------------------------------------
;;;; Setting variables by platform
;;;; ---------------------------------------------------------------------------

(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
    ;; Set the necessary variables for Windows
    (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
    ))
 ((string-equal system-type "darwin")   ; Mac OS X
  (progn
    ;; Set the necessary variables for OSX
    (add-to-list 'exec-path "/usr/local/bin/")
    ))
 ;; ((string-equal system-type "gnu/linux") ; linux
 ;;  (progn
 ;;    ;; Set the necessary variables for Linux
 ;;    ;; TODO: what are they?
 ;;    ))
 )


;;;; ---------------------------------------------------------------------------
;;;; Mode-specific stuff
;;;; ---------------------------------------------------------------------------

;; MELPA
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; nginx-mode stuff
(add-to-list 'auto-mode-alist '("/etc/nginx/.*\.conf" . nginx-mode))


;;;; ---------------------------------------------------------------------------
;;;; My helper functions
;;;; ---------------------------------------------------------------------------

(load "~/.emacs.d/mike-stuff/mike-functions.el")


;;;; ---------------------------------------------------------------------------
;;;; Keybindings
;;;; ---------------------------------------------------------------------------

(global-set-key "\C-\M-h" 'er/contract-region)
(global-set-key "\C-\\" (lambda () (interactive) (window-configuration-to-register 'a)))
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\C-x\C-\\" 'kill-emacs)
(global-set-key "\C-x\C-g" 'MikeGrepForFiles)
(global-set-key "\C-x\C-r" 'helm-resume)
(global-set-key "\C-x\C-k" 'kill-buffer)
(global-set-key "\C-x\C-l" 'helm-occur)
(global-set-key "\C-x\C-n" 'flycheck-next-error)
(global-set-key "\C-x\C-o" 'flycheck-list-errors)
(global-set-key "\C-x\C-p" 'flycheck-previous-error)
(global-set-key "\C-xf" 'helm-do-ag)
(global-set-key "\C-xg" 'MikeGrepForFiles)
(global-set-key "\C-xl" 'helm-occur)
(global-set-key "\C-xn" 'flycheck-next-error)
(global-set-key "\C-xp" 'flycheck-previous-error)
(global-set-key "\C-y" 'clipboard-yank)
(global-set-key "\M-(" 'MikeDeIndent)
(global-set-key "\M-)" 'MikeIndent)
(global-set-key "\M-U" 'upcase-word)
(global-set-key "\M-\\" (lambda () (interactive) (jump-to-register 'a)))
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-h" 'er/expand-region)
(global-set-key "\M-j" 'MikeGetIndentation)
(global-set-key "\M-k" 'mark-paragraph)
(global-set-key "\M-l" 'recenter-top-bottom)
(global-set-key "\M-n" 'MikeDownSomeLines)
(global-set-key "\M-o" 'other-window)
(global-set-key "\M-p" 'MikeUpSomeLines)
(global-set-key "\M-u" 'downcase-word)
(global-set-key "\M-w" 'clipboard-kill-ring-save)

(global-set-key (kbd "<C-backspace>") 'delete-backward-char)
(global-set-key (kbd "<C-return>") 'newline)
(global-set-key (kbd "<f6>") 'autopair-mode)
(global-set-key (kbd "C-M-n") 'MikeScrollDownOneLine)
(global-set-key (kbd "C-M-p") 'MikeScrollUpOneLine)
(global-set-key (kbd "C-x C-b") 'electric-buffer-list)
(global-set-key (kbd "C-x C-h") 'helm-command-prefix)
(global-set-key (kbd "M-*") 'mike-next-tag)
(global-set-key (kbd "M-,") 'pop-tag-mark)
(global-set-key (kbd "M-SPC") 'hippie-expand)
(global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "M-SPC") 'company-complete)

(global-unset-key (kbd "C-c C-c"))
(global-unset-key (kbd "C-x c"))


;;;; ---------------------------------------------------------------------------
;;;; Emacs options
;;;; ---------------------------------------------------------------------------

(load-theme 'monokai t)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(autopair-global-mode)
(delete-selection-mode 1)
(helm-mode 1)
(ido-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(set-default-font "Menlo-12")
(setq auto-save-default nil)
(setq c-default-style "bsd" c-basic-offset 4)
(setq case-fold-search t)
(setq css-indent-offset 4)
(setq dabbrev-case-fold-search nil)
(setq ediff-split-window-function 'split-window-horizontally)
(setq emacs-startup-directory default-directory)
(setq frame-title-format "%b")
(setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
(setq helm-buffers-fuzzy-matching t)
(setq helm-ff-file-name-history-use-recentf t)
(setq helm-ff-search-library-in-sexp t)
(setq helm-move-to-line-cycle-in-source t)
(setq helm-quick-update t)
(setq helm-scroll-amount 8)
(setq helm-split-window-in-side-p t)
(setq ido-enable-last-directory-history nil)
(setq ido-everywhere t)
(setq ido-max-work-directory-list 0)
(setq ido-max-work-file-list 0)
(setq ido-record-commands nil)
(setq ido-use-faces nil)
(setq indent-line-function 'insert-tab)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq ispell-list-command "list")
(setq ispell-program-name "aspell")
(setq make-backup-files nil)
(setq python-indent 4)
(setq ring-bell-function 'ignore)
(setq ruby-indent-level 2)
(setq scroll-conservatively 10000)
(setq scroll-step 1)
(setq vc-handled-backends nil)
(setq-default column-number-mode 1)
(setq-default fill-column 79)
(setq-default indent-tabs-mode nil)
(setq-default standard-indent 4)
(setq-default tab-width 4)
(setq-default word-wrap t)


;;;; ---------------------------------------------------------------------------
;;;; Hooks
;;;; ---------------------------------------------------------------------------

;; Emacs init hooks
(add-hook 'after-init-hook (lambda ()
                             (global-flycheck-mode)))

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Overriding dired-mode binding
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "M-o") 'other-window)
            ))

;; Find file hooks
(add-hook 'find-file-hook
          (lambda ()
            (setq default-directory emacs-startup-directory)))

;; go-mode hooks
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (setq company-idle-delay 0)
                          (company-mode)
                          (add-hook 'before-save-hook 'gofmt-before-save)))

;; markdown-mode hooks
(add-hook 'markdown-mode-hook
          (lambda ()
            (define-key markdown-mode-map (kbd "M-n") 'MikeDownSomeLines)
            (define-key markdown-mode-map (kbd "M-p") 'MikeUpSomeLines)
            )
          )

;; Add highlighting of TODOs
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(FIXME:\\|TODO:\\|BUG:\\|NOTE:\\)" 1 font-lock-warning-face t)))))

;; python-mode hooks
(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))
