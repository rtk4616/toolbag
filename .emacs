;;;; Mike Wilkerson's Emacs configuration file.
;;
;; Sections:
;; - Setting variables by platform
;; - Mode-specific stuff
;; - My helper functions
;; - Keybindings
;; - Filetype associations
;; - Emacs options
;; - Defadvice goes here
;; - Hippie-expand setup
;; - Hooks


;;;; ---------------------------------------------------------------------------
;;;; Setting variables by platform
;;;; ---------------------------------------------------------------------------

(cond
 ((string-equal system-type "windows-nt")
  (progn
    ;; Set the necessary variables for Windows
    (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
    ))
 ((string-equal system-type "darwin")
  (progn
    ;; Set the necessary variables for OSX
    (add-to-list 'exec-path "/usr/local/bin/")
    (package-initialize)
    (set-default-font "Menlo-12")
    (set-variable 'ycmd-server-command '("python"))
    (add-to-list 'ycmd-server-command (expand-file-name "~/.ycmd_rundir/ycmd") t)
    (exec-path-from-shell-initialize)
    ))
 ((string-equal system-type "gnu/linux")
  (progn
    ;; Set the necessary variables for Linux
    (package-initialize)
    (set-variable 'ycmd-server-command '("python"))
    (add-to-list 'ycmd-server-command (expand-file-name "~/.ycmd_rundir/ycmd") t)
    ))
 )


;;;; ---------------------------------------------------------------------------
;;;; Mode-specific stuff
;;;; ---------------------------------------------------------------------------

;; Yasnippet stuff
(yas-global-mode 1)

;; desktop-save-mode stuff
(setq desktop-path '("~/"))
(setq desktop-dirname "~/")
(setq desktop-base-file-name ".emacs-desktop")
(setq desktop-load-locked-desktop nil)
(setq desktop-save t)
(setq desktop-restore-frames t)
(setq desktop-restore-in-current-display t)
(setq desktop-restore-forces-onscreen nil)

;; Magit
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-push-always-verify nil)

;; Helm stuff
(helm-mode 1)
(setq helm-ag-base-command "ag --nocolor --nogroup --hidden -U --smart-case")
(setq helm-buffers-fuzzy-matching t)
(setq helm-ff-file-name-history-use-recentf t)
(setq helm-ff-search-library-in-sexp t)
(setq helm-move-to-line-cycle-in-source t)
(setq helm-quick-update t)
(setq helm-scroll-amount 8)
(setq helm-split-window-in-side-p t)
(add-to-list 'helm-completing-read-handlers-alist '(dired-do-copy . nil))
(add-to-list 'helm-completing-read-handlers-alist '(dired-do-rename . nil))
(add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
(add-to-list 'helm-completing-read-handlers-alist '(ido-find-file . nil))

;; ycmd-mode
(require 'ycmd)
(add-hook 'after-init-hook #'global-ycmd-mode)

;; textile-mode
(require 'textile-mode)

;; projectile-mode
(require 'helm-projectile)
(setq projectile-switch-project-action 'MikeUpdateDirectory)
(projectile-global-mode)


;; company-mode
(setq company-backends '(company-ycmd
                         ;; company-sample-backend
                         ;; company-bbdb
                         ;; company-nxml
                         ;; company-css
                         ;; company-eclim
                         ;; company-semantic
                         ;; company-clang
                         ;; company-xcode
                         ;; company-ropemacs
                         ;; company-cmake
                         ;; ;; company-capf
                         ;; (company-dabbrev-code
                         ;;  company-gtags
                         ;;  company-etags
                         ;;  company-keywords)
                         ;; company-oddmuse
                         ;; company-files
                         ;; company-dabbrev
                         ))

;; Flycheck mode
(setq flycheck-check-syntax-automatically '(save
                                            ;; idle-change
                                            ;; new-line
                                            ;; mode-enabled
                                            ))

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

;; multi-web-mode setup
(eval-after-load "multi-web-mode" '(require 'html-mode-expansions))
(eval-after-load "multi-web-global-mode" '(require 'html-mode-expansions))
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags
      '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
        (js2-mode  "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
        (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5" "hbs"))
(multi-web-global-mode 1)


;;;; ---------------------------------------------------------------------------
;;;; My helper functions
;;;; ---------------------------------------------------------------------------

(load "~/.emacs.d/mike-stuff/mike-functions.el")


;;;; ---------------------------------------------------------------------------
;;;; Keybindings
;;;; ---------------------------------------------------------------------------

(global-set-key "\C-\M-h" 'er/contract-region)
(global-set-key "\C-\\" 'desktop-save-in-desktop-dir)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\C-x\C-\\" 'kill-emacs)
(global-set-key "\C-x\C-e" 'flycheck-list-errors)
(global-set-key "\C-x\C-k" 'kill-buffer)
(global-set-key "\C-x\C-l" 'helm-occur)
;; (global-set-key "\C-x\C-n" 'next-buffer)
(global-set-key "\C-x\C-n" (lambda() (interactive) (ffap (ffap-file-at-point))))
(global-set-key "\C-x\C-o" 'find-tag)
(global-set-key "\C-x\C-p" 'previous-buffer)
(global-set-key "\C-x\C-r" 'helm-resume)
(global-set-key "\C-x\C-t" 'MikeTrampFindFile)
(global-set-key "\C-xf" (lambda() (interactive) (helm-do-ag emacs-startup-directory)))
(global-set-key "\C-xl" 'helm-occur)
(global-set-key "\C-xn" 'flycheck-next-error)
(global-set-key "\C-xp" 'flycheck-previous-error)
(global-set-key "\C-xt" 'MikeTrampFindFile)
(global-set-key "\C-xvb" 'magit-blame)
(global-set-key "\C-xvh" 'magit-log-buffer-file)
(global-set-key "\C-xvl" 'magit-log-current)
(global-set-key "\C-xvs" 'magit-status)
(global-set-key "\C-y" 'clipboard-yank)
(global-set-key "\M-(" 'MikeDeIndent)
(global-set-key "\M-)" 'MikeIndent)
(global-set-key "\M-U" 'upcase-word)
(global-set-key "\M-\\" 'mike-desktop-read)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-h" 'er/expand-region)
(global-set-key "\M-j" 'MikeGetIndentation)

(global-set-key (kbd "C-t") 'toggle-control-lock)
(global-set-key (kbd "M-t") 'toggle-meta-lock)

(global-set-key "\M-k" 'mark-paragraph)
(global-set-key "\M-l" 'recenter-top-bottom)
(global-set-key "\M-n" 'MikeDownSomeLines)
(global-set-key "\M-o" 'other-window)
(global-set-key "\M-p" 'MikeUpSomeLines)
(global-set-key "\M-u" 'downcase-word)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
;; (global-set-key "\C-\\" 'window-configuration-to-register)
;; (global-set-key "\M-\\" 'jump-to-register)

(global-set-key (kbd "<C-backspace>") 'delete-backward-char)
(global-set-key (kbd "<C-return>") (kbd "C-m"))
(global-set-key (kbd "<f5>") 'linum-mode)
(global-set-key (kbd "<f6>") 'autopair-mode)
(global-set-key (kbd "C-M-d") 'duplicate-current-line-or-region)
(global-set-key (kbd "C-M-f") 'helm-projectile-find-file)
(global-set-key (kbd "C-M-k") 'kill-whole-line)
(global-set-key (kbd "C-M-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-p") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-M-s") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-M-t") 'helm-projectile-switch-project)
(global-set-key (kbd "C-x C-b") 'electric-buffer-list)
(global-set-key (kbd "C-x C-h") 'helm-command-prefix)
(global-set-key (kbd "M-*") 'mike-next-tag)
(global-set-key (kbd "M-,") 'pop-tag-mark)
(global-set-key (kbd "M-.") 'helm-etags-select)
(global-set-key (kbd "M-RET") 'sgml-close-tag)
(global-set-key (kbd "M-SPC") 'hippie-expand)
(global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-M-n") 'MikeScrollDownOneLine)
;; (global-set-key (kbd "C-M-p") 'MikeScrollUpOneLine)
;; (global-set-key (kbd "M-SPC") 'company-complete)

(global-unset-key (kbd "C-M-l"))
(global-unset-key (kbd "C-c C-c"))
(global-unset-key (kbd "C-x c"))


;;;; ---------------------------------------------------------------------------
;;;; Filetype associations
;;;; ---------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '(".*crontab\\'" . crontab-mode))
(add-to-list 'auto-mode-alist '("\\.git.*config\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))


;;;; ---------------------------------------------------------------------------
;;;; Emacs options
;;;; ---------------------------------------------------------------------------

(load-theme 'noctilux t)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(autopair-global-mode)
(delete-selection-mode 1)
(global-linum-mode t)
(ido-mode t)
(set-fringe-mode 0)
(setenv "TMPDIR" "/tmp")

(fset 'yes-or-no-p 'y-or-n-p)
(setq auto-save-default nil)
(setq c-default-style "bsd" c-basic-offset 4)
(setq case-fold-search t)
(setq case-replace nil)
(setq company-dabbrev-code-everywhere t)
(setq company-dabbrev-code-ignore-case t)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case t)
(setq company-etags-ignore-case t)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(setq create-lockfiles nil)
(setq css-indent-offset 4)
(setq dabbrev-case-fold-search nil)
(setq ediff-split-window-function 'split-window-horizontally)
(setq emacs-startup-directory default-directory)
(setq frame-title-format "%b")
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
(setq tramp-default-method "ssh")
(setq tramp-shell-prompt-pattern "\\(?:^\\|\\)[^]#$%>\n]*#?[]#$%>].* *\\(\\[[0-9;]*[a-zA-Z] *\\)*")
(setq vc-handled-backends nil)
(setq-default column-number-mode 1)
(setq-default fill-column 79)
(setq-default indent-tabs-mode nil)
(setq-default standard-indent 4)
(setq-default tab-width 4)
(setq-default word-wrap t)


;;;; ---------------------------------------------------------------------------
;;;; Defadvice goes here
;;;; ---------------------------------------------------------------------------

;; Add a little padding around the line numbers. Dynamically determine
;; character width for the line numbers column, and add a space for padding as
;; well.
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string w) "d  ")))
    ad-do-it))


;;;; ---------------------------------------------------------------------------
;;;; Hippie-expand setup
;;;; ---------------------------------------------------------------------------

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         ;; try-expand-all-abbrevs
                                         ;; try-expand-list
                                         ;; try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))


;;;; ---------------------------------------------------------------------------
;;;; Hooks
;;;; ---------------------------------------------------------------------------

;; Emacs init hooks
(add-hook 'after-init-hook
          (lambda ()
            (global-flycheck-mode)
            (global-company-mode)
            (let (
                  (the-tags-file (concat emacs-startup-directory "TAGS"))
                  )
              (if (file-exists-p the-tags-file)
                  (visit-tags-table the-tags-file)
                )
              )
            ))
(add-hook 'window-setup-hook
          (lambda ()
            (if (display-graphic-p (selected-frame))
                (set-face-background 'default "#282C34" (selected-frame))
              (set-face-background 'default "unspecified-bg" (selected-frame))
              )))

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Overriding dired-mode binding
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "M-o") 'other-window)
            ))

;; Overriding company-mode binding
(add-hook 'company-mode-hook
          (lambda ()
            (define-key company-active-map (kbd "M-n") nil)
            (define-key company-active-map (kbd "M-p") nil)
            (define-key company-active-map (kbd "C-n") #'company-select-next)
            (define-key company-active-map (kbd "C-p") #'company-select-previous)
            (define-key company-active-map (kbd "<return>") nil)
            (define-key company-active-map (kbd "RET") nil)
            (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
            (define-key company-active-map (kbd "TAB") #'company-complete-selection)
            ))

;; ;; Find file hooks
;; (add-hook 'find-file-hook
;;           (lambda ()
;;             (setq default-directory emacs-startup-directory)))

;; go-mode hooks
(add-hook 'go-mode-hook (lambda ()
                          (global-set-key (kbd "M-.") 'godef-jump)
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)
                          (add-hook 'before-save-hook 'gofmt-before-save)))

;; js-mode hooks
(add-hook 'js-mode-hook (lambda ()
                          (local-set-key (kbd "M-,") 'pop-tag-mark)
                          (local-set-key (kbd "M-.") 'helm-etags-select)
                          ))

;; yaml-mode hooks
(add-hook 'yaml-mode-hook
          (lambda ()
            (modify-syntax-entry ?' "w")
            (modify-syntax-entry ?- ".")
            ))

;; html-mode hooks
(add-hook 'html-mode-hook
          (lambda ()
            (modify-syntax-entry ?\" "\"\"")
            ))

;; multi-web-mode hooks
(add-hook 'multi-web-mode-hook
          (lambda ()
            (modify-syntax-entry ?\" "\"\"")
            ))

;; multiple-cursors-mode hooks
(add-hook 'multiple-cursors-mode-hook
          (lambda ()
            (define-key mc/keymap (kbd "<return>") (kbd "C-m"))
            ))

;; makefile-mode hooks
(add-hook 'makefile-mode-hook
          (lambda ()
            (define-key makefile-mode-map (kbd "M-n") 'MikeDownSomeLines)
            (define-key makefile-mode-map (kbd "M-p") 'MikeUpSomeLines)
            ))

;; markdown-mode hooks
(add-hook 'markdown-mode-hook
          (lambda ()
            (define-key markdown-mode-map (kbd "M-n") 'MikeDownSomeLines)
            (define-key markdown-mode-map (kbd "M-p") 'MikeUpSomeLines)
            (modify-syntax-entry ?\` "\"`")
            (modify-syntax-entry ?\" "\"\"")
            (setq autopair-handle-action-fns
                  (list 'autopair-default-handle-action
                        'autopair-python-triple-quote-action))
            ))

;; Add highlighting of TODOs
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(FIXME:\\|TODO:\\|BUG:\\|NOTE:\\)" 1 font-lock-warning-face t)))))

;; python-mode hooks
(add-hook 'python-mode-hook
          (lambda ()
            (setq autopair-handle-action-fns
                  (list 'autopair-default-handle-action
                        'autopair-python-triple-quote-action))
            ))

;; ycmd-mode hooks
(add-hook 'ycmd-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.") 'ycmd-goto)
            ))
