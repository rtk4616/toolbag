;; Initialize the package system.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Add custom code to the load path. `ext' contains Lisp code that I didn't
;; write but that is not in melpa, while `lisp' is for Lisp code I wrote.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Make use-package available.
(eval-when-compile
  (require 'use-package))

(use-package diminish
  :ensure t
  :pin melpa)

(require 'diminish)
(require 'bind-key)

;; desktop-save-mode stuff
(use-package desktop
  :init
  (setq desktop-path '("~/"))
  (setq desktop-dirname "~/")
  (setq desktop-base-file-name ".emacs-desktop")
  (setq desktop-load-locked-desktop nil)
  (setq desktop-save t)
  (setq desktop-restore-reuses-frames t)
  (setq desktop-restore-frames t)
  (setq desktop-restore-in-current-display t)
  (setq desktop-restore-forces-onscreen nil)
  (desktop-save-mode t)
  ;; (add-hook 'emacs-startup-hook 'toggle-frame-maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(use-package conf-mode
  :ensure t
  :mode (("\\.ini\\'" . conf-mode)
         ("\\.conf\\'" . conf-mode)
         ("\\.conf\\.erb\\'" . conf-mode)
         ("\\.properties\\.erb\\'" . conf-javaprop-mode)))

(use-package textile-mode
  :ensure t)

(use-package powershell
  :ensure t
  :pin melpa)

(use-package diminish
  :ensure t)

(use-package lady
  :demand t)

(use-package flycheck
  :ensure t
  :demand t
  :bind (("C-x C-e" . flycheck-list-errors)
         ("C-x n" . flycheck-next-error)
         ("C-x p" . flycheck-previous-error))
  :init
  (setq flycheck-check-syntax-automatically '(save))
  (setq flycheck-disabled-checkers '(chef-foodcritic))
  :config
  (global-flycheck-mode))

(use-package dired-x
  :demand t)

(use-package neotree
  :ensure t
  :init
  (setq neo-show-hidden-files t)
  (setq neo-smart-open t)
  (setq neo-theme 'arrow)
  :config
  (define-key neotree-mode-map (kbd "o") 'neotree-enter)
  (define-key neotree-mode-map (kbd "C-o") 'neotree-quick-look)
  (define-key neotree-mode-map (kbd "b") 'neotree-select-up-node)
  (define-key neotree-mode-map (kbd "f") 'neotree-select-down-node)
  (bind-key* "C-\\ C-\\" 'neotree-toggle))

(use-package mike-functions
  :after helm-projectile
  :demand t
  :init
  (add-hook 'lisp-mode-hook (lambda () (setq tab-width 2)))
  (add-hook 'lisp-interaction-mode-hook (lambda () (setq tab-width 2)))
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq tab-width 2)))
  (add-hook 'web-mode-hook (lambda ()
                                    (local-set-key (kbd "M-<return>") 'sgml-close-tag)))
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-follow-mouse 't)
  (setq-default indent-tabs-mode nil
                column-number-mode 1
                fill-column 79
                standard-indent 4
                tab-width 4
                word-wrap t)
  (setq inhibit-startup-message t
        initial-scratch-message nil
        auto-save-default nil
        case-fold-search t
        case-replace nil
        create-lockfiles nil
        dabbrev-case-fold-search nil
        ediff-split-window-function 'split-window-horizontally
        emacs-startup-directory default-directory
        frame-title-format "%b"
        indent-line-function 'insert-tab
        inhibit-startup-message t
        initial-scratch-message nil
        ispell-list-command "list"
        ispell-program-name "aspell"
        make-backup-files nil
        markdown-gfm-use-electric-backquote nil
        ns-pop-up-frames nil
        python-indent 4
        ring-bell-function 'ignore
        ruby-indent-level 2
        scroll-conservatively 10000
        scroll-step 1
        shift-select-mode nil
        split-height-threshold nil
        tramp-default-method "ssh"
        tramp-shell-prompt-pattern "\\(?:^\\|\\)[^]#$%>\n]*#?[]#$%>].* *\\(\\[[0-9;]*[a-zA-Z] *\\)*"
        vc-handled-backends nil)
  (set-default-font "Roboto Mono-12")
  (blink-cursor-mode 0)
  (fset 'yes-or-no-p 'y-or-n-p)
  (delete-selection-mode 1)
  (global-linum-mode -1)
  (setenv "TMPDIR" "/tmp")
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

  ;; Some global keybindings, because use-package doesn't let you bind lambdas with :bind
  (bind-key* "<C-return>" (kbd "C-m"))
  (bind-key* "C-M-d" 'duplicate-current-line-or-region)
  (bind-key* "C-M-x C-M-n" 'mike/mark-all-in-region)
  (bind-key* "C-M-x C-M-w" 'mike/copy-full-file-name)
  (bind-key* "C-M-x w" 'mike/copy-only-file-name)
  (bind-key* "C-\\ M-\\" 'desktop-save-in-desktop-dir)
  ;; (bind-key* "C-x C-o" 'find-tag)
  (bind-key* "M-SPC" 'company-complete)
  (bind-key* "M-\\" 'mike-desktop-read)
  (bind-key* "M-k" 'mark-paragraph)
  (bind-key* "M-{" 'backward-paragraph)
  (bind-key* "M-}" 'forward-paragraph)

  (global-set-key (kbd "C-x C-.") (lambda() (interactive) (ffap (ffap-file-at-point))))
  (global-set-key (kbd "C-x v f") (lambda() (interactive) (magit-fetch-all "-p") (magit-status)))
  (global-set-key (kbd "C-x v f") (lambda() (interactive) (magit-fetch-all "-p") (magit-status)))
  (global-set-key (kbd "M-_") (lambda() (interactive) (insert "—")))
  :config
  (define-key isearch-mode-map "\r" 'mike/isearch-set-region)
  (define-key isearch-mode-map (kbd "<return>") 'mike/isearch-set-region)
  (add-hook 'prog-mode-hook
            (lambda () (font-lock-add-keywords nil '(("\\<\\(FIXME:\\|TODO:\\|BUG:\\|NOTE:\\)" 1 font-lock-warning-face t)))))
  :bind (("<M-down>" . enlarge-window)
         ("<M-left>" . shrink-window-horizontally)
         ("<M-right>" . enlarge-window-horizontally)
         ("<M-up>" . shrink-window)
         ("C-M-SPC" . mike/expand-to-matching-pair)
         ("C-M-k" . kill-whole-line)
         ("C-c C-b" . compile)
         ("C-r" . isearch-backward-regexp)
         ("C-s" . isearch-forward-regexp)
         ("C-w" . clipboard-kill-region)
         ("C-x C-\\" . kill-emacs)
         ("C-x C-k" . kill-buffer)
         ("C-x C-n" . next-buffer)
         ("C-x C-p" . previous-buffer)
         ("C-x M-b" . electric-buffer-list)
         ("C-x M-g" . goto-char)
         ("C-x vb" . magit-blame)
         ("C-x vh" . magit-log-buffer-file)
         ("C-x vl" . magit-log-current)
         ("C-x vs" . magit-status)
         ("C-y" . clipboard-yank)
         ("M-(" . MikeDeIndent)
         ("M-)" . MikeIndent)
         ("M-*" . mike-next-tag)
         ("M-," . pop-tag-mark)
         ("M-;" . toggle-comment-region-or-line)
         ("M-U" . upcase-word)
         ("M-g" . goto-line)
         ("M-j" . MikeGetIndentation)
         ("M-l" . recenter-top-bottom)
         ("M-n" . MikeDownSomeLines)
         ("M-o" . other-window)
         ("M-p" . MikeUpSomeLines)
         ("M-u" . downcase-word)
         ("M-w" . clipboard-kill-ring-save)))

(use-package dracula-theme
  :ensure t
  :defer t
  :init
  (load-theme 'dracula t))

(use-package lsp-mode
  :demand t
  :ensure t)

(use-package lsp-go
  :demand t
  :ensure t
  :init
  (add-hook 'go-mode-hook #'lsp-go-enable))

(use-package lsp-python
  :demand t
  :ensure t
  :init
  (add-hook 'python-mode-hook #'lsp-python-enable))

(use-package expand-region
  :ensure t
  :config
  (bind-key* "C-M-h" 'er/contract-region)
  (bind-key* "M-h" 'er/expand-region)
  :init
  (setq er/try-expand-list '(er/mark-comment
                             er/mark-defun
                             er/mark-email
                             er/mark-html-attribute
                             er/mark-inner-tag
                             er/mark-inside-pairs
                             er/mark-inside-quotes
                             er/mark-method-call
                             er/mark-next-accessor
                             er/mark-outer-tag
                             er/mark-outside-pairs
                             er/mark-outside-quotes
                             er/mark-symbol
                             er/mark-symbol-with-prefix
                             er/mark-url
                             er/mark-word)))

(use-package magit
  :ensure t
  :demand t
  :commands (magit-status magit-checkout)
  :bind (("C-x v s" . magit-status)
         ("C-x v b" . magit-blame)
         ("C-x v l" . magit-log-current)
         ("C-x v h" . magit-log-buffer-file))
  :init
  (setq magit-cherry-pick-arguments (quote ("-x"))
        magit-last-seen-setup-instructions "1.4.0"
        magit-push-always-verify nil
        magit-diff-refine-hunk t)
  :config
  ;; Don't show recent commits in magit status.
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpushed-to-upstream
                          'magit-insert-unpushed-to-upstream-or-recent
                          'replace))

(use-package autopair
  :config
  (autopair-global-mode)
  :ensure t)

(use-package gitignore-mode :ensure t)

;; Helm-related things.
(use-package helm
  :ensure t
  :demand t
  :after projectile
  :diminish helm-mode
  :bind (("C-c C-e" . helm-ag-edit)
         ;; ("C-x C-f" . helm-find-files)
         ("C-x C-h" . helm-command-prefix)
         ("C-x C-l" . helm-occur)
         ("C-x C-r" . helm-resume)
         ("C-x M-f" . helm-do-ag)
         ("C-x t" . lady/tramp-connect)
         ("C-x C-t" . lady/tramp-connect)
         ("C-x M-l" . helm-do-ag-this-file)
         ("C-x b" . helm-buffers-list)
         ("C-x l" . helm-occur)
         ("M-x" . helm-M-x))
  :config
  (helm-mode 1)
  (setq helm-M-x-fuzzy-match t
        helm-buffer-skip-remote-checking t
        helm-buffers-fuzzy-matching t
        helm-display-header-line nil
        helm-ff-file-name-history-use-recentf t
        helm-ff-search-library-in-sexp t
        helm-move-to-line-cycle-in-source t
        helm-quick-update t
        helm-scroll-amount 8
        helm-split-window-in-side-p t
        helm-window-prefer-horizontal-split t
        ;; NOTE: Due to an issue in Emacs 24 with Helm/Tramp and DNS resolution, Emacs
        ;; will sometimes take a very long time to start up. According to...
        ;;
        ;; https://github.com/emacs-helm/helm/issues/1000
        ;;
        ;; ...if we set the tramp-ssh-controlmaster-options variable to its correct
        ;; value *before* starting Helm, we can eliminate the long pause for failed DNS
        ;; resolution (which otherwise makes us wait for the request to time out).
        ;;
        ;; You can find the correct value for tramp-ssh-controlmaster-options by
        ;; letting emacs start up, and then doing:
        ;;
        ;; C-h v tramp-ssh-controlmaster-options RET
        ;;
        tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
  ;; Some global keybindings, because use-package doesn't let you bind lambdas with :bind
  ;; (define-key helm-find-files-map (kbd "<tab>") 'helm-execute-persistent-action)
  (global-set-key (kbd "C-x b") (lambda() (interactive) (helm-buffers-list)))
  (global-set-key (kbd "C-x C-b") (lambda() (interactive) (helm-buffers-list)))
  ;; No idea why here find-file is set to nil (so it uses the native find-file
  ;; for Emacs. This makes stuff like (find-file (read-file-name ...)) work with
  ;; Helm again.
  (setq helm-completing-read-handlers-alist '((debug-on-entry . helm-completing-read-symbols)
                                              (describe-function . helm-completing-read-symbols)
                                              (describe-symbol . helm-completing-read-symbols)
                                              (describe-variable . helm-completing-read-symbols)
                                              (disassemble . helm-completing-read-symbols)
                                              (execute-extended-command)
                                              (find-file . nil)
                                              (find-file-at-point . nil)
                                              (find-function . helm-completing-read-symbols)
                                              (find-tag . helm-completing-read-default-find-tag)
                                              (kill-buffer . nil)
                                              (org-capture . helm-org-completing-read-tags)
                                              (org-set-tags . helm-org-completing-read-tags)
                                              (tmm-menubar)
                                              (trace-function . helm-completing-read-symbols)
                                              (trace-function-background . helm-completing-read-symbols)
                                              (trace-function-foreground . helm-completing-read-symbols))))

(use-package helm-projectile
  :ensure t
  :demand t
  :after helm
  :config
  ;; Some global keybindings, because use-package doesn't let you bind lambdas with :bind
  :bind (("C-M-f" . helm-projectile-find-file)
         ("C-M-t" . helm-projectile-switch-project)))

(use-package helm-ag
  :ensure t
  :bind (("C-x M-f" . helm-do-ag)
         ("C-c C-e" . helm-ag-edit)
         ("C-x M-l" . helm-do-ag-this-file))
  :init
  (setq helm-ag-base-command "ag --nocolor --nogroup --hidden -U --smart-case"))

;; (use-package ido
;;   :ensure t
;;   :config
;;   (setq ido-enable-last-directory-history nil
;;         ido-everywhere t
;;         ido-max-work-directory-list 0
;;         ido-max-work-file-list 0
;;         ido-record-commands nil
;;         ido-use-faces nil
;;         ido-ignore-buffers '("^ "
;;                              "*Completions*"
;;                              "*Shell Command Output*"
;;                              "*Messages*"
;;                              "*scratch*"
;;                              "Async Shell Command"
;;                              "*helm occur*"
;;                              "*Helm Completions*"
;;                              "*helm-ag*"
;;                              "*helm projectile*"))
;;   (ido-mode +1))

(use-package projectile
  :ensure t
  :demand t
  :commands (projectile-find-file projectile-switch-project)
  :diminish projectile-mode
  :init
  (setq projectile-switch-project-action 'magit-status
        projectile-mode-line "")
  (use-package helm-projectile
    :ensure t
    :bind (("s-p" . helm-projectile-find-file)
           ("s-P" . helm-projectile-switch-project)))
  :config
  (bind-key* "C-x f" 'mike/ag-in-project)
  (projectile-global-mode))

(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :init
  (setq company-idle-delay 0.2
        company-dabbrev-code-everywhere t
        company-dabbrev-code-ignore-case t
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t
        company-etags-ignore-case t
        company-minimum-prefix-length 1)
  (global-company-mode)
  :config
  (bind-key* "M-." 'xref-find-definitions)
  (bind-key* "C-x C-o" 'xref-find-references)
  (add-hook 'prog-mode-hook 'company-mode)
  (setq company-backends '(company-robe
                           company-capf
                           (company-dabbrev :with company-yasnippet)))
  (add-hook 'company-mode-hook
            (lambda ()
              (define-key company-active-map (kbd "M-n") nil)
              (define-key company-active-map (kbd "M-p") nil)
              (define-key company-active-map (kbd "C-n") #'company-select-next)
              (define-key company-active-map (kbd "C-p") #'company-select-previous)
              (define-key company-active-map (kbd "<return>") nil)
              (define-key company-active-map (kbd "RET") nil)
              (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
              (define-key company-active-map (kbd "TAB") #'company-complete-selection))))

;; Snippets.
(use-package yasnippet
  :ensure t
  :demand t
  :diminish yas-minor-mode
  :config
  (progn
    (global-unset-key (kbd "s-e"))
    (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
    (setq yas-triggers-in-field t)
    (define-key yas-keymap (kbd "C-d") nil)
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    ;; (define-key yas-minor-mode-map (kbd "s-e") 'yas-expand)
    (yas-global-mode t))
  )

;; Correctly load $PATH and $MANPATH on OSX (GUI).
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (progn
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH")))

(use-package multiple-cursors
  :ensure t
  :init
  (setq mc/keymap (make-sparse-keymap))
  (define-key mc/keymap (kbd "C-g") 'mc/keyboard-quit)
  (when (fboundp 'phi-search)
    (define-key mc/keymap (kbd "C-s") 'phi-search))
  (when (fboundp 'phi-search-backward)
    (define-key mc/keymap (kbd "C-r") 'phi-search-backward))
  :config
  (bind-key* "C-M-n" 'mc/mark-next-like-this)
  (bind-key* "C-M-p" 'mc/unmark-next-like-this)
  (bind-key* "C-M-s" 'mc/skip-to-next-like-this)
  (bind-key* "C-M-l" 'mc/edit-ends-of-lines)
  (bind-key* "C-x C-g" 'mc/mark-all-like-this))

;; Scale the text of all the windows/frames at the same time.
(use-package default-text-scale
  :ensure t
  :bind (("s-=" . default-text-scale-increase)
         ("s--" . default-text-scale-decrease)))

;; Modes for programming languages and such.
(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.erb\\'" . web-mode)
         ("\\.xml\\.erb\\'" . web-mode)
         ("\\.eex\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package js
  ;; built-in
  :init
  (setq js-indent-level 2))

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'"
  :init
  (setq css-indent-offset 2))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.mkd\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :bind (("M-{" . markdown-backward-block)
         ("M-}" . markdown-forward-block)
         ("C-c C-p" . markdown-preview)
         ("C-c TAB" . markdown-insert-italic)
         ("C-c i" . markdown-insert-image)
         ("C-c C-k" . markdown-insert-link)
         ("C-c C-b" . markdown-insert-bold)
         ("C-c C-t" . markdown-insert-gfm-checkbox)
         ("C-c C-c" . markdown-toggle-gfm-checkbox)
         )
  :init
  (setq markdown-mode-map (make-sparse-keymap))
  (setq markdown-css-paths (list (expand-file-name "~/toolbag/markdown.css")))
  (setq markdown-italic-underscore t)
  (setq markdown-indent-on-enter 'indent-and-new-item)
  (define-key markdown-mode-map (kbd "C-m") 'markdown-enter-key)
  (define-key markdown-mode-map (kbd "TAB") 'markdown-cycle)
  (define-key markdown-mode-map (kbd "M-<return>") 'mike/markdown-newline-indent-no-list-item)
  :config
  (add-hook 'markdown-mode-hook
            (lambda ()
              (modify-syntax-entry ?\` "\"`")
              (modify-syntax-entry ?\" "\"\"")
              (setq autopair-handle-action-fns
                    (list 'autopair-default-handle-action
                          'autopair-python-triple-quote-action)))))

(use-package ruby-mode
  ;; built-in
  :config
  (add-hook 'ruby-mode-hook (lambda () (setq tab-width 2)))
  :init
  ;; Don't insert the "coding utf8" comment when saving Ruby files.
  (setq ruby-insert-encoding-magic-comment nil))

(use-package yaml-mode
  :ensure t
  :mode "\\.e?ya?ml$")

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (setq-local tab-width 4)
              (setq gofmt-command "goimports")
              (add-hook 'before-save-hook 'gofmt-before-save))))

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)))

(use-package cider
  :ensure t
  :defer t
  :commands (cider cider-connect cider-jack-in)
  :diminish subword-mode
  :config
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
  (setq nrepl-log-messages t
        cider-repl-use-clojure-font-lock t
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t
        cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing))


(use-package json-mode
  :ensure t
  :defer t
  :mode "\\.reandeploy\\'"
  :mode "\\.json\\'"
  :bind (("C-c C-g" . jsons-print-path))
  :config
  (require 'json-snatcher))

(use-package json-reformat :ensure t :defer t)

(use-package json-snatcher :ensure t :defer t)

;; TODO: Move this to another file.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-babel-load-languages (quote ((ruby . t) (python . t) (shell . t))))
 '(package-selected-packages
   (quote
    (autopair yasnippet yaml-mode web-mode use-package scss-mode multiple-cursors markdown-mode magit helm-projectile helm-ag go-mode gitignore-mode expand-region exec-path-from-shell dracula-theme default-text-scale company))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t (:inherit nil :background "#2eb131063f08")))))
