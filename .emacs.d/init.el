;; Initialize the package system.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
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
(require 'diminish)
(require 'bind-key)

;; Hide minor modes from the mode bar.
(use-package diminish :ensure t)

;; My packages (in the lisp/ directory)
(use-package lady)
(use-package mike-functions
  :init
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
        python-indent 4
        ring-bell-function 'ignore
        ruby-indent-level 2
        scroll-conservatively 10000
        scroll-step 1
        shift-select-mode nil
        split-height-threshold nil
        tramp-default-method "ssh"
        tramp-shell-prompt-pattern "\\(?:^\\|\\)[^]#$%>\n]*#?[]#$%>].* *\\(\\[[0-9;]*[a-zA-Z] *\\)*"
        vc-handled-backends nil
        fill-column 79
        indent-tabs-mode nil
        standard-indent 4
        tab-width 4
        word-wrap t)
  (blink-cursor-mode 0)
  (fset 'yes-or-no-p 'y-or-n-p)
  (delete-selection-mode 1)
  (global-linum-mode -1)
  (set-fringe-mode 0)
  (setenv "TMPDIR" "/tmp")
  (global-set-key (kbd "<C-return>") (kbd "C-m"))
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  :bind (("<M-down>" . enlarge-window)
   ("<M-left>" . shrink-window-horizontally)
   ("<M-right>" . enlarge-window-horizontally)
   ("<M-up>" . shrink-window)
   ("C-\\" . desktop-save-in-desktop-dir)
   ("C-c C-b" . compile)
   ("C-c C-p" . preview-markdown)
   ("C-M-d" . duplicate-current-line-or-region)
   ("C-M-k" . kill-whole-line)
   ("C-M-x C-M-n" . mike/mark-all-in-region)
   ("C-r" . isearch-backward-regexp)
   ("C-s" . isearch-forward-regexp)
   ("C-w" . clipboard-kill-region)
   ("C-x C-\\" . kill-emacs)
   ("C-x C-e" . flycheck-list-errors)
   ("C-x C-k" . kill-buffer)
   ("C-x C-n" . next-buffer)
   ("C-x C-p" . previous-buffer)
   ("C-x C-t" . lady/tramp-connect)
   ("C-x M-b" . electric-buffer-list)
   ("C-x t" . lady/tramp-connect)
   ("C-x vb" . magit-blame)
   ("C-x vh" . magit-log-buffer-file)
   ("C-x vl" . magit-log-current)
   ("C-x vs" . magit-status)
   ("M-*" . mike-next-tag)
   ("M-," . pop-tag-mark)
   ("M-;" . toggle-comment-region-or-line)
   ("M-o" . other-window)
   ("M-{" . backward-paragraph)
   ("M-}" . forward-paragraph)
   ("C-x n" . flycheck-next-error)
   ("C-x p" . flycheck-previous-error)
   ("C-y" . clipboard-yank)
   ("M-(" . MikeDeIndent)
   ("M-)" . MikeIndent)
   ("M-U" . upcase-word)
   ("M-\\" . mike-desktop-read)
   ("M-g" . goto-line)
   ("M-j" . MikeGetIndentation)
   ("M-k" . mark-paragraph)
   ("M-l" . recenter-top-bottom)
   ("M-n" . MikeDownSomeLines)
   ("M-p" . MikeUpSomeLines)
   ("M-u" . downcase-word)
   ("M-w" . clipboard-kill-ring-save)
   )

  ;; ("C-M-x C-M-w") (lambda() (interactive) (kill-new (buffer-file-name))))
  ;; ("C-M-x w") (lambda() (interactive) (kill-new (car (last (split-string (buffer-file-name) "/"))))))
  ;; ("C-x b") (lambda() (interactive) (helm-buffers-list)))
  ;; ("C-x C-.") (lambda() (interactive) (ffap (ffap-file-at-point))))
  ;; ("C-x C-b") (lambda() (interactive) (helm-buffers-list)))
  ;; ("C-x v f") (lambda() (interactive) (magit-fetch-all "-p") (magit-status)))
  ;; ("M-_") (lambda() (interactive) (insert "—")))
  ;; ("C-x v f") (lambda() (interactive) (magit-fetch-all "-p") (magit-status)))
  ;; ("C-x f") (lambda() (interactive) (helm-do-ag (if (projectile-project-root) (projectile-project-root) (pwd)))))
  ;; ("C-x C-o") 'find-tag
  ;; ("C-x f" (lambda() (interactive) (helm-do-ag (if (projectile-project-root) (projectile-project-root) (pwd)))))
)

(use-package dracula-theme
  :ensure t
  :defer t
  :init
  (load-theme 'dracula t))


(use-package expand-region
  :ensure t
  :bind (("C-M-h" . er/contract-region)
         ("M-h" . er/expand-region))
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
  :commands (magit-status magit-checkout)
  :bind (("C-x v s" . magit-status)
         ("C-x v b" . magit-blame)
   ;; TODO:
         ;; ("C-x v f" . (lambda() (interactive) (magit-fetch-all "-p") (magit-status)))
         ("C-x v l" . magit-log-current)
         ("C-x v h" . magit-log-buffer-file))
  :init
  (setq magit-cherry-pick-arguments (quote ("-x"))
        magit-last-seen-setup-instructions "1.4.0"
        magit-push-always-verify nil
        magit-diff-refine-hunk t))

; ;; Shows git additions/deletions/edits on the fringe.
; (use-package git-gutter-fringe
;   :ensure t
;   :diminish git-gutter-mode
;   :demand t
;   :bind (("C-c h n" . git-gutter:next-hunk)
;          ("C-c h p" . git-gutter:previous-hunk))
;   :config
;   (progn
;     (global-git-gutter-mode t)
;     (define-fringe-bitmap 'git-gutter-fr:added
;       [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
;       nil nil 'center)
;     (define-fringe-bitmap 'git-gutter-fr:modified
;       [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
;       nil nil 'center)
;     (define-fringe-bitmap 'git-gutter-fr:deleted
;       [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
;       nil nil 'center)))

; ;; Show a small popup with the blame for the current line only.
; (use-package git-messenger
;   :ensure t
;   :bind ("C-c g p" . git-messenger:popup-message)
;   :init
;   (setq git-messenger:show-detail t)
;   :config
;   (progn
;     (define-key git-messenger-map (kbd "RET") 'git-messenger:popup-close)))

; ;; Navigate throught the history of the current file.
; (use-package git-timemachine
;   :ensure t
;   :bind ("C-c g t" . git-timemachine-toggle))

;; Mode for .gitignore files.
(use-package gitignore-mode :ensure t)

;; Helm-related things.
(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("C-c C-e" . helm-ag-edit)
         ("C-x C-f" . helm-find-files)
         ("C-x C-h" . helm-command-prefix)
         ("C-x C-l" . helm-occur)
         ("C-x C-r" . helm-resume)
         ("C-x M-f" . helm-do-ag)
         ("C-x M-l" . helm-do-ag-this-file)
         ("C-x b" . helm-buffers-list)
         ("C-x l" . helm-occur)
         ("M-." . helm-etags-select)
         ("M-x" . helm-M-x))
  :init
  (setq helm-buffer-skip-remote-checking t
        helm-buffers-fuzzy-matching t
        helm-display-header-line nil
        helm-ff-file-name-history-use-recentf t
        helm-ff-search-library-in-sexp t
        helm-M-x-fuzzy-match t
        helm-move-to-line-cycle-in-source t
        helm-quick-update t
        helm-scroll-amount 8
        helm-split-window-in-side-p t
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
  :config
  ;; No idea why here find-file is set to nil (so it uses the native find-file
  ;; for Emacs. This makes stuff like (find-file (read-file-name ...)) work with
  ;; Helm again.
  (helm-mode 1)
  (add-to-list 'helm-completing-read-handlers-alist '(find-file . helm-completing-read-symbols))
  (add-to-list 'helm-completing-read-handlers-alist '(dired-do-copy . nil))
  (add-to-list 'helm-completing-read-handlers-alist '(dired-do-rename . nil))
  (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
  (add-to-list 'helm-completing-read-handlers-alist '(ido-find-file . nil)))

(use-package helm-projectile
  :ensure t
  :bind (("C-M-f" . helm-projectile-find-file)
         ("C-M-t" . helm-projectile-switch-project)))

(use-package helm-ag
  :ensure t
  :bind (("C-x M-f" . helm-do-ag)
         ("C-c C-e" . helm-ag-edit)
   ;; TODO:
         ;; ("C-x f" . (lambda() (interactive) (helm-do-ag (if (projectile-project-root) (projectile-project-root) (pwd)))))
         ("C-x M-l" . helm-do-ag-this-file))
  :init
  (setq helm-ag-base-command "ag --nocolor --nogroup --hidden -U --smart-case"))

(use-package ido
  :ensure t
  :config
  (setq ido-enable-last-directory-history nil
        ido-everywhere t
        ido-max-work-directory-list 0
        ido-max-work-file-list 0
        ido-record-commands nil
        ido-use-faces nil
        ido-ignore-buffers '("^ "
                             "*Completions*"
                             "*Shell Command Output*"
                             "*Messages*"
                             "*scratch*"
                             "Async Shell Command"
                             "*helm occur*"
                             "*Helm Completions*"
                             "*helm-ag*"
                             "*helm projectile*"))
  (ido-mode +1))

(use-package projectile
  :ensure t
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
  :config
  (add-hook 'prog-mode-hook 'company-mode)
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
  :defer 4
  :diminish yas-minor-mode
  ; :config
  ; (progn
  ;   (global-unset-key (kbd "s-e"))
  ;   (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
  ;   (define-key yas-minor-mode-map (kbd "<tab>") nil)
  ;   (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;   (define-key yas-minor-mode-map (kbd "s-e") 'yas-expand)
  ;   (yas-global-mode t))
  )

;; Correctly load $PATH and $MANPATH on OSX (GUI).
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (progn
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-M-n" . mc/mark-next-like-this)
         ("C-M-p" . mc/unmark-next-like-this)
         ("C-M-s" . mc/skip-to-next-like-this)
         ("C-x C-g" . mc/mark-all-like-this)))

;; Scale the text of all the windows/frames at the same time.
(use-package default-text-scale
  :ensure t
  :bind (("s-=" . default-text-scale-increase)
         ("s--" . default-text-scale-decrease)))

;; Modes for programming languages and such.
(use-package web-mode
  :ensure t
  :mode (("\\.html\\.erb\\'" . web-mode)
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
  :mode ("\\.md\\'" "\\.mkd\\'" "\\.markdown\\'")
  :config
  (add-hook 'markdown-mode-hook
            (lambda ()
              (local-set-key (kbd "M-n") 'MikeDownSomeLines)
              (local-set-key (kbd "M-p") 'MikeUpSomeLines)
              (modify-syntax-entry ?\` "\"`")
              (modify-syntax-entry ?\" "\"\"")
              (setq autopair-handle-action-fns
                    (list 'autopair-default-handle-action
                          'autopair-python-triple-quote-action)))))

(use-package ruby-mode
  ;; built-in
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

;; Only maximize the window now because doing so earlier causes weird
;; behaviours.
(when (display-graphic-p)
  (toggle-frame-maximized))
