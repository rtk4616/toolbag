;; ==============================================================
;; Initial setup
;; ==============================================================

;; Set up Marmalade.
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Set paths
(add-to-list 'load-path "~/.elisp")
(let ((default-directory "~/.elisp/"))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'custom-theme-load-path "~/.elisp/themes")

;; Set up my MikeFuzzyFind stuff
(require 'MikeFuzzyFind)

;; Set up expand region.
(require 'expand-region)
(global-set-key (kbd "M-h") 'er/expand-region)
;; (global-set-key (kbd "") 'er/expand-region)

;; Set up auto-complete.
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.elisp/ac-dict")
(require 'auto-complete-config)
(ac-config-default)
(setq ac-delay 0.2)

;; Autoload actionscript-mode.
(autoload 'actionscript-mode "actionscript-mode" "Major mode for editing ActionScript." t)

;; Autoload js2-mode.
(autoload 'js2-mode "js2-mode" "Major mode for editing Javascript stuffs." t)
;; (require 'js2-mode)

;; Multi-web-mode...
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags
  '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
    (js2-mode  "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
    (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

;; Make go-mode autoload for .go files.
(require 'go-mode-load)
;; TODO: I don't think we need this because go-mode-load.el is in .elisp, which
;; is already added to the load path above.
;; (add-to-list 'load-path "PATH CONTAINING go-mode-load.el" t)

;; Load PHP-mode.
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)

;; Load PHP-mode.
(autoload 'textile-mode "textile-mode" "Major mode for editing textile files." t)

;; Load markdown-mode.
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)

;; Use aspell as the spell-checker
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")

;; Set up the Emacs theme.
(load-theme 'tomorrow-night t)
;; (load-theme 'monokai t)

;; Set SSH as the default method for Tramp.
(setq tramp-default-method "ssh")

;; Make F6 toggle autopair on and off for the purposes of pasting.
(global-set-key (kbd "<f6>") 'autopair-mode)

;; Set up ido mode.
;; (require 'flx-ido)
(ido-mode t)
(setq ido-everywhere t)
;; (flx-ido-mode 1)
(setq ido-use-faces nil)

;; Ignore extensions in completion
(setq completion-ignored-extensions
      '(".o" ".lo" ".mh" ".elc" "~" ".bin" ".lbin" ".fasl" ".dvi" ".toc" ".aux"
        ".lof" ".blg" ".bbl" ".glo" ".idx" ".lot" ".pyc"))

;; Load yaml-mode
(require 'yaml-mode)

;; Load the Autopair module.
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers
(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))

;; Set auto mode rules.
;; This is where you define the major modes for different file extensions.
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.textile$" . textile-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.htaccess$" . conf-javaprop-mode))
(add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.vert$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.frag$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cu$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cl$" . c++-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("rc$" . sh-mode))
(add-to-list 'auto-mode-alist '(".gitconfig$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))

;;
;; end Initial setup
;;



;; ==============================================================
;; Custom functions
;; ==============================================================

;; Need this or else MikeGrepInFiles will fail with "Wrong type argument: stringp, nil"
(grep-compute-defaults)

;; Set up ignored files and directories when using grep.
(eval-after-load "grep"
  '(progn
     (add-to-list 'grep-find-ignored-files "*.pyc")
     (add-to-list 'grep-find-ignored-directories "migrations")
     (add-to-list 'grep-find-ignored-directories "ve")
     ))

(defun MikeGrepForSymbol (search_string)
  (interactive "sGrep for symbol: ")
  (rgrep (concat "^\\ *\\(class\\|def\\|func\\|function!*\\)\\ .*" search_string) "*" "./"))

(defun MikeGrepInFiles (search_string file_extension)
  (interactive "sSearch for: \nsIn files ending in: ")
  (rgrep search_string (concat "*" file_extension) "./"))
  ;; (rgrep search_string (format "*.%s" file_extension) (pwd)))

(defun MikeGrepForFiles (search_string)
  (interactive "sSearch for file: ")
  (find-name-dired "." (concat "*" search_string "*")))


;; ==============================================================
;; START Keybindings
;; ==============================================================

;; Use the default shortcut for regexp isearch to activate custom rgrep.
(global-set-key "\C-x\C-r" 'MikeGrepForSymbol)

;; List all lines matching a pattern in the current file.
(global-set-key "\M-l" 'recenter-top-bottom)

;; List all lines matching a pattern in the current file.
(global-set-key "\C-xl" 'list-matching-lines)
(global-set-key "\C-x\C-l" 'list-matching-lines)

;; iMenu binding.
(global-set-key "\C-x\C-j" 'imenu)
(global-set-key "\C-xj" 'imenu)

;; Use the default shortcut for regexp isearch to find files by partial name.
;; (global-set-key "\C-x\C-g" 'MikeGrepForFiles)
;; (global-set-key "\C-xg" 'MikeGrepForFiles)
(global-set-key "\C-x\C-g" 'MikeFuzzyFileFinder)
(global-set-key "\C-xg" 'MikeFuzzyFileFinder)

;; Use the default shortcut for regexp isearch to activate custom rgrep.
(global-set-key "\C-xf" 'MikeGrepInFiles)

;; Rebind Ctrl-s and Ctrl-r to use the regexp versions of isearch.
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)

;; Ctrl-\ saves the window layout.
(global-set-key "\C-\\" (lambda () (interactive) (window-configuration-to-register 'a)))
;; Alt-\ loads the window layout.
(global-set-key "\M-\\" (lambda () (interactive) (jump-to-register 'a)))

;; Make pressing enter while still holding down the control key insert a
;; newline.
(global-set-key (kbd "<C-return>") 'newline)

;; Make autocomplete feature use a better shortcut
(global-set-key (kbd "M-SPC") 'dabbrev-expand)

;; Make dabbrev-expand respect case...
(setq dabbrev-case-fold-search nil)

;; Some aliases to make things easier!
(defalias 'qrr 'query-replace-regexp)
(defalias 'ff 'find-name-dired)
(defalias 'sc 'diff-buffer-with-file)

;; Bind F4 to see changes between the current buffer and the version on disk.
(global-set-key (kbd "<f4>") 'sc)

;; Use electric buffer list for buffer navigation
(global-set-key "\C-x\C-b" 'electric-buffer-list)
;; (global-set-key "\C-xb" 'electric-buffer-list)

;; Make it so we don't have to release ctrl when switching buffers...
(global-set-key "\C-x\C-o" 'other-window)
(global-set-key "\M-o" 'other-window)

;; ...or killing buffers.
(global-set-key "\C-x\C-k" 'kill-buffer)

;; Custom other-window binding.
(global-set-key (kbd "C-]") 'other-window)

;; Make emacs use the system keyboard
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)

;; Make Alt-P and Alt-N act like Vim's Ctrl-y and Ctrl-e
(global-set-key "\M-p" (lambda () (interactive) (previous-line 6)))
(global-set-key "\M-n" (lambda () (interactive) (next-line 6)))

;; Bindings for changing buffers.
;; (global-set-key (kbd "M-<left>") 'previous-buffer)
;; (global-set-key (kbd "M-<right>") 'next-buffer)
(global-set-key "\C-x\C-p" 'previous-buffer)
(global-set-key "\C-x\C-n" 'next-buffer)
(global-set-key "\C-xp" 'previous-buffer)
(global-set-key "\C-xn" 'next-buffer)
;;
;; END Keybindings
;;


;; ==============================================================
;; START misc stuff here...
;; ==============================================================

;; Make rgrep searches be case insensitive.
(setq case-fold-search t)

;; *********************
;; Turn on line numbers!
;; *********************
;;
;; (NOTE: Only do this in the GUI. Selecting by line in the terminal version with line
;; numbers enabled results in the line numbers also being selected)
;;
;; (if window-system
;;     ;; If we are using the GUI version of Emacs...
;;     (add-hook 'find-file-hook (lambda () (linum-mode 1)))
;;   )

;; make emacs keep the current working directory when opening files.
(add-hook 'find-file-hook
          (lambda ()
            (setq default-directory command-line-default-directory)))


;; Add a little padding around the line numbers.
;; Dynamically determine character width for the line numbers column, and add a
;; space for padding as well.
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string w) "d ")))
    ad-do-it))
;; (setq linum-format " %d  ")


;; Make F5 toggle line numbers on and off.
(global-set-key (kbd "<f5>") 'linum-mode)

;; Ediff with vertically split windows.
(setq ediff-split-window-function 'split-window-horizontally)

;; Allow smooth scrolling.
(setq scroll-step            1
      scroll-conservatively  10000)

;; Set default font.
;; (set-default-font "DejaVu Sans Mono-9")
(set-default-font "Menlo-12")

;; Set character wrapping.
(setq-default fill-column 79)

;; Put autosave and backup files in the system temp folder.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Add highlighting of TODO, BUG, NOTE, and FIXME.
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME:\\|TODO:\\|BUG:\\|NOTE:\\)" 1 font-lock-warning-face t)))))


;; ************************************************************************************************
;; Disable requiring the newline at EOF
;;
;; NOTE: This doesn't work for some reason... need to look into why, when I have the time.
;;
;; Ensure we do this *after* default.el is loaded, otherwise, when Emacs
;; executes the default.el file after your .emacs file, this value will get
;; changed.
;;
;; Reference: http://rura.org/blog/2004/10/26/emacs-require-final-newline-on-fedora-how-to-kill-it/
;;
(add-hook 'after-init-hook
	   '(lambda ()
	      (setq require-final-newline nil)))
;; ************************************************************************************************


;; Require the newline at EOF
(setq require-final-newline nil)

;; Disable useless decorations.
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Accept y or n when presented with yes or no.
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable beeps.
(setq ring-bell-function 'ignore)

;; Always use syntax highlighting.
(global-font-lock-mode 1)

;; Show the column number in addition to the line number.
(setq-default column-number-mode 1)

;; Make the frame title show the file name.
(setq frame-title-format "%b")

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Remove trailing whitespace before saving files.
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; ================================================================
;;                      Indentation settings
;; ================================================================

;; Indent only with spaces (default 4), never tabs.
(setq-default standard-indent 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Use BSD style for formatting.
(setq c-default-style "bsd"
      c-basic-offset 4)

;; Indentation settings for Ruby.
(setq ruby-indent-level 2)

;; Indentation for CSS
(setq css-indent-offset 4)

;; Indentation for Python
(setq python-indent 4)
