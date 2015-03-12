;;; company-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "company" "../.emacs.d/elpa/company-20150306.1548/company.el"
;;;;;;  (21758 3138 0 0))
;;; Generated autoloads from ../.emacs.d/elpa/company-20150306.1548/company.el

(autoload 'company-mode "company" "\
\"complete anything\"; is an in-buffer completion framework.
Completion starts automatically, depending on the values
`company-idle-delay' and `company-minimum-prefix-length'.

Completion can be controlled with the commands:
`company-complete-common', `company-complete-selection', `company-complete',
`company-select-next', `company-select-previous'.  If these commands are
called before `company-idle-delay', completion will also start.

Completions can be searched with `company-search-candidates' or
`company-filter-candidates'.  These can be used while completion is
inactive, as well.

The completion data is retrieved using `company-backends' and displayed
using `company-frontends'.  If you want to start a specific back-end, call
it interactively or use `company-begin-backend'.

regular keymap (`company-mode-map'):

\\{company-mode-map}
keymap during active completions (`company-active-map'):

\\{company-active-map}

\(fn &optional ARG)" t nil)

(defvar global-company-mode nil "\
Non-nil if Global-Company mode is enabled.
See the command `global-company-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-company-mode'.")

(custom-autoload 'global-company-mode "company" nil)

(autoload 'global-company-mode "company" "\
Toggle Company mode in all buffers.
With prefix ARG, enable Global-Company mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Company mode is enabled in all buffers where
`company-mode-on' would do it.
See `company-mode' for more information on Company mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "company-abbrev" "../.emacs.d/elpa/company-20150306.1548/company-abbrev.el"
;;;;;;  (21758 3138 0 0))
;;; Generated autoloads from ../.emacs.d/elpa/company-20150306.1548/company-abbrev.el

(autoload 'company-abbrev "company-abbrev" "\
`company-mode' completion back-end for abbrev.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-bbdb" "../.emacs.d/elpa/company-20150306.1548/company-bbdb.el"
;;;;;;  (21758 3138 0 0))
;;; Generated autoloads from ../.emacs.d/elpa/company-20150306.1548/company-bbdb.el

(autoload 'company-bbdb "company-bbdb" "\
`company-mode' completion back-end for BBDB.

\(fn COMMAND &optional ARG &rest IGNORE)" t nil)

;;;***

;;;### (autoloads nil "company-css" "../.emacs.d/elpa/company-20150306.1548/company-css.el"
;;;;;;  (21758 3138 0 0))
;;; Generated autoloads from ../.emacs.d/elpa/company-20150306.1548/company-css.el

(autoload 'company-css "company-css" "\
`company-mode' completion back-end for `css-mode'.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-dabbrev" "../.emacs.d/elpa/company-20150306.1548/company-dabbrev.el"
;;;;;;  (21758 3138 0 0))
;;; Generated autoloads from ../.emacs.d/elpa/company-20150306.1548/company-dabbrev.el

(autoload 'company-dabbrev "company-dabbrev" "\
dabbrev-like `company-mode' completion back-end.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-dabbrev-code" "../.emacs.d/elpa/company-20150306.1548/company-dabbrev-code.el"
;;;;;;  (21758 3138 0 0))
;;; Generated autoloads from ../.emacs.d/elpa/company-20150306.1548/company-dabbrev-code.el

(autoload 'company-dabbrev-code "company-dabbrev-code" "\
dabbrev-like `company-mode' back-end for code.
The back-end looks for all symbols in the current buffer that aren't in
comments or strings.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-elisp" "../.emacs.d/elpa/company-20150306.1548/company-elisp.el"
;;;;;;  (21758 3138 0 0))
;;; Generated autoloads from ../.emacs.d/elpa/company-20150306.1548/company-elisp.el

(autoload 'company-elisp "company-elisp" "\
`company-mode' completion back-end for Emacs Lisp.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-etags" "../.emacs.d/elpa/company-20150306.1548/company-etags.el"
;;;;;;  (21758 3138 0 0))
;;; Generated autoloads from ../.emacs.d/elpa/company-20150306.1548/company-etags.el

(autoload 'company-etags "company-etags" "\
`company-mode' completion back-end for etags.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-files" "../.emacs.d/elpa/company-20150306.1548/company-files.el"
;;;;;;  (21758 3138 0 0))
;;; Generated autoloads from ../.emacs.d/elpa/company-20150306.1548/company-files.el

(autoload 'company-files "company-files" "\
`company-mode' completion back-end existing file names.
Completions works for proper absolute and relative files paths.
File paths with spaces are only supported inside strings.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-gtags" "../.emacs.d/elpa/company-20150306.1548/company-gtags.el"
;;;;;;  (21758 3138 0 0))
;;; Generated autoloads from ../.emacs.d/elpa/company-20150306.1548/company-gtags.el

(autoload 'company-gtags "company-gtags" "\
`company-mode' completion back-end for GNU Global.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-ispell" "../.emacs.d/elpa/company-20150306.1548/company-ispell.el"
;;;;;;  (21758 3138 0 0))
;;; Generated autoloads from ../.emacs.d/elpa/company-20150306.1548/company-ispell.el

(autoload 'company-ispell "company-ispell" "\
`company-mode' completion back-end using Ispell.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-keywords" "../.emacs.d/elpa/company-20150306.1548/company-keywords.el"
;;;;;;  (21758 3138 0 0))
;;; Generated autoloads from ../.emacs.d/elpa/company-20150306.1548/company-keywords.el

(autoload 'company-keywords "company-keywords" "\
`company-mode' back-end for programming language keywords.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-nxml" "../.emacs.d/elpa/company-20150306.1548/company-nxml.el"
;;;;;;  (21758 3138 0 0))
;;; Generated autoloads from ../.emacs.d/elpa/company-20150306.1548/company-nxml.el

(autoload 'company-nxml "company-nxml" "\
`company-mode' completion back-end for `nxml-mode'.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-oddmuse" "../.emacs.d/elpa/company-20150306.1548/company-oddmuse.el"
;;;;;;  (21758 3138 0 0))
;;; Generated autoloads from ../.emacs.d/elpa/company-20150306.1548/company-oddmuse.el

(autoload 'company-oddmuse "company-oddmuse" "\
`company-mode' completion back-end for `oddmuse-mode'.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-pysmell" "../.emacs.d/elpa/company-20150306.1548/company-pysmell.el"
;;;;;;  (21758 3138 0 0))
;;; Generated autoloads from ../.emacs.d/elpa/company-20150306.1548/company-pysmell.el

(autoload 'company-pysmell "company-pysmell" "\
`company-mode' completion back-end for pysmell.
This requires pysmell.el and pymacs.el.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-semantic" "../.emacs.d/elpa/company-20150306.1548/company-semantic.el"
;;;;;;  (21758 3138 0 0))
;;; Generated autoloads from ../.emacs.d/elpa/company-20150306.1548/company-semantic.el

(autoload 'company-semantic "company-semantic" "\
`company-mode' completion back-end using CEDET Semantic.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-tempo" "../.emacs.d/elpa/company-20150306.1548/company-tempo.el"
;;;;;;  (21758 3138 0 0))
;;; Generated autoloads from ../.emacs.d/elpa/company-20150306.1548/company-tempo.el

(autoload 'company-tempo "company-tempo" "\
`company-mode' completion back-end for tempo.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-xcode" "../.emacs.d/elpa/company-20150306.1548/company-xcode.el"
;;;;;;  (21758 3138 0 0))
;;; Generated autoloads from ../.emacs.d/elpa/company-20150306.1548/company-xcode.el

(autoload 'company-xcode "company-xcode" "\
`company-mode' completion back-end for Xcode projects.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-yasnippet" "../.emacs.d/elpa/company-20150306.1548/company-yasnippet.el"
;;;;;;  (21758 3138 0 0))
;;; Generated autoloads from ../.emacs.d/elpa/company-20150306.1548/company-yasnippet.el

(autoload 'company-yasnippet "company-yasnippet" "\
`company-mode' back-end for `yasnippet'.

This back-end should be used with care, because as long as there are
snippets defined for the current major mode, this back-end will always
shadow back-ends that come after it.  Recommended usages:

* In a buffer-local value of `company-backends', grouped with a back-end or
  several that provide actual text completions.

  (add-hook 'js-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '((company-dabbrev-code company-yasnippet)))))

* After keyword `:with', grouped with other back-ends.

  (push '(company-semantic :with company-yasnippet) company-backends)

* Not in `company-backends', just bound to a key.

  (global-set-key (kbd \"C-c y\") 'company-yasnippet)

\(fn COMMAND &optional ARG &rest IGNORE)" t nil)

;;;***

;;;### (autoloads nil nil ("../.emacs.d/elpa/company-20150306.1548/company-capf.el"
;;;;;;  "../.emacs.d/elpa/company-20150306.1548/company-clang.el"
;;;;;;  "../.emacs.d/elpa/company-20150306.1548/company-cmake.el"
;;;;;;  "../.emacs.d/elpa/company-20150306.1548/company-eclim.el"
;;;;;;  "../.emacs.d/elpa/company-20150306.1548/company-pkg.el" "../.emacs.d/elpa/company-20150306.1548/company-ropemacs.el"
;;;;;;  "../.emacs.d/elpa/company-20150306.1548/company-template.el")
;;;;;;  (21758 3138 489638 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; company-autoloads.el ends here