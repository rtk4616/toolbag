;;; crontab-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "crontab-mode" "../.emacs.d/elpa/crontab-mode-20090510.1355/crontab-mode.el"
;;;;;;  (21739 40050 0 0))
;;; Generated autoloads from ../.emacs.d/elpa/crontab-mode-20090510.1355/crontab-mode.el

(autoload 'crontab-mode "crontab-mode" "\
Major mode for editing crontabs.
Defines commands for getting and applying crontabs for hosts.
Sets up command `font-lock-mode'.

\\{crontab-mode-map}

\(fn)" t nil)

(autoload 'crontab-get "crontab-mode" "\
Get the crontab for the HOST into a buffer.

\(fn HOST)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; crontab-mode-autoloads.el ends here
