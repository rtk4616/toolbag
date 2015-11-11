(defvar rmacs-port 52698)   ; The port on which rmacs will listen.
(defvar rmacs-clients '())  ; A list of clients, where each element is (process "message string").

(defun rmacs-start nil
  (interactive)
  (unless (process-status "rmacs")
    (make-network-process
     :name "rmacs"
     :buffer "*rmacs*"
     :family 'ipv4
     :host "localhost"
     :service rmacs-port
     :sentinel 'rmacs-sentinel
     :filter 'rmacs-filter
     :server 't)
    (setq rmacs-clients '())
    )
  )

(defun rmacs-stop nil
  (interactive)
  (while  rmacs-clients
    (delete-process (car (car rmacs-clients)))
    (setq rmacs-clients (cdr rmacs-clients)))
  (delete-process "rmacs")
  )

(defun rmacs-filter (proc string)
  (message (concat "in rmacs-sentinel with message " string))
  (let (
        ;; Get proc from the list of clients, if it is already in there.
        (pending (assoc proc rmacs-clients))
        message
        index)

    (unless pending
      ;; Proc is not already in the client list; this is a new connection.
      ;; Add proc to the front of rmacs-clients.
      (setq rmacs-clients (cons (cons proc "") rmacs-clients))
      ;; Set pending to be the proc element from the alist.
      (setq pending  (assoc proc rmacs-clients)))
    ;; Get the message from pending.
    (setq message (concat (cdr pending) string))

    ;; Loop through all lines in message
    (while (setq index (string-match "\n" message))
      (setq index (1+ index))
      ;; Send the first line back to proc.
      (process-send-string proc (substring message 0 index))
      ;; Output the line to the rmacs buffer.
      (rmacs-log  (substring message 0 index) proc)
      ;; Trim the line we just echoed from message.
      (setq message (substring message index)))
    ;;
    (setcdr pending message))
  )

(defun rmacs-sentinel (proc msg)
  (message (concat "in rmacs-sentinel with message" msg))
  (message "sending connection success")
  (process-send-string proc "connection success\n")
  (when (string= msg "connection broken by remote peer\n")
    (setq rmacs-clients (assq-delete-all proc rmacs-clients))
    (rmacs-log (format "client %s has quit" proc))))

(defun rmacs-log (string &optional client)
  (if (get-buffer "*rmacs*")
      (with-current-buffer "*rmacs*"
        (goto-char (point-max))
        (insert (current-time-string)
                (if client (format " %s:" client) " ")
                string)
        (or (bolp) (newline)))))

(defun range (start-number)
  (interactive "nStarting at: ")
  (mc/insert-numbers start-number)
  )

(defun toggle-control-lock ()
  (interactive)
  (if (= extra-keyboard-modifiers 0)
      (setq extra-keyboard-modifiers ?\C-a)
    (setq extra-keyboard-modifiers 0)
    )
  )

(defun toggle-meta-lock ()
  (interactive)
  (if (= extra-keyboard-modifiers 0)
      (setq extra-keyboard-modifiers ?\M-a)
    (setq extra-keyboard-modifiers 0)
    )
  )

(defun mike-saved-session-exists ()
  (file-exists-p (concat (file-name-as-directory desktop-dirname) desktop-base-file-name)))

(defun mike-lock-file-exists ()
  (file-exists-p (concat (file-name-as-directory desktop-dirname) ".emacs.desktop.lock")))

(defun mike-desktop-read ()
  "Restore a saved emacs session."
  (interactive)
  (if (mike-saved-session-exists)
      (if (mike-lock-file-exists)
          (message "Desktop file is currently in use!")
        (desktop-read))
    (message "No desktop found.")))

(defun revert ()
  (interactive)
  (revert-buffer nil t t)
  )

(defun MikeUpdateDirectory ()
  "Custom function to be called on each projectile project switch."
  (interactive)
  ;; First, update the variable used for my custom with the newly-updated
  ;; working directory (default-directory, which will be set by projectile).
  (setq emacs-startup-directory default-directory)
  ;; Now kill all open windows...
  (mapc 'kill-buffer (buffer-list))
  ;; After killing all open windows, the value of default-directory will have
  ;; changed. Let's re-assign the value that projectile gave it.
  (setq default-directory emacs-startup-directory)
  )

(defun MikeTrampFindFile (host-string user-b)
  "Tramp wrapper for easy ssh and su to another user."
  (interactive
   (list
    (completing-read "Connect to: " (with-temp-buffer (insert-file-contents "~/.ssh/known_hosts") (split-string (buffer-string) "[, ].*[$\n]" t)))
    (read-string "User to su to (leave blank for none):")
    )
   )
  ;; (insert host-string user-b)
  (if (= (length user-b) 0)
      (find-file (concat "/ssh:" host-string ":"))
    (find-file (concat "/ssh:" host-string "|sudo:|sudo:" user-b "@" (car (last (split-string host-string "@"))) ":"))
    )
  )


(defun duplicate-current-line-or-region (arg)
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))


(defun MikeUpSomeLines ()
  (interactive)
  (previous-line 6)
  )


(defun MikeDownSomeLines ()
  (interactive)
  (next-line 6)
  )


(defun MikeScrollUpOneLine ()
  (interactive)
  ;; Emacs "down" means something different than I mean ;)
  (scroll-down 1)
  )


(defun MikeScrollDownOneLine ()
  (interactive)
  ;; Emacs "up" means something different than I mean ;)
  (scroll-up 1)
  )


(defun mike-next-tag ()
  (interactive)
  (let ((current-prefix-arg t)) ;; emulate C-u
    (call-interactively 'find-tag)
    )
  )


(defun convert-something-to-string (input-symbol)
  "Converts the supplied symbol to a string."
  (cond

   ((stringp input-symbol)
    input-symbol)

   ((numberp input-symbol)
    (number-to-string input-symbol))

   (t
    ;; We handle only symbols and lists.
    ;; TODO: Better error handling here...
    (error "Error in convert-something-to-string!"))
   )
  )


(defun get-dired-string-from-file-location (file-location-string)
  "Returns a string that can be parsed as a valid file by dired-mode."
  (let (
        (attribute-list (file-attributes file-location-string 'string))
        )
    (if attribute-list
        (progn
          ;; Return the string to insert.
          (mapconcat 'eval '(
                             (funcall 'convert-something-to-string(nth 8 attribute-list))
                             (funcall 'convert-something-to-string(nth 1 attribute-list))
                             (funcall 'convert-something-to-string(nth 2 attribute-list))
                             (funcall 'convert-something-to-string(nth 3 attribute-list))
                             (funcall 'convert-something-to-string(nth 7 attribute-list))
                             (funcall 'format-time-string "%b %d %H:%M" (nth 5 attribute-list))
                             file-location-string
                             ) " ")
          )
      ;; Otherwise return nil.
      nil
      )))


(defun get-regex-alternation-from-list (list-of-strings &optional prepend-string)
  (if prepend-string
      (setq list-of-strings (mapcar (function (lambda (x) (concat prepend-string x))) mike-ignore-directories)))

  ;; Escape periods in preparation for regex.
  (setq list-of-strings (mapcar (function (lambda (x) (replace-regexp-in-string "\\." "\\\\." x))) list-of-strings))

  ;; Join the ignore directories into a regex string.
  (setq the-regex-search-string (format "\\(%s\\)" (mapconcat 'identity list-of-strings "\\|"))))


(defun insert-directory-files-recursive (directory match maxdepth)
  "Need some documentation about this function here..."
  (let* (
         (files-list '())
         (current-directory-list (directory-files directory t))
         )

    ;; While current-directory-list is not empty...
    (while current-directory-list
      (let ((f (car current-directory-list))) ; Set f as the first file in the list.
        (cond

         ((and
           ;; If ignore-files-regex-string is not nil...
           ignore-files-regex-string
           ;; ...is a regular file...
           (file-regular-p f)
           ;; ...and f is one of the ignored files...
           (string-match ignore-files-regex-string f))
          ;; ...then skip this file.

          ;; TODO: This is a debug line below...
          ;; (message (concat "ignoring file " f " based on \"" (match-string 1 f) "\""))
          nil)

         ((and
           ;; If ignore-dirs-regex-string is not nil...
           ignore-dirs-regex-string
           ;; ...and f is a directory...
           (file-directory-p f)
           ;; ...which is one of the ignored directories...
           (string-match ignore-dirs-regex-string f))
          ;; ...then skip this directory

          ;; TODO: This is a debug line below...
          ;; (message (concat "ignoring directory " f " based on \"" (match-string 1 f) "\""))
          nil)

         ((and
           ;; If f is a regular, readable file that matches the search string...
           (file-regular-p f)
           (file-readable-p f)
           (string-match match f))
          ;; Insert the file into the current buffer
          (insert (concat (get-dired-string-from-file-location f) "\n")))

         ((and
           ;; If f is a directory...
           (file-directory-p f)
           ;; ...is readable...
           (file-readable-p f)
           ;; ...is not the the current or previous directory...
           (not (string-equal ".." (substring f -2)))
           (not (string-equal "." (substring f -1)))
           ;; ...and we have not reached the max depth...
           (> maxdepth 0))
          ;; Then recursively call this function
          (insert-directory-files-recursive f match (- maxdepth -1))
          )
         (t)))
      ;; Remove f from the working list of files in the current directory.
      (setq current-directory-list (cdr current-directory-list)))

    ;; All done... return files-list!
    files-list)
  )


(defun MikeFuzzyFileFinder (match-string)
  "Searches recursively for all files that match the file wildcard in the current directory"
  (interactive "sSearch for files matching: \n")

  (switch-to-buffer "*MikeFuzzyFind Results*")
  (fundamental-mode)
  (setq buffer-read-only nil)
  (erase-buffer)

  (insert-directory-files-recursive emacs-startup-directory
                                    (replace-regexp-in-string " " ".*" match-string)
                                    10)

  (indent-region (point-min) (point-max) 2)

  (dired-mode)
  (if (fboundp 'dired-simple-subdir-alist)
      ;; will work even with nested dired format (dired-nstd.el,v 1.15
      ;; and later)
      (dired-simple-subdir-alist)
    ;; else we have an ancient tree dired (or classic dired, where
    ;; this does no harm)
    (set (make-local-variable 'dired-subdir-alist)
         (list (cons emacs-startup-directory (point-min-marker)))))
  (goto-line 2))


;; A list of ignored file names.
(setq mike-ignore-files '(".pyc$"))

;; A list of ignored directory names.
(setq mike-ignore-directories '("ve"
                                ".idea"
                                ".git"
                                "migrations"
                                "scala-2.10"
                                "*.egg-info"
                                "_book"
                                "node_modules"
                                "elpa"
                                "build"
                                "static"))

;; Get the alternation for a list of files.
(setq ignore-files-regex-string (get-regex-alternation-from-list mike-ignore-files))

;; Get the alternation for a list of directories by prepending a slash to the directory names.
(setq ignore-dirs-regex-string (get-regex-alternation-from-list mike-ignore-directories "/"))


(defun MikeDeIndent (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (if (and (= beg (line-beginning-position))
             (= end (line-end-position)))
        (indent-rigidly beg end -4)
      (progn
        (indent-rigidly beg end -4)
        (goto-char end-pos)
        (push-mark nil t t)
        (goto-char start-pos)))))


(defun MikeIndent (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (if (and (= beg (line-beginning-position))
             (= end (line-end-position)))
        (indent-rigidly beg end 4)
      (progn
        (indent-rigidly beg end 4)
        (goto-char end-pos)
        (push-mark nil t t)
        (goto-char start-pos)))))


(defun MikeGetIndentationEnd (&optional REVERSE)
  "Return the furthest location in the file, going forward, before we reach a
line that is indented less than the starting indentation.

Specifying REVERSE as t will result in traversing the file backward."
  (interactive)
  (cond
   ;; If the function was called when the current line's indentation is zero...
   ((= 0 (current-indentation))
    ;; Just return the beginning of the current line.
    (line-beginning-position))

   ;; Otherwise, compute and return the appropriate position.
   (t
    (let ((starting-indentation (current-indentation))
          (to-continue t)
          (this-here-line nil)
          (to-return nil))

      ;; Set initial return value based on the REVERSE flag.
      (if REVERSE
          ;; Beginning of the starting line.
          (setq to-return (line-beginning-position))
        ;; End of the starting line.
        (setq to-return (line-end-position)))

      (unwind-protect
          ;; Save current cursor position, etc, so we can restore when done.
          (save-excursion
            ;; Loop until we break indentation.
            (while to-continue
              ;; Get the current line as a string.
              (setq
               this-here-line
               (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

              ;; Determine if we should continue looping, and whether or not to
              ;; update the return value.
              (cond
               ;; If the current line is greater or equal to the starting
               ;; indentation...
               ((>= (current-indentation) starting-indentation)
                (progn
                  (setq to-continue t)
                  ;; Update the return value.
                  (if REVERSE
                      (setq to-return (line-beginning-position))
                    (setq to-return (line-end-position)))))

               ;; If the current line is just whitespace...
               ((string-match "^$" this-here-line)
                (setq to-continue t))

               ;; Otherwise, don't continue.
               (t (setq to-continue nil)))

              ;; Go forward or backward depending on the given direction.
              (condition-case ex
                  (if REVERSE
                      (previous-line)
                    (next-line))
                ('error
                 ;; Catch error moving, like trying to move past the beginning or end
                 ;; of the buffer.
                 (message (format "Caught exception: [%s]" ex))
                 (setq to-continue nil)))))

        ;; (message "At the end of unwind-protect!"))
        (message "Done."))

      ;; Return the point.
      to-return))))


(defun MikeGetIndentation ()
  (interactive)
  (let ((start-pos (MikeGetIndentationEnd t))
        (end-pos (MikeGetIndentationEnd nil)))
    (if (= start-pos end-pos)
        (message "Nothing indented here to select!")
      (goto-char end-pos)
      (push-mark nil t t)
      (goto-char start-pos))))


;; Need to compute grep defaults, or MikeGrepInFiles will fail with the
;; following error:
;;
;; "Wrong type argument: stringp, nil"
;;
(grep-compute-defaults)
(defun MikeGrepInFiles (search_string file_extension)
  (interactive "sFind string: \nsLook in files ending with: ")
  (rgrep search_string (concat "*" file_extension) "./"))
