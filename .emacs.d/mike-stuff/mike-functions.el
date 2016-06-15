;; rmacs variables
(defvar rmacs-port 52698)   ; The port on which rmacs will listen.
(defvar rmacs-clients '())  ; A list of clients, where each element is (process "message string").


(defun mike/get-pwd-as-string ()
  (nth 1 (split-string (pwd)))
  )


(defun mike/get-rsync-config (directory-string)
  (assoc directory-string rsync-project-mapping))


(defun mike/rsync-project (&optional dry-run)
  (interactive)
  (if (projectile-project-root)
      (let ((config (mike/get-rsync-config (projectile-project-root))))
        (if config
            (let* ((rsync-from-location (nth 0 config))
                   (rsync-to-location (nth 1 config))
                   (rsync-flags (if dry-run "-crlpvtDn" "-crlpvtD")))
              (save-window-excursion
                (with-temp-buffer
                  (pop-to-buffer (current-buffer))
                  (insert (concat "Rsyncing " rsync-from-location " to " rsync-to-location "...\n"))
                  (when dry-run (insert "(Dry run only - no files changed)\n"))
                  (insert "\n")
                  (call-process "rsync" nil (current-buffer) t rsync-flags "--delete" rsync-from-location rsync-to-location)
                  )))
          (message (concat "No rsync mapping found for " (projectile-project-root)))))
    (message "You are not currently in a project!")))


(defun rmacs/start-server nil
  (interactive)
  (unless (process-status "rmacs")
    (make-network-process
     :name "rmacs"
     :buffer "*rmacs*"
     :family 'ipv4
     :host "localhost"
     :service rmacs-port
     :sentinel 'rmacs/sentinel
     :filter 'rmacs/process-message
     :server 't)
    (setq rmacs-clients '())))


(defun rmacs/stop-server nil
  (interactive)
  (while  rmacs-clients
    (delete-process (car (car rmacs-clients)))
    (setq rmacs-clients (cdr rmacs-clients)))
  (delete-process "rmacs"))


(defun rmacs/get-value (string)
  "Return the value in a string of format key: value"
  (s-trim (nth 1 (split-string string ":" \t))))


(defun rmacs/get-lines (string)
  "Return a list of a string's lines"
  (split-string string "\n"))


(defun rmacs/nth-line (index string)
  "Return the index-th line of a string, starting with 0"
   (nth index (rmacs/get-lines string)))


(defun rmacs/strip-rmate-headers (string)
  "Strip the rmate headers from a string"
  (mapconcat 'identity
             ;; The rmate string contains the file content after the 8th line
             ;; of the string it sends across the network, but we don't want
             ;; the final line which only contains a period.
             (butlast (nthcdr 7 (rmacs/get-lines string)) 2)
             "\n"))


(defun rmacs/get-remote-address (string)
  "Return the remote connection's address from the headers of an rmate string"
  ;; The remote address is the value on the 3rd line.
  (rmacs/get-value (rmacs/nth-line 2 string)))


(defun rmacs/get-remote-filepath (string)
  "Return the remote file's full path from the headers of an rmate string"
  ;; The remote address is the value on the 5th line.
  (rmacs/get-value (rmacs/nth-line 4 string)))


(defun rmacs/get-buffer-name (string)
  "Return a buffer name based on the headers of an rmate string"
  (mapconcat 'identity
             (list
              ;; The remote connection's address.
              (rmacs/get-remote-address string)
              ;; The full path of the remote file.
              (rmacs/get-remote-filepath string))
             ;; Joined by a colon.
             ":"))


(defun rmacs/get-rmate-headers (string)
  "Returns the first 7 lines of a string"
  (let ((lines (rmacs/get-lines string)))
    (mapconcat 'identity
               (list
                (pop lines)
                (pop lines)
                (pop lines)
                (pop lines)
                (pop lines)
                (pop lines)
                (pop lines))
               "\n")))


(defun rmacs/add-new-client (process header-string)
  "Add a new incoming process to the list of rmacs clients and create file buffer"
  (setq rmacs-clients (cons (cons process header-string) rmacs-clients))
  ;; (get-buffer-create (rmacs/get-buffer-name header-string))
  ;; (setq buffer-read-only nil)
  ;; (erase-buffer)
  )


(defun rmacs/put-content-to-buffer (content name-of-buffer)
  "Insert the content string into the specified buffer"
  (switch-to-buffer name-of-buffer)
  ;; Set the major mode of the buffer based off of filename.
  (let ((buffer-file-name (buffer-name))) (set-auto-mode))
  (insert content)
  )


(defun rmacs/process-message (process string)
  (let (
        ;; See if the process is in our list of clients.
        (client (assoc process rmacs-clients))
        ;; By default, we assume that the entire string is file content.
        (content string)
        )
    (unless client
      ;; New connection. Add it to our list of clients.
      ;; A client is an object of the form (process . header-string)
      (rmacs/add-new-client process string)
      ;; Try again to get the client from the list.
      (setq client (assoc process rmacs-clients))
      ;; New connections include the rmate headers. We need to strip this from
      ;; the string to get the file content.
      (setq content (rmacs/get-rmate-headers string)))
    ;; Display a message with the content.
    (rmacs/log-message content)
    ))


(defun rmacs/sentinel (proc msg)
  (rmacs/log-message (concat "Message from rmacs-sentinel: " msg))
  ;; (rmacs/log-message "sending connection success")
  ;; (process-send-string proc "connection success\n")
  (when (string= msg "connection broken by remote peer\n")
    (setq rmacs-clients (assq-delete-all proc rmacs-clients))
    (rmacs/log-message (format "client %s has quit" proc))))


(defun rmacs/log-message (string &optional client)
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


(defun MikeTrampFindFile (&optional prompt-sudo)
  "Tramp wrapper for easy ssh and su to another user."
  (interactive)
  (setq host-string (completing-read (concat "Connect " (if prompt-sudo '"(WITH SUDO) to: " '"to: ")) (with-temp-buffer (insert-file-contents "~/.ssh/known_hosts") (split-string (buffer-string) "[, ].*[$\n]" t))))
  (when prompt-sudo
    (setq user-b (read-string "User to su to (leave blank for none):")))
  (if prompt-sudo
      (find-file (concat "/ssh:" host-string "|sudo:|sudo:" user-b "@" (car (last (split-string host-string "@"))) ":"))
    (find-file (concat "/ssh:" host-string ":")))
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
  (let ((deactivate-mark))
    (indent-code-rigidly beg end (* -1 tab-width))))

(defun MikeIndent (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (line-beginning-position) (line-end-position))))
  (let ((deactivate-mark))
    (indent-code-rigidly beg end tab-width)))


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
