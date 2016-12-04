(eval-when-compile (require 'subr-x))

(defun lady/read-connection-string ()
  (completing-read "Connection string: "
                   (with-temp-buffer
                     (if (file-exists-p "~/.lady") (insert-file-contents "~/.lady"))
                     (split-string (buffer-string) "[$\n]" t))))

(defun lady/connection-already-saved (connectionString)
  (if (member connectionString
              (split-string
               (with-temp-buffer (insert-file-contents "~/.lady") (buffer-string))
               "[$\n]"
               t))
      t
    nil))

(defun lady/string-between (searchString startRegex endRegex)
  (car (split-string (car (last (split-string searchString startRegex))) endRegex))
  )

(defun lady/get-rest-of-string (arg)
  (if (string-match " " arg)
      (string-trim (substring arg (+ 1 (string-match " " arg))))
    nil))

(defun lady/pop-host-string (arg)
  (if (string-match " " arg)
      (string-trim (substring arg 0 (+ 1 (string-match " " arg))))
    arg))

(defun lady/parse-username (arg)
  (if (string-match "@" arg)
      (lady/string-between arg "^" "[@#>]")
    nil))

(defun lady/parse-hostname (arg)
  (lady/string-between arg "[@]" "[#>]"))

(defun lady/parse-port (arg)
  (if (string-match "#" arg)
      (lady/string-between arg "#" "[>]")
    nil))

(defun lady/parse-sudo-user (arg)
  (if (string-match ">" arg)
      (lady/string-between arg ">" "$")
    nil))

(defun lady/build-ssh-string (userName hostName port)
  (concat "/ssh:"
          (if userName (concat userName "@") nil)
          hostName
          (if port (concat "#" port) nil)
          ":"))

(defun lady/do-tramp-call (connectionString)
  (let* ((hostString (lady/pop-host-string connectionString))
         (theRest (lady/get-rest-of-string connectionString))
         (userName (lady/parse-username hostString))
         (hostName (lady/parse-hostname hostString))
         (port (lady/parse-port hostString))
         (sudoUser (lady/parse-sudo-user hostString)))
    (if theRest
        (progn
          (add-to-list 'tramp-default-proxies-alist
                         (list hostName sudoUser (lady/build-ssh-string userName hostName port)))
          (lady/do-tramp-call theRest))
      (if sudoUser
          (progn
            (add-to-list 'tramp-default-proxies-alist
                         (list hostName sudoUser (lady/build-ssh-string userName hostName port)))
            (find-file (concat "/sudo:" sudoUser "@" hostName ":")))
        (if userName
            (find-file (concat "/ssh:" userName "@" hostName ":"))
          (find-file (concat "/ssh:" hostName ":")))))))

(defun lady/tramp-connect ()
  (interactive)
  (let (
        (connectionString (lady/read-connection-string))
        )
    ;; Save the connection string if we haven't done so previously.
    (if (and
         (file-exists-p "~/.lady")
         (lady/connection-already-saved connectionString))
        (ignore)
      (append-to-file (concat "\n" (string-trim connectionString)) nil "~/.lady"))
    (setq tramp-default-proxies-alist nil)
    (lady/do-tramp-call connectionString)
    ))
