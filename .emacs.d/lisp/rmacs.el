;;; rmacs --- Summary
;;;
;;; Commentary:
;;;
;;;

;;; Code:

;; rmacs variables
(defvar rmacs-port 52698)   ; The port on which rmacs will listen.
(defvar rmacs-clients '())  ; A list of clients, where each element is (process "message string").

(defun rmacs/start-server nil
  "Start the rmacs process to listen for incoming connections."
  (interactive)
  (unless (process-status "rmacs")
    (make-network-process
     :name "rmacs"
     :buffer "*rmacs*"
     :family 'ipv4
     :host "127.0.0.1"
     :service rmacs-port
     :sentinel 'rmacs/sentinel
     :filter 'rmacs/filter
     :server 't)
    (setq rmacs-clients '())))

(defun rmacs/stop-server nil
  "Stop the rmacs process."
  (interactive)
  (while rmacs-clients
    (delete-process (car (car rmacs-clients)))
    (setq rmacs-clients (cdr rmacs-clients)))
  (delete-process "rmacs"))

(defun rmacs/get-value (string)
  "Return the value in a string of format key: value."
  (string-trim (nth 1 (split-string string ":" \t))))

(defun rmacs/get-lines (string)
  "Return a list of a string's lines"
  (split-string string "\n"))

(defun rmacs/nth-line (index string)
  "Return the index-th line of a string, starting with 0"
   (nth index (rmacs/get-lines string)))

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

(defun rmacs/put-content-to-buffer (content name-of-buffer)
  "Insert the content string into the specified buffer"
  (switch-to-buffer name-of-buffer)
  ;; Set the major mode of the buffer based off of filename.
  (let ((buffer-file-name (buffer-name))) (set-auto-mode))
  (insert content)
  )

(defun rmacs/filter (process message-string)
  (let (
        ;; See if the process is in our list of clients.
        (client (assoc process rmacs-clients))
        )
    (unless client
      ;; New connection. Add it to our list of clients.
      (setq rmacs-clients (cons (cons process (rmacs/get-rmate-headers message-string)) rmacs-clients))
      ;; Try again to get the client from the list.
      (setq client (assoc process rmacs-clients)))
    (message "(car client) is %s" (car client))
    (message "(cdr client) is %s" (cdr client))
    ;; (rmacs/log-message (concat "Headers:\n" (nth 1 client) "\n"))
    ;; (rmacs/log-message (concat "Content:\n" (rmacs/strip-rmate-headers message-string) "\n"))
    ))

(defun rmacs/sentinel (proc msg)
  (rmacs/log-message (concat "Message from rmacs-sentinel: " msg))
  (rmacs/log-message "sending connection success")
  (process-send-string proc "connection success\n")
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

(provide 'rmacs)
;;; rmacs.el ends here
