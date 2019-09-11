;;; bfuture --- basic future concept -*- lexical-binding: t -*-

;;; Commentary:
;; Based on the code of pfuture.el - https://github.com/Alexander-Miller/pfuture

;; Have the basic concepts of spawning a process and retrieving the
;; output. But this works with tramp to launch processes on a remote
;; host. Which is sadly not working in pfuture.

;;; Code:
(require 'cl-lib)
(require 'dash)
(require 's)

(defun bfuture-new (&rest cmd)
  "Create a new future process for command CMD.
Any arguments after the command are interpreted as arguments to
the command. Access the output using:
\(process-get process 'buffer\)."
  (let* ((buffer (generate-new-buffer "*bfuture-process*"))
         (proc (apply 'start-file-process `(,(buffer-name buffer) ,buffer ,@cmd))))
    ;; Disable "Process *bfuture-process* finished" when running locally.
    (set-process-sentinel proc #'ignore)
    (process-put proc 'buffer buffer)
    (process-put proc 'remote (file-remote-p default-directory))
    proc))

(defun bfuture-await-to-finish (process)
  "Wait until PROCESS is finished."
  (let (inhibit-quit)
    (while (accept-process-output process))))

(defun bfuture-result (process)
  "Return the output from PROCESS as a string.
This will delete the buffer associated with the PROCESS"
  (let ((buffer (process-get process 'buffer))
        (remote (process-get process 'remote)))
    (unless buffer
      (error "No buffer found associated with %s" process))
    (let ((output
           (with-current-buffer buffer
             (buffer-substring-no-properties (point-min) (point-max)))))
      (kill-buffer buffer)
      output)))

(defun bfuture-result-when-done (proc)
  "Return the result of PROC when the process has finished.
This will wait(block) unti the process is finished"
  (bfuture-await-to-finish proc)
  (bfuture-result proc))

(cl-defun bfuture-result-when-all-done (procs &key (result 'bfuture-result))
  "Retrieve the result of all PROCS when the processes are finished.
Return a list with corresponding output as a string.

Note: This will block until all are done.
Optional keywords:

RESULT: function called to fetch the output from a process.
Default is `bfuture-result'."
  (-each procs 'bfuture-await-to-finish)
  (--map (funcall result it) procs))

(defun bfuture-take-last (n result)
  "Take last N lines of RESULT.
Where RESULT is a string and each line is separate by '\n'.
Return a string containing the last N lines."
  (unless (stringp result)
    (error "RESULT must be a string"))
  (s-join "\n" (-take-last n (s-lines result))))

(provide 'bfuture)
;;; bfuture.el ends here
