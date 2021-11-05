;;; bfuture --- basic future concept -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021 Fredrik Salomonsson
;; Author: Fredrik Salomonsson <plattfot@posteo.net>
;; Created: 15 Feb 2020
;; Package-Requires: ((emacs "27.2") cl-lib seq subr-x)
;; Keywords: lisp
;; Version 1.0.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Based on the code of pfuture.el[0]
;; [0] https://github.com/Alexander-Miller/pfuture

;; Have the basic concepts of spawning a process and retrieving the
;; output. This also works with tramp to launch processes on a remote
;; host. Which is sadly is not working in pfuture.

;;; Code:
(require 'subr-x)
(require 'cl-lib)
(require 'seq)

(defun bfuture-new (&rest cmd)
  "Create a new future process for command CMD.
Any arguments after the command are interpreted as arguments to
the command. Access the output using:
\(process-get process 'buffer)."
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
    (while (accept-process-output process)))
  process)

(defun bfuture-result (process)
  "Return the output from PROCESS as a string.
This will delete the buffer associated with the PROCESS"
  (let ((buffer (process-get process 'buffer)))
    (unless buffer
      (error "No buffer found associated with %s" process))
    (let ((output
           (with-current-buffer buffer
             (buffer-substring-no-properties (point-min) (point-max)))))
      (kill-buffer buffer)
      output)))

(cl-defun bfuture-result-when-done (proc &key (result 'bfuture-result))
  "Return the result of PROC when the process has finished.
This will wait (block) until the process is finished.
Optional keywords:

RESULT: function called to fetch the output from a process.
Default is `bfuture-result'."
  (bfuture-await-to-finish proc)
  (funcall result proc))

(cl-defun bfuture-result-when-all-done (procs &key (result 'bfuture-result))
  "Retrieve the result of all PROCS when the processes are finished.
Return a list of strings.

Note: This will block until all are done.
Optional keywords:

RESULT: function called to fetch the output from a process.
Default is `bfuture-result'."
  (seq-map (lambda (it) (bfuture-result-when-done it :result result)) procs))

(defun bfuture-take-last (n result)
  "Take last N lines of RESULT.
Where RESULT is a string and each line is separate by '\n'.
Return a string containing the last N lines."
  (unless (stringp result)
    (error "RESULT must be a string"))
  (string-join (seq-reverse (seq-take (seq-reverse (split-string result "\n")) n))
               "\n"))

(provide 'bfuture)
;;; bfuture.el ends here
