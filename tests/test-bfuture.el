;;; test-bfuture --- tests for bfuture -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'ert)
(require 'bfuture)
(require 'seq)

(ert-deftest test-bfuture-new ()
  "Test spawning a process."
  (let* ((proc (bfuture-new "echo" "Test bfuture"))
         (buffer (process-get proc 'buffer)))
    (should (not (null buffer)))
    (bfuture-await-to-finish proc)
    (should (null (process-live-p proc)))
    (with-current-buffer buffer
      (should (string-equal (buffer-substring-no-properties
                          (point-min) (point-max))
                         "Test bfuture\n")))
    ;; Clean up
    (kill-buffer buffer)))

(ert-deftest test-bfuture-result-when-all-done ()
  "Test spawning multiple processes and getting the result."
  (let* ((nprocs (number-sequence 0 5))
         (procs (--map (bfuture-new "echo" (format "Test bfuture%s" it)) nprocs))
         (result (bfuture-result-when-all-done procs)))
    (seq-each (lambda (it) (should (null (process-live-p it)))) procs)
    (should (listp result))
    (seq-each
     (lambda (it)
       (should (string-equal (car it) (format "Test bfuture%s\n" (cdr it)))))
     (mapcar* 'cons result nprocs))
    ;; Clean up
    (seq-each (lambda (it)
                (should-not (buffer-name it))
                ;; Clean up incase something went wrong
                (when (bufferp it)
                  (kill-buffer it)))
              (seq-map (lambda (it) (process-get it 'buffer)) procs))))

(ert-deftest test-bfuture-take-last ()
  "Test picking out last lines of output from `bfuture-result'."
  (should (string-equal
           (bfuture-take-last
            2
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut
enim ad minim veniam, quis nostrud exercitation ullamco laboris
nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in
reprehenderit in voluptate velit esse cillum dolore eu fugiat
nulla pariatur. Excepteur sint occaecat cupidatat non proident,
sunt in culpa qui officia deserunt mollit anim id est
laborum.")
           "sunt in culpa qui officia deserunt mollit anim id est\nlaborum.")))

(provide 'test-bfuture)
;;; test-bfuture.el ends here
