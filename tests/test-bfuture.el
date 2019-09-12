;;; test-bfuture --- tests for bfuture -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'ert)
(require 'bfuture)
(require 's)
(require 'dash)

(ert-deftest test-bfuture-new ()
  "Test spawning a process."
  (let* ((proc (bfuture-new "echo" "Test bfuture"))
         (buffer (process-get proc 'buffer)))
    (should (not (null buffer)))
    (bfuture-await-to-finish proc)
    (should (null (process-live-p proc)))
    (with-current-buffer buffer
      (should (s-equals? (buffer-substring-no-properties
                          (point-min) (point-max))
                         "Test bfuture\n")))
    ;; Clean up
    (kill-buffer buffer)))

(ert-deftest test-bfuture-result-when-all-done ()
  "Test spawning multiple processes and getting the result."
  (let* ((nprocs (number-sequence 0 5))
         (procs (--map (bfuture-new "echo" (format "Test bfuture%s" it)) nprocs))
         (result (bfuture-result-when-all-done procs)))
    (--each procs (should (null (process-live-p it))))
    (should (listp result))
    (--each (-zip result nprocs)
      (should (s-equals? (car it)
                         (format "Test bfuture%s\n" (cdr it)))))
    ;; Clean up
    (let ((buffers (--map (process-get it 'buffer) procs)))
      (--each buffers
        (should-not (buffer-name it))
        ;; Clean up incase something went wrong
        (when (bufferp it)
          (kill-buffer it))))))

(ert-deftest test-bfuture-take-last ()
  "Test picking out last lines of output from `bfuture-result'."
  (should (s-equals?
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
