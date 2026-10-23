;;; run-markdown.el --- Markdown test runner -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(load (expand-file-name "markdown.el" (file-name-directory load-file-name)) nil t)

(defvar wgn-markdown-report nil
  "The full report of the last `wgn-markdown-run-tests'.")

(defun wgn-markdown-run-tests ()
  "Run the Markdown tests and return their report.
Unless every test has its expected result or is skipped, signal an
error so emacsclient exits with a nonzero status.  The error carries
only the report's summary, since emacsclient cuts an error's message
short; `wgn-markdown-report' holds the rest, for writing to a file:
emacsclient prints a long value on one line, and before Emacs 30 can
garble it."
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (when format-string
                     (let ((text (apply #'format format-string args)))
                       (princ text)
                       (terpri)
                       text)))))
        ;; Before Emacs 30, ERT learns of a test's failure, skip or
        ;; quit through `debugger', which a signal reaches only past a
        ;; handler naming `debug'; without this one, the server's own
        ;; handler takes the first and ends the run.
        (let ((stats (condition-case err
                         (ert-run-tests-batch "^wgn-markdown-")
                       ((debug error quit) (signal (car err) (cdr err))))))
          (setq wgn-markdown-report (buffer-string))
          ;; A quit test counts as neither expected nor unexpected, and
          ;; ERT's summary names it only among unexpected results, so
          ;; without any the error names it.
          (when (or (zerop (ert-stats-total stats))
                    (/= (ert-stats-total stats)
                        (+ (ert-stats-completed-expected stats) (ert-stats-skipped stats))))
            (error "%s%s"
                   (substring wgn-markdown-report
                              (or (string-search "\nRan " wgn-markdown-report) 0))
                   (if (> (ert-stats-completed-unexpected stats) 0)
                       ""
                     (mapconcat (lambda (test)
                                  (let ((result (ert-test-most-recent-result test)))
                                    (if (or (null result) (ert-test-quit-p result))
                                        (format "     QUIT  %s\n" (ert-test-name test))
                                      "")))
                                (ert--stats-tests stats) ""))))
          wgn-markdown-report)))))

;;; run-markdown.el ends here
