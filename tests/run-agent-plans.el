;;; run-agent-plans.el --- Agent plan test runner -*- lexical-binding: t; -*-

(load (expand-file-name "agent-plans.el" (file-name-directory load-file-name)) nil t)

(defun wgn-agent-plans-run-tests ()
  "Run the agent plan tests and return their report.
Signal an error on failure so emacsclient exits with a nonzero status."
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (when format-string
                     (let ((text (apply #'format format-string args)))
                       (princ text)
                       (terpri)
                       text)))))
        ;; The server evaluates this inside a `condition-case' on
        ;; `error'.  Emacs 29's ERT records a failure or skip only
        ;; through `debugger', which that handler suppresses, so the
        ;; first one would abort the run; the `debug' handler here lets
        ;; ERT's debugger see it.
        (let ((stats (condition-case err
                         (ert-run-tests-batch "^wgn-agent-plans-")
                       ((debug error) (signal (car err) (cdr err))))))
          (when (or (zerop (ert-stats-total stats))
                    (> (ert-stats-completed-unexpected stats) 0))
            ;; emacsclient prints only what arrives in its first read
            ;; of an error (BUFSIZ bytes), so leave out the passes.
            (error "%s" (replace-regexp-in-string
                         "^ *passed .*\n" "" (buffer-string))))
          (buffer-string))))))

;;; run-agent-plans.el ends here
