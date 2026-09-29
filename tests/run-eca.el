;;; run-eca.el --- ECA test runner -*- lexical-binding: t; -*-

(load (expand-file-name "eca.el" (file-name-directory load-file-name)) nil t)

(defun wgn-eca-run-tests ()
  "Run the ECA tests and return their report.
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
        (let ((stats (ert-run-tests-batch "^wgn-eca-")))
          (when (or (zerop (ert-stats-total stats))
                    (> (ert-stats-completed-unexpected stats) 0))
            (error "%s" (buffer-string)))
          (buffer-string))))))

;;; run-eca.el ends here
