;;; agent-plans.el --- Agent plan browsing tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-agenda)
(load (expand-file-name "../lisp/wgn-agent-plans.el"
                        (file-name-directory load-file-name)) nil t)

(defun wgn-agent-plans-test--write (file contents)
  "Write CONTENTS to FILE, making its directory."
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert contents)))

(defconst wgn-agent-plans-test--fake
  "#!/bin/sh
# Answers `status --format FORMAT PLAN-FILE' from PLAN-FILE.status-FORMAT.
[ \"$1\" = status ] || exit 2
for last; do :; done
cat \"$last.status-$3\"
"
  "A stand-in for `agent-plan'.")

(defmacro wgn-agent-plans-test--with-plans (&rest body)
  "Run BODY with a plans directory and a fake `agent-plan' under a temp dir.
Binds `root' to the temp dir, whose plans directory is configured through
AGENT_TOOLS_CONFIG, and `agent-plan' is found on `exec-path'."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "agent-plans-" t)))
          (plans (expand-file-name "plans" root))
          (bin (expand-file-name "bin" root))
          (config (expand-file-name "config.json" root))
          (exec-path (cons bin exec-path))
          (process-environment
           (cons (concat "AGENT_TOOLS_CONFIG=" config)
                 (cons (concat "PATH=" bin ":" (getenv "PATH"))
                       process-environment))))
     (unwind-protect
         (progn
           (wgn-agent-plans-test--write
            config (format "{\"plans\": {\"dir\": %S, \"host\": \"mac\"}}" plans))
           (wgn-agent-plans-test--write
            (expand-file-name "agent-plan" bin) wgn-agent-plans-test--fake)
           (set-file-modes (expand-file-name "agent-plan" bin) #o755)
           ,@body)
       (delete-directory root t))))

(defun wgn-agent-plans-test--plan (plans host name title state verified total)
  "Make plan NAME for HOST under PLANS and return its plan file.
Its part and log files sit beside it."
  (let* ((base (expand-file-name (concat name "." host) plans))
         (file (concat base ".org")))
    (wgn-agent-plans-test--write
     file
     (format "* Plan\n** Goal\nGoal.\n** Steps\n*** TODO Do it\n:PROPERTIES:\n:ID: S1\n:END:\n"))
    (wgn-agent-plans-test--write (concat base ".P1.org") "* TODO Part\n")
    (wgn-agent-plans-test--write (concat base ".log.org") "")
    (wgn-agent-plans-test--write
     (concat file ".status-json")
     (format "{\"plan\": %S, \"title\": %S, \"status\": %S, \"steps\": [], \"verified\": %d, \"total\": %d, \"implemented\": \"%d/%d\"}\n"
             file title state verified total verified total))
    (wgn-agent-plans-test--write
     (concat file ".status-org")
     (format "* Status of %s\n| Step | Title | Done | Verified | Commit |\n|-\n| S1 | Do it | yes | no | |\n\n- Status: %s\n" title state))
    file))

(ert-deftest wgn-agent-plans-directory-comes-from-the-tools-config ()
  (wgn-agent-plans-test--with-plans
    (should (equal (wgn/agent-plans-directory) plans)))
  (let ((process-environment
         (cons "AGENT_TOOLS_CONFIG=/nonexistent/config.json" process-environment)))
    (should (equal (wgn/agent-plans-directory)
                   (expand-file-name "~/git/notes/agents")))))

(ert-deftest wgn-agent-plans-files-are-plan-files-newest-first ()
  (wgn-agent-plans-test--with-plans
    (let ((a (wgn-agent-plans-test--plan plans "mac" "2026-10-01-a" "A" "working" 1 2))
          (b (wgn-agent-plans-test--plan plans "devbox" "2026-10-02-b" "B" "handed-back" 2 2)))
      ;; Parts, logs and the fake's status files sit beside the plans.
      (should (file-exists-p (expand-file-name "2026-10-01-a.mac.P1.org" plans)))
      (should (file-exists-p (expand-file-name "2026-10-01-a.mac.log.org" plans)))
      (should (equal (wgn/agent-plans-files) (list b a))))))

(ert-deftest wgn-agent-plans-summary-reads-title-status-and-progress ()
  (wgn-agent-plans-test--with-plans
    (let ((dir (wgn-agent-plans-test--plan plans "mac" "2026-10-01-a" "Add a thing"
                                           "working" 1 3)))
      (should (equal (wgn/agent-plans-summary dir)
                     '(:title "Add a thing" :status "working" :progress "1/3"))))))

(ert-deftest wgn-agent-plans-open-visits-the-chosen-plan ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--plan plans "mac" "2026-10-01-a" "First" "working" 1 3)
    (let ((b (wgn-agent-plans-test--plan plans "devbox" "2026-10-02-b" "Second"
                                         "handed-back" 2 2))
          offered)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (setq offered (all-completions "" collection))
                   (seq-find (lambda (c) (string-match-p "Second" c)) offered))))
        (unwind-protect
            (progn
              (wgn/agent-plans)
              (should (equal (buffer-file-name) b))
              (should (cl-some (lambda (c)
                                 (and (string-match-p "First" c)
                                      (string-match-p "working" c)
                                      (string-match-p "1/3" c)))
                               offered))
              (should (cl-some (lambda (c)
                                 (and (string-match-p "Second" c)
                                      (string-match-p "handed-back" c)
                                      (string-match-p "2/2" c)))
                               offered)))
          (when (buffer-file-name) (kill-buffer)))))))

(ert-deftest wgn-agent-plans-dynamic-block-holds-the-status-table ()
  (wgn-agent-plans-test--with-plans
    (let ((dir (wgn-agent-plans-test--plan plans "mac" "2026-10-01-a" "Add a thing"
                                           "working" 1 3)))
      (with-temp-buffer
        (org-mode)
        (insert (format "* Notes\n#+BEGIN: agent-plan :plan %S\n#+END:\n" dir))
        (goto-char (point-min))
        (re-search-forward "^#\\+BEGIN")
        (beginning-of-line)
        (org-update-dblock)
        (let ((text (buffer-string)))
          (should (string-match-p "^| S1 | Do it | yes | no | |$" text))
          (should (string-match-p "^- Status: working$" text))
          ;; A heading inside the block would end the section it sits in.
          (should-not (string-match-p "Status of" text))
          (should (string-match-p "^#\\+END:$" text)))))))

(ert-deftest wgn-agent-plans-insert-status-creates-and-fills-the-block ()
  (wgn-agent-plans-test--with-plans
    (let ((dir (wgn-agent-plans-test--plan plans "mac" "2026-10-01-a" "Add a thing"
                                           "working" 1 3)))
      (with-temp-buffer
        (org-mode)
        (wgn/agent-plans-insert-status dir)
        (let ((text (buffer-string)))
          (should (string-match-p "^#\\+BEGIN: agent-plan :plan " text))
          (should (string-match-p "^| S1 | Do it | yes | no | |$" text)))))))

(ert-deftest wgn-agent-plans-agenda-reads-plans-without-global-changes ()
  (wgn-agent-plans-test--with-plans
    (let ((a (wgn-agent-plans-test--plan plans "mac" "2026-10-01-a" "A" "working" 0 1))
          (org-agenda-files '("~/org"))
          (org-refile-targets '((nil :maxlevel . 9)))
          (command (wgn/agent-plans-agenda-command)))
      (should (equal (car command) "A"))
      (let ((org-agenda-custom-commands (list command)))
        (should (equal (eval (cadr (assq 'org-agenda-files (nth 4 command))) t)
                       (list a)))
        (let ((out (with-output-to-string (org-batch-agenda "A"))))
          (should (string-match-p "Do it" out))
          (should-not (string-match-p "Part" out))))
      (should (equal org-agenda-files '("~/org")))
      (should (equal org-refile-targets '((nil :maxlevel . 9)))))))

(defconst wgn-agent-plans-test--statusline-fake
  "#!/bin/sh
# Records its arguments and stdin in $RECORD, then prints $HINT_FILE.
echo \"$@\" >> \"$RECORD.args\"
cat >> \"$RECORD\"
echo >> \"$RECORD\"
cat \"$HINT_FILE\"
"
  "A stand-in for `agent-plan statusline'.")

(defmacro wgn-agent-plans-test--with-statusline (&rest body)
  "Run BODY with a fake `agent-plan statusline' on `exec-path'.
Binds `record' to the file receiving its stdin and `hint' to the file whose
content it prints.  The cache is empty on entry."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "agent-plans-" t)))
          (bin (expand-file-name "bin" root))
          (record (expand-file-name "record" root))
          (hint (expand-file-name "hint" root))
          (dir (file-name-as-directory (expand-file-name "work" root)))
          (exec-path (cons bin exec-path))
          (process-environment
           (append (list (concat "RECORD=" record) (concat "HINT_FILE=" hint))
                   process-environment))
          (wgn/agent-plans-questions-interval 30)
          (wgn/agent-plans-questions--cache (make-hash-table :test #'equal))
          (wgn/agent-plans-questions--string "")
          (wgn/agent-plans-questions--last-directory nil))
     (unwind-protect
         (progn
           (make-directory dir t)
           (wgn-agent-plans-test--write hint "2 open questions · /questions")
           (wgn-agent-plans-test--write
            (expand-file-name "agent-plan" bin)
            wgn-agent-plans-test--statusline-fake)
           (set-file-modes (expand-file-name "agent-plan" bin) #o755)
           ,@body)
       (delete-directory root t))))

(defun wgn-agent-plans-test--statusline (directory)
  "Run the indicator's update in DIRECTORY and wait for its process."
  (with-temp-buffer
    (setq default-directory directory)
    (wgn/agent-plans-questions--update))
  (let ((deadline (+ (float-time) 5)))
    (while (and (< (float-time) deadline)
                (process-list)
                (cl-some (lambda (p)
                           (string-prefix-p "agent-plan-statusline" (process-name p)))
                         (process-list)))
      (accept-process-output nil 0.05))))

(defun wgn-agent-plans-test--runs (record)
  "Return how many times the fake ran, given its RECORD file."
  (let ((file (concat record ".args")))
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (count-lines (point-min) (point-max)))
      0)))

(ert-deftest wgn-agent-plans-questions-indicator-shows-the-hint ()
  (wgn-agent-plans-test--with-statusline
    (wgn-agent-plans-test--statusline dir)
    (should (string-match-p "2 open questions · /questions"
                            wgn/agent-plans-questions--string))
    (should (get-text-property 0 'face wgn/agent-plans-questions--string))
    (should (get-text-property 0 'local-map wgn/agent-plans-questions--string))
    (with-temp-buffer
      (insert-file-contents record)
      (let ((json (json-parse-string (buffer-string))))
        (should (equal (gethash "cwd" json) dir))))
    (with-temp-buffer
      (insert-file-contents (concat record ".args"))
      (should (equal (string-trim (buffer-string)) "statusline")))))

(ert-deftest wgn-agent-plans-questions-indicator-clears-on-empty-output ()
  (wgn-agent-plans-test--with-statusline
    (wgn-agent-plans-test--statusline dir)
    (should-not (string-empty-p wgn/agent-plans-questions--string))
    (wgn-agent-plans-test--write hint "")
    (setq wgn/agent-plans-questions-interval 0)
    (wgn-agent-plans-test--statusline dir)
    (should (equal wgn/agent-plans-questions--string ""))))

(ert-deftest wgn-agent-plans-questions-indicator-is-cached-per-directory ()
  (wgn-agent-plans-test--with-statusline
    (wgn-agent-plans-test--statusline dir)
    (wgn-agent-plans-test--statusline dir)
    (should (= (wgn-agent-plans-test--runs record) 1))
    (should (string-match-p "open questions" wgn/agent-plans-questions--string))
    (let ((other (file-name-as-directory (expand-file-name "other" root))))
      (make-directory other)
      (wgn-agent-plans-test--statusline other)
      (should (= (wgn-agent-plans-test--runs record) 2))
      (wgn-agent-plans-test--statusline dir)
      (should (= (wgn-agent-plans-test--runs record) 2))
      (should (string-match-p "open questions"
                              wgn/agent-plans-questions--string)))))

(ert-deftest wgn-agent-plans-questions-indicator-refreshes-when-stale ()
  (wgn-agent-plans-test--with-statusline
    (wgn-agent-plans-test--statusline dir)
    (setq wgn/agent-plans-questions-interval 0)
    (wgn-agent-plans-test--statusline dir)
    (should (= (wgn-agent-plans-test--runs record) 2))))

(ert-deftest wgn-agent-plans-questions-indicator-skips-remote-directories ()
  (wgn-agent-plans-test--with-statusline
    (wgn-agent-plans-test--statusline dir)
    (wgn-agent-plans-test--statusline "/ssh:nowhere.invalid:/tmp/")
    (should (= (wgn-agent-plans-test--runs record) 1))
    (should (equal wgn/agent-plans-questions--string ""))))

(ert-deftest wgn-agent-plans-questions-indicator-needs-the-executable ()
  (wgn-agent-plans-test--with-statusline
    (let ((exec-path nil)
          (process-environment (cons "PATH=/nonexistent" process-environment)))
      (wgn-agent-plans-test--statusline dir))
    (should (equal wgn/agent-plans-questions--string ""))
    (should (= (wgn-agent-plans-test--runs record) 0))))

(ert-deftest wgn-agent-plans-questions-mode-adds-and-removes-the-indicator ()
  (let ((global-mode-string '("" display-time-string)))
    (unwind-protect
        (progn
          (wgn/agent-plans-questions-mode 1)
          (should (memq 'wgn/agent-plans-questions--string global-mode-string))
          (should (memq 'display-time-string global-mode-string))
          (wgn/agent-plans-questions-mode 1)
          (should (= 1 (cl-count 'wgn/agent-plans-questions--string
                                 global-mode-string)))
          (wgn/agent-plans-questions-mode -1)
          (should-not (memq 'wgn/agent-plans-questions--string
                            global-mode-string))
          (should (memq 'display-time-string global-mode-string)))
      (wgn/agent-plans-questions-mode -1))))

;;; agent-plans.el ends here
