;;; agent-plans.el --- Agent plan browsing tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-agenda)
;; Loaded now, so that `wgn-agent-plans-test--with-plans' binds its
;; variables dynamically.
(require 'org-id)
(require 'autorevert)
;; Loaded now, so that its autoload does not undo a test's stand-in.  It
;; provides no feature to require.
(load "userlock" nil t)
(load (expand-file-name "../lisp/wgn-agent-plans.el"
                        (file-name-directory load-file-name)) nil t)

(defun wgn-agent-plans-test--write (file contents)
  "Write CONTENTS to FILE, making its directory, as agent-plan would.
It is copied into place by cp, since `wgn/agent-plans-protect-mode'
refuses Emacs's own writes and copies onto plan files."
  (make-directory (file-name-directory file) t)
  (let ((temp (make-temp-file "agent-plans-write-")))
    (unwind-protect
        (progn
          (with-temp-file temp (insert contents))
          (unless (eq 0 (call-process "cp" nil nil nil temp file))
            (error "cp %s %s failed" temp file)))
      (delete-file temp))))

(defconst wgn-agent-plans-test--fake
  "#!/bin/sh
# Answers `status --format FORMAT PLAN-FILE' from PLAN-FILE.status-FORMAT,
# and `questions PLAN-FILE ...' from PLAN-FILE.questions or with none.
case \"$1\" in
  status) for last; do :; done; cat \"$last.status-$3\" ;;
  questions) if [ -f \"$2.questions\" ]; then cat \"$2.questions\"; else echo '[]'; fi ;;
  *) exit 2 ;;
esac
"
  "A stand-in for `agent-plan'.")

(defmacro wgn-agent-plans-test--with-plans (&rest body)
  "Run BODY with a plans directory and a fake `agent-plan' under a temp dir.
Binds `root' to the temp dir, whose plans directory is configured through
AGENT_TOOLS_CONFIG, and `agent-plan' is found on `exec-path'.  The IDs
Org records, and saves with every Org buffer, stay under the temp dir
too, out of `user-emacs-directory', which the Nix build sandbox lacks."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "agent-plans-" t)))
          (plans (expand-file-name "plans" root))
          (bin (expand-file-name "bin" root))
          (config (expand-file-name "config.json" root))
          (org-id-locations-file (expand-file-name "org-id-locations" root))
          (org-id-locations nil)
          (org-id-files nil)
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
  ;; Nothing else names it.
  (let ((process-environment
         (cons "AGENT_TOOLS_CONFIG=/nonexistent/config.json" process-environment)))
    (should-not (wgn/agent-plans-directory))))

(ert-deftest wgn-agent-plans-directory-is-only-an-absolute-path ()
  (wgn-agent-plans-test--with-plans
    ;; As agent-plan refuses them: empty, relative to somewhere, or ~.
    (let ((stamp 0))
      (dolist (dir '("" "plans" "~/plans"))
        (wgn-agent-plans-test--write
         config (format "{\"plans\": {\"dir\": %S, \"host\": \"mac\"}}" dir))
        (set-file-times config (time-add (current-time) (cl-incf stamp)))
        (should-not (wgn/agent-plans-directory))))))

(ert-deftest wgn-agent-plans-views-say-the-config-is-missing ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (wgn-agent-plans-test--write config "{\"plans\": {\"host\": \"mac\"}}")
      (let ((missing (format "agent-tools config is missing: %s sets no plans.dir" config))
            (org-agenda-files '("~/org"))
            (org-link-parameters (copy-sequence org-link-parameters))
            (org-execute-file-search-functions nil))
        (wgn/agent-plans-links-setup)
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (error "Prompted"))))
          (should (equal (cadr (should-error (wgn/agent-plans) :type 'user-error)) missing))
          (should (equal (cadr (should-error (org-link-open-from-string "[[plan:2026-10-14-x.mac]]")
                                             :type 'user-error))
                         missing))
          (with-temp-buffer
            (should (equal (cadr (should-error (org-link-open-from-string "[[q:Q1]]")
                                               :type 'user-error))
                           missing))))
        (let ((blocks (unwind-protect (wgn-agent-plans-test--agenda-blocks)
                        (org-agenda-quit))))
          (should (string-match-p (concat "^  " (regexp-quote missing) "$")
                                  (cdr (assoc "Waiting on you" blocks)))))))))

(ert-deftest wgn-agent-plans-config-is-read-once-per-change ()
  (wgn-agent-plans-test--with-plans
    (let ((reads 0))
      (cl-letf* ((parse (symbol-function 'json-parse-buffer))
                 ((symbol-function 'json-parse-buffer)
                  (lambda (&rest args) (setq reads (1+ reads)) (apply parse args))))
        (dotimes (_ 3)
          (wgn/agent-plans-directory)
          (wgn/agent-plans-host))
        (should (= reads 1))
        (wgn-agent-plans-test--write
         config (format "{\"plans\": {\"dir\": %S, \"host\": \"devbox\"}}" plans))
        (set-file-times config (time-add (current-time) 5))
        (should (equal (wgn/agent-plans-host) "devbox"))
        (should (= reads 2))))))

(ert-deftest wgn-agent-plans-config-that-does-not-parse-is-no-config ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((other (expand-file-name "other.txt" root)))
        (wgn-agent-plans-test--write config "{\"plans\": {\"dir\": ")
        (should-not (wgn/agent-plans-directory))
        ;; Every write asks whether it is onto a plan.
        (with-temp-file other (insert "x"))
        (should (file-exists-p other))))))

(ert-deftest wgn-agent-plans-files-are-plan-files-newest-first ()
  (wgn-agent-plans-test--with-plans
    (let ((a (wgn-agent-plans-test--plan plans "mac" "2026-10-01-a" "A" "working" 1 2))
          (b (wgn-agent-plans-test--plan plans "devbox" "2026-10-02-b" "B" "handed-back" 2 2)))
      ;; Parts, logs and the fake's status files sit beside the plans.
      (should (file-exists-p (expand-file-name "2026-10-01-a.mac.P1.org" plans)))
      (should (file-exists-p (expand-file-name "2026-10-01-a.mac.log.org" plans)))
      (should (equal (wgn/agent-plans-files) (list b a))))))

(ert-deftest wgn-agent-plans-files-of-both-names-are-newest-first ()
  (wgn-agent-plans-test--with-plans
    (let ((old (wgn-agent-plans-test--plan plans "mac" "2026-10-14-a" "A" "working" 1 2))
          (new (wgn-agent-plans-test--plan plans "mac" "20261013090000-b" "B" "working" 1 2))
          (newest (wgn-agent-plans-test--plan plans "devbox" "20261015090000-c" "C" "working" 1 2)))
      ;; A date with no time, or a part of one, is neither name.
      (wgn-agent-plans-test--write (expand-file-name "20261016-d.mac.org" plans) "* Plan\n")
      (wgn-agent-plans-test--write (expand-file-name "202610161200-e.mac.org" plans) "* Plan\n")
      ;; By the date in the name, whichever way it is written.
      (should (equal (wgn/agent-plans-files) (list newest old new)))
      ;; With their parts, and never their logs.
      (should (equal (mapcar #'file-name-nondirectory (wgn/agent-plans-agenda-files))
                     '("20261015090000-c.devbox.org" "20261015090000-c.devbox.P1.org"
                       "2026-10-14-a.mac.org" "2026-10-14-a.mac.P1.org"
                       "20261013090000-b.mac.org" "20261013090000-b.mac.P1.org"))))))

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

;;;; Plan files in Org: the blocker, mirrors and reverts

(defconst wgn-agent-plans-test--header
  "#+TODO: TODO STARTED | DONE VERIFIED ABANDONED
#+TODO: ASK | DECIDED(@) DROPPED(@) SUPERSEDED
"
  "The keyword lines `agent-plan new' writes.")

(defun wgn-agent-plans-test--plan-text (title &rest sections)
  "Return a plan file's text titled TITLE with SECTIONS' entries.
SECTIONS alternate a section name and its entries' text."
  (concat "#+title: " title "\n" wgn-agent-plans-test--header "\n* Plan\n"
          (mapconcat (lambda (name)
                       (concat "** " name "\n"
                               (or (plist-get sections (intern (concat ":" name))) "")))
                     '("Goal" "Steps" "Distribution" "Questions" "Follow-ups" "Verification")
                     "")))

(defvar wgn-agent-plans-test--id-property "ID"
  "The property test entries hold their IDs in.
\"ID\" is what `agent-plan' writes; \"CUSTOM_ID\" is the form plans
take as org-roam nodes, which the views read as well.")

(defmacro wgn-agent-plans-test--in-both-forms (name &rest body)
  "Define test NAME running BODY with entries' IDs in ID.
Also NAME-by-custom-id, running BODY with them in CUSTOM_ID."
  (declare (indent 1))
  `(progn
     (ert-deftest ,name ()
       (let ((wgn-agent-plans-test--id-property "ID")) ,@body))
     (ert-deftest ,(intern (concat (symbol-name name) "-by-custom-id")) ()
       (let ((wgn-agent-plans-test--id-property "CUSTOM_ID")) ,@body))))

(defun wgn-agent-plans-test--entry (keyword title id &optional after)
  "Return an entry heading KEYWORD TITLE with ID, waiting for AFTER.
The ID is in `wgn-agent-plans-test--id-property'."
  (concat "*** " keyword " " title "\n:PROPERTIES:\n:" wgn-agent-plans-test--id-property
          ": " id "\n"
          (if after (concat ":AFTER: " after "\n") "")
          ":END:\n"))

(defun wgn-agent-plans-test--id-here ()
  "Return the ID of the entry at point, in either property."
  (or (org-entry-get nil "CUSTOM_ID") (org-entry-get nil "ID")))

(defmacro wgn-agent-plans-test--visiting (file &rest body)
  "Run BODY in a buffer visiting FILE, then kill the buffer unsaved."
  (declare (indent 1))
  `(let ((buffer (find-file-noselect ,file)))
     (unwind-protect
         (with-current-buffer buffer ,@body)
       (with-current-buffer buffer (set-buffer-modified-p nil))
       (kill-buffer buffer))))

(defun wgn-agent-plans-test--goto (id)
  "Move to the heading whose ID is ID."
  (goto-char (point-min))
  (re-search-forward (format "^:\\(?:CUSTOM_\\)?ID: %s$" (regexp-quote id)))
  (org-back-to-heading t))

(defun wgn-agent-plans-test--try (id state)
  "Try to change entry ID to STATE, as the agenda does; return its state."
  (wgn-agent-plans-test--goto id)
  (let ((inhibit-read-only t)
        (org-inhibit-logging t))
    (org-todo state))
  (org-get-todo-state))

(defmacro wgn-agent-plans-test--protected (&rest body)
  "Run BODY with `wgn/agent-plans-protect-mode' on, and off again after."
  (declare (indent 0))
  `(let ((org-blocker-hook nil)
         (find-file-hook nil)
         (org-mode-hook nil)
         (write-file-functions nil)
         (after-revert-hook nil))
     ;; `revert-buffer-restore-functions' exists from Emacs 30.
     (cl-progv (and (boundp 'revert-buffer-restore-functions)
                    '(revert-buffer-restore-functions))
         (and (boundp 'revert-buffer-restore-functions)
              (list revert-buffer-restore-functions))
       (unwind-protect
           (progn (wgn/agent-plans-protect-mode 1) ,@body)
         (wgn/agent-plans-protect-mode -1)))))

(defun wgn-agent-plans-test--waiting-plan (plans &optional name)
  "Make plan NAME.mac under PLANS, with a part P1, and return it.
NAME is 2026-10-14-x unless given.  S2 waits for the part's P1.S1, and
Q2 for Q1."
  (let* ((name (or name "2026-10-14-x"))
         (plan (expand-file-name (concat name ".mac.org") plans)))
    (wgn-agent-plans-test--write
     plan
     (wgn-agent-plans-test--plan-text
      "Waiting"
      :Steps (concat (wgn-agent-plans-test--entry "TODO" "First" "S1")
                     (wgn-agent-plans-test--entry "TODO" "After the part" "S2" "P1.S1"))
      :Distribution (wgn-agent-plans-test--entry "TODO" "Part one" "P1")
      :Questions (concat (wgn-agent-plans-test--entry "ASK" "Keep it?" "Q1")
                         (wgn-agent-plans-test--entry "ASK" "Rename it?" "Q2" "Q1"))))
    (wgn-agent-plans-test--write
     (expand-file-name (concat name ".mac.P1.org") plans)
     (wgn-agent-plans-test--plan-text
      "Part one" :Steps (wgn-agent-plans-test--entry "TODO" "The part's step" "P1.S1")))
    plan))

(ert-deftest wgn-agent-plans-host-follows-agent-plan ()
  (wgn-agent-plans-test--with-plans
    (should (equal (wgn/agent-plans-host) "mac"))
    (wgn-agent-plans-test--write config (format "{\"plans\": {\"dir\": %S}}" plans))
    (should (equal (wgn/agent-plans-host)
                   (car (split-string (system-name) "\\."))))))

(wgn-agent-plans-test--in-both-forms wgn-agent-plans-blocker-waits-for-after-items
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--waiting-plan plans)))
        ;; Each buffer opened for a deliberate edit.
        (wgn-agent-plans-test--visiting plan
          (read-only-mode -1)
          (should (equal (wgn-agent-plans-test--try "S2" "DONE") "TODO"))
          (should (equal (wgn-agent-plans-test--try "Q2" "DECIDED") "ASK"))
          (should (equal (wgn-agent-plans-test--try "S2" "STARTED") "STARTED"))
          (should (equal (wgn-agent-plans-test--try "Q1" "DECIDED") "DECIDED"))
          (should (equal (wgn-agent-plans-test--try "Q2" "DECIDED") "DECIDED")))
        ;; The part's step, settled in its own file, unblocks S2.
        (wgn-agent-plans-test--visiting (expand-file-name "2026-10-14-x.mac.P1.org" plans)
          (read-only-mode -1)
          (should (equal (wgn-agent-plans-test--try "P1.S1" "ABANDONED") "ABANDONED"))
          (save-buffer))
        (wgn-agent-plans-test--visiting plan
          (read-only-mode -1)
          (should (equal (wgn-agent-plans-test--try "S2" "DONE") "DONE")))))))

(ert-deftest wgn-agent-plans-blocker-leaves-other-files-alone ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((other (expand-file-name "notes.org" root)))
        (wgn-agent-plans-test--write
         other (concat "* TODO Waits for nothing here\n:PROPERTIES:\n:ID: S1\n:AFTER: S9\n:END:\n"))
        (wgn-agent-plans-test--visiting other
          (should (equal (wgn-agent-plans-test--try "S1" "DONE") "DONE")))))))

(ert-deftest wgn-agent-plans-mirrors-are-read-only ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let* ((mirror (expand-file-name "2026-10-14-y.devbox.org" plans))
             (text (wgn-agent-plans-test--plan-text
                    "Theirs"
                    :Steps (wgn-agent-plans-test--entry "TODO" "Their step" "S1")
                    :Questions (wgn-agent-plans-test--entry "ASK" "Their question?" "Q1"))))
        (wgn-agent-plans-test--write mirror text)
        (wgn-agent-plans-test--visiting mirror
          (should buffer-read-only)
          (should (equal (wgn-agent-plans-test--try "Q1" "DECIDED") "ASK"))
          (should (equal (wgn-agent-plans-test--try "S1" "STARTED") "TODO"))
          (goto-char (point-max))
          (should-error (let ((inhibit-read-only t)) (insert "edited\n")) :type 'user-error)
          (let ((inhibit-read-only t) (inhibit-modification-hooks t))
            (insert "edited\n"))
          (should-error (save-buffer) :type 'user-error))
        (should (equal (with-temp-buffer (insert-file-contents mirror) (buffer-string))
                       text))))))

(ert-deftest wgn-agent-plans-plan-files-open-read-only-naming-the-commands ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--waiting-plan plans))
            (log (expand-file-name "2026-10-14-x.mac.log.org" plans))
            (mirror (expand-file-name "2026-10-14-y.devbox.org" plans))
            (other (expand-file-name "notes.org" root)))
        (wgn-agent-plans-test--write log "* done S1\n")
        (wgn-agent-plans-test--write mirror "* Plan\n")
        (wgn-agent-plans-test--write other "* Notes\n")
        (dolist (file (list plan (expand-file-name "2026-10-14-x.mac.P1.org" plans)))
          (wgn-agent-plans-test--visiting file
            (should buffer-read-only)
            (should (string-match-p "answer questions with /questions in a Claude session"
                                    header-line-format))
            ;; agent-plan rewrote it.
            (revert-buffer t t t)
            (should buffer-read-only)
            (should (string-match-p "answer questions with /questions in a Claude session"
                                    header-line-format))))
        (wgn-agent-plans-test--visiting log
          (should buffer-read-only)
          (should (string-match-p "agent-plan log alone" header-line-format)))
        (wgn-agent-plans-test--visiting mirror
          (should buffer-read-only)
          (should (string-match-p "devbox's plan, written there" header-line-format)))
        (wgn-agent-plans-test--visiting other
          (should-not buffer-read-only)
          (should-not header-line-format))))))

;; The operator's Emacs reverts a buffer agent-plan rewrote, and the rewrite may
;; change the file's keyword lines.
(ert-deftest wgn-agent-plans-revert-reads-new-keywords ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (expand-file-name "2026-10-14-x.mac.org" plans)))
        (wgn-agent-plans-test--write
         plan "#+TODO: TODO NEXT | DONE\n* Plan\n** Steps\n*** TODO Step\n:PROPERTIES:\n:ID: S1\n:END:\n")
        (wgn-agent-plans-test--visiting plan
          (should-not (member "VERIFIED" org-todo-keywords-1))
          (wgn-agent-plans-test--write
           plan (wgn-agent-plans-test--plan-text
                 "Rewritten" :Steps (wgn-agent-plans-test--entry "VERIFIED" "Step" "S1")))
          ;; As auto-revert does: the buffer keeps its modes.
          (revert-buffer t t t)
          (should (member "VERIFIED" org-todo-keywords-1))
          (wgn-agent-plans-test--goto "S1")
          (should (equal (org-get-todo-state) "VERIFIED"))
          ;; A second revert is handled as the first.
          (wgn-agent-plans-test--write
           plan (wgn-agent-plans-test--plan-text
                 "Rewritten" :Questions (wgn-agent-plans-test--entry "ASK" "Keep it?" "Q1")))
          (revert-buffer t t t)
          (wgn-agent-plans-test--goto "Q1")
          (should (equal (org-get-todo-state) "ASK")))))))

;;;; The A view

(defun wgn-agent-plans-test--sign-event (kind sign &optional branches worktree base)
  "Return a log event of KIND for sign-pass SIGN, as agent-plan logs it.
Its pass signs BRANCHES, comma-separated, in WORKTREE from BASE."
  (concat "* " kind " 2026-10-14-x.mac — " sign "\n:PROPERTIES:\n:EVENT: e-" kind "-" sign
          "\n:ITEM: 2026-10-14-x.mac\n:KIND: " kind "\n"
          (if branches (concat ":BRANCH: " branches "\n") "")
          (if worktree (concat ":WORKTREE: " worktree "\n") "")
          (if base (concat ":BASE: " base "\n") "")
          ":SIGN: " sign "\n:END:\n[2026-10-14 Tue 12:00]\n"))

(defun wgn-agent-plans-test--agenda-plans (plans)
  "Make an own plan, its part and another host's plan under PLANS.
Return the other host's plan file."
  (wgn-agent-plans-test--write
   (expand-file-name "2026-10-14-x.mac.org" plans)
   (wgn-agent-plans-test--plan-text
    "Ours"
    :Steps (concat (wgn-agent-plans-test--entry "TODO" "First step" "S1")
                   (wgn-agent-plans-test--entry "TODO" "After the first" "S2" "S1")
                   "*** TODO Postponed step\nSCHEDULED: <2099-01-01 Thu>\n:PROPERTIES:\n:"
                   wgn-agent-plans-test--id-property ": S3\n:END:\n"
                   (wgn-agent-plans-test--entry "DONE" "Finished step" "S4"))
    :Distribution (wgn-agent-plans-test--entry "TODO" "Part one" "P1")
    :Questions (concat (wgn-agent-plans-test--entry "ASK" "Keep it?" "Q1")
                       (wgn-agent-plans-test--entry "ASK" "Rename it?" "Q2" "Q1")
                       (wgn-agent-plans-test--entry "DECIDED" "Settled question?" "Q3"))
    :Follow-ups (wgn-agent-plans-test--entry "TODO" "Guard accepts --draft=false" "F1")))
  (wgn-agent-plans-test--write
   (expand-file-name "2026-10-14-x.mac.P1.org" plans)
   (wgn-agent-plans-test--plan-text
    "Part one" :Steps (wgn-agent-plans-test--entry "TODO" "The part's step" "P1.S1")))
  ;; As agent-plan questions gives them: Q2 waits for Q1.
  (wgn-agent-plans-test--write
   (expand-file-name "2026-10-14-x.mac.org.questions" plans)
   (format "[{\"id\": \"Q1\", \"file\": %S, \"question\": \"Keep it?\", \"recommendation\": \"yes\"}]"
           (expand-file-name "2026-10-14-x.mac.org" plans)))
  (wgn-agent-plans-test--write
   (expand-file-name "2026-10-14-x.mac.log.org" plans)
   (concat "* TODO Not an entry\n:PROPERTIES:\n:ID: S9\n:END:\n"
           (wgn-agent-plans-test--sign-event "sign-requested" "2026-10-14-x.mac.0000aaaa" "signed-one")
           (wgn-agent-plans-test--sign-event "signed" "2026-10-14-x.mac.0000aaaa")
           (wgn-agent-plans-test--sign-event "sign-requested" "2026-10-14-x.mac.0000bbbb"
                                             "pending-one,pending-two")))
  (let ((mirror (expand-file-name "2026-10-13-y.devbox.org" plans)))
    (wgn-agent-plans-test--write
     mirror
     (wgn-agent-plans-test--plan-text
      "Theirs"
      :Steps (wgn-agent-plans-test--entry "TODO" "Their step" "S1")
      :Questions (wgn-agent-plans-test--entry "ASK" "Their question?" "Q1")))
    (wgn-agent-plans-test--write
     (concat mirror ".questions")
     (format "[{\"id\": \"Q1\", \"file\": %S, \"question\": \"Their question?\", \"recommendation\": \"no\"}]"
             mirror))
    mirror))

(defun wgn-agent-plans-test--agenda-blocks ()
  "Build the A view and return its visible lines, by block header.
An entry hidden as blocked stays in the buffer as invisible text, so a
line counts only when its start is visible."
  (let ((org-agenda-custom-commands (list (wgn/agent-plans-agenda-command)))
        (org-agenda-sticky nil)
        blocks)
    (org-agenda nil "A")
    (with-current-buffer org-agenda-buffer
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (cond
           ((invisible-p (line-beginning-position)))
           ((member line '("Waiting on you" "Ready" "Follow-ups"))
            (push (list line) blocks))
           (blocks (push line (cdar blocks)))))
        (forward-line 1)))
    (mapcar (lambda (block) (cons (car block) (string-join (cdr block) "\n"))) blocks)))

(wgn-agent-plans-test--in-both-forms wgn-agent-plans-agenda-lists-what-is-ready
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (wgn-agent-plans-test--agenda-plans plans)
      (let* ((org-agenda-files '("~/org"))
             (org-refile-targets '((nil :maxlevel . 9)))
             (blocks (unwind-protect (wgn-agent-plans-test--agenda-blocks)
                       (org-agenda-quit)))
             (waiting (or (cdr (assoc "Waiting on you" blocks)) ""))
             (ready (or (cdr (assoc "Ready" blocks)) ""))
             (followups (or (cdr (assoc "Follow-ups" blocks)) "")))
        (should (string-match-p "^  2026-10-14-x\\.mac Q1 Keep it\\?  (recommended: yes)$" waiting))
        (should (string-match-p
                 "^    answer it with /questions in a Claude session on 2026-10-14-x\\.mac$" waiting))
        ;; Another host's question is answered there.
        (should-not (string-match-p "Their question" waiting))
        (should (string-match-p
                 "^    git sign-pass --notify 2026-10-14-x\\.mac\\.0000bbbb pending-one pending-two$"
                 waiting))
        (should (string-match-p
                 "^  2026-10-14-x\\.mac 2026-10-14-x\\.mac\\.0000bbbb sign-pass of pending-one pending-two, in a worktree its event does not name$"
                 waiting))
        ;; That pass has its result.
        (should-not (string-match-p "0000aaaa" waiting))
        (dolist (title '("First step" "Part one" "The part's step" "Their step"))
          (should (string-match-p (regexp-quote title) ready)))
        ;; Waits for S1, scheduled for later, done, and in a log.
        (should-not (string-match-p "After the first\\|Postponed step\\|Finished step\\|Not an entry" ready))
        (should (string-match-p "Guard accepts --draft=false" followups))
        (should-not (string-match-p "step" followups))
        (should (equal org-agenda-files '("~/org")))
        (should (equal org-refile-targets '((nil :maxlevel . 9))))))))

(ert-deftest wgn-agent-plans-agenda-refuses-changes-to-mirrors ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let* ((mirror (wgn-agent-plans-test--agenda-plans plans))
             (before (with-temp-buffer (insert-file-contents mirror) (buffer-string)))
             (org-agenda-custom-commands (list (wgn/agent-plans-agenda-command))))
        (unwind-protect
            (progn
              (org-agenda nil "A")
              (with-current-buffer org-agenda-buffer
                (goto-char (point-min))
                (re-search-forward "Their step")
                (ignore-errors (org-agenda-todo "DONE"))
                (ignore-errors (org-agenda-priority ?A)))
              (with-current-buffer (find-buffer-visiting mirror)
                (wgn-agent-plans-test--goto "S1")
                (should (equal (org-get-todo-state) "TODO"))
                (should-not (buffer-modified-p))
                ;; Even after a deliberate edit.
                (read-only-mode -1)
                (should-error (insert "edited") :type 'user-error)
                (let ((inhibit-modification-hooks t)) (insert "edited"))
                (should-error (save-buffer) :type 'user-error)))
          (org-agenda-quit)
          (let ((buffer (find-buffer-visiting mirror)))
            (when buffer
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))
        (should (equal (with-temp-buffer (insert-file-contents mirror) (buffer-string))
                       before))))))

(defconst wgn-agent-plans-test--agenda-edits
  '((org-agenda-todo "DONE")
    (org-agenda-todo-nextset)
    (org-agenda-priority ?A)
    (org-agenda-priority-up)
    (org-agenda-set-tags "x" on)
    (org-agenda-schedule nil "2099-01-02")
    (org-agenda-deadline nil "2099-01-03")
    (org-agenda-set-property)
    (org-agenda-toggle-archive-tag)
    (org-agenda-add-note))
  "Agenda commands that would write the entry at point, with arguments.")

(ert-deftest wgn-agent-plans-agenda-refuses-every-edit-of-a-plan ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (wgn-agent-plans-test--agenda-plans plans)
      (let* ((plan (expand-file-name "2026-10-14-x.mac.org" plans))
             (before (with-temp-buffer (insert-file-contents plan) (buffer-string)))
             (org-agenda-custom-commands (list (wgn/agent-plans-agenda-command)))
             (org-agenda-sticky nil))
        (unwind-protect
            (progn
              (org-agenda nil "A")
              (with-current-buffer org-agenda-buffer
                (dolist (edit wgn-agent-plans-test--agenda-edits)
                  (goto-char (point-min))
                  (re-search-forward "First step")
                  (should (equal (list (car edit)
                                       (car (should-error (apply (car edit) (cdr edit))
                                                          :type 'user-error)))
                                 (list (car edit) 'user-error))))
                ;; A question decides only through agent-plan.
                (goto-char (point-min))
                (re-search-forward "Keep it\\?")
                (should-error (org-agenda-todo "DECIDED")))
              (with-current-buffer (find-buffer-visiting plan)
                (should (equal (buffer-string) before))
                (should-not (buffer-modified-p))
                ;; Even when modified past its change functions, nothing
                ;; writes it but agent-plan.
                (let ((inhibit-read-only t) (inhibit-modification-hooks t))
                  (goto-char (point-max)) (insert "x"))
                (should-error (save-buffer) :type 'user-error)
                ;; Saving every buffer skips it, still modified.
                (org-save-all-org-buffers)
                (should (buffer-modified-p))
                (set-buffer-modified-p nil)
                (revert-buffer t t t))
              (should (equal (with-temp-buffer (insert-file-contents plan) (buffer-string))
                             before))
              ;; C-x C-q is the deliberate edit.
              (with-current-buffer (find-buffer-visiting plan)
                (read-only-mode -1))
              (with-current-buffer org-agenda-buffer
                (goto-char (point-min))
                (re-search-forward "First step")
                (let ((org-inhibit-logging t)) (org-agenda-todo "DONE")))
              (with-current-buffer (find-buffer-visiting plan)
                (wgn-agent-plans-test--goto "S1")
                (should (equal (org-get-todo-state) "DONE"))
                (save-buffer)))
          (org-agenda-quit)
          (let ((buffer (find-buffer-visiting plan)))
            (when buffer
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))
        (should (string-match-p "^\\*\\*\\* DONE First step$"
                                (with-temp-buffer (insert-file-contents plan) (buffer-string))))))))

(ert-deftest wgn-agent-plans-plan-buffer-refuses-edits-in-place ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--waiting-plan plans)))
        (wgn-agent-plans-test--visiting plan
          ;; As code that binds `inhibit-read-only' would.
          (should (equal (wgn-agent-plans-test--try "S1" "DONE") "TODO"))
          ;; A mode restart keeps the refusal.
          (org-mode)
          (should buffer-read-only)
          (should header-line-format)
          (wgn-agent-plans-test--goto "S1")
          (let ((before (buffer-string))
                (inhibit-read-only t))
            (dolist (edit (list (lambda () (org-entry-put nil "X" "1"))
                                (lambda () (org-set-property "X" "1"))
                                (lambda () (org-id-get-create t))
                                (lambda () (goto-char (point-max)) (insert "x"))))
              (should-error (funcall edit) :type 'user-error)
              ;; Emacs drops a change function that signals; it is put back.
              (accept-process-output nil 0.05)
              (should (equal (buffer-string) before))))
          ;; Folding is no edit.
          (org-fold-show-all)
          (org-cycle-overview)
          (should-not (buffer-modified-p))
          ;; Past every change function, the save is still refused.
          (let ((inhibit-read-only t) (inhibit-modification-hooks t))
            (goto-char (point-max)) (insert "x"))
          (should-error (save-buffer) :type 'user-error)
          (set-buffer-modified-p nil)
          ;; Opened for a deliberate edit, it changes and saves.
          (revert-buffer t t t)
          (read-only-mode -1)
          (wgn-agent-plans-test--goto "S1")
          (org-entry-put nil "X" "1")
          (save-buffer)
          (should (string-match-p "^:X: +1$" (with-temp-buffer (insert-file-contents plan)
                                                               (buffer-string)))))))))

(ert-deftest wgn-agent-plans-plan-buffer-locks-again-after-a-revert ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--waiting-plan plans)))
        (wgn-agent-plans-test--visiting plan
          ;; Opened for a deliberate edit, then agent-plan rewrote it and
          ;; auto-revert, which binds `buffer-read-only' around its revert,
          ;; reverted it.
          (read-only-mode -1)
          (wgn-agent-plans-test--write plan (wgn-agent-plans-test--plan-text "Rewritten"))
          (set-file-times plan (time-add (current-time) 5))
          (let ((auto-revert-use-notify nil))
            (auto-revert-mode 1)
            (auto-revert-handler)
            (auto-revert-mode -1))
          (should (string-match-p "Rewritten" (buffer-string)))
          (should buffer-read-only)
          (should-error (let ((inhibit-read-only t)) (insert "x")) :type 'user-error)
          ;; auto-revert finding nothing new leaves a deliberate edit open.
          (accept-process-output nil 0.05)
          (read-only-mode -1)
          (let ((auto-revert-use-notify nil))
            (auto-revert-mode 1)
            (auto-revert-handler)
            (auto-revert-mode -1))
          (should-not buffer-read-only)
          ;; So does a plain revert.
          (revert-buffer t t)
          (should buffer-read-only))))))

(ert-deftest wgn-agent-plans-plan-buffer-stays-under-global-auto-revert ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--waiting-plan plans)))
        (wgn-agent-plans-test--visiting plan
          ;; As `global-auto-revert-mode' marks the buffers it reverts.
          (setq-local auto-revert--global-mode t)
          (dolist (title '("Once" "Twice"))
            (wgn-agent-plans-test--write plan (wgn-agent-plans-test--plan-text title))
            (set-file-times plan (time-add (current-time) (if (equal title "Once") 5 10)))
            (let ((auto-revert-use-notify nil))
              (auto-revert-handler))
            (should (string-match-p title (buffer-string)))
            (should auto-revert--global-mode)))))))

(ert-deftest wgn-agent-plans-plan-buffer-guard-keeps-the-other-change-functions ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--waiting-plan plans)))
        (wgn-agent-plans-test--visiting plan
          (add-hook 'before-change-functions #'ignore nil t)
          (let ((before before-change-functions))
            (should-error (let ((inhibit-read-only t)) (insert "x")) :type 'user-error)
            (accept-process-output nil 0.05)
            (should (equal before-change-functions before)))
          ;; Nor does a refused change leave a lock file in the plans directory.
          (should-not (directory-files plans nil "\\`\\.#")))))))

(ert-deftest wgn-agent-plans-plan-buffer-locks-the-file-while-it-is-edited ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--waiting-plan plans))
            (mirror (expand-file-name "2026-10-14-y.devbox.org" plans))
            (create-lockfiles t))
        (wgn-agent-plans-test--write mirror "* Plan\n")
        (wgn-agent-plans-test--visiting plan
          (read-only-mode -1)
          (goto-char (point-max))
          (insert "edited\n")
          ;; agent-plan refuses to write while the lock file is there.
          (should (file-locked-p plan))
          (save-buffer)
          (read-only-mode 1)
          (should-error (let ((inhibit-read-only t)) (insert "x")) :type 'user-error)
          (should-not (file-locked-p plan)))
        ;; A mirror refuses changes however it is opened.
        (wgn-agent-plans-test--visiting mirror
          (read-only-mode -1)
          (should-error (insert "x") :type 'user-error)
          (should-not (file-locked-p mirror)))))))

(ert-deftest wgn-agent-plans-plan-buffer-settles-its-changes-before-it-locks ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--waiting-plan plans))
            answer asked)
        (cl-letf (((symbol-function 'read-multiple-choice)
                   (lambda (prompt &rest _)
                     (push prompt asked)
                     (if answer (list answer) (signal 'quit nil)))))
          (wgn-agent-plans-test--visiting plan
            (read-only-mode -1)
            (goto-char (point-max))
            (insert "edited\n")
            ;; C-x C-q with neither choice: it stays open, edits kept.
            (read-only-mode 1)
            (should-not buffer-read-only)
            (should (buffer-modified-p))
            ;; Saved, then locked.
            (setq answer ?s)
            (read-only-mode 1)
            (should buffer-read-only)
            (should-not (buffer-modified-p))
            (should (string-match-p "edited" (with-temp-buffer (insert-file-contents plan)
                                                                (buffer-string))))
            ;; A mode restart discarding the changes.
            (read-only-mode -1)
            (insert "discarded edit\n")
            (setq answer ?d)
            (org-mode)
            (should buffer-read-only)
            (should-not (buffer-modified-p))
            (should-not (string-match-p "discarded edit" (buffer-string)))
            (should (= (length asked) 3))))))))

(ert-deftest wgn-agent-plans-plan-buffer-keeps-its-guard-when-left-unlocked ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--waiting-plan plans)))
        (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) (signal 'quit nil))))
          (wgn-agent-plans-test--visiting plan
            (read-only-mode -1)
            (goto-char (point-max))
            (insert "edited\n")
            ;; A mode restart the operator does not let lock it.
            (org-mode)
            (should-not buffer-read-only)
            (should header-line-format)
            (should (memq #'wgn/agent-plans--guard-change before-change-functions))
            (save-buffer)
            (read-only-mode 1)
            (should buffer-read-only)
            (should header-line-format)
            (wgn-agent-plans-test--goto "S1")
            (should-error (org-set-property "X" "1") :type 'user-error)
            (should-not (buffer-modified-p))))))))

(ert-deftest wgn-agent-plans-plan-buffer-stays-unlocked-when-its-save-fails ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--waiting-plan plans)))
        (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) (list ?s)))
                  ;; "changed since visited or saved.  Save anyway?"
                  ((symbol-function 'yes-or-no-p) #'ignore))
          (wgn-agent-plans-test--visiting plan
            (read-only-mode -1)
            (goto-char (point-max))
            (insert "edited\n")
            ;; agent-plan wrote the file meanwhile.
            (wgn-agent-plans-test--write plan (wgn-agent-plans-test--plan-text "Rewritten"))
            (set-file-times plan (time-add (current-time) 5))
            (read-only-mode 1)
            (should-not buffer-read-only)
            (should (buffer-modified-p))
            ;; A mode restart, too, leaves a buffer it cannot save unlocked.
            (let ((inhibit-read-only t) (inhibit-modification-hooks t))
              (setq buffer-read-only t))
            (org-mode)
            (should-not buffer-read-only)
            (should (buffer-modified-p))
            (should (string-match-p "Rewritten" (with-temp-buffer (insert-file-contents plan)
                                                                   (buffer-string))))))))))

(ert-deftest wgn-agent-plans-plan-buffer-locks-its-indirect-buffers ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--waiting-plan plans)))
        (wgn-agent-plans-test--visiting plan
          (let ((base (current-buffer))
                (indirect (make-indirect-buffer (current-buffer) "plan indirect")))
            (unwind-protect
                (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) (list ?s))))
                  (with-current-buffer indirect
                    (should buffer-read-only)
                    (should header-line-format)
                    (should-error (let ((inhibit-read-only t)) (insert "x")) :type 'user-error)
                    ;; Its lock is its base's.
                    (read-only-mode -1)
                    (should-not (buffer-local-value 'buffer-read-only base))
                    (goto-char (point-max))
                    (insert "edited\n")
                    (read-only-mode 1)
                    (should (buffer-local-value 'buffer-read-only base))
                    (should-not (buffer-modified-p base)))
                  (should (string-match-p "edited" (with-temp-buffer (insert-file-contents plan)
                                                                      (buffer-string))))
                  (read-only-mode -1)
                  (should-not (buffer-local-value 'buffer-read-only indirect))
                  (let ((another (make-indirect-buffer base "plan indirect 2")))
                    (should-not (buffer-local-value 'buffer-read-only another))
                    (kill-buffer another))
                  (read-only-mode 1)
                  (should (buffer-local-value 'buffer-read-only indirect)))
              (kill-buffer indirect)))
          (should-not (buffer-modified-p)))))))

(ert-deftest wgn-agent-plans-plan-buffer-keeps-its-folds-across-a-revert ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--waiting-plan plans)))
        (wgn-agent-plans-test--visiting plan
          (org-fold-show-all)
          (wgn-agent-plans-test--goto "S1")
          (org-fold-subtree t)
          (let ((folded (point)))
            (should (org-fold-folded-p (line-end-position)))
            (revert-buffer t t t)
            (goto-char folded)
            (should (org-fold-folded-p (line-end-position)))
            (wgn-agent-plans-test--goto "Q1")
            (should-not (org-fold-folded-p (line-end-position)))))))))

(ert-deftest wgn-agent-plans-old-revisions-of-a-plan-are-left-alone ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--waiting-plan plans)))
        ;; As magit's blob buffers set their mode.
        (with-temp-buffer
          (insert "* Plan\n")
          (let ((buffer-file-name plan))
            (org-mode))
          (should-not buffer-read-only)
          (should-not header-line-format))))))

(ert-deftest wgn-agent-plans-protect-mode-off-leaves-plan-buffers-plain ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--waiting-plan plans)))
        (wgn-agent-plans-test--visiting plan
          (should header-line-format)
          (wgn/agent-plans-protect-mode -1)
          (should-not header-line-format)
          (should-not (memq #'wgn/agent-plans--guard-change before-change-functions))
          (read-only-mode -1)
          (insert "x")
          (set-buffer-modified-p nil)
          ;; Turned on again, it protects the plan buffers there are,
          ;; leaving one with unsaved changes unlocked without asking.
          (wgn-agent-plans-test--visiting (expand-file-name "2026-10-14-x.mac.P1.org" plans)
            (should-not header-line-format)
            (read-only-mode -1)
            (insert "x")
            (let ((part (current-buffer)))
              (with-temp-buffer
                (wgn/agent-plans-protect-mode 1))
              (should (memq #'wgn/agent-plans--guard-change before-change-functions))
              (should header-line-format)
              (should-not buffer-read-only)
              (set-buffer-modified-p nil)
              (should (eq part (current-buffer)))))
          (should buffer-read-only)
          (should header-line-format)
          (should-error (let ((inhibit-read-only t)) (insert "x")) :type 'user-error))))))

(ert-deftest wgn-agent-plans-plan-buffer-refuses-to-move-entries-out ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let* ((plan (wgn-agent-plans-test--waiting-plan plans))
             (other (expand-file-name "other.org" root))
             (before (with-temp-buffer (insert-file-contents plan) (buffer-string)))
             (org-archive-location (concat other "::* Archive"))
             (org-refile-targets nil))
        (wgn-agent-plans-test--write other "* Archive\n")
        (wgn-agent-plans-test--visiting plan
          (wgn-agent-plans-test--goto "S1")
          ;; Each would write the entry into the other file first.
          (should-error (org-archive-subtree) :type 'user-error)
          (should-error (org-refile nil nil (list "Archive" other nil 1)) :type 'user-error)
          ;; Copying and jumping leave the plan as it is.
          (org-refile 3 nil (list "Archive" other nil 1))
          (with-current-buffer (find-buffer-visiting other)
            (set-buffer-modified-p nil)
            (kill-buffer)))
        (should (equal (with-temp-buffer (insert-file-contents plan) (buffer-string)) before))
        (should (equal (with-temp-buffer (insert-file-contents other) (buffer-string)) "* Archive\n"))))))

(ert-deftest wgn-agent-plans-locked-buffer-refuses-to-write-its-own-file ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let* ((plan (wgn-agent-plans-test--waiting-plan plans))
             (log (expand-file-name "2026-10-14-x.mac.log.org" plans))
             (mirror (expand-file-name "2026-10-14-y.devbox.org" plans))
             (files (list plan (expand-file-name "2026-10-14-x.mac.P1.org" plans) log mirror))
             (other (expand-file-name "other.txt" root))
             (create-lockfiles t)
             buffers)
        (wgn-agent-plans-test--write log "* done S1\n")
        (wgn-agent-plans-test--write mirror "* Plan\n")
        (unwind-protect
            (progn
              (dolist (file files)
                (let* ((buffer (find-file-noselect file))
                       (written (concat (with-temp-buffer (insert-file-contents file) (buffer-string))
                                        "* Written by agent-plan\n")))
                  (push buffer buffers)
                  (with-current-buffer buffer
                    (let ((name (buffer-name))
                          (modtime (visited-file-modtime))
                          (header header-line-format))
                      (should buffer-read-only)
                      (wgn-agent-plans-test--write file written)
                      (set-file-times file (time-add (current-time) 5))
                      ;; C-x C-w RET, its directory or no name at all is
                      ;; a write onto its own file, which stays agent-plan's.
                      (dolist (target (list file (file-name-directory file) ""))
                        (should-error (write-file target) :type 'user-error))
                      (should (equal buffer-file-name file))
                      (should (equal (buffer-name) name))
                      (should buffer-read-only)
                      (should-not (buffer-modified-p))
                      (should (equal (visited-file-modtime) modtime))
                      (should (equal header-line-format header))
                      (should-error (insert "x"))
                      (should-not (file-locked-p file))
                      (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
                                     written))))))
              ;; None is left modified, which would stop every save.
              (wgn-agent-plans-test--visiting other
                (insert "unsaved\n")
                (save-some-buffers t)
                (should-not (buffer-modified-p)))
              ;; Unlocked for a deliberate edit, a plan's buffer writes its file.
              (with-current-buffer (find-buffer-visiting plan)
                (revert-buffer t t t)
                (read-only-mode -1)
                (write-file plan)
                (should-not buffer-read-only)
                (should-not (buffer-modified-p))))
          (dolist (buffer buffers)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)))))))

(ert-deftest wgn-agent-plans-plan-paths-refuse-a-buffer-taking-their-name ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let* ((plan (wgn-agent-plans-test--waiting-plan plans))
             (log (expand-file-name "2026-10-14-x.mac.log.org" plans))
             (new (expand-file-name "2026-10-14-z.mac.org" plans))
             (other (expand-file-name "other.txt" root))
             (before (with-temp-buffer (insert-file-contents plan) (buffer-string)))
             (create-lockfiles t)
             (buffer (generate-new-buffer "unsaved")))
        (wgn-agent-plans-test--write log "* done S1\n")
        (unwind-protect
            (with-current-buffer buffer
              (insert "* Plan\n")
              (dolist (target (list plan (expand-file-name "2026-10-14-x.mac.P1.org" plans) log new))
                ;; C-x C-s in a buffer visiting no file asks for a file
                ;; for it to visit, as these two are given one.
                (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) target))
                          ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                  (should-error (save-buffer) :type 'user-error))
                (should-error (set-visited-file-name target) :type 'user-error)
                (should-error (rename-visited-file target) :type 'user-error)
                (should-not buffer-file-name)
                (should-not buffer-file-number)
                (should-not buffer-auto-save-file-name)
                (should (eq major-mode 'fundamental-mode))
                (should (equal (buffer-name) "unsaved"))
                (should (buffer-modified-p))
                (do-auto-save t)
                (should-not (directory-files plans nil "\\`[.#]#")))
              ;; The plan is visited in a buffer of its own, and no
              ;; buffer is left that would stop every save.
              (let ((visiting (find-file-noselect plan)))
                (unwind-protect
                    (progn
                      (should-not (eq visiting buffer))
                      (should (buffer-local-value 'buffer-read-only visiting)))
                  (kill-buffer visiting)))
              (wgn-agent-plans-test--visiting other
                (insert "unsaved\n")
                (save-some-buffers t)
                (should-not (buffer-modified-p))))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))
        (should (equal (with-temp-buffer (insert-file-contents plan) (buffer-string)) before))
        (should-not (file-exists-p new))
        (should (equal (with-temp-buffer (insert-file-contents log) (buffer-string)) "* done S1\n"))))))

(defun wgn-agent-plans-test--contents (file)
  "Return the contents of FILE."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun wgn-agent-plans-test--agent-writes (file)
  "Add an entry to FILE as `agent-plan' does, unless a lock file refuses it.
Return non-nil when it wrote."
  (unless (file-locked-p file)
    (wgn-agent-plans-test--write
     file (concat (wgn-agent-plans-test--contents file) "* Written by agent-plan\n"))
    (set-file-times file (time-add (current-time) 5))
    t))

(defun wgn-agent-plans-test--kept-p (file wrote)
  "Return non-nil when FILE keeps `agent-plan''s entry, or WROTE is nil."
  (or (not wrote)
      (string-match-p "^\\* Written by agent-plan$" (wgn-agent-plans-test--contents file))))

(defmacro wgn-agent-plans-test--confirming (&rest body)
  "Run BODY answering yes to every prompt, as an operator pressing on would."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
             ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
             ((symbol-function 'ask-user-about-supersession-threat) #'ignore))
     ,@body))

(defun wgn-agent-plans-test--kill-visiting (&rest files)
  "Kill every buffer visiting one of FILES, unsaved."
  (dolist (buffer (buffer-list))
    (when (member (buffer-file-name buffer) files)
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer))))

(ert-deftest wgn-agent-plans-plan-buffer-saves-only-over-what-it-read ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (expand-file-name "2026-10-14-x.mac.org" plans))
            (create-lockfiles t))
        (wgn-agent-plans-test--write
         plan (wgn-agent-plans-test--plan-text
               "Café · naïve"
               :Steps (wgn-agent-plans-test--entry "TODO" "Résumé" "S1")))
        (wgn-agent-plans-test--confirming
          (wgn-agent-plans-test--visiting plan
            ;; Unlocked but unchanged, it holds no lock file, so agent-plan writes.
            (read-only-mode -1)
            (should (wgn-agent-plans-test--agent-writes plan))
            (goto-char (point-max))
            (insert "* Operator's edit\n")
            (let ((refusal (cadr (should-error (save-buffer) :type 'user-error))))
              (should (string-match-p "revert-buffer" refusal)))
            (should (buffer-modified-p))
            (should (string-match-p "Operator's edit" (buffer-string)))
            (should (wgn-agent-plans-test--kept-p plan t))
            ;; Nor by a precious save, or a write of the region.
            (let ((file-precious-flag t))
              (should-error (save-buffer) :type 'user-error))
            (should-error (write-region nil nil plan) :type 'user-error)
            (should (wgn-agent-plans-test--kept-p plan t))
            ;; Read again, it saves, and saves again over what it wrote.
            (revert-buffer t t t)
            (read-only-mode -1)
            (goto-char (point-max))
            (insert "* Operator's edit\n")
            (save-buffer)
            (insert "* Another\n")
            (save-buffer)
            (should (string-match-p "Written by agent-plan\n\\* Operator's edit\n\\* Another\n\\'"
                                    (wgn-agent-plans-test--contents plan)))
            ;; A buffer with no record of what it read saves nothing.
            (setq wgn/agent-plans--seen nil)
            (insert "* Unread\n")
            (should-error (save-buffer) :type 'user-error)
            (should-not (string-match-p "Unread" (wgn-agent-plans-test--contents plan)))))))))

(ert-deftest wgn-agent-plans-plan-buffer-records-what-it-read ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--waiting-plan plans))
            (create-lockfiles t))
        (wgn-agent-plans-test--visiting plan
          (should (wgn/agent-plans--seen-current-p plan))
          (should (wgn-agent-plans-test--agent-writes plan))
          (should-not (wgn/agent-plans--seen-current-p plan))
          (revert-buffer t t t)
          (should (wgn/agent-plans--seen-current-p plan))
          (should (wgn-agent-plans-test--agent-writes plan))
          ;; Turning the mode on reverts the buffer through
          ;; `auto-revert-handler', which from Emacs 31 reverts a file's
          ;; buffer only while an auto-revert mode is on in it.
          (let ((auto-revert-use-notify nil))
            (auto-revert-mode 1))
          (unwind-protect
              (progn
                (should (string-match-p "Written by agent-plan\n\\* Written by agent-plan\n\\'"
                                        (buffer-string)))
                (should (wgn/agent-plans--seen-current-p plan)))
            (auto-revert-mode -1)))))))

(defmacro wgn-agent-plans-test--recovering (&rest body)
  "Run BODY confirming every prompt, recovering an auto-save file too.
Asked to save or discard a buffer's changes before it locks, it saves."
  (declare (indent 0))
  `(wgn-agent-plans-test--confirming
     (cl-letf (((symbol-function 'read-answer) (lambda (&rest _) "yes"))
               ((symbol-function 'read-multiple-choice)
                (lambda (_prompt choices &rest _) (assq ?s choices)))
               ;; Visiting a file with newer auto-save data waits a second.
               ((symbol-function 'sit-for) (lambda (&rest _) t)))
       ,@body)))

(defun wgn-agent-plans-test--crash (buffer)
  "Auto-save BUFFER, then kill it unsaved, as a crash does.
Its auto-save file stays, dated after the plan's date that
`wgn-agent-plans-test--agent-writes' sets, for `recover-file' to take it."
  (with-current-buffer buffer
    (auto-save-mode 1)
    (do-auto-save t)
    (let ((auto-save buffer-auto-save-file-name))
      (should (file-exists-p auto-save))
      (set-file-times auto-save (time-add (current-time) 10))
      (set-buffer-modified-p nil)
      (kill-buffer buffer)
      (should (file-exists-p auto-save)))))

(defun wgn-agent-plans-test--recovered (plan)
  "Check the current buffer recovered over agent-plan's write to PLAN.
It holds the operator's edit, unsaved, and PLAN agent-plan's entry; the
save is refused, naming how to compare them, until a revert reads PLAN."
  (should (buffer-modified-p))
  (should (string-match-p "Operator's edit" (buffer-string)))
  (should (wgn-agent-plans-test--kept-p plan t))
  (let ((refusal (cadr (should-error (save-buffer) :type 'user-error))))
    (should (string-match-p "auto-save" refusal))
    (should (string-match-p "diff-buffer-with-file" refusal))
    (should (string-match-p "ediff-current-file" refusal)))
  (should (buffer-modified-p))
  (should (string-match-p "Operator's edit" (buffer-string)))
  (should (wgn-agent-plans-test--kept-p plan t))
  (should-not (string-match-p "Operator's edit" (wgn-agent-plans-test--contents plan)))
  (revert-buffer t t t)
  (read-only-mode -1)
  (goto-char (point-max))
  (insert "* Operator's edit again\n")
  (save-buffer)
  (should (string-match-p "Written by agent-plan\n\\* Operator's edit again\n\\'"
                          (wgn-agent-plans-test--contents plan))))

(ert-deftest wgn-agent-plans-plan-buffer-recovered-after-a-crash-keeps-agent-plan-writes ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--waiting-plan plans))
            (create-lockfiles t))
        (unwind-protect
            (wgn-agent-plans-test--recovering
              (let ((buffer (find-file-noselect plan)))
                (should (wgn-agent-plans-test--agent-writes plan))
                (with-current-buffer buffer
                  ;; Emacs asks whether to edit over the file's change; told to go on.
                  (read-only-mode -1)
                  (goto-char (point-max))
                  (insert "* Operator's edit\n"))
                (wgn-agent-plans-test--crash buffer)
                (with-current-buffer (find-file-noselect plan)
                  (read-only-mode -1)
                  (recover-this-file)
                  ;; Protecting the buffers already visited keeps it unread,
                  ;; though the file is unchanged since its visit.
                  (wgn/agent-plans-protect-mode -1)
                  (wgn/agent-plans-protect-mode 1)
                  (wgn-agent-plans-test--recovered plan))))
          (wgn-agent-plans-test--kill-visiting plan))))))

(ert-deftest wgn-agent-plans-plan-buffer-recovered-in-place-keeps-agent-plan-writes ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--waiting-plan plans))
            (create-lockfiles t))
        (unwind-protect
            (wgn-agent-plans-test--recovering
              (with-current-buffer (find-file-noselect plan)
                (should (wgn-agent-plans-test--agent-writes plan))
                (read-only-mode -1)
                (goto-char (point-max))
                (insert "* Operator's edit\n")
                (auto-save-mode 1)
                (do-auto-save t)
                (set-file-times buffer-auto-save-file-name (time-add (current-time) 10))
                (recover-this-file)
                (wgn-agent-plans-test--recovered plan)))
          (wgn-agent-plans-test--kill-visiting plan))))))

(ert-deftest wgn-agent-plans-plan-buffer-reverted-from-auto-save-keeps-agent-plan-writes ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--waiting-plan plans))
            (create-lockfiles t))
        (unwind-protect
            (wgn-agent-plans-test--recovering
              (with-current-buffer (find-file-noselect plan)
                (should (wgn-agent-plans-test--agent-writes plan))
                (read-only-mode -1)
                (goto-char (point-max))
                (insert "* Operator's edit\n")
                (auto-save-mode 1)
                (do-auto-save t)
                ;; C-u M-x revert-buffer, answering yes to the auto-save file.
                (revert-buffer nil t)
                (should (string-match-p "Operator's edit" (buffer-string)))
                (wgn-agent-plans-test--recovered plan)))
          (wgn-agent-plans-test--kill-visiting plan))))))

(ert-deftest wgn-agent-plans-plan-buffer-written-back-then-recovered-keeps-agent-plan-writes ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let* ((plan (wgn-agent-plans-test--waiting-plan plans))
             (copy (expand-file-name "copy.org" root))
             (create-lockfiles t))
        (unwind-protect
            (wgn-agent-plans-test--recovering
              (let ((buffer (find-file-noselect plan)))
                (with-current-buffer buffer
                  (write-file copy t)
                  (goto-char (point-max))
                  (insert "* Operator's edit\n"))
                (should (wgn-agent-plans-test--agent-writes plan))
                (with-current-buffer buffer
                  (set-visited-file-name plan)
                  (should-error (save-buffer) :type 'user-error))
                (wgn-agent-plans-test--crash buffer)
                (with-current-buffer (find-file-noselect plan)
                  (read-only-mode -1)
                  (recover-this-file)
                  (wgn-agent-plans-test--recovered plan))))
          (wgn-agent-plans-test--kill-visiting plan copy))))))

(ert-deftest wgn-agent-plans-plan-buffer-written-away-and-back-keeps-agent-plan-writes ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let* ((plan (wgn-agent-plans-test--waiting-plan plans))
             (text (wgn-agent-plans-test--contents plan))
             (copy (expand-file-name "copy.org" root))
             (create-lockfiles t))
        (dolist (variant '(unlocked-write-file locked-write-file unlocked-svfn locked-svfn
                           locked-other-buffer))
          (ert-info ((symbol-name variant))
            (wgn-agent-plans-test--write plan text)
            (let ((buffer (find-file-noselect plan))
                  wrote)
              (unwind-protect
                  (wgn-agent-plans-test--confirming
                    (with-current-buffer buffer
                      (when (memq variant '(unlocked-write-file unlocked-svfn))
                        (read-only-mode -1)
                        (goto-char (point-max))
                        (insert "* Operator's edit\n"))
                      (write-file copy t))
                    (when (eq variant 'locked-other-buffer)
                      (find-file-noselect plan))
                    (setq wrote (wgn-agent-plans-test--agent-writes plan))
                    (with-current-buffer buffer
                      (if (memq variant '(unlocked-svfn locked-svfn))
                          (progn
                            (set-visited-file-name plan)
                            ;; Back on its plan with changes, it holds the lock file.
                            (should (file-locked-p plan))
                            (ignore-errors (save-buffer)))
                        (ignore-errors (write-file plan t)))
                      (should (wgn-agent-plans-test--kept-p plan wrote))
                      (when (and (equal buffer-file-name plan) (not buffer-read-only))
                        (goto-char (point-max))
                        (insert "x")
                        (should (file-locked-p plan)))))
                (wgn-agent-plans-test--kill-visiting plan copy)))))))))

(ert-deftest wgn-agent-plans-plan-buffer-renamed-onto-its-plan-under-vc-keeps-agent-plan-writes ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let* ((plan (wgn-agent-plans-test--waiting-plan plans))
             (text (wgn-agent-plans-test--contents plan))
             (away (concat plans "-away"))
             (create-lockfiles t)
             (process-environment (append '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1")
                                          process-environment)))
        (require 'dired-aux)
        ;; The plans directory is a repository, as the operator's is.
        (let ((default-directory (file-name-as-directory plans)))
          (dolist (args '(("init" "-q") ("add" ".")
                          ("-c" "commit.gpgsign=false" "-c" "user.name=t" "-c" "user.email=t@t"
                           "commit" "-qm" "plans")))
            (should (eq 0 (apply #'call-process "git" nil nil nil args)))))
        (dolist (variant '(svfn-own dired-dir-away-back))
          (ert-info ((symbol-name variant))
            (wgn-agent-plans-test--write plan text)
            (let ((buffer (find-file-noselect plan))
                  wrote)
              (unwind-protect
                  (wgn-agent-plans-test--confirming
                    (with-current-buffer buffer
                      (vc-refresh-state)
                      (should vc-mode)
                      (should buffer-read-only)
                      ;; With vc-mode on, `set-visited-file-name' makes the
                      ;; buffer writable.
                      (pcase variant
                        ('svfn-own (set-visited-file-name buffer-file-name))
                        ('dired-dir-away-back
                         (dired-rename-file plans away nil)
                         (dired-rename-file away plans nil)))
                      (should (equal buffer-file-name plan))
                      (setq wrote (wgn-agent-plans-test--agent-writes plan))
                      (when buffer-read-only
                        (read-only-mode -1))
                      (goto-char (point-max))
                      (insert "* Operator's edit\n")
                      (should (file-locked-p plan))
                      (ignore-errors (save-buffer))
                      (should (wgn-agent-plans-test--kept-p plan wrote))))
                (wgn-agent-plans-test--kill-visiting plan)))))))))

(ert-deftest wgn-agent-plans-plan-buffer-renamed-away-and-back-still-auto-reverts ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--waiting-plan plans))
            (away (concat plans "-away"))
            (create-lockfiles t))
        (require 'dired-aux)
        (wgn-agent-plans-test--visiting plan
          ;; As `global-auto-revert-mode' marks the buffers it reverts.
          (setq-local auto-revert--global-mode t)
          (dired-rename-file plans away nil)
          (dired-rename-file away plans nil)
          (should buffer-read-only)
          (should-not (buffer-modified-p))
          (should (wgn-agent-plans-test--agent-writes plan))
          (let ((auto-revert-use-notify nil))
            (auto-revert-handler))
          (should (string-match-p "Written by agent-plan" (buffer-string)))
          (should buffer-read-only)
          (read-only-mode -1)
          (goto-char (point-max))
          (insert "* Operator's edit\n")
          (save-buffer)
          (should (wgn-agent-plans-test--kept-p plan t))
          ;; agent-plan writes it while it is away.
          (dired-rename-file plans away nil)
          (should (wgn-agent-plans-test--agent-writes
                   (expand-file-name (file-name-nondirectory plan) away)))
          (dired-rename-file away plans nil)
          (should-not (buffer-modified-p))
          (let ((auto-revert-use-notify nil))
            (auto-revert-handler))
          (should (string-match-p "Operator's edit\n\\* Written by agent-plan\n\\'" (buffer-string)))
          (should buffer-read-only))))))

(ert-deftest wgn-agent-plans-log-buffer-stays-locked-and-unmodified ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((log (expand-file-name "2026-10-14-x.mac.log.org" plans))
            (other (expand-file-name "other.org" root))
            (create-lockfiles t))
        (wgn-agent-plans-test--waiting-plan plans)
        (wgn-agent-plans-test--write log "* done S1\n")
        (wgn-agent-plans-test--write other "* Other\n")
        (wgn-agent-plans-test--confirming
          (wgn-agent-plans-test--visiting other
            (let ((other-buffer (current-buffer)))
              (wgn-agent-plans-test--visiting log
                (should buffer-read-only)
                ;; Org's commands get past read-only, not past the guard,
                ;; which names nothing that would let them through.
                (let ((refusal (cadr (should-error (org-set-property "X" "1") :type 'user-error))))
                  (should (string-match-p "agent-plan log alone writes" refusal))
                  (should-not (string-match-p "C-x C-q" refusal)))
                ;; C-x C-q leaves it locked.
                (read-only-mode -1)
                (should buffer-read-only)
                (should-error (let ((inhibit-read-only t)) (insert "x")) :type 'user-error)
                (should-not (buffer-modified-p))
                ;; C-x C-w RET, and renaming it onto its own name, leave it unmodified.
                (should-error (write-file buffer-file-name t) :type 'user-error)
                (should-not (buffer-modified-p))
                (set-visited-file-name buffer-file-name)
                (should-not (buffer-modified-p))
                (should buffer-read-only)
                ;; Modified past every guard, a save of it discards the
                ;; change, and saving every buffer goes on to the others.
                (dolist (save (list (lambda () (save-some-buffers t)) #'org-save-all-org-buffers
                                    #'save-buffer))
                  (let ((inhibit-read-only t) (inhibit-modification-hooks t))
                    (goto-char (point-max))
                    (insert "x\n"))
                  (with-current-buffer other-buffer
                    (goto-char (point-max))
                    (insert "* More\n"))
                  (funcall save)
                  (should-not (buffer-modified-p))
                  (should buffer-read-only)
                  (unless (eq save #'save-buffer)
                    (should-not (buffer-modified-p other-buffer)))
                  (should (equal (wgn-agent-plans-test--contents log) "* done S1\n")))
                (with-current-buffer other-buffer
                  (set-buffer-modified-p nil))))))))))

(defun wgn-agent-plans-test--saves-past (buffer other)
  "Check that saving every buffer skips BUFFER, whose save is refused.
BUFFER visits a plan file, and OTHER an Org file, after BUFFER in
`buffer-list', which is saved.  Each skip warns once and leaves BUFFER
modified, which exiting Emacs then asks about; BUFFER's own save is
refused still."
  (let* ((plan (buffer-file-name buffer))
         (before (wgn-agent-plans-test--contents plan)))
    (dolist (save (list (lambda () (save-some-buffers t)) #'org-save-all-org-buffers))
      (with-current-buffer other
        (goto-char (point-max))
        (insert "* More\n"))
      (bury-buffer other)
      (should (memq other (memq buffer (buffer-list))))
      (when (get-buffer "*Warnings*")
        (kill-buffer "*Warnings*"))
      ;; A save of every buffer runs in `save-window-excursion', which
      ;; takes a warning's window away again, so the warning is delayed
      ;; to after the command.
      (let ((delayed-warnings-list nil))
        (funcall save)
        (should (equal (mapcar #'car delayed-warnings-list) '(wgn/agent-plans)))
        (should (string-match-p (regexp-quote (buffer-name buffer))
                                (cadr (car delayed-warnings-list))))
        (should-not (get-buffer-window "*Warnings*" t))
        (display-delayed-warnings))
      (should (buffer-modified-p buffer))
      (should-not (buffer-modified-p other))
      (should (equal (wgn-agent-plans-test--contents (buffer-file-name other))
                     (with-current-buffer other (buffer-string))))
      (should (equal (wgn-agent-plans-test--contents plan) before))
      (with-current-buffer "*Warnings*"
        (should (equal (how-many (regexp-quote "(wgn/agent-plans)") (point-min) (point-max)) 1))
        (goto-char (point-min))
        (should (search-forward (buffer-name buffer) nil t))))
    ;; Exiting asks about it, as a buffer modified (not auto-saved since)
    ;; or as the plan buffer it is, and is refused.
    (let ((about (concat "\\`Modified buffers exist\\|\\`" (regexp-quote (buffer-name buffer))
                         " left unsaved"))
          (prompts nil)
          (killed nil)
          (confirm-kill-emacs nil)
          (confirm-kill-processes nil))
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (prompt)
                   (push prompt prompts)
                   (not (string-match-p about prompt))))
                ((symbol-function 'kill-emacs) (lambda (&rest _) (setq killed t))))
        (save-buffers-kill-emacs t))
      (should-not killed)
      (should (seq-find (lambda (prompt) (string-match-p about prompt)) prompts)))
    (with-current-buffer buffer
      (should-error (save-buffer) :type 'user-error)
      (should (buffer-modified-p)))
    (should (equal (wgn-agent-plans-test--contents plan) before))))

(ert-deftest wgn-agent-plans-saving-every-buffer-skips-a-recovered-plan ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--waiting-plan plans))
            (other (expand-file-name "other.org" root))
            (create-lockfiles t))
        (wgn-agent-plans-test--write other "* Other\n")
        (unwind-protect
            (wgn-agent-plans-test--recovering
              (let ((buffer (find-file-noselect plan)))
                (should (wgn-agent-plans-test--agent-writes plan))
                (with-current-buffer buffer
                  (read-only-mode -1)
                  (goto-char (point-max))
                  (insert "* Operator's edit\n"))
                (wgn-agent-plans-test--crash buffer)
                (with-current-buffer (find-file-noselect plan)
                  (read-only-mode -1)
                  (recover-this-file)
                  (should (wgn/agent-plans--unknown-base-p))
                  (wgn-agent-plans-test--saves-past (current-buffer)
                                                    (find-file-noselect other)))))
          (wgn-agent-plans-test--kill-visiting plan other))))))

(ert-deftest wgn-agent-plans-saving-every-buffer-skips-a-plan-written-since-read ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--waiting-plan plans))
            (other (expand-file-name "other.org" root))
            (create-lockfiles t))
        (wgn-agent-plans-test--write other "* Other\n")
        (unwind-protect
            (wgn-agent-plans-test--confirming
              (let ((buffer (find-file-noselect plan)))
                (with-current-buffer buffer
                  (read-only-mode -1)
                  (should (wgn-agent-plans-test--agent-writes plan))
                  (goto-char (point-max))
                  (insert "* Operator's edit\n"))
                (wgn-agent-plans-test--saves-past buffer (find-file-noselect other))))
          (wgn-agent-plans-test--kill-visiting plan other))))))

(ert-deftest wgn-agent-plans-saving-every-buffer-skips-a-locked-plan-marked-modified ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--waiting-plan plans))
            (other (expand-file-name "other.org" root)))
        (wgn-agent-plans-test--write other "* Other\n")
        (unwind-protect
            (wgn-agent-plans-test--confirming
              ;; Neither changes the text, so the change guard lets it through.
              (dolist (mark (list (lambda () (not-modified t))
                                  (lambda () (set-buffer-file-coding-system 'utf-8-dos))))
                (let ((buffer (find-file-noselect plan)))
                  (with-current-buffer buffer
                    (should buffer-read-only)
                    (funcall mark)
                    (should (buffer-modified-p)))
                  (wgn-agent-plans-test--saves-past buffer (find-file-noselect other))
                  (wgn-agent-plans-test--kill-visiting plan))))
          (wgn-agent-plans-test--kill-visiting plan other))))))

(ert-deftest wgn-agent-plans-saving-every-buffer-stops-at-other-failures ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--waiting-plan plans))
            (failing (expand-file-name "failing.org" root))
            (other (expand-file-name "other.org" root)))
        (wgn-agent-plans-test--write failing "* Failing\n")
        (wgn-agent-plans-test--write other "* Other\n")
        (unwind-protect
            (let ((other-buffer (find-file-noselect other)))
              ;; Another buffer's failure, and a plan buffer's other than
              ;; this file's refusal, stop the save as they would anyway.
              (pcase-dolist (`(,file ,failure) `((,failing error) (,plan user-error)))
                (let ((buffer (find-file-noselect file)))
                  (with-current-buffer buffer
                    (read-only-mode -1)
                    (add-hook 'write-file-functions (lambda () (signal failure '("Failed"))) nil t)
                    (goto-char (point-max))
                    (insert "* Edit\n"))
                  (with-current-buffer other-buffer
                    (goto-char (point-max))
                    (insert "* More\n"))
                  (bury-buffer other-buffer)
                  (should (memq other-buffer (memq buffer (buffer-list))))
                  (dolist (save (list (lambda () (save-some-buffers t)) #'org-save-all-org-buffers))
                    (let ((err (should-error (funcall save) :type 'error)))
                      (should (equal err (list failure "Failed"))))
                    (should (buffer-modified-p buffer))
                    (should (buffer-modified-p other-buffer)))
                  (wgn-agent-plans-test--kill-visiting file))))
          (wgn-agent-plans-test--kill-visiting plan failing other))))))

(ert-deftest wgn-agent-plans-saving-every-buffer-knows-it-was-called-interactively ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((messages nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (when format-string
                       (push (apply #'format format-string args) messages))))
                  ((symbol-function 'buffer-list) (lambda (&optional _) nil)))
          (call-interactively #'save-some-buffers))
        (should (member "(No files need saving)" messages))))))

(ert-deftest wgn-agent-plans-saving-every-buffer-is-advised-only-with-the-mode ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (should (advice-member-p #'wgn/agent-plans--save-all 'save-some-buffers))
      (should (advice-member-p #'wgn/agent-plans--skip-refused 'basic-save-buffer))
      (should (memq #'wgn/agent-plans--confirm-exit kill-emacs-query-functions))
      (wgn/agent-plans-protect-mode -1)
      (should-not (advice-member-p #'wgn/agent-plans--save-all 'save-some-buffers))
      (should-not (advice-member-p #'wgn/agent-plans--skip-refused 'basic-save-buffer))
      (should-not (memq #'wgn/agent-plans--confirm-exit kill-emacs-query-functions)))))

(ert-deftest wgn-agent-plans-plan-paths-refuse-other-buffers-writes ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let* ((plan (wgn-agent-plans-test--waiting-plan plans))
             (log (expand-file-name "2026-10-14-x.mac.log.org" plans))
             (new (expand-file-name "2026-10-14-z.mac.org" plans))
             (before (with-temp-buffer (insert-file-contents plan) (buffer-string))))
        (wgn-agent-plans-test--write log "* done S1\n")
        (with-temp-buffer
          (let ((name (buffer-name)))
            (insert "* Plan\n")
            (should-error (write-file plan) :type 'user-error)
            (should-error (write-file new) :type 'user-error)
            (should-error (write-file log) :type 'user-error)
            ;; Each refused write leaves the buffer as it was, with no
            ;; lock or auto-save file in the plans directory.
            (should-not buffer-file-name)
            (should-not buffer-file-number)
            (should-not buffer-auto-save-file-name)
            (should (eq major-mode 'fundamental-mode))
            (should (equal (buffer-name) name))
            (do-auto-save t)
            (should-not (directory-files plans nil "\\`[.#]#"))
            ;; The refusal names no C-x C-q, which would not let this through.
            (dolist (write (list (lambda () (write-file plan))
                                 (lambda () (write-region (point-min) (point-max) plan))))
              (let ((refusal (cadr (should-error (funcall write) :type 'user-error))))
                (should (string-match-p "by hand only from its own buffer" refusal))))
            (should-error (append-to-file (point-min) (point-max) log) :type 'user-error)
            (set-buffer-modified-p nil)))
        ;; Nor is a file copied or moved onto one, as dired's C and R do.
        (let ((elsewhere (expand-file-name "2026-10-14-x.mac.org" root)))
          (with-temp-file elsewhere (insert "* Elsewhere\n"))
          (should-error (copy-file elsewhere plan t) :type 'user-error)
          (should-error (copy-file elsewhere (file-name-as-directory plans) t) :type 'user-error)
          (should-error (rename-file elsewhere log t) :type 'user-error)
          (should (file-exists-p elsewhere)))
        (should (equal (with-temp-buffer (insert-file-contents plan) (buffer-string)) before))
        (should-not (file-exists-p new))
        (should (equal (with-temp-buffer (insert-file-contents log) (buffer-string)) "* done S1\n"))
        ;; A plan's buffer saves to its own file alone, and a refused
        ;; write elsewhere leaves it as it was.
        (wgn-agent-plans-test--visiting plan
          (let ((name (buffer-name)))
            (should-error (write-file new) :type 'user-error)
            (should (equal buffer-file-name plan))
            (should (equal (buffer-name) name))
            (should buffer-read-only)
            (should-not (buffer-modified-p))
            (should (verify-visited-file-modtime))
            (wgn-agent-plans-test--write plan before)
            (set-file-times plan (time-add (current-time) 5))
            (should-not (verify-visited-file-modtime))
            (revert-buffer t t t)
            (read-only-mode -1)
            (should-error (write-file new) :type 'user-error)
            (should-not buffer-read-only)
            (should-not (buffer-modified-p))
            (should-not (file-exists-p new))
            ;; Its own save may still move a file onto it.
            (let ((file-precious-flag t))
              (goto-char (point-max))
              (insert "* Saved\n")
              (save-buffer))
            (should (string-match-p "^\\* Saved$" (with-temp-buffer (insert-file-contents plan)
                                                                    (buffer-string))))
            (wgn-agent-plans-test--write plan before)))
        ;; A log, never.
        (wgn-agent-plans-test--visiting log
          (read-only-mode -1)
          (should-error (insert "x")))
        (should (equal (with-temp-buffer (insert-file-contents log) (buffer-string)) "* done S1\n"))))))

;;;; A stand-in agent-plan that records its calls

(defconst wgn-agent-plans-test--cli-fake
  "#!/bin/sh
# Records each call's arguments in $RECORD, one per line, then a line \"--\".
for a; do printf '%s\\n' \"$a\" >> \"$RECORD\"; done
echo -- >> \"$RECORD\"
if [ -n \"$REFUSE\" ]; then
  echo \"agent-plan: refused: $REFUSE\" >&2
  exit 2
fi
case \"$1\" in
  questions) cat \"$QUESTIONS\" ;;
  *) echo '{}' ;;
esac
"
  "A stand-in for `agent-plan' that records what it is asked to do.
`questions' prints $QUESTIONS; with $REFUSE set, every call is refused.")

(defmacro wgn-agent-plans-test--with-cli (&rest body)
  "Run BODY as `wgn-agent-plans-test--with-plans' does, with a recording CLI.
Also binds `record', the file of its calls, and `questions', the file its
`questions' prints, which holds no question on entry."
  (declare (indent 0))
  `(wgn-agent-plans-test--with-plans
     (let* ((record (expand-file-name "record" root))
            (questions (expand-file-name "questions.json" root))
            (process-environment
             (append (list (concat "RECORD=" record) (concat "QUESTIONS=" questions))
                     process-environment)))
       (wgn-agent-plans-test--write questions "[]")
       (wgn-agent-plans-test--write
        (expand-file-name "agent-plan" bin) wgn-agent-plans-test--cli-fake)
       (unwind-protect
           (progn ,@body)
         (dolist (buffer (buffer-list))
           (when (and (buffer-file-name buffer)
                      (string-prefix-p root (buffer-file-name buffer)))
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer)))))))

(defun wgn-agent-plans-test--calls (record)
  "Return the calls RECORD holds, each a list of its arguments."
  (when (file-exists-p record)
    (mapcar (lambda (call) (split-string call "\n" t))
            (split-string (with-temp-buffer (insert-file-contents record) (buffer-string))
                          "^--\n" t))))

(defun wgn-agent-plans-test--question-plan (plans)
  "Make plan 2026-10-14-x.mac under PLANS with questions Q1 and, in part P1, P1.Q1.
Return the plan file."
  (let ((plan (expand-file-name "2026-10-14-x.mac.org" plans)))
    (wgn-agent-plans-test--write
     plan
     (wgn-agent-plans-test--plan-text
      "Questions"
      :Steps (wgn-agent-plans-test--entry "TODO" "First" "S1")
      :Distribution (wgn-agent-plans-test--entry "TODO" "Part one" "P1")
      :Questions (concat (wgn-agent-plans-test--entry "ASK" "Use ghostel?" "Q1")
                         "Commands need a TTY.\n- a compile buffer\n- vterm\n")))
    (wgn-agent-plans-test--write
     (expand-file-name "2026-10-14-x.mac.P1.org" plans)
     (wgn-agent-plans-test--plan-text
      "Part one" :Questions (wgn-agent-plans-test--entry "ASK" "Split it?" "P1.Q1")))
    plan))

(defun wgn-agent-plans-test--questions-json (plans)
  "Return what `agent-plan questions --json' prints for the question plan under PLANS."
  (format "[{\"id\": \"Q1\", \"file\": %S, \"question\": \"Use ghostel?\",
             \"context\": \"Commands need a TTY.\", \"recommendation\": \"yes: the operator's terminal\",
             \"alternatives\": [\"a compile buffer\", \"vterm\"]},
            {\"id\": \"P1.Q1\", \"file\": %S, \"question\": \"Split it?\", \"recommendation\": \"no\"}]"
          (expand-file-name "2026-10-14-x.mac.org" plans)
          (expand-file-name "2026-10-14-x.mac.P1.org" plans)))

(ert-deftest wgn-agent-plans-pending-signs-carry-their-command ()
  (wgn-agent-plans-test--with-plans
    (let* ((worktree (file-name-as-directory (expand-file-name "wt" root)))
           (plan (expand-file-name "2026-10-14-x.mac.org" plans)))
      (make-directory worktree t)
      (wgn-agent-plans-test--write
       plan (concat "* Plan\n:PROPERTIES:\n:ID: 2026-10-14-x.mac\n"
                    ":BRANCHES: main..pending-one pending-one..pending-two\n"
                    ":WORKTREES: /nonexistent " worktree "\n:END:\n"))
      (wgn-agent-plans-test--write
       (expand-file-name "2026-10-14-x.mac.log.org" plans)
       (concat (wgn-agent-plans-test--sign-event "sign-requested" "2026-10-14-x.mac.0000bbbb"
                                                 "pending-one,pending-two" worktree)
               (wgn-agent-plans-test--sign-event "sign-requested" "2026-10-14-x.mac.0000cccc"
                                                 "pending-two" worktree "pending-one")
               ;; The plan's BRANCHES and WORKTREES are not the pass's.
               (wgn-agent-plans-test--sign-event "sign-requested" "2026-10-14-x.mac.0000eeee"
                                                 "pending-one")
               (wgn-agent-plans-test--sign-event "sign-requested" "2026-10-14-x.mac.0000ffff"
                                                 "pending-one" "/nonexistent")
               (wgn-agent-plans-test--sign-event "sign-refused" "2026-10-14-x.mac.0000dddd")))
      (should (equal (wgn/agent-plans-pending-signs plan)
                     `((:id "2026-10-14-x.mac.0000bbbb" :branches ("pending-one" "pending-two")
                        :directory ,worktree
                        :command "git sign-pass --notify 2026-10-14-x.mac.0000bbbb pending-one pending-two")
                       (:id "2026-10-14-x.mac.0000cccc" :branches ("pending-two")
                        :directory ,worktree
                        :command "git sign-pass --base pending-one --notify 2026-10-14-x.mac.0000cccc pending-two")
                       (:id "2026-10-14-x.mac.0000eeee" :branches ("pending-one")
                        :directory nil
                        :command "git sign-pass --notify 2026-10-14-x.mac.0000eeee pending-one")
                       (:id "2026-10-14-x.mac.0000ffff" :branches ("pending-one")
                        :directory nil
                        :command "git sign-pass --notify 2026-10-14-x.mac.0000ffff pending-one")))))))

(ert-deftest wgn-agent-plans-waiting-asks-agent-plan-once-per-change ()
  (wgn-agent-plans-test--with-cli
    (let ((plan (wgn-agent-plans-test--question-plan plans))
          (halted (expand-file-name "2026-10-13-h.mac.org" plans))
          (wgn/agent-plans--questions-cache (make-hash-table :test #'equal)))
      (wgn-agent-plans-test--write
       halted "* Plan\n:PROPERTIES:\n:ID: 2026-10-13-h.mac\n:STATUS: halted\n:END:\n")
      (wgn-agent-plans-test--write questions (wgn-agent-plans-test--questions-json plans))
      (wgn/agent-plans-waiting)
      (wgn/agent-plans-waiting)
      (should (equal (mapcar #'cadr (wgn-agent-plans-test--calls record)) (list plan)))
      ;; Nothing it lists stays visited.
      (should-not (find-buffer-visiting plan))
      ;; agent-plan changed a part.
      (set-file-times (expand-file-name "2026-10-14-x.mac.P1.org" plans)
                      (time-add (current-time) 10))
      (wgn/agent-plans-waiting)
      (should (equal (length (wgn-agent-plans-test--calls record)) 2)))))

(defun wgn-agent-plans-test--waiting-line (pattern)
  "Build the A view and move to the line matching PATTERN in its agenda buffer."
  (org-agenda nil "A")
  (set-buffer org-agenda-buffer)
  (goto-char (point-min))
  (re-search-forward pattern)
  (beginning-of-line))

(wgn-agent-plans-test--in-both-forms wgn-agent-plans-waiting-is-a-list-to-read
  (wgn-agent-plans-test--with-cli
    (wgn-agent-plans-test--protected
      (let* ((plan (wgn-agent-plans-test--question-plan plans))
             (before (with-temp-buffer (insert-file-contents plan) (buffer-string)))
             (org-agenda-custom-commands (list (wgn/agent-plans-agenda-command)))
             (org-agenda-sticky nil))
        (wgn-agent-plans-test--write questions (wgn-agent-plans-test--questions-json plans))
        (wgn-agent-plans-test--write
         (expand-file-name "2026-10-14-x.mac.log.org" plans)
         (wgn-agent-plans-test--sign-event "sign-requested" "2026-10-14-x.mac.0000bbbb" "one"))
        (unwind-protect
            (save-current-buffer
              (wgn-agent-plans-test--waiting-line "git sign-pass --notify")
              ;; Nothing runs from here: a key does no more than visit.
              (should-not (commandp (lookup-key (get-text-property (point) 'keymap)
                                                (kbd "C-c C-c"))))
              ;; Nor are links in agents' text opened from here.
              (should (eq (lookup-key (get-text-property (point) 'keymap) (kbd "C-c C-o"))
                          #'wgn/agent-plans-waiting-visit))
              (should-error (org-agenda-todo "DONE"))
              (wgn-agent-plans-test--waiting-line "P1\\.Q1 Split it")
              (should (looking-at-p (concat "  2026-10-14-x\\.mac P1\\.Q1 Split it\\?  (recommended: no)\n"
                                            "    answer it with /questions in a Claude session"
                                            " on 2026-10-14-x\\.mac$")))
              (should-error (org-agenda-todo "DECIDED"))
              ;; RET visits it.
              (call-interactively (lookup-key (get-text-property (point) 'keymap) (kbd "RET")))
              (should (equal (buffer-file-name) (expand-file-name "2026-10-14-x.mac.P1.org" plans)))
              (should (equal (wgn-agent-plans-test--id-here) "P1.Q1")))
          (org-agenda-quit))
        ;; agent-plan was asked for its questions, and nothing else.
        (should (equal (mapcar #'car (wgn-agent-plans-test--calls record)) '("questions")))
        (should (equal (with-temp-buffer (insert-file-contents plan) (buffer-string)) before))))))

(ert-deftest wgn-agent-plans-waiting-shows-a-plan-agent-plan-refused ()
  (wgn-agent-plans-test--with-cli
    (wgn-agent-plans-test--protected
      (wgn-agent-plans-test--question-plan plans)
      (let ((process-environment (cons "REFUSE=no questions today" process-environment)))
        (should (equal (mapcar (lambda (item) (plist-get item :title)) (wgn/agent-plans-waiting))
                       '("agent-plan: refused: no questions today")))))))

;;;; Links

(defmacro wgn-agent-plans-test--with-links (&rest body)
  "Run BODY with the link types registered, and unregistered after."
  (declare (indent 0))
  `(let ((org-link-parameters (copy-sequence org-link-parameters))
         (org-execute-file-search-functions nil))
     (wgn/agent-plans-links-setup)
     ,@body))

(defun wgn-agent-plans-test--follow (link)
  "Follow LINK, an Org link's text, from the current buffer."
  (org-link-open-from-string link))

(wgn-agent-plans-test--in-both-forms wgn-agent-plans-plan-link-visits-the-item
  (wgn-agent-plans-test--with-cli
    (wgn-agent-plans-test--with-links
      (wgn-agent-plans-test--question-plan plans)
      (with-temp-buffer
        (wgn-agent-plans-test--follow "[[plan:2026-10-14-x.mac]]")
        (should (equal (buffer-file-name) (expand-file-name "2026-10-14-x.mac.org" plans))))
      (with-temp-buffer
        (wgn-agent-plans-test--follow "[[plan:2026-10-14-x.mac#P1.Q1]]")
        (should (equal (buffer-file-name) (expand-file-name "2026-10-14-x.mac.P1.org" plans)))
        (should (equal (wgn-agent-plans-test--id-here) "P1.Q1")))
      (with-temp-buffer
        (should-error (wgn-agent-plans-test--follow "[[plan:2026-10-14-x.mac#S9]]")
                      :type 'user-error)
        (should-error (wgn-agent-plans-test--follow "[[plan:2026-10-14-nope.mac]]")
                      :type 'user-error)))))

(wgn-agent-plans-test--in-both-forms wgn-agent-plans-question-link-prefers-the-plan-it-is-in
  (wgn-agent-plans-test--with-cli
    (wgn-agent-plans-test--with-links
      (let ((plan (wgn-agent-plans-test--question-plan plans))
            (other (expand-file-name "2026-10-15-z.mac.org" plans)))
        (wgn-agent-plans-test--write
         other (wgn-agent-plans-test--plan-text
                "Other" :Questions (wgn-agent-plans-test--entry "DECIDED" "Other Q1?" "Q1")))
        ;; In a plan, its own Q1.
        (wgn-agent-plans-test--visiting plan
          (wgn-agent-plans-test--follow "[[q:Q1]]")
          (should (equal (buffer-file-name) plan))
          (should (equal (org-get-heading t t t t) "Use ghostel?")))
        ;; Named.
        (with-temp-buffer
          (wgn-agent-plans-test--follow "[[q:2026-10-15-z.mac#Q1]]")
          (should (equal (buffer-file-name) other)))
        ;; Elsewhere, asked which, by title, the open question first.
        (with-temp-buffer
          (let (offered)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _)
                         (setq offered (all-completions "" collection))
                         (cadr offered))))
              (wgn-agent-plans-test--follow "[[q:Q1]]"))
            (should (equal offered '("Questions (2026-10-14-x.mac): open"
                                     "Other (2026-10-15-z.mac): settled")))
            (should (equal (buffer-file-name) other))))))))

;; Plans named the org-roam way, YYYYMMDDHHMMSS-slug, are read beside
;; those named YYYY-MM-DD-slug.
(ert-deftest wgn-agent-plans-org-roam-named-plans-are-plans ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (wgn-agent-plans-test--with-links
        (let* ((wgn-agent-plans-test--id-property "CUSTOM_ID")
               (plan (wgn-agent-plans-test--waiting-plan plans "20261014120000-x"))
               (part (expand-file-name "20261014120000-x.mac.P1.org" plans))
               (log (expand-file-name "20261014120000-x.mac.log.org" plans)))
          ;; An org-roam node: a file-level ID before the title.
          (wgn-agent-plans-test--write
           plan (concat ":PROPERTIES:\n:ID: 0f8d3c2e-5b1a-4e6f-9c7d-2a4b6c8e0f13\n:END:\n"
                        (with-temp-buffer (insert-file-contents plan) (buffer-string))))
          (wgn-agent-plans-test--write log "* done S1\n")
          (wgn-agent-plans-test--visiting plan
            (should buffer-read-only)
            (read-only-mode -1)
            (should (equal (wgn-agent-plans-test--try "S2" "DONE") "TODO")))
          ;; The part's step, settled in its own file, unblocks S2.
          (wgn-agent-plans-test--visiting part
            (should buffer-read-only)
            (read-only-mode -1)
            (should (equal (wgn-agent-plans-test--try "P1.S1" "ABANDONED") "ABANDONED"))
            (save-buffer))
          (wgn-agent-plans-test--visiting plan
            (read-only-mode -1)
            (should (equal (wgn-agent-plans-test--try "S2" "DONE") "DONE")))
          (wgn-agent-plans-test--visiting log
            (should (string-match-p "agent-plan log alone" header-line-format)))
          (with-temp-buffer
            (wgn-agent-plans-test--follow "[[plan:20261014120000-x.mac#P1.S1]]")
            (should (equal (buffer-file-name) part))
            (should (equal (wgn-agent-plans-test--id-here) "P1.S1"))))))))

;;;; Org-roam

(ert-deftest wgn-agent-plans-org-roam-exclude-matches-parts-logs-and-locks ()
  ;; Org-roam matches it against names relative to its directory.
  (dolist (file '("agents/plans/20261014120000-x.mac.P1.org"
                  "agents/plans/20261014120000-x.mac.log.org"
                  "agents/plans/2026-10-14-x.mac.P12.org"
                  "agents/plans/2026-10-14-x.mac.log.org"
                  "agents/plans/20261014120000-x.mac.log.org.lock"
                  "agents/plans/.sync.lock"))
    (should (string-match-p wgn/agent-plans-org-roam-exclude-regexp file)))
  (dolist (file '("agents/plans/20261014120000-x.mac.org"
                  "agents/plans/2026-10-14-x.mac.org"
                  "agents/20261023150000-next_session.org"
                  "20200101000000-a_note.org"
                  "notes/20261014120000-x.mac.log.org.txt"
                  ;; A name that only ends like a log's.
                  "notes/my-2026-10-14-a.mac.log.org"))
    (should-not (string-match-p wgn/agent-plans-org-roam-exclude-regexp file))))

(defvar org-roam-directory)
(defvar org-roam-db-location)
(defvar org-roam-file-exclude-regexp)
(defvar org-roam-db-node-include-function)
(defvar org-roam-db-autosync-mode)
(defvar wgn/agent-plans-org-roam-directory)
(declare-function org-roam-db-autosync-mode "org-roam-db" (&optional arg))
(declare-function org-roam-db-sync "org-roam-db" (&optional force))
(declare-function org-roam-db-query "org-roam-db" (sql &rest args))
(declare-function org-roam-db--close "org-roam-db" (&optional db))

(defun wgn-agent-plans-test--standard (variable)
  "Return a fresh copy of VARIABLE's standard value."
  (eval (car (get variable 'standard-value)) t))

(defmacro wgn-agent-plans-test--with-roam (&rest body)
  "Run BODY with org-roam set to its defaults on a notes directory of its own.
Skipped where org-roam is not on `load-path': give its directory with
-L to run it.  Binds `root', the notes directory, which
`wgn/agent-plans-org-roam-directory' names, as the machine file does,
and `plans', its agents/plans; `node' returns a file node's text, and
`nodes' the database's nodes.  `org-roam-directory' starts elsewhere,
and `org-roam-db-autosync-mode' is turned off after."
  (declare (indent 0))
  `(progn
     (skip-unless (require 'org-roam nil t))
     (let* ((root (file-name-as-directory
                   (file-truename (make-temp-file "agent-plans-roam-" t))))
            (plans (expand-file-name "agents/plans" root))
            (wgn/agent-plans-org-roam-directory root)
            ;; Never the operator's.
            (process-environment
             (cons (concat "AGENT_TOOLS_CONFIG=" (expand-file-name "no-config.json" root))
                   process-environment))
            (org-roam-directory (expand-file-name "not-the-notes/" root))
            (org-roam-db-location (expand-file-name "roam.db" root))
            (org-roam-file-exclude-regexp
             (wgn-agent-plans-test--standard 'org-roam-file-exclude-regexp))
            (org-roam-db-node-include-function
             (wgn-agent-plans-test--standard 'org-roam-db-node-include-function))
            (org-id-locations-file (expand-file-name "org-id-locations" root))
            (org-id-locations nil)
            (org-id-files nil))
       (cl-flet ((node (id title &optional body)
                   (concat ":PROPERTIES:\n:ID: " id "\n:END:\n#+title: " title "\n" body))
                 (nodes ()
                   (sort (org-roam-db-query [:select [id title] :from nodes])
                         (lambda (a b) (string< (car a) (car b))))))
         (unwind-protect
             (progn ,@body)
           (org-roam-db-autosync-mode -1)
           (org-roam-db--close)
           (delete-directory root t))))))

(ert-deftest wgn-agent-plans-org-roam-sees-plans-but-not-parts-or-logs ()
  (wgn-agent-plans-test--with-roam
    (wgn/agent-plans-org-roam-setup)
    (wgn-agent-plans-test--write
     (expand-file-name "20261014120000-a.mac.org" plans)
     (node "a1b2c3d4-0000-4000-8000-00000000000a" "Plan A"
           (concat "* Plan\n:PROPERTIES:\n:CUSTOM_ID: 20261014120000-a.mac\n:END:\n"
                   "** Related\n"
                   "- [[id:a1b2c3d4-0000-4000-8000-00000000000b][Plan B]]: shares its parser\n"
                   "** Steps\n*** TODO First\n:PROPERTIES:\n:CUSTOM_ID: S1\n:END:\n")))
    (wgn-agent-plans-test--write
     (expand-file-name "20261015120000-b.mac.org" plans)
     (node "a1b2c3d4-0000-4000-8000-00000000000b" "Plan B"))
    ;; Each would be a node, were it not left out.
    (wgn-agent-plans-test--write
     (expand-file-name "20261014120000-a.mac.P1.org" plans)
     (node "a1b2c3d4-0000-4000-8000-0000000000a1" "Part one"))
    (wgn-agent-plans-test--write
     (expand-file-name "20261014120000-a.mac.log.org" plans)
     (node "a1b2c3d4-0000-4000-8000-0000000000a2" "Log"))
    (wgn-agent-plans-test--write
     (expand-file-name "agents/20261023150000-next_session.org" root)
     (node "a1b2c3d4-0000-4000-8000-00000000000c" "Next session"))
    (org-roam-db-sync)
    (should (equal (nodes)
                   '(("a1b2c3d4-0000-4000-8000-00000000000a" "Plan A")
                     ("a1b2c3d4-0000-4000-8000-00000000000b" "Plan B")
                     ("a1b2c3d4-0000-4000-8000-00000000000c" "Next session"))))
    (should (equal (org-roam-db-query [:select [source dest] :from links
                                       :where (= type "id")])
                   '(("a1b2c3d4-0000-4000-8000-00000000000a"
                      "a1b2c3d4-0000-4000-8000-00000000000b"))))))

(ert-deftest wgn-agent-plans-org-roam-setup-takes-only-an-absolute-directory ()
  (wgn-agent-plans-test--with-roam
    ;; "" would make org-roam index Emacs's current directory.
    (dolist (directory '("" "notes" "~/notes"))
      (let ((wgn/agent-plans-org-roam-directory directory))
        (wgn/agent-plans-org-roam-setup)
        (should (equal org-roam-directory (expand-file-name "not-the-notes/" root)))
        (should-not org-roam-db-autosync-mode)))))

(defconst wgn-agent-plans-test--repository
  (file-name-directory (directory-file-name (file-name-directory load-file-name)))
  "The checkout these tests are in.")

(defun wgn-agent-plans-test--repository-text (file)
  "Return the text of FILE in the checkout, skipping the test if it is absent."
  (let ((path (expand-file-name file wgn-agent-plans-test--repository)))
    (unless (file-exists-p path)
      (ert-skip (format "%s is not in the checkout" path)))
    (with-temp-buffer (insert-file-contents path) (buffer-string))))

(ert-deftest wgn-agent-plans-org-roam-machine-file-is-the-one-home-manager-writes ()
  (should (string-search
           (concat "\".emacs.d/" wgn/agent-plans-org-roam-machine-file "\" = ")
           (wgn-agent-plans-test--repository-text "flake.nix"))))

(ert-deftest wgn-agent-plans-org-roam-form-loads-the-machine-file-before-setup ()
  (let* ((text (wgn-agent-plans-test--repository-text "default.el"))
         (form (with-temp-buffer
                 (insert text)
                 (goto-char (point-min))
                 (re-search-forward "^(use-package org-roam$")
                 (goto-char (match-beginning 0))
                 (read (current-buffer))))
         (config (cdr (memq :config form))))
    (should (equal (seq-take config 2)
                   '((load (expand-file-name wgn/agent-plans-org-roam-machine-file
                                             user-emacs-directory)
                           t t)
                     (wgn/agent-plans-org-roam-setup))))))

;; Org is loaded by then, so the org-roam form syncs as it is evaluated:
;; before no-littering moves `org-roam-db-location', a first database
;; would be made at the default one.
(ert-deftest wgn-agent-plans-org-roam-form-follows-no-littering ()
  (let ((text (wgn-agent-plans-test--repository-text "default.el")))
    (should (< (string-match "^(use-package no-littering$" text)
               (string-match "^(use-package org-roam$" text)))))

(ert-deftest wgn-agent-plans-org-roam-setup-quietly-does-nothing-without-a-directory ()
  (wgn-agent-plans-test--with-roam
    (let ((wgn/agent-plans-org-roam-directory nil)
          ;; Agent-plan's config names the notes repository: it is not
          ;; org-roam's directory.
          (process-environment
           (cons (concat "AGENT_TOOLS_CONFIG=" (expand-file-name "config.json" root))
                 process-environment))
          messages)
      (wgn-agent-plans-test--write
       (expand-file-name "config.json" root)
       (format "{\"plans\": {\"dir\": %S, \"host\": \"mac\", \"repo\": %S}}" plans root))
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest args) (push args messages))))
        (wgn/agent-plans-org-roam-setup))
      (should-not messages)
      (should (equal org-roam-directory (expand-file-name "not-the-notes/" root)))
      (should (equal org-roam-file-exclude-regexp
                     (wgn-agent-plans-test--standard 'org-roam-file-exclude-regexp)))
      (should-not (eq org-roam-db-node-include-function #'wgn/agent-plans-org-roam-node-p))
      (should-not org-roam-db-autosync-mode))))

(ert-deftest wgn-agent-plans-org-roam-setup-syncs-the-directory-if-there ()
  (wgn-agent-plans-test--with-roam
    (setq wgn/agent-plans-org-roam-directory (expand-file-name "missing" root))
    (wgn/agent-plans-org-roam-setup)
    (should (equal org-roam-directory (expand-file-name "missing" root)))
    (should-not org-roam-db-autosync-mode)
    (setq wgn/agent-plans-org-roam-directory root)
    (wgn/agent-plans-org-roam-setup)
    (wgn/agent-plans-org-roam-setup)
    (should (equal org-roam-directory root))
    (should org-roam-db-autosync-mode)
    (should (equal org-roam-file-exclude-regexp
                   (cons wgn/agent-plans-org-roam-exclude-regexp
                         (wgn-agent-plans-test--standard 'org-roam-file-exclude-regexp))))
    (should (eq org-roam-db-node-include-function #'wgn/agent-plans-org-roam-node-p))))

(ert-deftest wgn-agent-plans-org-roam-makes-each-plan-one-node ()
  (wgn-agent-plans-test--with-roam
    (wgn/agent-plans-org-roam-setup)
    ;; As agent-plan writes plans: every entry's ID in :ID:, the same
    ;; IDs in every plan.
    (dolist (name '("2026-10-14-p" "2026-10-15-q"))
      (wgn-agent-plans-test--write
       (expand-file-name (concat name ".mac.org") plans)
       (concat "#+title: " name "\n* Plan\n:PROPERTIES:\n:ID: " name ".mac\n:END:\n** Steps\n"
               (wgn-agent-plans-test--entry "TODO" "First" "S1"))))
    ;; A node whose entry org-id gave an ID of its own.
    (wgn-agent-plans-test--write
     (expand-file-name "20261016120000-r.mac.org" plans)
     (node "a1b2c3d4-0000-4000-8000-00000000000d" "Plan R"
           (concat "* Plan\n:PROPERTIES:\n:CUSTOM_ID: 20261016120000-r.mac\n:END:\n"
                   "*** TODO First\n:PROPERTIES:\n:CUSTOM_ID: S1\n"
                   ":ID: a1b2c3d4-0000-4000-8000-0000000000d1\n:END:\n")))
    ;; The operator's notes keep their headings' nodes.
    (wgn-agent-plans-test--write
     (expand-file-name "20200101000000-a_note.org" root)
     (node "a1b2c3d4-0000-4000-8000-00000000000e" "A note"
           "* A heading\n:PROPERTIES:\n:ID: a1b2c3d4-0000-4000-8000-0000000000e1\n:END:\n"))
    (org-roam-db-sync)
    (should (equal (nodes)
                   '(("a1b2c3d4-0000-4000-8000-00000000000d" "Plan R")
                     ("a1b2c3d4-0000-4000-8000-00000000000e" "A note")
                     ("a1b2c3d4-0000-4000-8000-0000000000e1" "A heading"))))))

(defmacro wgn-agent-plans-test--with-repo (&rest body)
  "Run BODY with a git repository at `repo', ROOT/github.com/wesnel/thing.
`wgn/agent-plans-repository-directories' finds it, and its origin is on
GitHub.  `commit' commits a file's new text and returns the commit."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "agent-plans-repo-" t)))
          (repo (file-name-as-directory (expand-file-name "github.com/wesnel/thing" root)))
          (wgn/agent-plans-repository-directories
           (list (expand-file-name "github.com/*" root)))
          (process-environment
           (append '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1"
                     "GIT_AUTHOR_NAME=t" "GIT_AUTHOR_EMAIL=t@t" "GIT_COMMITTER_NAME=t"
                     "GIT_COMMITTER_EMAIL=t@t")
                   process-environment)))
     (cl-flet ((commit (file text)
                 (wgn-agent-plans-test--write (expand-file-name file repo) text)
                 (let ((default-directory repo))
                   (should (eq 0 (call-process "git" nil nil nil "add" file)))
                   (should (eq 0 (call-process "git" nil nil nil "-c" "commit.gpgsign=false"
                                               "commit" "-q" "-m" file)))
                   (wgn/agent-plans--git repo "rev-parse" "HEAD"))))
       (unwind-protect
           (progn
             (make-directory repo t)
             (let ((default-directory repo))
               (should (eq 0 (call-process "git" nil nil nil "init" "-q")))
               (should (eq 0 (call-process "git" nil nil nil "remote" "add" "origin"
                                           "git@github.com:wesnel/thing.git"))))
             ,@body)
         (delete-directory root t)))))

(ert-deftest wgn-agent-plans-repository-is-found-by-name ()
  (wgn-agent-plans-test--with-repo
    (should (equal (wgn/agent-plans--repository "thing") repo))
    (should (equal (wgn/agent-plans--repository "wesnel/thing") repo))
    (should (equal (wgn/agent-plans--repository repo) repo))
    (should-not (wgn/agent-plans--repository "someone/thing" t))
    (should-error (wgn/agent-plans--repository "nothing") :type 'user-error)))

(ert-deftest wgn-agent-plans-git-links-open-magit ()
  (wgn-agent-plans-test--with-repo
    (wgn-agent-plans-test--with-links
      (let (shown)
        (cl-letf (((symbol-function 'magit-log-other)
                   (lambda (revs &rest _) (push (list 'log revs default-directory) shown)))
                  ((symbol-function 'magit-show-commit)
                   (lambda (rev &rest _) (push (list 'commit rev default-directory) shown)))
                  ((symbol-function 'magit-status)
                   (lambda (directory &rest _) (push (list 'status directory) shown))))
          (wgn-agent-plans-test--follow "[[branch:thing:org-plans]]")
          (wgn-agent-plans-test--follow "[[commit:wesnel/thing:abc1234]]")
          (wgn-agent-plans-test--follow (format "[[wt:%s]]" (directory-file-name repo)))
          (should-error (wgn-agent-plans-test--follow "[[branch:thing]]") :type 'user-error))
        (should (equal (reverse shown)
                       `((log ("org-plans") ,repo) (commit "abc1234" ,repo) (status ,repo))))))))

(ert-deftest wgn-agent-plans-pr-link-uses-forge-else-the-browser ()
  (wgn-agent-plans-test--with-repo
    (wgn-agent-plans-test--with-links
      (let (shown)
        (cl-letf (((symbol-function 'forge-get-pullreq)
                   (lambda (number) (and (= number 3) (list 'pullreq number default-directory))))
                  ((symbol-function 'forge-topic-setup-buffer)
                   (lambda (topic) (push topic shown)))
                  ((symbol-function 'browse-url)
                   (lambda (url &rest _) (push url shown))))
          (wgn-agent-plans-test--follow "[[pr:thing#3]]")
          ;; Not pulled into forge.
          (wgn-agent-plans-test--follow "[[pr:thing#4]]")
          ;; Not cloned here.
          (wgn-agent-plans-test--follow "[[pr:wesnel/elsewhere#5]]")
          (should-error (wgn-agent-plans-test--follow "[[pr:elsewhere#5]]") :type 'user-error)
          (should-error (wgn-agent-plans-test--follow "[[pr:thing#x]]") :type 'user-error))
        (should (equal (reverse shown)
                       `((pullreq 3 ,repo)
                         "https://github.com/wesnel/thing/pull/4"
                         "https://github.com/wesnel/elsewhere/pull/5")))))))

(ert-deftest wgn-agent-plans-links-export-as-text-or-their-address ()
  (wgn-agent-plans-test--with-repo
    (wgn-agent-plans-test--with-links
      (require 'ox-html)
      (require 'ox-md)
      (let ((org-export-with-toc nil)
            (org-html-postamble nil))
        (let ((html (org-export-string-as
                     (concat "[[pr:thing#4][the PR & more]] and [[plan:2026-10-14-x.mac#Q1]]"
                             " and [[commit:thing:a<b]] and [[plan:x][a & b]]")
                     'html t)))
          (should (string-match-p
                   "<a href=\"https://github.com/wesnel/thing/pull/4\">the PR &amp; more</a>" html))
          (should (string-match-p "<code>plan:2026-10-14-x.mac#Q1</code>" html))
          (should (string-match-p "<code>commit:thing:a&lt;b</code>" html))
          ;; A description arrives exported already.
          (should (string-match-p " a &amp; b" html)))
        (require 'ox-latex)
        (let ((latex (org-export-string-as
                      "[[plan:2026-10-14-x.mac#Q_1]] and [[pr:thing#4][the PR & more]]" 'latex t)))
          (should (string-match-p (regexp-quote "\\texttt{plan:2026-10-14-x.mac\\#Q\\_1}") latex))
          (should (string-match-p (regexp-quote "\\href{https://github.com/wesnel/thing/pull/4}{the PR \\& more}")
                                  latex)))
        (should (string-match-p "`branch:thing:main`"
                                (org-export-string-as "[[branch:thing:main]]" 'md t)))))))

(ert-deftest wgn-agent-plans-citation-goes-to-the-line-and-warns-of-drift ()
  (wgn-agent-plans-test--with-repo
    (wgn-agent-plans-test--with-links
      (let* ((cited (commit "a.el" "one\ntwo\nthree\n"))
             (file (expand-file-name "a.el" repo))
             warnings)
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (_type message &rest _) (push message warnings))))
          (with-temp-buffer
            (wgn-agent-plans-test--follow (format "[[file:%s::2@%s]]" file (substring cited 0 7)))
            (should (equal (buffer-file-name) file))
            (should (equal (line-number-at-pos) 2))
            (should-not warnings)
            (kill-buffer))
          (commit "b.el" "other\n")
          (with-temp-buffer
            (wgn-agent-plans-test--follow (format "[[file:%s::3@%s]]" file cited))
            (should (equal (line-number-at-pos) 3))
            (kill-buffer))
          (should (string-match-p "the file is the same there" (car warnings)))
          (commit "a.el" "zero\none\ntwo\nthree\n")
          (with-temp-buffer
            (wgn-agent-plans-test--follow (format "[[file:%s::3@%s]]" file cited))
            (kill-buffer))
          (should (string-match-p "differs, so the line may have moved" (car warnings)))
          (with-temp-buffer
            (wgn-agent-plans-test--follow (format "[[file:%s::1@deadbeef]]" file))
            (kill-buffer))
          (should (string-match-p "deadbeef is not a commit" (car warnings))))))))

;;;; Capture

(defun wgn-agent-plans-test--capture (key answers body &optional abort)
  "Capture with template KEY, answering its prompts with ANSWERS, then BODY.
Finalize it, or with ABORT leave it open; return the capture buffer."
  (require 'org-capture)
  (let ((org-capture-templates (wgn/agent-plans-capture-templates))
        (org-capture-mode-hook (list #'wgn/agent-plans--capture-header))
        (before (buffer-list)))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (pop answers)))
              ((symbol-function 'read-string) (lambda (&rest _) (pop answers))))
      (org-capture nil key))
    (let ((capture (seq-find (lambda (buffer)
                               (and (buffer-local-value 'org-capture-mode buffer)
                                    (not (memq buffer before))))
                             (buffer-list))))
      (with-current-buffer capture
        (goto-char (point-max))
        (insert body)
        (unless abort (org-capture-finalize)))
      capture)))

(wgn-agent-plans-test--in-both-forms wgn-agent-plans-capture-followup-runs-agent-plan-followup
  (wgn-agent-plans-test--with-cli
    (wgn-agent-plans-test--protected
      (let* ((plan (wgn-agent-plans-test--question-plan plans))
             (before (with-temp-buffer (insert-file-contents plan) (buffer-string))))
        (wgn-agent-plans-test--visiting plan
          (wgn-agent-plans-test--goto "S1")
          (let ((offered-default nil))
            (cl-letf (((symbol-function 'org-completing-read)
                       (lambda (_prompt _choices &optional _p _r _i _h default &rest _)
                         (setq offered-default default)
                         (completing-read ""))))
              (let ((capture (wgn-agent-plans-test--capture
                              "Af" (list "Guard accepts --draft=false" "S1")
                              "The guard reads the flag\nas a bare word.\n\n- in pr-loop\n- in sign-pass\n"
                              t)))
                (with-current-buffer capture
                  (should (string-match-p "paragraphs joined by ¶" header-line-format))
                  (org-capture-finalize))))
            (should (equal offered-default "S1"))))
        ;; agent-plan takes a single line: nothing is dropped.
        (should (equal (wgn-agent-plans-test--calls record)
                       `(("followup" ,plan "--title" "Guard accepts --draft=false" "--found-in" "S1"
                          "--context" "The guard reads the flag as a bare word. ¶ - in pr-loop - in sign-pass"))))
        (should-not (seq-find (lambda (buffer) (string-prefix-p " *agent-plan capture" (buffer-name buffer)))
                              (buffer-list)))
        (should (equal (with-temp-buffer (insert-file-contents plan) (buffer-string)) before))))))

;; org-id gives an entry an ID of its own, as `org-id-get-create' or
;; `org-store-link' do; the entry's plan ID is still its CUSTOM_ID.
(ert-deftest wgn-agent-plans-capture-offers-custom-id-over-an-org-id ()
  (wgn-agent-plans-test--with-cli
    (wgn-agent-plans-test--protected
      (let ((plan (expand-file-name "2026-10-14-x.mac.org" plans))
            offered-default)
        (wgn-agent-plans-test--write
         plan (wgn-agent-plans-test--plan-text
               "Both" :Steps (concat "*** TODO First\n:PROPERTIES:\n:CUSTOM_ID: S1\n"
                                     ":ID: 3f2a9c1e-7b4d-4e8a-9f10-2c3d4e5f6a7b\n:END:\n")))
        (wgn-agent-plans-test--visiting plan
          (wgn-agent-plans-test--goto "S1")
          (cl-letf (((symbol-function 'org-completing-read)
                     (lambda (_prompt _choices &optional _p _r _i _h default &rest _)
                       (setq offered-default default)
                       (completing-read ""))))
            (wgn-agent-plans-test--capture "Af" (list "Found here" "S1") "")))
        (should (equal offered-default "S1"))))))

(ert-deftest wgn-agent-plans-capture-question-runs-agent-plan-ask-in-the-part ()
  (wgn-agent-plans-test--with-cli
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--question-plan plans)))
        (wgn-agent-plans-test--visiting (expand-file-name "2026-10-14-x.mac.P1.org" plans)
          (wgn-agent-plans-test--goto "P1.Q1")
          (let ((capture (wgn-agent-plans-test--capture
                          "Aq" (list "Keep the flag?" "yes: scripts use it")
                          "Two scripts pass it.\n\nBoth are ours.\n- drop it\n- rename it\n" t)))
            (with-current-buffer capture
              (should (string-match-p "list items become the alternatives" header-line-format))
              (goto-char (point-min))
              (insert "* [#A]")
              (delete-region (point) (progn (skip-chars-forward "* ") (point)))
              (insert " ")
              (org-entry-put nil "AFTER" "P1.Q1 S1")
              (org-capture-finalize))))
        (should (equal (wgn-agent-plans-test--calls record)
                       `(("ask" ,plan "--question" "Keep the flag?" "--recommendation" "yes: scripts use it"
                          "--priority" "A" "--context" "Two scripts pass it. ¶ Both are ours."
                          "--alternative" "drop it" "--alternative" "rename it"
                          "--after" "P1.Q1" "--after" "S1" "--part" "P1"))))))))

(ert-deftest wgn-agent-plans-capture-question-keeps-sub-items-with-their-alternative ()
  (wgn-agent-plans-test--with-cli
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--question-plan plans)))
        (wgn-agent-plans-test--visiting plan
          (wgn-agent-plans-test--goto "S1")
          (wgn-agent-plans-test--capture
           "Aq" (list "Keep the flag?" "yes")
           "Why.\n- drop it\n  - only the flag\n  - and its tests\n- rename it\n  to --x\n  1. in one pass\n\nAfter.\n"))
        (should (equal (wgn-agent-plans-test--calls record)
                       `(("ask" ,plan "--question" "Keep the flag?" "--recommendation" "yes"
                          "--context" "Why. ¶ After."
                          "--alternative" "drop it: only the flag; and its tests"
                          "--alternative" "rename it to --x: in one pass"))))))))

(ert-deftest wgn-agent-plans-capture-refuses-to-refile ()
  (wgn-agent-plans-test--with-cli
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--question-plan plans)))
        (wgn-agent-plans-test--visiting plan
          (wgn-agent-plans-test--goto "S1")
          (dolist (key '("Af" "Aq"))
            (let ((capture (wgn-agent-plans-test--capture key (list "Refiled" "S1") "" t)))
              (with-current-buffer capture
                (should-error (org-capture-refile) :type 'user-error)
                (should (buffer-live-p capture))
                (org-capture-kill)))))
        (should-not (wgn-agent-plans-test--calls record))
        (should-not (seq-find (lambda (buffer)
                                (string-prefix-p " *agent-plan capture" (buffer-name buffer)))
                              (buffer-list)))))))

(ert-deftest wgn-agent-plans-capture-stays-open-when-refused ()
  (wgn-agent-plans-test--with-cli
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--question-plan plans))
            (process-environment (cons "REFUSE=no such item S9" process-environment)))
        (wgn-agent-plans-test--visiting plan
          (let ((capture (wgn-agent-plans-test--capture "Af" (list "Something" "S9") "" t)))
            (with-current-buffer capture
              (should-error (org-capture-finalize) :type 'user-error)
              (should (buffer-live-p capture))
              (org-capture-kill))
            (should-not (buffer-live-p capture))
            (should-not (seq-find (lambda (buffer)
                                    (string-prefix-p " *agent-plan capture" (buffer-name buffer)))
                                  (buffer-list)))))
        ;; The refused call, and nothing for the abort.
        (should (equal (length (wgn-agent-plans-test--calls record)) 1))))))

(ert-deftest wgn-agent-plans-capture-elsewhere-asks-for-the-plan ()
  (wgn-agent-plans-test--with-cli
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--question-plan plans))
            (mirror (expand-file-name "2026-10-14-y.devbox.org" plans)))
        (wgn-agent-plans-test--write mirror "* Plan\n")
        (with-temp-buffer
          (cl-letf (((symbol-function 'wgn/agent-plans--read) (lambda () plan)))
            (wgn-agent-plans-test--capture "Af" (list "Found elsewhere" "S1") "")))
        (should (equal (wgn-agent-plans-test--calls record)
                       `(("followup" ,plan "--title" "Found elsewhere" "--found-in" "S1"))))
        ;; Another host's plan is refused before any prompt.
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (error "Prompted")))
                  ((symbol-function 'read-string) (lambda (&rest _) (error "Prompted")))
                  (org-capture-templates (wgn/agent-plans-capture-templates)))
          (with-temp-buffer
            (cl-letf (((symbol-function 'wgn/agent-plans--read) (lambda () mirror)))
              (should-error (org-capture nil "Af") :type 'user-error)))
          (wgn-agent-plans-test--visiting mirror
            (should-error (org-capture nil "Aq") :type 'user-error)))
        (should (equal (length (wgn-agent-plans-test--calls record)) 1))))))

(ert-deftest wgn-agent-plans-captures-at-once-keep-their-own-text ()
  (wgn-agent-plans-test--with-cli
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--question-plan plans)))
        (wgn-agent-plans-test--visiting plan
          (wgn-agent-plans-test--goto "S1")
          (let ((first (wgn-agent-plans-test--capture "Af" (list "First" "S1") "One.\n" t))
                (second (with-current-buffer (find-buffer-visiting plan)
                          (wgn-agent-plans-test--capture "Aq" (list "Second?" "yes") "Two.\n" t))))
            (with-current-buffer second (org-capture-finalize))
            (with-current-buffer first (org-capture-finalize))))
        (should (equal (wgn-agent-plans-test--calls record)
                       `(("ask" ,plan "--question" "Second?" "--recommendation" "yes" "--context" "Two.")
                         ("followup" ,plan "--title" "First" "--found-in" "S1" "--context" "One."))))
        (should-not (seq-find (lambda (buffer)
                                (string-prefix-p " *agent-plan capture" (buffer-name buffer)))
                              (buffer-list)))))))

;;;; Against the real agent-plan
;;
;; Skipped where `agent-plan' is not on `exec-path'; nix-config's
;; `agent-plans' check runs it with the agent-plan of the same commit.

(defun wgn-agent-plans-test--agent-plan (&rest args)
  "Run the real `agent-plan' with ARGS and return its output; fail on error."
  (with-temp-buffer
    (let ((status (apply #'call-process "agent-plan" nil t nil args)))
      (unless (eq status 0)
        (ert-fail (list args status (buffer-string))))
      (buffer-string))))

(ert-deftest wgn-agent-plans-views-read-what-agent-plan-writes ()
  (skip-unless (executable-find "agent-plan"))
  (let* ((root (file-name-as-directory (make-temp-file "agent-plans-real-" t)))
         (plans (expand-file-name "notes/agents" root))
         (config (expand-file-name "config.json" root))
         ;; git reports a worktree by its true name.
         (worktree (file-truename (expand-file-name "wt" root)))
         (process-environment
          (append (list (concat "AGENT_TOOLS_CONFIG=" config)
                        "GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1"
                        "CLAUDE_CODE_SESSION_ID")
                  process-environment))
         (org-agenda-custom-commands (list (wgn/agent-plans-agenda-command)))
         (wgn/agent-plans-questions-interval 30)
         (wgn/agent-plans-questions--cache (make-hash-table :test #'equal))
         (wgn/agent-plans-questions--string "")
         (wgn/agent-plans-questions--last-directory nil))
    (unwind-protect
        (wgn-agent-plans-test--protected
          (wgn-agent-plans-test--write
           config (format "{\"plans\": {\"dir\": %S, \"host\": \"mac\"}}" plans))
          (make-directory (expand-file-name "lisp" worktree) t)
          (let ((default-directory (file-name-as-directory worktree)))
            (should (eq 0 (call-process "git" nil nil nil "init" "-q"))))
          (let* ((plan (gethash "plan" (json-parse-string
                                        (wgn-agent-plans-test--agent-plan
                                         "new" "--title" "Real plan" "--slug" "real"
                                         "--repo" "wesnel/emacs-config" "--worktree" worktree)))))
            ;; Agents write steps by hand.
            (with-temp-buffer
              (insert-file-contents plan)
              (goto-char (point-min))
              (re-search-forward "^\\*\\* Steps\n")
              (insert (wgn-agent-plans-test--entry "TODO" "First step" "S1")
                      (wgn-agent-plans-test--entry "TODO" "Second step" "S2"))
              (wgn-agent-plans-test--write plan (buffer-string)))
            (wgn-agent-plans-test--agent-plan
             "ask" plan "--question" "Keep the old flag?" "--recommendation" "no")
            (wgn-agent-plans-test--agent-plan
             "ask" plan "--question" "Rename it too?" "--recommendation" "yes" "--after" "Q1")
            (wgn-agent-plans-test--agent-plan
             "ask" plan "--question" "Keep tests offline?" "--recommendation" "yes"
             "--answer" "yes: CI has no network" "--role" "single")
            (wgn-agent-plans-test--agent-plan
             "followup" plan "--title" "Guard accepts --draft=false" "--found-in" "S1")
            (wgn-agent-plans-test--agent-plan "log" plan "--item" "S1" "--kind" "done")
            ;; The A view.
            (let* ((blocks (unwind-protect (wgn-agent-plans-test--agenda-blocks)
                             (org-agenda-quit)))
                   (questions (or (cdr (assoc "Waiting on you" blocks)) ""))
                   (ready (or (cdr (assoc "Ready" blocks)) ""))
                   (followups (or (cdr (assoc "Follow-ups" blocks)) "")))
              (should (string-match-p "Keep the old flag" questions))
              (should-not (string-match-p "Rename it too\\|Keep tests offline" questions))
              (should (string-match-p "Second step" ready))
              (should-not (string-match-p "First step" ready))
              (should (string-match-p "Guard accepts --draft=false" followups)))
            ;; The dynamic block.
            (with-temp-buffer
              (org-mode)
              (insert (format "* Notes\n#+BEGIN: agent-plan :plan %S\n#+END:\n" plan))
              (goto-char (point-min))
              (re-search-forward "^#\\+BEGIN")
              (beginning-of-line)
              (org-update-dblock)
              (should (string-match-p "^| S1 | First step | yes | no | |$" (buffer-string)))
              (should (string-match-p "^- Steps ready: S2$" (buffer-string))))
            ;; The indicator, given only a directory in the worktree.
            (wgn-agent-plans-test--statusline
             (file-name-as-directory (expand-file-name "lisp" worktree)))
            (should (string-match-p "1 open question · /questions"
                                    wgn/agent-plans-questions--string))
            (dolist (buffer (buffer-list))
              (when (and (buffer-file-name buffer)
                         (string-prefix-p root (buffer-file-name buffer)))
                (with-current-buffer buffer (set-buffer-modified-p nil))
                (kill-buffer buffer)))))
      (delete-directory root t))))

;;; agent-plans.el ends here
