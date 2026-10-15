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
          (wgn/agent-plans-questions--last-key nil))
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

(defun wgn-agent-plans-test--statusline (directory &optional setup)
  "Run the indicator's update in DIRECTORY and wait for its process.
SETUP, a function, makes the buffer an agent's first."
  (with-temp-buffer
    (setq default-directory directory)
    (when setup (funcall setup))
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

(defun wgn-agent-plans-test--sent (record)
  "Return what the fake was sent, given its RECORD file, as parsed JSON."
  (with-temp-buffer
    (insert-file-contents record)
    (mapcar (lambda (line) (json-parse-string line :object-type 'alist))
            (split-string (buffer-string) "\n" t))))

(defun wgn-agent-plans-test--agent-shell (identifier used size)
  "Return a function making the buffer an agent-shell of IDENTIFIER.
Its context is USED of SIZE tokens, as agent-shell keeps them from ACP's
usage_update."
  (lambda ()
    (setq-local major-mode 'agent-shell-mode)
    (setq-local agent-shell--state
                `((:agent-config . ((:identifier . ,identifier)))
                  (:usage . ((:total-tokens . 0)
                             (:context-used . ,used)
                             (:context-size . ,size)))))))

(defun wgn-agent-plans-test--eca-chat (model tokens limit)
  "Return a function making the buffer an ECA chat with MODEL.
Its context is TOKENS of LIMIT, as eca.el keeps them."
  (lambda ()
    (setq-local major-mode 'eca-chat-mode)
    (setq-local eca-chat--selected-model model)
    (setq-local eca-chat--session-tokens tokens)
    (setq-local eca-chat--session-limit-context limit)))

(ert-deftest wgn-agent-plans-questions-indicator-sends-the-agent-session ()
  (wgn-agent-plans-test--with-statusline
    (wgn-agent-plans-test--statusline
     dir (wgn-agent-plans-test--agent-shell 'claude-code 42000 100000))
    (wgn-agent-plans-test--statusline
     dir (wgn-agent-plans-test--eca-chat "openai/gpt-5.5" 52000 200000))
    (wgn-agent-plans-test--statusline
     dir (wgn-agent-plans-test--agent-shell 'gemini-cli 0 0))
    (should (equal (wgn-agent-plans-test--sent record)
                   `(((cwd . ,dir) (provider . "claude")
                      (context_window . ((used_percentage . 42))))
                     ((cwd . ,dir) (provider . "codex")
                      (context_window . ((used_percentage . 26))))
                     ((cwd . ,dir)))))))

(ert-deftest wgn-agent-plans-questions-indicator-is-cached-per-session ()
  (wgn-agent-plans-test--with-statusline
    (let ((shell (wgn-agent-plans-test--agent-shell 'codex 1000 100000)))
      (wgn-agent-plans-test--statusline dir shell)
      (wgn-agent-plans-test--statusline dir shell)
      (should (= (wgn-agent-plans-test--runs record) 1))
      ;; Another session, or the same one further on, is asked about.
      (wgn-agent-plans-test--statusline
       dir (wgn-agent-plans-test--agent-shell 'codex 5000 100000))
      (should (= (wgn-agent-plans-test--runs record) 2))
      ;; A session before its first usage report sends no context.
      (wgn-agent-plans-test--statusline
       dir (wgn-agent-plans-test--eca-chat "anthropic/claude-opus-5-5" nil nil))
      (should (equal (car (last (wgn-agent-plans-test--sent record)))
                     `((cwd . ,dir) (provider . "claude")))))))

(ert-deftest wgn-agent-plans-questions-cache-stays-bounded ()
  (wgn-agent-plans-test--with-statusline
    (dotimes (used 20)
      (wgn-agent-plans-test--statusline
       dir (wgn-agent-plans-test--agent-shell 'codex (1+ used) 7)))
    (should (= (hash-table-count wgn/agent-plans-questions--cache) 1))
    ;; A context that rounds to the same percentage asks nothing more.
    (let ((runs (wgn-agent-plans-test--runs record)))
      (wgn-agent-plans-test--statusline
       dir (wgn-agent-plans-test--agent-shell 'codex 20.001 7))
      (should (= (wgn-agent-plans-test--runs record) runs)))))

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

(defun wgn-agent-plans-test--entry (keyword title id &optional after)
  "Return an entry heading KEYWORD TITLE with ID, waiting for AFTER."
  (concat "*** " keyword " " title "\n:PROPERTIES:\n:ID: " id "\n"
          (if after (concat ":AFTER: " after "\n") "")
          ":END:\n"))

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
  (re-search-forward (format "^:ID: %s$" (regexp-quote id)))
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
         (after-revert-hook nil))
     (unwind-protect
         (progn (wgn/agent-plans-protect-mode 1) ,@body)
       (wgn/agent-plans-protect-mode -1))))

(defun wgn-agent-plans-test--waiting-plan (plans)
  "Make plan 2026-10-14-x.mac under PLANS, with a part P1, and return it.
S2 waits for the part's P1.S1, and Q2 for Q1."
  (let ((plan (expand-file-name "2026-10-14-x.mac.org" plans)))
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
     (expand-file-name "2026-10-14-x.mac.P1.org" plans)
     (wgn-agent-plans-test--plan-text
      "Part one" :Steps (wgn-agent-plans-test--entry "TODO" "The part's step" "P1.S1")))
    plan))

(ert-deftest wgn-agent-plans-host-follows-agent-plan ()
  (wgn-agent-plans-test--with-plans
    (should (equal (wgn/agent-plans-host) "mac"))
    (wgn-agent-plans-test--write config (format "{\"plans\": {\"dir\": %S}}" plans))
    (should (equal (wgn/agent-plans-host)
                   (car (split-string (system-name) "\\."))))))

(ert-deftest wgn-agent-plans-blocker-waits-for-after-items ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (let ((plan (wgn-agent-plans-test--waiting-plan plans)))
        (wgn-agent-plans-test--visiting plan
          (should (equal (wgn-agent-plans-test--try "S2" "DONE") "TODO"))
          (should (equal (wgn-agent-plans-test--try "Q2" "DECIDED") "ASK"))
          (should (equal (wgn-agent-plans-test--try "S2" "STARTED") "STARTED"))
          (should (equal (wgn-agent-plans-test--try "Q1" "DECIDED") "DECIDED"))
          (should (equal (wgn-agent-plans-test--try "Q2" "DECIDED") "DECIDED")))
        ;; The part's step, settled in its own file, unblocks S2.
        (wgn-agent-plans-test--visiting (expand-file-name "2026-10-14-x.mac.P1.org" plans)
          (should (equal (wgn-agent-plans-test--try "P1.S1" "ABANDONED") "ABANDONED"))
          (save-buffer))
        (wgn-agent-plans-test--visiting plan
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
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert "edited\n"))
          (should-error (save-buffer) :type 'user-error))
        (should (equal (with-temp-buffer (insert-file-contents mirror) (buffer-string))
                       text))))))

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

(defun wgn-agent-plans-test--agenda-plans (plans)
  "Make an own plan, its part and another host's plan under PLANS.
Return the other host's plan file."
  (wgn-agent-plans-test--write
   (expand-file-name "2026-10-14-x.mac.org" plans)
   (wgn-agent-plans-test--plan-text
    "Ours"
    :Steps (concat (wgn-agent-plans-test--entry "TODO" "First step" "S1")
                   (wgn-agent-plans-test--entry "TODO" "After the first" "S2" "S1")
                   "*** TODO Postponed step\nSCHEDULED: <2099-01-01 Thu>\n:PROPERTIES:\n:ID: S3\n:END:\n"
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
  (wgn-agent-plans-test--write (expand-file-name "2026-10-14-x.mac.log.org" plans)
                               "* TODO Not an entry\n:PROPERTIES:\n:ID: S9\n:END:\n")
  (let ((mirror (expand-file-name "2026-10-13-y.devbox.org" plans)))
    (wgn-agent-plans-test--write
     mirror
     (wgn-agent-plans-test--plan-text
      "Theirs"
      :Steps (wgn-agent-plans-test--entry "TODO" "Their step" "S1")
      :Questions (wgn-agent-plans-test--entry "ASK" "Their question?" "Q1")))
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
           ((member line '("Questions for the operator" "Ready" "Follow-ups"))
            (push (list line) blocks))
           (blocks (push line (cdar blocks)))))
        (forward-line 1)))
    (mapcar (lambda (block) (cons (car block) (string-join (cdr block) "\n"))) blocks)))

(ert-deftest wgn-agent-plans-agenda-lists-what-is-ready ()
  (wgn-agent-plans-test--with-plans
    (wgn-agent-plans-test--protected
      (wgn-agent-plans-test--agenda-plans plans)
      (let* ((org-agenda-files '("~/org"))
             (org-refile-targets '((nil :maxlevel . 9)))
             (blocks (unwind-protect (wgn-agent-plans-test--agenda-blocks)
                       (org-agenda-quit)))
             (questions (or (cdr (assoc "Questions for the operator" blocks)) ""))
             (ready (or (cdr (assoc "Ready" blocks)) ""))
             (followups (or (cdr (assoc "Follow-ups" blocks)) "")))
        (should (string-match-p "Keep it\\?" questions))
        ;; Waits for Q1, settled, and another host's to answer there.
        (should-not (string-match-p "Rename it\\|Settled question\\|Their question" questions))
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
                (should-error (save-buffer) :type 'user-error)))
          (org-agenda-quit)
          (let ((buffer (find-buffer-visiting mirror)))
            (when buffer
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))
        (should (equal (with-temp-buffer (insert-file-contents mirror) (buffer-string))
                       before))))))

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

(defun wgn-agent-plans-test--agent-plan-input (input &rest args)
  "Run the real `agent-plan' with ARGS and INPUT on stdin; fail on error."
  (with-temp-buffer
    (insert input)
    (let ((status (apply #'call-process-region (point-min) (point-max) "agent-plan" t t nil args)))
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
                        (concat "XDG_STATE_HOME=" (expand-file-name "state" root))
                        "GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1"
                        "CLAUDE_CODE_SESSION_ID")
                  process-environment))
         (org-agenda-custom-commands (list (wgn/agent-plans-agenda-command)))
         (wgn/agent-plans-questions-interval 30)
         (wgn/agent-plans-questions--cache (make-hash-table :test #'equal))
         (wgn/agent-plans-questions--string "")
         (wgn/agent-plans-questions--last-key nil))
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
              (write-region nil nil plan))
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
                   (questions (or (cdr (assoc "Questions for the operator" blocks)) ""))
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
            ;; An agent-shell session's context, and the quota a Claude
            ;; Code session on this host recorded.
            (wgn-agent-plans-test--agent-plan-input
             (format "{\"session_id\":\"other\",\"rate_limits\":{\"five_hour\":{\"used_percentage\":20,\"resets_at\":%d}}}"
                     (+ (truncate (float-time)) 3600))
             "statusline")
            (wgn-agent-plans-test--statusline
             (file-name-as-directory (expand-file-name "lisp" worktree))
             (wgn-agent-plans-test--agent-shell 'claude-code 42000 100000))
            (should (equal (substring-no-properties wgn/agent-plans-questions--string)
                           " ctx 42% · 5h 20% · 1 open question · /questions"))
            (dolist (buffer (buffer-list))
              (when (and (buffer-file-name buffer)
                         (string-prefix-p root (buffer-file-name buffer)))
                (with-current-buffer buffer (set-buffer-modified-p nil))
                (kill-buffer buffer)))))
      (delete-directory root t))))

;;; agent-plans.el ends here
