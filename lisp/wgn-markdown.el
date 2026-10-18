;;; wgn-markdown.el --- Code blocks in Markdown and Org others wrote -*- lexical-binding: t; -*-

;; markdown-mode, Org (source and export blocks alike), shell-maker's
;; markdown-overlays (chatgpt-shell, agent-shell) and gptel-agent's tool
;; output fontify a code block by calling the major mode its language
;; or file name names in a scratch buffer, and the text comes from
;; people and models: pull request bodies and comments, forge topics,
;; ECA, gptel and agent replies.  A name is only what someone chose, so
;; the mode is called only when it is one of an allowed set, and then
;; inertly: without mode hooks, file-local or directory-local variables,
;; a file name or auto-saving, and without what its body adds to the
;; alists of file modes or, as closures, to global hooks outliving it.

(require 'cl-lib)
(require 'subr-x)

(defvar markdown-code-lang-modes)
(defvar org-src-lang-modes)
(declare-function markdown-overlays--resolve-internal-language "markdown-overlays")

(defconst wgn/fence-modes '(fundamental-mode markdown-mode gfm-mode)
  "Modes a code block may be fontified in whatever they derive from.")

(defconst wgn/fence-mode-families
  '(prog-mode text-mode conf-mode diff-mode css-base-mode sgml-mode)
  "Modes whose derived modes a code block may be fontified in.")

(defconst wgn/fence-denied-families
  '(special-mode comint-mode compilation-mode
    message-mode mail-mode log-edit-mode git-commit-elisp-text-mode forge-post-mode
    org-mode TeX-mode tex-mode latex-mode ses-mode
    image-mode archive-mode tar-mode doc-view-mode hexl-mode reader-mode
    nxml-mode plstore-mode go-asm-mode vhdl-mode idlwave-mode edebug-eval-mode reb-lisp-mode
    cider-history-mode slime-xref-mode dape-disassemble-mode
    antlr-mode cperl-mode verilog-mode bibtex-mode ps-mode nix-drv-mode)
  "Modes inside `wgn/fence-mode-families' whose derived modes a code
block is never fontified in: they read or write files, run processes,
leave timers or global hooks behind, choose another mode from the
text, set up mail, version control or REPL state, or evaluate what
the buffer holds.  Each is denied by name before anything loads.")

(defconst wgn/fence-denied-names
  (rx (or "--" "TeX" (seq "-maybe-mode" eos)
          (seq bos (or "inferior-" "agent-shell-" "chatgpt-shell-" "eca-" "forge-"
                       "magit-" "git-" "cider-" "slime-" "dape-" "gdb-" "gud-"
                       "edebug-"))))
  "Mode names a code block is never fontified in, matched with case:
internal modes, AUCTeX's, modes that choose another mode from the
buffer's text, and the modes of tools that act on a session,
repository or process.")

(defconst wgn/fence-loadable-modes
  '(html-mode makefile-mode)
  "Autoloaded modes no alist of file modes names that a code block may
load to be fontified in.  Loading a library runs it, so a mode joins
this list only once loading it is seen to leave no timer, process,
prompt, global mode or new entry in an alist of file modes behind.
The built-in tree-sitter modes fail the last: loading one claims its
file names in `auto-mode-alist' when its grammar is installed.")

(defun wgn/fence--named-p (mode)
  "Return non-nil when an alist of file or language modes names MODE."
  (cl-some (lambda (alist)
             (cl-some (lambda (entry)
                        (and (consp entry)
                             (eq mode (if (consp (cdr entry)) (cadr entry) (cdr entry)))))
                      alist))
           (list auto-mode-alist interpreter-mode-alist magic-mode-alist
                 (bound-and-true-p major-mode-remap-alist)
                 (bound-and-true-p markdown-code-lang-modes))))

(defun wgn/fence--load (mode)
  "Load MODE's definition when it is still an autoload of a function,
and an alist of file modes names it or it is in
`wgn/fence-loadable-modes'.  A code block's language must not choose
which other library loads: loading one runs it.  A named library loads
before the mode is known to be minor or denied, as artist.el does for
a ditaa block, along with what its `with-eval-after-load' forms load:
an alist already maps a file or language to it, so the session would
load it for that anyway."
  (let ((definition (symbol-function mode)))
    (when (and (autoloadp definition) (memq (nth 4 definition) '(nil t))
               (or (memq mode wgn/fence-loadable-modes) (wgn/fence--named-p mode)))
      (ignore-errors (autoload-do-load definition mode)))))

(defun wgn/fence-mode-p (mode)
  "Return non-nil when a code block may be fontified by calling MODE.
MODE must be in `wgn/fence-modes', or derive from one of
`wgn/fence-mode-families' and none of `wgn/fence-denied-families'
nor markdown-mode, and its name must not match
`wgn/fence-denied-names'.  What the name decides is judged first, and
only then may an autoloaded MODE be loaded, see `wgn/fence--load', so
that what it derives from is known."
  (and mode (symbolp mode) (fboundp mode)
       (not (let ((case-fold-search nil))
              (string-match-p wgn/fence-denied-names (symbol-name mode))))
       (not (memq mode wgn/fence-denied-families))
       (not (memq mode minor-mode-list))
       (progn (wgn/fence--load mode) t)
       (not (memq mode minor-mode-list))
       (or (memq mode wgn/fence-modes)
           (and (apply #'provided-mode-derived-p mode wgn/fence-mode-families)
                (not (apply #'provided-mode-derived-p mode wgn/fence-denied-families))
                (not (provided-mode-derived-p mode 'markdown-mode))))
       t))

(defvar wgn/fence--fontifying nil
  "Non-nil while a code block is fontified, when a language's mode is
judged by `wgn/fence-mode-p'.  Elsewhere, such as when the operator
edits a block of their own, the language's mode is left alone.")

(defconst wgn/fence--scratch-buffers
  (rx bos " *" (or "markdown-code" "markdown-overlays" "org-src") "-fontification:")
  "The names of the scratch buffers code blocks are fontified in.")

(defun wgn/fence--scrub ()
  "Leave every code block scratch buffer without a file or delayed hooks."
  (dolist (buffer (buffer-list))
    (when (string-match-p wgn/fence--scratch-buffers (buffer-name buffer))
      (with-current-buffer buffer
        (when buffer-auto-save-file-name
          (auto-save-mode -1))
        (setq buffer-file-name nil
              buffer-file-truename nil
              buffer-auto-save-file-name nil)
        (set-buffer-modified-p nil)))))

(defvar wgn/fence--old-buffers nil
  "The buffers that existed when the outermost code block fontification began.")

(defconst wgn/fence--watched-alists
  '(auto-mode-alist interpreter-mode-alist magic-mode-alist magic-fallback-mode-alist
    major-mode-remap-alist)
  "The alists of file modes a mode's body must not change for good.")

(defvar wgn/fence--alist-cells nil
  "One cell per code block fontification under way, innermost first.
Each cell's car holds copies of the values of
`wgn/fence--watched-alists' to restore when that fontification ends,
as (SYMBOL . VALUE).  A library loading refreshes every cell, so an
enclosing fontification, such as of a Markdown block holding another
block, keeps what the library added.")

(defvar wgn/fence--hooks nil
  "The (HOOK . FUNCTION)s a mode's body added to hooks' global values
while a code block was fontified, to remove after it.")

(defvar wgn/fence--load-file nil
  "`load-file-name' when the outermost code block fontification began:
a different one while it runs means a library is loading inside it.")

(defun wgn/fence--snapshot ()
  "Return copies of the current values of `wgn/fence--watched-alists'.
They are copies so that an edit in place, such as `assoc-delete-all'
or `setcdr', shows as a change."
  (mapcar (lambda (symbol)
            (cons symbol (and (boundp symbol) (copy-alist (default-value symbol)))))
          wgn/fence--watched-alists))

(defun wgn/fence--after-load (&rest _)
  "Keep what a library loaded during fontification adds to the alists.
A library's load-time setup is for the whole session; only what a
mode's body does afterwards is undone."
  (dolist (cell wgn/fence--alist-cells)
    (setcar cell (wgn/fence--snapshot))))

(defun wgn/fence--add-hook (add hook function &optional depth local)
  "Call ADD with HOOK, FUNCTION, DEPTH and LOCAL, noting a global closure.
While a code block is fontified, a function that is not a symbol added
to HOOK's global value outside a library's load is a mode body's
per-buffer state leaking into every buffer, such as clojure-mode's
docstring indentation; `wgn/fence--inert' removes it after.  So is a
closure that a `with-eval-after-load' form in the mode's body adds at
once, its feature being loaded already.  Named functions stay: a
library may add them once, as cc-mode does."
  (when (and wgn/fence--fontifying (not local)
             (equal load-file-name wgn/fence--load-file)
             (not (symbolp function))
             (not (and (boundp hook)
                       (let ((value (default-value hook)))
                         (if (functionp value) (equal value function) (member function value))))))
    (push (cons hook function) wgn/fence--hooks))
  (funcall add hook function depth local))

(defun wgn/fence--inert (fontify &rest args)
  "Call FONTIFY with ARGS so that the mode it calls for a code block is inert.
No file-local or directory-local variable is read, so the block's
text and the directory it is shown from cannot set anything, and
`wgn/fence--run-mode-hooks' keeps mode hooks from running.  What the
mode's body adds to the alists of file modes or, as closures, to hooks'
global values is undone after.  All of it ends however FONTIFY does,
returning, signalling or quit."
  (let ((wgn/fence--load-file (if wgn/fence--fontifying wgn/fence--load-file load-file-name))
        (wgn/fence--fontifying t)
        (wgn/fence--old-buffers (or wgn/fence--old-buffers (buffer-list)))
        (wgn/fence--alist-cells (cons (list (wgn/fence--snapshot)) wgn/fence--alist-cells))
        (wgn/fence--hooks nil)
        (enable-local-variables nil)
        (enable-dir-local-variables nil)
        (auto-save-default nil))
    (unwind-protect
        (apply fontify args)
      (pcase-dolist (`(,hook . ,function) wgn/fence--hooks)
        (remove-hook hook function))
      (pcase-dolist (`(,symbol . ,value) (car (car wgn/fence--alist-cells)))
        (unless (equal (and (boundp symbol) (default-value symbol)) value)
          (set-default symbol value)))
      (wgn/fence--scrub))))

(defun wgn/fence--run-mode-hooks (run &rest hooks)
  "Call RUN with HOOKS, except in a buffer a code block's fontification made.
That is a code block's scratch buffer, or any buffer that did not exist
when the fontification began, such as one the mode makes.  There a
mode's hooks, `after-change-major-mode-hook' and the file-local
variables `run-mode-hooks' would read are all skipped.  A buffer that
existed before keeps its hooks.  A timer or process filter that runs
while fontification waits is treated as part of it: a buffer it makes
skips its hooks, and a closure it adds to a global hook is removed
after.  No mode a code block may be fontified in is known to wait.
`delay-mode-hooks' cannot
do this: it is local to each buffer, and those buffers are made
inside the fontifying function."
  (unless (and wgn/fence--fontifying
               (or (string-match-p wgn/fence--scratch-buffers (buffer-name))
                   (not (memq (current-buffer) wgn/fence--old-buffers))))
    (apply run hooks)))

(defun wgn/fence--diff-syntax (fontify &rest args)
  "Call FONTIFY with ARGS unless a code block is fontified.
FONTIFY is `diff-syntax-fontify-hunk', which with
`diff-font-lock-syntax' set to hunk-also or hunk-only sets a mode
from the file name in the diff's header, or reads that file."
  (unless wgn/fence--fontifying
    (apply fontify args)))

(defun wgn/fence--mode-only (mode)
  "Return MODE, or nil when a code block is fontified and MODE may not be."
  (if (or (not wgn/fence--fontifying) (wgn/fence-mode-p mode)) mode nil))

(defun wgn/fence--markdown-overlays (fontify start end lang &rest args)
  "Call FONTIFY on a code block in LANG, inertly, as `wgn/fence--inert'.
FONTIFY is `markdown-overlays--fontify-source-block', which calls
LANG-mode itself: a LANG whose mode may not be called is replaced by
one that names no mode.  START, END and ARGS pass through."
  (let* ((name (and (stringp lang)
                    (or (markdown-overlays--resolve-internal-language lang)
                        (downcase (string-trim lang)))))
         (mode (and name (intern-soft (concat name "-mode")))))
    (apply #'wgn/fence--inert fontify start end
           (if (and mode (fboundp mode) (not (wgn/fence-mode-p mode)))
               "wgn-no-fence"
             lang)
           args)))

(defun wgn/fence--file-mode (file)
  "Return the mode `auto-mode-alist' gives FILE by its name alone."
  (let* ((case-fold-search auto-mode-case-fold)
         (mode (assoc-default (file-name-sans-versions file) auto-mode-alist
                              #'string-match-p)))
    (if (consp mode) (car mode) mode)))

(defun wgn/fence--gptel-agent (fontify path-or-mode start end)
  "Call FONTIFY on START to END, inertly, as `wgn/fence--inert'.
FONTIFY is `gptel-agent--fontify-block', which fontifies a tool's
text as the file a model named, PATH-OR-MODE, choosing the mode with
`set-auto-mode' from that name and the text's own mode cookie.  It is
given a mode instead: the one the file name alone gives, when it may
fontify a code block, else `fundamental-mode'."
  (wgn/fence--inert
   (lambda ()
     (let ((mode (if (symbolp path-or-mode)
                     path-or-mode
                   (and (stringp path-or-mode) (wgn/fence--file-mode path-or-mode)))))
       (funcall fontify (if (wgn/fence-mode-p mode) mode 'fundamental-mode) start end)))))

(defun wgn/markdown-guard-code-block-modes ()
  "Fontify code blocks in Markdown and Org only in allowed modes, inertly.
Each function advised here may be defined after this runs; the advice
still applies."
  (advice-add 'run-mode-hooks :around #'wgn/fence--run-mode-hooks)
  (advice-add 'add-hook :around #'wgn/fence--add-hook)
  (add-hook 'after-load-functions #'wgn/fence--after-load)
  (advice-add 'gptel-agent--fontify-block :around #'wgn/fence--gptel-agent)
  (advice-add 'diff-syntax-fontify-hunk :around #'wgn/fence--diff-syntax)
  (advice-add 'markdown-fontify-code-block-natively :around #'wgn/fence--inert)
  (advice-add 'markdown-get-lang-mode :filter-return #'wgn/fence--mode-only)
  (advice-add 'org-src-font-lock-fontify-block :around #'wgn/fence--inert)
  (advice-add 'org-src-get-lang-mode :filter-return #'wgn/fence--mode-only)
  (advice-add 'markdown-overlays--fontify-source-block :around
              #'wgn/fence--markdown-overlays))

(provide 'wgn-markdown)
;;; wgn-markdown.el ends here
