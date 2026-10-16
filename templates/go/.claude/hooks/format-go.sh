#!/bin/sh
# PostToolUse (Edit|Write|MultiEdit): gofmt the edited .go file inside the project.
# Reads the hook JSON on stdin. Never fails the hook.
set -u

command -v gofmt >/dev/null 2>&1 || exit 0
command -v jq >/dev/null 2>&1 || exit 0

file=$(jq -r '.tool_input.file_path // empty' 2>/dev/null) || exit 0
case $file in
*.go) ;;
*) exit 0 ;;
esac
[ -f "$file" ] || exit 0

root=$(cd "${CLAUDE_PROJECT_DIR:-.}" 2>/dev/null && pwd -P) || exit 0
dir=$(cd "$(dirname "$file")" 2>/dev/null && pwd -P) || exit 0
case $dir/ in
"$root"/*) ;;
*) exit 0 ;;
esac

before=$(cksum <"$file")
gofmt -w "$file" >/dev/null 2>&1 || exit 0
after=$(cksum <"$file")
[ "$before" = "$after" ] && exit 0

jq -n --arg m "gofmt reformatted $file; re-read it before editing it again." \
	'{hookSpecificOutput: {hookEventName: "PostToolUse", additionalContext: $m}}'
exit 0
