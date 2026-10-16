#!/bin/sh
# Stop: lint the packages with uncommitted .go changes; block the stop once on failure.
# Reads the hook JSON on stdin. Skips quietly when there is nothing to lint.
set -u

command -v jq >/dev/null 2>&1 || exit 0
command -v git >/dev/null 2>&1 || exit 0

input=$(cat)
active=$(printf '%s' "$input" | jq -r '.stop_hook_active // false' 2>/dev/null) || exit 0
[ "$active" = true ] && exit 0

cd "${CLAUDE_PROJECT_DIR:-.}" 2>/dev/null || exit 0
[ -f go.mod ] || exit 0
top=$(git rev-parse --show-toplevel 2>/dev/null) || exit 0
[ "$(cd "$top" && pwd -P)" = "$(pwd -P)" ] || exit 0

if command -v golangci-lint >/dev/null 2>&1; then
	lint='golangci-lint run'
elif command -v go >/dev/null 2>&1; then
	lint='go vet'
else
	exit 0
fi

files=$(
	{
		git diff --name-only HEAD -- '*.go' 2>/dev/null
		git ls-files --others --exclude-standard -- '*.go' 2>/dev/null
	} | sort -u
)
[ -n "$files" ] || exit 0

dirs=$(
	printf '%s\n' "$files" | while IFS= read -r f; do
		[ -f "$f" ] || continue
		d=$(dirname "$f")
		if [ "$d" = . ]; then echo .; else echo "./$d"; fi
	done | sort -u
)
[ -n "$dirs" ] || exit 0

set --
while IFS= read -r d; do
	set -- "$@" "$d"
done <<EOF
$dirs
EOF

timeout=
command -v timeout >/dev/null 2>&1 && timeout='timeout 120'
# shellcheck disable=SC2086
out=$($timeout $lint "$@" 2>&1)
status=$?
[ "$status" -eq 0 ] && exit 0
[ "$status" -eq 124 ] && exit 0

reason=$(printf 'Fix the lint failures before stopping:\n%s' "$(printf '%s\n' "$out" | head -n 30)")
jq -n --arg r "$reason" '{decision: "block", reason: $r}'
exit 0
