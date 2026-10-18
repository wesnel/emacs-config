# Appended to the guest's init, which runs as root under `set -eu' and defines
# `log', before it starts sandboxd.
#
# `runner' is for code that must not reach what a root process in the guest
# holds: a root shell can run it with
#
#   setpriv --reuid=runner --regid=runner --init-groups --no-new-privs -- CMD
#
# and CMD can then neither read that process's memory, environment or open
# files under /proc nor signal or trace it. What CMD may write has to be made
# its own first.
#
# Name the user, never its ID: `runner' exists only once the steps below
# have succeeded, so setpriv refuses to run anything where they did not,
# where uid 1000 might own the binaries root runs.
#
# Gondolin's mounts are FUSE mounts without allow_other, which only root can
# enter. Root can give `runner' a read-only view of one by mounting an
# overlay over it, which reaches the mount with root's credentials:
#
#   mkdir -p /run/empty
#   mount -t overlay overlay -o ro,lowerdir=DIR:/run/empty DIR

# The image builder leaves the root file system owned by the user and group
# that built it, which a user in the guest could share; owned by root, none
# of it is writable to `runner' whatever its IDs. chown also clears setuid
# and setgid bits, which were set for the builder rather than for root.
#
# It runs on a bind mount of / alone, without what is mounted below it: find
# -xdev still visits a mount point, whose owner is the mounted directory's,
# and the workspace is a host directory.
#
# What counts is the result, not chown's exit status: the check runs again
# and finds nothing but `runner''s own home.
runner_safe=0
mkdir -p /run/rootfs
if mount --bind / /run/rootfs; then
  find /run/rootfs -xdev \( ! -user 0 -o ! -group 0 \) -exec chown -h 0:0 {} + 2>/dev/null || true
  if [ -z "$(find /run/rootfs -xdev -path /run/rootfs/home/runner -prune -o \( ! -user 0 -o ! -group 0 \) -print | head -n 1)" ]; then
    runner_safe=1
  fi
  umount /run/rootfs || runner_safe=0
fi

# Without user namespaces, `runner' cannot become root in one of its own and
# reach kernel interfaces that need capabilities there. Nothing the guest
# runs as root creates one.
echo 0 >/proc/sys/user/max_user_namespaces 2>/dev/null || runner_safe=0
[ "$(cat /proc/sys/user/max_user_namespaces 2>/dev/null)" = 0 ] || runner_safe=0

if [ "$runner_safe" = 1 ]; then
  if ! id runner >/dev/null 2>&1; then
    adduser -D -u 1000 runner || log "[init] adding the runner user failed"
  fi
else
  log "[init] the root file system is not root's alone or user namespaces are on; no runner user"
  if id runner >/dev/null 2>&1; then
    deluser runner || log "[init] removing the runner user failed"
  fi
fi
