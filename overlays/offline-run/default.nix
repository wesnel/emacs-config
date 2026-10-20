# One `offline-run` for every host: a Gondolin micro-VM on Darwin, where there
# are no namespaces, and bubblewrap on Linux, which needs no hardware
# virtualization. Both take the same options.
{
  callPackage,
  stdenvNoCC,
}:
if stdenvNoCC.hostPlatform.isDarwin
then callPackage ./gondolin.nix {}
else callPackage ./bwrap.nix {}
