# Emacs Round Two Implementation Plan

Goal: share command bindings between idle entry points and loaded modules, and share extension discovery between bootstrap and runtime lookup.

Design: config-org remains authoritative. core-bindings owns the shared binding declarations; both startup and feature modules consume them. Prefix setup reuses existing maps. core-extensions owns discovery, path resolution and cache validation; bootstrap adds paths from that index. Existing public extension lookup names remain compatible. Keep the key registry redesign for a later round rather than mixing a storage migration into these two seams.

- [x] Add behavioral regressions and confirm failures before implementation.
- [x] Centralize the nine existing idle command groups, preserve configurable Majutsu keys and shared prefixes, and handle commands already autoloaded before module setup.
- [x] Extract extension indexing, route bootstrap and direct lookup through it, and validate nested changes and empty results.
- [x] Update documentation and source-location tests, tangle repository outputs, run focused/full ERT and startup profiling, inspect diffs.

Baseline from round one: 196/197 tests pass; the existing third-party ty.json parentheses setting test fails. Preserve staged round-one changes; do not commit or modify extension submodules.

Validation: the initial eight regressions failed before implementation. All 16 round-two regressions now pass, including command retry, Lisp argument forwarding, binding replay/reload, disabled/immediate module filtering, duplicate resolution, nested additions/removals, empty indices and valid-cache scan avoidance. Full ERT: 212/213 pass; only the same third-party ty.json test fails. Four tests tied to old source locations now check shared binding behavior or the prefix helper.

The directory signature watches paths from the last discovery and only checks timestamps while valid. A changed directory triggers a new walk and refreshes watched paths. The nested-addition fixture uses an existing intermediate directory, keeping top-level timestamps unchanged while the deeper parent changes, as on an ordinary filesystem.

Commands run from emacs/:

```text
make build-core build-modules-completion build-modules-tools build-modules-editor build-modules-org site-lisp/config/init.el
emacs -Q --batch -L site-lisp/config/core -l test/run-tests.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -l test/round-two-startup-smoke.el
```

The independent smoke script loads the actual configuration, checks that Consult preserved the early search keys, then invokes the lazy compile error command with navigation replaced and verifies module registration. It passes with no module load errors; generated config load measured 915.1ms, 25 immediate entries and 14 idle entries. This is a batch measurement, not a GUI first-frame benchmark. No manual GUI validation or commits were performed.
