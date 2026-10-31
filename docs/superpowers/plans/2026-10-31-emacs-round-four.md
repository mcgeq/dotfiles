# Emacs Round Four Implementation Plan

Goal: keep Org storage paths consistent, preserve private capture templates, limit presentation settings to Org buffers, and make repeated initialization predictable.

Architecture: core-org owns Org storage options and helpers without requiring Org; core-lib re-exports it. Org, agenda and capture consume those options. Explicit mcg-org-set-storage-root resets derived paths and synchronizes Org's runtime variables; set per-path overrides afterward. Template registration fills missing keys and keeps user entries, including Hugo keys. Org mode installs its own display locals. Babel uses a named hook and caches successful language setup by effective options. Export backends initialize after ox loads. Agenda and capture remain available through the existing idle Org module and existing command names.

Baseline: 232/232 ERT pass. Keep current staged changes; no commits, installation, or third-party changes. The user has confirmed that they may stage work during execution.

- [x] Add behavioral regressions for template preservation/reload, local display settings, storage synchronization, Babel setup/retry and export loading; confirm failures.
- [x] Extract core-org options and helpers, consolidate their consumers and update source-location tests.
- [x] Preserve user capture/Hugo templates; replace anonymous capture and Babel hooks; avoid repeated successful Babel language loading.
- [x] Keep display settings local and defer export backend setup; remove obsolete private-config initialization callback.
- [x] Tangle core/Org outputs, run focused/full ERT and isolated Org/startup smoke tests, update docs and inspect diffs.

Verification (2026-10-31):

- Generated core, Org modules and private example using repository-local Make targets.
- Full ERT: 240/240 passed, 0 unexpected (including isolated Org initialization, private capture preservation and deferred export smoke).
- Real startup smoke: passed; startup keys preserved and first-use compile available.
- `git diff HEAD --check`: passed.
- Updated English/Chinese Org documentation and private configuration examples.
- GUI interaction was not exercised. Existing staged changes were preserved; no commit was created.
