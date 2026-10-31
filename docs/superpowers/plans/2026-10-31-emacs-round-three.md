# Emacs Round Three Implementation Plan

**Goal:** Make key registration reflect the target keymap and actual successful mutations, and isolate this responsibility from core-lib.

**Architecture:** Extract key registration and reports to config-org/core/core-keybindings.org, requiring only core-bootstrap and cl-lib. core-lib re-exports it. Use an outer hash with eq keymap identity and an inner equal hash for canonical key descriptions. Keep mcg-bind-key/mcg-unbind-key arguments and global defaults. Extend mcg-lookup-keybinding and mcg-list-keybindings with an optional keymap; list rows remain (key command source). Conflict records keep their first three fields and add the target map. Reports group bindings by map and identify the map in conflicts. Normalize strings through kbd and accept vectors directly. Update records only after define-key succeeds, and discard descendants removed by prefix replacement/unbinding.

Flat legacy registries are imported into global-map during initialization. This cannot recover map identity that the old format never recorded; existing sessions should restart after rebuilding. Map identity must not depend on mutable map contents. Existing policy defaults to reject; overwrite retains error propagation when Emacs cannot install a key beneath a non-prefix command.

Scope adjustment after full-suite verification: the previous round's new-directory test failed intermittently (1/10 isolated repetitions). Holding the parent mtime constant reproduces missed container children and a new preferred lisp/ directory deterministically. Extend directory state with container entry names and candidate lisp/ state, while preserving watched-path validation and avoiding a full package rediscovery for valid caches. Bump the persistent format version. This is a correction of an observed earlier-round regression, not a registry dependency.

**Validation baseline:** Round two has 212/213 passing tests, with only the existing third-party ty.json completeFunctionParentheses test failing. Preserve all staged/unstaged earlier work and extension submodules. No commits or installs.

- [x] Add behavior regressions for map identity, prefix conflict scoping, normalization, failed mutations, prefix cleanup, query defaults and report context; observe failures.
- [x] Extract the existing registry section into core-keybindings, retain core-lib's require entry, implement map-scoped storage and atomic registration.
- [x] Preserve global query/list shapes, update two local-map tests to pass their map explicitly, and cover conflict policies and prefix preservation.
- [x] Document the interface and migration; tangle repository outputs; run focused/full ERT, the real startup smoke, and git diff checks.

Validation: the initial 13 registry regressions failed before implementation and passed after it. The two deterministic directory-state regressions also failed before their fix. All 18 round-three regressions pass. Full ERT now passes 232/232, including the corrected ty test and an additional check that the LSP module selects the repository user directory. The actual startup smoke passes with no module load errors, preserving search entries and first-use compile loading. Generated files were rebuilt with `make build-core`; no installation or commits were performed.

The previous baseline ty failure was a false positive: the assertion read the extension submodule's bundled JSON, while init-lsp-bridge selects the existing tracked `emacs/langservers/ty.json`, which already enables `settings.ty.completions.completeFunctionParentheses`. The test now parses that JSON and checks its command and boolean setting. Executing the actual Python backend's `get_lang_server_path` function in isolation confirmed that it selects this override. No third-party configuration change was needed.

The user confirmed that index updates during this round were intentional. Preserve their staging; subsequent test and plan edits remain unstaged for review.
