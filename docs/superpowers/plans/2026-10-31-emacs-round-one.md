# Emacs Round One Implementation Plan

**Goal:** Make private options and development tool selection independent of module load timing, and report initialization failures accurately.

**Architecture:** Keep config-org as the source of truth. Load the existing private/config.el once before feature modules; use mcg-after-init-hook for post-module work and with-eval-after-load for idle/deferred modules. Extract tool selection into core-dev-tools without activating language modes. Required dependencies and setup failures propagate to the module loader; optional dependencies continue to allow fallback.

**Validation:** Add ERT behavior tests before implementation, tangle only changed sources into repository outputs, run focused tests and the full suite, and inspect git diff. Baseline: 186/187 passing; mcg-lsp-bridge-ty-completions-insert-function-parentheses fails against the pre-existing third-party ty.json.

- [x] Add regression tests for private option ordering, post-init hooks, required dependency failures, retry, and tool selection before language modules load.
- [x] Move private loading before feature initialization and document the two phases in the example and both READMEs.
- [x] Propagate required dependency/setup errors to core-modules; preserve explicitly optional fallbacks. Keep loaded/error registries consistent on hook failure and deferred retry.
- [x] Extract Python/Web/Zig tool detection and command construction into core-dev-tools; reuse it from language, compile, format, and doctor modules while preserving existing command names and user options.
- [x] Tangle changed Org sources without installing into the user's Emacs directory; run regression and full ERT suites and inspect the final diff.

Validation results: the first eight regressions failed before implementation and passed afterward; two additional tests cover LSP package failure and optional package fallback. The full suite now has 197 tests, with 196 passing and only the same baseline ty.json failure. Batch startup profiling completes with no module load errors; GUI first-frame behavior has not been manually verified. Existing tests that asserted the old source locations now include core-dev-tools or test command behavior instead.

Implementation proceeds in the current clean checkout, as authorized by the request to begin the first round. No commits or third-party package updates are part of this round.
