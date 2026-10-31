# Emacs Round Five Implementation Plan

Goal: make tests independent of the calling directory and ensure Make test commands use generated configuration matching config-org sources.

Architecture: keep test/run-tests.el as a small bootstrap and suite discovery entrypoint. Move its existing assertions to config-tests.el and shared setup to test-helper.el; make path expectations and the startup subprocess independent of the caller's directory. Discover sorted *-tests.el files, excluding run-tests.el. Preserve existing test names and round files. Add a repository-local generate target and make test/test-verbose depend on it. Split early-init generation from installation: generate includes both repository copies, while all retains installation via install-early-init (now performed on each all invocation). Use generated directory targets for tangle-dir instead of a shell loop.

The user authorized continuing the proposed test/build refactoring. Preserve their staged changes and work in the current checkout; no commits or installation actions.

- [x] Add regressions for suite discovery, invocation from another directory, repository-local generation and single-directory generation; confirm failures.
- [x] Extract test-helper.el and config-tests.el; replace the test entrypoint with sorted suite discovery.
- [x] Add generate and test dependencies; remove duplicate rule expansion and make tangle-dir use existing directory targets.
- [x] Run full ERT through make test, direct entrypoint from the repository root, and startup/Org smoke checks. Verify Make dry runs and diffs; update both READMEs.

Validation commands (from emacs/ unless noted):

- `make test`
- `make -n generate` (must not install early-init or create private config.el)
- `make -n tangle-dir DIR=modules/org` (must use existing build rules)
- `emacs -Q --batch -l emacs/test/run-tests.el -f ert-run-tests-batch-and-exit` from repository root
- `emacs -Q --batch -l test/round-two-startup-smoke.el`
- `git diff HEAD --check`

Results (2026-10-31): five new tests failed against the old entrypoints while all 240 existing tests passed. After refactoring, make test and the direct repository-root invocation each passed 245/245, including the isolated Org smoke. Real startup smoke passed. Dry-run assertions confirmed repository-local generation, generation before ERT and shared directory rules without duplicate-recipe warnings. Final diff checks passed. No GUI verification, commits, index changes or installation commands were performed.
