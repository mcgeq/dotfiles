# Emacs Six-Round Cumulative Review

Scope: working-tree changes relative to HEAD, including the untracked round-six plan/tests. Preserve the user's staging. Review both documented standards and the six implementation plans. Two read-only reviewers inspected standards/correctness and spec/correctness independently; findings were reproduced before fixes.

Standards: no hard documented-standard violations found. One P2 correctness finding: the selected lisp/ directory's file membership was absent from extension cache signatures. Removing its last Elisp file while retaining directory mtime kept an invalid load path. Record selected-directory Elisp membership and increment cache format to 6.

Spec: two P2 findings. Partial module initialization could overwrite lazy stubs, letting background failures or sibling commands bypass setup retry. Track module command ownership and restore all associated entries on failure, preserving underlying definitions and avoiding nested wrappers. Babel cached Jupyter option updates without applying the defaults helper. Apply that helper before recording successful options, including when the executable becomes available later.

- [x] Independently reproduce all three findings with five failing regression tests.
- [x] Fix cache signatures, failed-module lazy entry restoration and Jupyter default application in config-org.
- [x] Verify all new regressions, full ERT, real startup and isolated command workflows; inspect diffs and document limits.

GUI verification limitation: both available desktop JavaScript runtimes failed to initialize because of the Windows sandbox setup-refresh error. Do not claim GUI visual validation. Use isolated command/keyboard workflows to exercise Capture, Agenda and file triggers, with temporary Org files and cleanup.

Verification results (2026-10-31):

- Five new behavioral regressions failed before the fixes and passed afterward.
- Final `make test`: 259/259 passed, zero unexpected results. This includes a sixth new test that runs the real command workflow in a fresh process.
- `test/cumulative-workflow-smoke.el`: Capture via `C-c n c z` saves a temporary private template; `C-c n a t` displays both temporary tasks; visiting Markdown/Python files loads and records the proper modules. Sticky Agenda buffers are checked through the displayed window rather than an assumed fixed buffer name. All temporary files are cleaned up.
- `test/round-two-startup-smoke.el`: startup key preservation and first-use compile passed.
- Both reviewers rechecked the fixes without new findings. The spec reviewer additionally verified three successive load-hook failures alternating sibling commands, followed by successful initialization; no nested/stale wrappers remained and errors cleared.
- Final diff formatting checks passed. Canonical changes are in config-org; generated files stay repository-local. The user's index was preserved; no commits, installs or third-party changes were made.
