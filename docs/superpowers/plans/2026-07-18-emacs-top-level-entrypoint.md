# Emacs Top Level Entrypoint Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `emacs/` a self-contained `.emacs.d` directory by adding a top-level `init.el` entrypoint that loads the generated main configuration.

**Architecture:** Keep `site-lisp/config/init.el` as the framework's real main initializer. Add a tiny root-level `init.el` that only finds the distribution root and loads `site-lisp/config/init.el`, then update build and docs so symlink installation and manual loading use the same entrypoint.

**Tech Stack:** GNU Emacs Lisp, Org Babel tangling, Makefile, ERT.

---

### Task 1: Characterize The Missing Top-Level Entrypoint

**Files:**
- Modify: `emacs/test/run-tests.el`

- [x] **Step 1: Write failing tests**

Add tests that assert:
- `emacs/init.el` exists.
- `emacs/init.el` contains `lexical-binding: t`.
- `emacs/init.el` loads `site-lisp/config/init.el`.
- README manual load examples point to `emacs/init.el`.

- [x] **Step 2: Run targeted tests**

Run: `emacs -Q --batch -L site-lisp/config -L site-lisp/config/core -l test/run-tests.el --eval '(ert-run-tests-batch-and-exit "top-level-entrypoint")'`

Expected: FAIL because `emacs/init.el` does not exist and README still points at `site-lisp/config/init.el`.

### Task 2: Add The Entrypoint And Build Rule

**Files:**
- Create: `emacs/config-org/root-init.org`
- Modify: `emacs/Makefile`
- Generated: `emacs/init.el`

- [x] **Step 1: Implement root init source**

Create `config-org/root-init.org` that tangles to `init.el` and loads `site-lisp/config/init.el` relative to its own file.

- [x] **Step 2: Update Makefile**

Add `emacs/init.el` as a build target from `config-org/root-init.org`, include it in `make all`, and teach `make tangle-file FILE=config-org/root-init.org` to write to the repository root instead of `site-lisp/config/root-init.el`.

- [x] **Step 3: Generate root init**

Run: `make tangle-file FILE=config-org/root-init.org`

Expected: creates or updates `emacs/init.el`.

- [x] **Step 4: Run targeted tests**

Run the same targeted ERT selector. Expected: root entrypoint tests pass except README tests if docs are not updated yet.

### Task 3: Align Documentation And Verify

**Files:**
- Modify: `emacs/README.org`
- Modify: `emacs/README_CN.org`
- Modify: `emacs/test/run-tests.el`

- [x] **Step 1: Update README manual load examples**

Change manual load snippets from `site-lisp/config/init.el` to the root `init.el`.

- [x] **Step 2: Run targeted tests**

Run: `emacs -Q --batch -L site-lisp/config -L site-lisp/config/core -l test/run-tests.el --eval '(ert-run-tests-batch-and-exit "top-level-entrypoint")'`

Expected: all top-level entrypoint tests pass.

- [x] **Step 3: Run full verification**

Run:
- `make test`
- `emacs -Q --batch -l init.el`
- `git diff --check`

Expected: all commands exit 0. Third-party extension warnings may still appear during batch load and are out of scope.
