# Emacs Round Six Implementation Plan

Goal: keep module declaration state consistent and make deferred/idle setup follow the declared strategy without accumulating timers.

Design: retain the mcg! syntax and command entrypoints. Within one declaration, repeated category/module keys keep their first position and use the last entry's enabled state and metadata. The shared deferred setup processes only enabled deferred declarations, respects category filters and reports missing triggers. The init wrapper delegates to it. Idle setup replaces its previous timer, clears its handle when drained, and schedules one next callback; failed modules remain explicitly retryable without an automatic retry loop.

Alternatives considered: reject repeated declarations (stricter but less useful for overrides), or keep duplicate rows and filter at every caller (leaves contradictory registry state). Normalize once while parsing instead. Keep loading and declarations in core-modules for this round; extracting another file would add indirection without removing state coupling.

Scope: config-org/core/core-modules.org, config-org/init.org, new test/round-six-tests.el and both READMEs. Preserve staged changes. No commits, installations, dependency updates or agents.

- [x] Add behavior regressions for repeated declaration states/order/metadata, strategy/category-aware deferred setup, missing triggers, timer replacement/draining, and default declaration consistency. Observe failures against the existing loader.
- [x] Normalize duplicate declarations; route init deferred setup through the shared strategy-aware implementation.
- [x] Make idle scheduling own a single timer and clear drained state; retain failure retry behavior.
- [x] Generate repository-local configuration, run full ERT and real startup smoke, update docs and inspect final diffs.

Validation: `make test`, `emacs -Q --batch -l test/round-two-startup-smoke.el`, repository-root ERT invocation, `git diff HEAD --check`. Real timers in tests must be canceled in unwind-protect; no sleeps or user configuration changes.

Results (2026-10-31): seven behavior regressions failed on the previous implementation; the default declaration integrity check already passed. After implementation, both make test and the direct repository-root invocation passed 253/253, including isolated Org initialization. Real startup smoke passed, preserving lazy command entries and first-use compilation. Repository-local generation and diff checks passed. English/Chinese loading documentation was updated. No GUI interaction was verified; the index was preserved and no commit or installation was performed.
