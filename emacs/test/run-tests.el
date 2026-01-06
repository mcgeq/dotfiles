;;; run-tests.el --- Test runner for Emacs config -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple test runner to verify configuration loads correctly

;;; Code:

(require 'ert)

;; Set up test environment
(defvar mcgemacs-root-dir (expand-file-name ".." (file-name-directory load-file-name)))

;; Add load paths
(add-to-list 'load-path (expand-file-name "site-lisp/config/core" mcgemacs-root-dir))

;; Load dependencies
(require 'init-const)
(require 'init-loadpath)

;;; Basic Tests

(ert-deftest test-extension-categories-defined ()
  "Test that extension categories are defined."
  (should (boundp 'mcg-extension-categories))
  (should (listp mcg-extension-categories))
  (should (> (length mcg-extension-categories) 0)))

(ert-deftest test-extension-registry-v2-defined ()
  "Test that extension registry V2 is defined."
  (should (boundp 'mcg-extension-registry-v2))
  (should (listp mcg-extension-registry-v2))
  (should (> (length mcg-extension-registry-v2) 0)))

(ert-deftest test-category-extraction-basic ()
  "Test basic category extraction from paths."
  (should (eq (mcg-get-category-from-path "core/dash") 'core))
  (should (eq (mcg-get-category-from-path "completion/vertico") 'completion))
  (should (eq (mcg-get-category-from-path "editor/beacon") 'editor))
  (should (eq (mcg-get-category-from-path "lsp/clojure/cider") 'lsp)))

(ert-deftest test-category-extraction-invalid ()
  "Test category extraction with invalid inputs."
  (should (null (mcg-get-category-from-path nil)))
  (should (null (mcg-get-category-from-path "")))
  (should (null (mcg-get-category-from-path "invalid")))
  (should (null (mcg-get-category-from-path "unknown/package"))))

(ert-deftest test-extension-info-returns-plist ()
  "Test that extension info returns a proper plist."
  (let ((info (mcg-extension-info "core/dash")))
    (should info)
    (should (plist-get info :name))
    (should (plist-get info :category))
    (should (plist-get info :description))))

(ert-deftest test-extension-dependencies-no-deps ()
  "Test dependency resolution for extension with no dependencies."
  (let ((deps (mcg-extension-dependencies "core/dash")))
    (should (null deps))))

(ert-deftest test-extension-dependencies-with-deps ()
  "Test dependency resolution for extension with dependencies."
  (let ((deps (mcg-extension-dependencies "core/f.el")))
    (should deps)
    (should (member "core/dash" deps))
    (should (member "core/s.el" deps))))

(ert-deftest test-extension-direct-depends ()
  "Test direct dependency retrieval."
  (let ((deps (mcg-extension-direct-depends "core/f.el")))
    (should deps)
    (should (member "core/dash" deps))
    (should (member "core/s.el" deps))))

(ert-deftest test-list-all-extensions ()
  "Test listing all extensions."
  (let ((extensions (mcg-list-all-extensions)))
    (should extensions)
    (should (member "core/dash" extensions))
    (should (member "completion/vertico" extensions))))

;;; Phase Loading Tests

(ert-deftest test-load-phases-defined ()
  "Test that load phases are defined."
  (should (boundp 'mcg-load-phases))
  (should (listp mcg-load-phases))
  (should (> (length mcg-load-phases) 0)))

(ert-deftest test-deferred-categories-defined ()
  "Test that deferred categories are defined."
  (should (boundp 'mcg-deferred-categories))
  (should (listp mcg-deferred-categories))
  (should (memq 'lsp mcg-deferred-categories))
  (should (memq 'org mcg-deferred-categories)))

(ert-deftest test-phase-categories ()
  "Test phase category retrieval."
  (let ((phase-1-cats (mcg-phase-categories 'phase-1)))
    (should phase-1-cats)
    (should (memq 'core phase-1-cats))))

(ert-deftest test-phase-deferred-p ()
  "Test deferred phase detection."
  ;; phase-1 (core) should not be deferred
  (should-not (mcg-phase-deferred-p 'phase-1))
  ;; phase-5 (lsp, git) should be deferred
  (should (mcg-phase-deferred-p 'phase-5))
  ;; phase-6 (org, docs) should be deferred
  (should (mcg-phase-deferred-p 'phase-6)))

(ert-deftest test-is-deferred-extension-p ()
  "Test deferred extension detection."
  ;; core extensions should not be deferred
  (should-not (mcg-is-deferred-extension-p "core/dash"))
  ;; lsp extensions should be deferred
  (should (mcg-is-deferred-extension-p "lsp/lsp-bridge"))
  ;; org extensions should be deferred
  (should (mcg-is-deferred-extension-p "org/org-modern")))

(ert-deftest test-load-status-returns-plist ()
  "Test that load status returns a proper plist."
  (let ((status (mcg-load-status)))
    (should status)
    (should (plist-member status :phases-loaded))
    (should (plist-member status :current-phase))
    (should (plist-member status :phase-timing))
    (should (plist-member status :deferred-complete))
    (should (plist-member status :total-time))))

;;; Deferred Loading Tests

(ert-deftest test-defer-time-defined ()
  "Test that defer time is defined and reasonable."
  (should (boundp 'mcg-defer-time))
  (should (numberp mcg-defer-time))
  (should (>= mcg-defer-time 0)))

(ert-deftest test-schedule-deferred-load-creates-timer ()
  "Test that scheduling deferred load creates a timer."
  ;; Cancel any existing timer first
  (mcg-cancel-deferred-load)
  (should (null mcg-deferred-load-timer))
  
  ;; Schedule deferred load
  (mcg-schedule-deferred-load)
  (should mcg-deferred-load-timer)
  
  ;; Clean up
  (mcg-cancel-deferred-load)
  (should (null mcg-deferred-load-timer)))

(ert-deftest test-cancel-deferred-load ()
  "Test that canceling deferred load works."
  ;; Schedule first
  (mcg-schedule-deferred-load)
  (should mcg-deferred-load-timer)
  
  ;; Cancel
  (mcg-cancel-deferred-load)
  (should (null mcg-deferred-load-timer)))

;;; Minimal Dependency Loading Tests

(ert-deftest test-load-extension-minimal-no-deps ()
  "Test minimal loading for extension with no dependencies."
  ;; This test verifies the function exists and returns properly
  ;; Actual loading depends on file system state
  (should (fboundp 'mcg-load-extension-minimal)))

;;; Version Manager Tests

(ert-deftest test-lock-file-path-defined ()
  "Test that lock file path is defined."
  (should (boundp 'mcg-lock-file-path))
  (should (stringp mcg-lock-file-path))
  (should (string-match-p "packages-lock\\.el$" mcg-lock-file-path)))

(ert-deftest test-lock-packages-function-exists ()
  "Test that lock packages function exists."
  (should (fboundp 'mcg-lock-packages)))

(ert-deftest test-restore-packages-function-exists ()
  "Test that restore packages function exists."
  (should (fboundp 'mcg-restore-packages)))

(ert-deftest test-rollback-package-function-exists ()
  "Test that rollback package function exists."
  (should (fboundp 'mcg-rollback-package)))

(ert-deftest test-compare-versions-function-exists ()
  "Test that compare versions function exists."
  (should (fboundp 'mcg-compare-versions)))

(ert-deftest test-collect-all-versions-returns-list ()
  "Test that collect all versions returns a list."
  (let ((versions (mcg-collect-all-versions)))
    (should (listp versions))))

(ert-deftest test-generate-lock-file-content-format ()
  "Test that generated lock file content has correct format."
  (let* ((test-versions '(("core/dash" :commit "abc123def456" :branch "master")))
         (content (mcg-generate-lock-file-content test-versions)))
    (should (stringp content))
    (should (string-match-p "packages-lock\\.el" content))
    (should (string-match-p "mcg-packages-lock" content))
    (should (string-match-p "mcg-lock-metadata" content))
    (should (string-match-p "core/dash" content))
    (should (string-match-p "abc123def456" content))))

(ert-deftest test-get-locked-version-returns-nil-when-no-lock ()
  "Test that get locked version returns nil when no lock data."
  (let ((mcg-packages-lock nil))
    (should (null (mcg-get-locked-version "nonexistent/package")))))

;;; Fuzzy Search Tests

(ert-deftest test-fuzzy-match-substring ()
  "Test fuzzy match with substring."
  (should (mcg-fuzzy-match-p "dash" "core/dash"))
  (should (mcg-fuzzy-match-p "vert" "completion/vertico"))
  (should (mcg-fuzzy-match-p "DASH" "core/dash")))  ; case insensitive

(ert-deftest test-fuzzy-match-character-sequence ()
  "Test fuzzy match with character sequence."
  (should (mcg-fuzzy-match-p "cds" "core/dash"))  ; c-o-r-e-/-d-a-s-h
  (should (mcg-fuzzy-match-p "vrt" "completion/vertico")))

(ert-deftest test-fuzzy-match-invalid-inputs ()
  "Test fuzzy match with invalid inputs."
  (should-not (mcg-fuzzy-match-p nil "test"))
  (should-not (mcg-fuzzy-match-p "test" nil))
  (should-not (mcg-fuzzy-match-p "" "test"))
  (should-not (mcg-fuzzy-match-p "test" "")))

(ert-deftest test-fuzzy-match-no-match ()
  "Test fuzzy match when no match."
  (should-not (mcg-fuzzy-match-p "xyz" "core/dash"))
  (should-not (mcg-fuzzy-match-p "zzz" "completion/vertico")))

(ert-deftest test-search-extension-by-name ()
  "Test search extension by name."
  (let ((results (mcg-search-extension "dash")))
    (should results)
    (should (member "core/dash" results))))

(ert-deftest test-search-extension-by-category ()
  "Test search extension by category."
  (let ((results (mcg-search-extension "core")))
    (should results)
    (should (seq-some (lambda (r) (string-prefix-p "core/" r)) results))))

(ert-deftest test-search-extension-empty-query ()
  "Test search extension with empty query."
  (should (null (mcg-search-extension "")))
  (should (null (mcg-search-extension nil))))

(ert-deftest test-search-extension-no-results ()
  "Test search extension with no results."
  (let ((results (mcg-search-extension "xyznonexistent123")))
    (should (null results))))

(ert-deftest test-search-extension-function-exists ()
  "Test that search extension function exists."
  (should (fboundp 'mcg-search-extension))
  (should (fboundp 'mcg-fuzzy-match-p)))

;;; Category Statistics Tests

(ert-deftest test-category-statistics-returns-plist ()
  "Test that category statistics returns a proper plist."
  (let ((stats (mcg-category-statistics 'core)))
    (should stats)
    (should (plist-member stats :total))
    (should (plist-member stats :loaded))
    (should (plist-member stats :required))
    (should (plist-member stats :with-deps))
    (should (plist-member stats :deferred))))

(ert-deftest test-category-statistics-core-has-required ()
  "Test that core category has required extensions."
  (let ((stats (mcg-category-statistics 'core)))
    (should (> (plist-get stats :required) 0))))

(ert-deftest test-category-statistics-lsp-is-deferred ()
  "Test that lsp category is marked as deferred."
  (let ((stats (mcg-category-statistics 'lsp)))
    (should (plist-get stats :deferred))))

(ert-deftest test-category-statistics-core-not-deferred ()
  "Test that core category is not deferred."
  (let ((stats (mcg-category-statistics 'core)))
    (should-not (plist-get stats :deferred))))

(ert-deftest test-display-categories-status-function-exists ()
  "Test that display categories status function exists."
  (should (fboundp 'mcg-display-categories-status))
  (should (fboundp 'mcg-category-statistics))
  (should (fboundp 'mcg-display-category-extensions)))

;;; Platform Detection Tests

(ert-deftest test-detect-platform-returns-symbol ()
  "Test that platform detection returns a valid symbol."
  (let ((platform (mcg-detect-platform)))
    (should platform)
    (should (symbolp platform))
    (should (memq platform '(windows macos linux bsd unknown)))))

(ert-deftest test-platform-info-is-plist ()
  "Test that platform info is a proper plist."
  (mcg-detect-platform)
  (should mcg-platform-info)
  (should (plist-member mcg-platform-info :platform))
  (should (plist-member mcg-platform-info :system-type))
  (should (plist-member mcg-platform-info :path-sep))
  (should (plist-member mcg-platform-info :line-ending))
  (should (plist-member mcg-platform-info :shell))
  (should (plist-member mcg-platform-info :home)))

(ert-deftest test-platform-predicates-exist ()
  "Test that platform predicate functions exist."
  (should (fboundp 'mcg-platform-windows-p))
  (should (fboundp 'mcg-platform-macos-p))
  (should (fboundp 'mcg-platform-linux-p))
  (should (fboundp 'mcg-platform-unix-like-p)))

(ert-deftest test-get-path-separator-returns-string ()
  "Test that path separator is a string."
  (let ((sep (mcg-get-path-separator)))
    (should sep)
    (should (stringp sep))
    (should (member sep '("/" "\\")))))

(ert-deftest test-normalize-path-handles-nil ()
  "Test that normalize path handles nil input."
  (should (null (mcg-normalize-path nil))))

(ert-deftest test-normalize-path-returns-string ()
  "Test that normalize path returns a string for valid input."
  (let ((result (mcg-normalize-path "~")))
    (should result)
    (should (stringp result))))

;;; External Tools Detection Tests

(ert-deftest test-detect-external-tools-returns-list ()
  "Test that external tools detection returns a list."
  (let ((tools (mcg-detect-external-tools t)))
    (should tools)
    (should (listp tools))))

(ert-deftest test-find-executable-function-exists ()
  "Test that find executable function exists."
  (should (fboundp 'mcg-find-executable)))

(ert-deftest test-tool-available-p-returns-boolean ()
  "Test that tool availability check returns boolean."
  (let ((result (mcg-tool-available-p 'git)))
    (should (or (eq result t) (eq result nil)))))

(ert-deftest test-get-tool-path-for-nonexistent ()
  "Test that get tool path returns nil for nonexistent tool."
  (should (null (mcg-get-tool-path 'nonexistent-tool-xyz))))

;;; Graceful Degradation Tests

(ert-deftest test-warn-degradation-function-exists ()
  "Test that degradation warning function exists."
  (should (fboundp 'mcg-warn-degradation))
  (should (fboundp 'mcg-check-feature-availability))
  (should (fboundp 'mcg-display-degradation-warnings))
  (should (fboundp 'mcg-clear-degradation-warnings)))

(ert-deftest test-clear-degradation-warnings ()
  "Test that clearing degradation warnings works."
  (mcg-clear-degradation-warnings)
  (should (null mcg-degradation-warnings)))

(ert-deftest test-initialize-platform-function-exists ()
  "Test that platform initialization function exists."
  (should (fboundp 'mcg-initialize-platform))
  (should (fboundp 'mcg-display-platform-info)))

(provide 'run-tests)
;;; run-tests.el ends here
