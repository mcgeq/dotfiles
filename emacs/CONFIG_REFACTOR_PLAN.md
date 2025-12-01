# Emacs Configuration Deep Refactoring Plan

## Current Analysis

### ✅ Strengths
- Extensions well-organized (12 categories)
- Basic modular structure exists
- LSP Bridge configured
- Delayed loading mechanism in place

### ❌ Pain Points
1. **Limited Language Support**: Only 4 languages (Rust, Lua, Web, C++)
2. **Unorganized Categories**:
   - `tools/` (11 files) - too broad, needs splitting
   - `etc/` (5 files) - unclear naming
3. **Missing Languages**: Python, Zig, TypeScript, JavaScript (proper)
4. **No Frontend Tooling**: ESLint, Prettier, etc.
5. **Simple Delayed Loading**: Not optimized by use case
6. **No Development Profiles**: Frontend vs Backend

---

## Refactoring Strategy

### Phase 1: Reorganize Config Structure

#### 1.1 Rename & Restructure

**Old Structure**:
```
config-org/
├── tools/      (11 files - too broad)
├── etc/        (5 files - unclear)
├── lang/       (5 files - limited)
```

**New Structure**:
```
config-org/
├── core/           # Core infrastructure
├── ui/             # User interface
├── editor/         # Editor enhancements
├── completion/     # Completion system
├── keybindings/    # From etc/ - clear purpose
├── search/         # From tools/ - blink-search, color-rg
├── input/          # From tools/ - rime, fingertip, wraplish
├── utils/          # From tools/ - auto-save, helpful, etc.
├── git/            # From etc/ - magit
├── lang/           # Language-specific configs
│   ├── backend/    # Backend languages
│   │   ├── lang-python.org
│   │   ├── lang-rust.org
│   │   ├── lang-zig.org
│   │   ├── lang-cpp.org
│   │   └── lang-lua.org
│   └── frontend/   # Frontend languages
│       ├── lang-typescript.org
│       ├── lang-javascript.org
│       ├── lang-web.org (HTML/CSS)
│       └── lang-jsx.org
├── dev/            # Development environment
│   ├── init-lsp.org
│   ├── init-format.org
│   ├── init-lint.org
│   └── init-debug.org
├── org/            # Org Mode
└── docs/           # Documentation tools
```

### Phase 2: Enhanced Language Support

#### 2.1 Backend Languages

**Python**
```elisp
- LSP: pyright or pylsp
- Formatter: black, yapf
- Linter: ruff, pylint
- Tools: pytest, ipython
- Virtual env: pyvenv
```

**Rust** (existing, enhance)
```elisp
- LSP: rust-analyzer ✓
- Formatter: rustfmt
- Cargo integration
- Clippy linter
```

**Zig** (new)
```elisp
- LSP: zls
- Build: zig build integration
- Formatter: zig fmt
```

**C/C++** (existing, enhance)
```elisp
- LSP: clangd
- Formatter: clang-format
- CMake integration
- Modern C++ features ✓
```

**Lua** (existing, enhance)
```elisp
- LSP: lua-language-server
- Formatter: stylua
```

#### 2.2 Frontend Languages

**TypeScript** (new)
```elisp
- LSP: typescript-language-server
- Tree-sitter: tsx support
- Formatter: prettier
- Linter: eslint
```

**JavaScript** (new)
```elisp
- LSP: typescript-language-server (for JS)
- Modern JS (ES6+)
- React/Vue support
- Node.js integration
```

**Web (HTML/CSS)** (existing, enhance)
```elisp
- HTML: emmet integration
- CSS: LSP (vscode-css-languageserver)
- TailwindCSS intellisense
- Live preview
```

**JSX/TSX** (new)
```elisp
- React development
- Component navigation
- Props completion
```

### Phase 3: Development Environment

#### 3.1 LSP Configuration
```elisp
;; Unified LSP setup
- Language-agnostic LSP settings
- Performance tuning
- Workspace management
- Multi-project support
```

#### 3.2 Formatting
```elisp
;; Format on save
- EditorConfig support
- Language-specific formatters
- Format region capability
```

#### 3.3 Linting
```elisp
;; Real-time linting
- Flymake integration
- ESLint for JS/TS
- Ruff for Python
- Clippy for Rust
```

#### 3.4 Debugging
```elisp
;; DAP integration (optional)
- Python debugging
- Node.js debugging
- Native debugging (C/C++)
```

### Phase 4: Performance Optimization

#### 4.1 Smart Loading Strategy

```elisp
;; Lazy load by file type
(defun mcg-load-backend-lang (lang)
  "Load backend language support on-demand")

(defun mcg-load-frontend-lang (lang)
  "Load frontend language support on-demand")

;; Auto-load based on project type
(defun mcg-detect-project-type ()
  "Detect project type and load appropriate config")
```

#### 4.2 Startup Performance

**Before**:
- All languages loaded after 1s delay
- No differentiation

**After**:
- Core: Immediate
- UI: 0.1s
- Editor: 0.2s
- Language-specific: On file open (auto-mode-alist)
- Heavy tools: On command (autoload)

### Phase 5: Developer Experience

#### 5.1 Project Management
```elisp
- projectile integration
- Project-specific settings
- Multi-project workflow
```

#### 5.2 Snippets & Templates
```elisp
- Language-specific snippets
- Project templates
- File templates
```

#### 5.3 Quick Commands
```elisp
;; Development commands
M-x mcg-dev-frontend  ;; Load frontend env
M-x mcg-dev-backend   ;; Load backend env
M-x mcg-dev-python    ;; Python-specific setup
M-x mcg-dev-web       ;; Web development setup
```

---

## Implementation Order

### Week 1: Structure Reorganization
1. [ ] Rename `etc/` → `keybindings/`
2. [ ] Split `tools/` into `search/`, `input/`, `utils/`
3. [ ] Create `lang/backend/` and `lang/frontend/` subdirs
4. [ ] Move existing lang configs to appropriate subdirs

### Week 2: Language Support - Backend
1. [ ] Create `lang-python.org`
2. [ ] Create `lang-zig.org`
3. [ ] Enhance `lang-rust.org`
4. [ ] Enhance `lang-cpp.org`

### Week 3: Language Support - Frontend
1. [ ] Create `lang-typescript.org`
2. [ ] Create `lang-javascript.org`
3. [ ] Create `lang-jsx.org`
4. [ ] Enhance `lang-web.org`

### Week 4: Development Environment
1. [ ] Create `dev/init-lsp.org` (unified LSP config)
2. [ ] Create `dev/init-format.org`
3. [ ] Create `dev/init-lint.org`
4. [ ] Implement smart loading

### Week 5: Performance & Polish
1. [ ] Benchmark startup time
2. [ ] Implement on-demand loading
3. [ ] Test all language configs
4. [ ] Documentation

---

## Expected Improvements

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| Supported Languages | 4 | 9 | +125% |
| Startup Time | ~1.2s | ~0.8s | +33% |
| Config Clarity | 7/10 | 9/10 | +29% |
| Dev Setup Time | 5min | 30s | 90% faster |

---

## Next Steps

1. Review and approve this plan
2. Start with Phase 1 (Structure)
3. Implement incrementally
4. Test after each phase

---

**Priority**: HIGH  
**Estimated Time**: 2-3 weeks (working part-time)  
**Risk**: LOW (can rollback at any phase)
