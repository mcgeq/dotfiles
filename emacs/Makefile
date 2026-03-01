# =============================================================================
# MCG Emacs Configuration Makefile
# =============================================================================

# OS Detection - Best Practice: 组合多种检测方式
ifeq ($(shell uname -s 2>/dev/null),Windows_NT)
    IS_UNIX := 0
else ifeq ($(shell command -v uname 2>/dev/null),)
    ifeq ($(shell command -v rm 2>/dev/null),)
        IS_UNIX := 0
    else
        IS_UNIX := 1
    endif
else
    IS_UNIX := 1
endif

# Define Emacs variable
EM ?= emacs
EE ?= $(EM) -Q --batch --eval "(progn (require 'ob-tangle) (setq org-confirm-babel-evaluate nil))"

# Source and target directories
SRC_DIR = config-org
DST_DIR = site-lisp/config
EXT_DIR = site-lisp/extensions

# Git repository root (relative to emacs/)
ROOT_DIR = ..

# Directories to process
DIRS = core modules/ui modules/editor modules/completion modules/tools modules/enhance modules/org modules/writing modules/lang private

# =============================================================================
# Build System
# =============================================================================

# Template function - cross-platform
define tangle_template
build-$(subst /,-,$(1)): $(patsubst $(SRC_DIR)/$(1)/%.org, $(DST_DIR)/$(1)/%.el, $(wildcard $(SRC_DIR)/$(1)/*.org))

$(DST_DIR)/$(1)/%.el: $(SRC_DIR)/$(1)/%.org
ifeq ($(IS_UNIX),1)
	@mkdir -p $(DST_DIR)/$(1)
else
	@powershell -Command "if (-not (Test-Path '$(DST_DIR)/$(1)')) { New-Item -ItemType Directory -Path '$(DST_DIR)/$(1)' -Force | Out-Null }"
endif
	@echo Tangling: $$< -> $$@
	$(EE) --eval '(org-babel-tangle-publish t "$$<" "$(DST_DIR)/$(1)/")'
endef

# init.el special handling
$(DST_DIR)/init.el: $(SRC_DIR)/init.org
ifeq ($(IS_UNIX),1)
	@mkdir -p $(DST_DIR)
else
	@powershell -Command "if (-not (Test-Path '$(DST_DIR)')) { New-Item -ItemType Directory -Path '$(DST_DIR)' -Force | Out-Null }"
endif
	@echo Tangling: $< -> $@
	$(EE) --eval '(org-babel-tangle-publish t "$<" "$(DST_DIR)/")'

# early-init.el special handling (copy to ~/.emacs.d/)
$(DST_DIR)/early-init.el: $(SRC_DIR)/early-init.org
ifeq ($(IS_UNIX),1)
	@mkdir -p $(DST_DIR)
else
	@powershell -Command "if (-not (Test-Path '$(DST_DIR)')) { New-Item -ItemType Directory -Path '$(DST_DIR)' -Force | Out-Null }"
endif
	@echo Tangling: $< -> $@
	$(EE) --eval '(org-babel-tangle-publish t "$<" "$(DST_DIR)/")'
ifeq ($(IS_UNIX),1)
	@echo Copying early-init.el to ~/.emacs.d/
	@mkdir -p ~/.emacs.d && cp -f $(DST_DIR)/early-init.el ~/.emacs.d/early-init.el
else
	@echo Copying early-init.el to %APPDATA%\.emacs.d/
	@powershell -Command "if (-not (Test-Path \"$$env:APPDATA\\.emacs.d\")) { New-Item -ItemType Directory -Path \"$$env:APPDATA\\.emacs.d\" -Force | Out-Null }; Copy-Item '$(DST_DIR)/early-init.el' \"$$env:APPDATA\\.emacs.d\\early-init.el\" -Force"
endif

# Generate all directory rules
$(foreach dir,$(DIRS),$(eval $(call tangle_template,$(dir))))

# Main build target
$(foreach dir,$(DIRS),$(eval $(call tangle_template,$(dir))))

# 主构建目标
all: $(DST_DIR)/early-init.el $(DST_DIR)/init.el $(foreach dir,$(DIRS),build-$(subst /,-,$(dir)))
	@echo [OK] Configuration generated from Org files!

# Tangle single directory
# Usage: make tangle-dir DIR=core or make tangle-dir DIR=modules/ui
tangle-dir:
ifndef DIR
	$(error DIR is required. Usage: make tangle-dir DIR=core)
endif
	@echo Tangling directory: $(SRC_DIR)/$(DIR)
ifeq ($(IS_UNIX),1)
	@mkdir -p $(DST_DIR)/$(DIR)
else
	@powershell -Command "if (-not (Test-Path '$(DST_DIR)/$(DIR)')) { New-Item -ItemType Directory -Path '$(DST_DIR)/$(DIR)' -Force | Out-Null }"
endif
	@for file in $(SRC_DIR)/$(DIR)/*.org; do \
		echo "Tangling: $$file"; \
		$(EM) -Q --batch --eval "(progn (require 'ob-tangle) (setq org-confirm-babel-evaluate nil))" --eval "(org-babel-tangle-file \"$$file\")"; \
	done
	@echo [OK] Tangled $(DIR)

# Tangle 单个文件 (手动指定)
# 用法: make tangle-file FILE=config-org/modules/editor/init-defaults.org
tangle-file:
ifndef FILE
	$(error FILE is required. Usage: make tangle-file FILE=config-org/modules/editor/init-defaults.org)
endif
	@echo Tangling: $(FILE)
	$(EE) --eval '(org-babel-tangle-file "$(FILE)")'
	@echo [OK] Tangled $(FILE)

# Clean generated files
clean:
	@echo Cleaning generated files in $(DST_DIR)/...
ifeq ($(IS_UNIX),1)
	@find $(DST_DIR) -name '*.el' -type f -delete 2>/dev/null || true
else
	@powershell -Command "Get-ChildItem -Path '$(DST_DIR)' -Filter '*.el' -Recurse | Remove-Item -Force"
endif
	@echo [OK] Generated .el files cleaned!

# Clean all generated files
clean-all:
	@echo Cleaning all generated files...
ifeq ($(IS_UNIX),1)
	@rm -rf $(DST_DIR)
else
	@powershell -Command "if (Test-Path '$(DST_DIR)') { Remove-Item -Recurse -Force '$(DST_DIR)' }"
endif
	@echo [OK] All files cleaned!

# Show file list
list:
	@echo Source .org files:
ifeq ($(IS_UNIX),1)
	@find $(SRC_DIR) -name '*.org' ! -name 'README.org' ! -name 'README.md' 2>/dev/null | sed 's|^$(SRC_DIR)/||' | sed 's|^|  |'
else
	@powershell -Command "Get-ChildItem -Path '$(SRC_DIR)' -Filter '*.org' -Recurse | Where-Object { $$_.Name -ne 'README.org' -and $$_.Name -ne 'README.md' } | ForEach-Object { Write-Host ('  ' + $$_.FullName.Substring((Get-Location).Path.Length + 1)) }"
endif
	@echo ""
	@echo Target .el files:
ifeq ($(IS_UNIX),1)
	@if [ -d "$(DST_DIR)" ]; then find $(DST_DIR) -name '*.el' -type f 2>/dev/null | sed 's|^$(DST_DIR)/||' | sed 's|^|  |'; fi
else
	@powershell -Command "if (Test-Path '$(DST_DIR)') { Get-ChildItem -Path '$(DST_DIR)' -Filter '*.el' -Recurse | ForEach-Object { Write-Host ('  ' + $$_.FullName.Substring((Get-Location).Path.Length + 1)) } }"
endif

.PHONY: all tangle-file clean clean-all list

# =============================================================================
# Byte Compile
# =============================================================================

elc:
	@echo Byte-compiling configuration files...
	$(EM) --batch -L "$(DST_DIR)" -L "$(DST_DIR)/core" \
		--eval '(byte-recompile-directory "$(DST_DIR)" 0 t)'
	@echo [OK] Byte compilation complete!

clean-elc:
	@echo Cleaning .elc files...
ifeq ($(IS_UNIX),1)
	@find $(DST_DIR) -name '*.elc' -type f -delete 2>/dev/null || true
else
	@powershell -Command "Get-ChildItem -Path '$(DST_DIR)' -Filter '*.elc' -Recurse | Remove-Item -Force"
endif
	@echo [OK] .elc files cleaned!

compile: all elc
	@echo [OK] Full compile complete!

.PHONY: elc clean-elc compile

# =============================================================================
# Testing
# =============================================================================

test:
	@echo Running tests...
	$(EM) -Q --batch \
		-L "$(DST_DIR)" \
		-L "$(DST_DIR)/core" \
		-l test/run-tests.el \
		-f ert-run-tests-batch-and-exit

test-verbose:
	@echo Running tests (verbose)...
	$(EM) -Q --batch \
		-L "$(DST_DIR)" \
		-L "$(DST_DIR)/core" \
		-l test/run-tests.el \
		--eval "(ert-run-tests-batch-and-exit t)"

.PHONY: test test-verbose

# =============================================================================
# Git Submodule 管理
# =============================================================================

# 初始化所有 Git Submodules (gsb = git submodule bootstrap)
gsb:
	@echo Step 1: Initializing all submodules...
	git -C $(ROOT_DIR) submodule update --init --recursive
	@echo [OK] Submodules initialized!

# 切换所有子模块到主分支 (gsbr = git submodule branch)
gsbr:
	@echo Step 2: Switching all submodules to main branch...
	git -C $(ROOT_DIR) submodule foreach ' \
		git show-ref --verify --quiet refs/remotes/origin/main && git checkout main || \
		git show-ref --verify --quiet refs/remotes/origin/master && git checkout master || \
		echo "No main/master branch found for $$name" \
	'
	@echo [OK] Submodules branch switched!

# 更新所有子模块到最新 (gsu = git submodule update)
gsu:
	@echo Step 3: Pulling latest updates for all submodules...
	git -C $(ROOT_DIR) submodule foreach 'git pull --rebase 2>/dev/null || echo "Failed to pull $$name"'
	@echo [OK] All submodules updated!

# 一键同步所有子模块：初始化 + 切换分支 + 更新 (gss = git submodule sync all)
gss: gsb gsbr gsu
	@echo [OK] All submodules synced!

# 重置所有子模块到远程 HEAD (gsr = git submodule reset)
gsr:
	@echo Resetting all submodules to remote HEAD...
	git -C $(ROOT_DIR) submodule foreach 'git reset --hard origin/$$(git rev-parse --abbrev-ref HEAD)'
	@echo [OK] Submodules reset completed!

# 添加新子模块 (gsa = git submodule add)
# 用法: make gsa URL=https://github.com/user/repo CAT=git NAME=magit
gsa:
ifndef URL
	$(error URL is required. Usage: make gsa URL=https://github.com/user/repo CAT=category NAME=package)
endif
ifndef CAT
	$(error CAT is required. Usage: make gsa URL=https://github.com/user/repo CAT=category NAME=package)
endif
ifndef NAME
	$(error NAME is required. Usage: make gsa URL=https://github.com/user/repo CAT=category NAME=package)
endif
	@echo Adding submodule: $(NAME) to $(CAT)...
	git -C $(ROOT_DIR) submodule add $(URL) emacs/$(EXT_DIR)/$(CAT)/$(NAME)
	@echo [OK] Submodule $(NAME) added!

# 移除子模块 (gsrm = git submodule remove)
# 用法: make gsrm CAT=git NAME=magit
gsrm:
ifndef CAT
	$(error CAT is required. Usage: make gsrm CAT=category NAME=package)
endif
ifndef NAME
	$(error NAME is required. Usage: make gsrm CAT=category NAME=package)
endif
	@echo Removing submodule: $(CAT)/$(NAME)...
	git -C $(ROOT_DIR) submodule deinit -f emacs/$(EXT_DIR)/$(CAT)/$(NAME)
	git -C $(ROOT_DIR) rm -f emacs/$(EXT_DIR)/$(CAT)/$(NAME)
	-powershell -Command "Remove-Item -Recurse -Force '$(ROOT_DIR)/.git/modules/emacs/$(EXT_DIR)/$(CAT)/$(NAME)' -ErrorAction SilentlyContinue"
	@echo [OK] Submodule $(NAME) removed!

# 同步 .gitmodules URL
gsync:
	@echo Syncing submodule URLs...
	git -C $(ROOT_DIR) submodule sync --recursive
	@echo [OK] Submodule URLs synced!

# 检查子模块状态
gsst:
	@echo Checking Git Submodules status...
	git -C $(ROOT_DIR) submodule status

# 编译扩展
compile-ext:
	@echo Compiling extension .el files...
	$(EM) -Q --batch \
		-L "$(EXT_DIR)" \
		--eval '(byte-recompile-directory "$(EXT_DIR)" 0 t)'
	@echo [OK] Extensions compiled!

clean-ext:
	@echo Cleaning extension .elc files...
	-powershell -Command "Get-ChildItem -Path '$(EXT_DIR)' -Filter '*.elc' -Recurse | Remove-Item -Force"
	@echo [OK] Extension .elc files cleaned!

# 快捷别名
init: gsb
update: gsu
status: gsst
up: gsu
st: gsst

.PHONY: gsb gsbr gsu gss gsr gsa gsrm gsync gsst compile-ext clean-ext init update status up st

# =============================================================================
# 完整构建
# =============================================================================

build: gsb all compile-ext elc
	@echo [OK] Full build complete!

.PHONY: build

# =============================================================================
# Help
# =============================================================================

help:
	@echo MCG Emacs Configuration Makefile
	@echo ================================
	@echo.
	@echo Build:
	@echo   make all        - Generate config from Org files (default)
	@echo   make compile    - Generate and byte-compile config
	@echo   make build      - Full build: init submodules + generate + compile
	@echo   make clean      - Clean generated .el files
	@echo   make clean-all  - Clean all generated files
	@echo   make list       - List source and target files
	@echo.
	@echo Byte Compile:
	@echo   make elc        - Byte-compile config files
	@echo   make compile-ext - Byte-compile extensions
	@echo   make clean-elc  - Clean config .elc files
	@echo   make clean-ext  - Clean extension .elc files
	@echo.
	@echo Git Submodule Management:
	@echo   make gsb        - Initialize all submodules (init)
	@echo   make gsbr       - Switch submodules to main/master branch
	@echo   make gsu        - Update all submodules to latest (up)
	@echo   make gss        - Sync all: init + branch + update
	@echo   make gsr        - Reset submodules to remote HEAD
	@echo   make gsst       - Check submodule status (st)
	@echo   make gsync      - Sync .gitmodules URLs
	@echo   make gsa        - Add submodule (URL=... CAT=... NAME=...)
	@echo   make gsrm       - Remove submodule (CAT=... NAME=...)
	@echo.
	@echo Testing:
	@echo   make test       - Run all tests
	@echo.
	@echo Quick shortcuts: init=gsb, up=gsu, st=gsst, h=help

h: help

.PHONY: help h

# 默认目标
.DEFAULT_GOAL := all
