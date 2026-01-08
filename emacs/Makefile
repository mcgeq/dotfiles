# =============================================================================
# MCG Emacs Configuration Makefile
# =============================================================================

# 定义 Emacs 变量
EM ?= emacs
EE ?= $(EM) -Q --batch --eval "(progn (require 'ob-tangle) (setq org-confirm-babel-evaluate nil))"

# 源目录和目标目录
SRC_DIR = config-org
DST_DIR = site-lisp/config

# 需要处理的目录
DIRS = core modules/ui modules/editor modules/completion modules/tools modules/input modules/org modules/docs modules/lang private

# =============================================================================
# Build System
# =============================================================================

# 模板函数
define tangle_template
build-$(subst /,-,$(1)): $(patsubst $(SRC_DIR)/$(1)/%.org, $(DST_DIR)/$(1)/%.el, $(wildcard $(SRC_DIR)/$(1)/*.org))

$(DST_DIR)/$(1)/%.el: $(SRC_DIR)/$(1)/%.org
	@powershell -Command "if (-not (Test-Path '$(DST_DIR)/$(1)')) { New-Item -ItemType Directory -Path '$(DST_DIR)/$(1)' -Force | Out-Null }"
	@echo Tangling: $$< -> $$@
	$(EE) --eval '(org-babel-tangle-publish t "$$<" "$(DST_DIR)/$(1)/")'
endef

# init.el 特殊处理
$(DST_DIR)/init.el: $(SRC_DIR)/init.org
	@powershell -Command "if (-not (Test-Path '$(DST_DIR)')) { New-Item -ItemType Directory -Path '$(DST_DIR)' -Force | Out-Null }"
	@echo Tangling: $< -> $@
	$(EE) --eval '(org-babel-tangle-publish t "$<" "$(DST_DIR)/")'

# early-init.el 特殊处理 (生成后自动复制到 ~/.emacs.d/)
$(DST_DIR)/early-init.el: $(SRC_DIR)/early-init.org
	@powershell -Command "if (-not (Test-Path '$(DST_DIR)')) { New-Item -ItemType Directory -Path '$(DST_DIR)' -Force | Out-Null }"
	@echo Tangling: $< -> $@
	$(EE) --eval '(org-babel-tangle-publish t "$<" "$(DST_DIR)/")'
	@echo Copying early-init.el to ~/.emacs.d/
	@powershell -Command "Copy-Item '$(DST_DIR)/early-init.el' \"$$env:APPDATA\\.emacs.d\\early-init.el\" -Force"

# 生成所有目录的规则
$(foreach dir,$(DIRS),$(eval $(call tangle_template,$(dir))))

# 主构建目标
all: $(DST_DIR)/early-init.el $(DST_DIR)/init.el $(foreach dir,$(DIRS),build-$(subst /,-,$(dir)))
	@echo [OK] Configuration generated!

# 清理生成的文件
clean:
	@echo Cleaning generated files...
	-powershell -Command "if (Test-Path '$(DST_DIR)') { Remove-Item -Recurse -Force '$(DST_DIR)' }"
	@echo [OK] Files cleaned!

# 显示文件列表
list:
	@echo Source .org files:
	@powershell -Command "Get-ChildItem -Path '$(SRC_DIR)' -Filter '*.org' -Recurse | Where-Object { $$_.Name -ne 'README.org' } | ForEach-Object { Write-Host ('  ' + $$_.FullName.Substring((Get-Location).Path.Length + 1)) }"
	@echo ""
	@echo Target .el files:
	@powershell -Command "if (Test-Path '$(DST_DIR)') { Get-ChildItem -Path '$(DST_DIR)' -Filter '*.el' -Recurse | ForEach-Object { Write-Host ('  ' + $$_.FullName.Substring((Get-Location).Path.Length + 1)) } }"

.PHONY: all clean list

# =============================================================================
# Byte Compile (可选)
# =============================================================================

elc:
	$(EM) --batch -l ./init.el -L "$(DST_DIR)" --eval '(byte-recompile-directory "$(DST_DIR)" 0 t)'

compile: all elc

.PHONY: elc compile

# =============================================================================
# Testing
# =============================================================================

test:
	@echo Running tests...
	$(EM) -Q --batch \
		-l test/test-v3-core-bootstrap.el \
		-l test/test-v3-core-lib.el \
		-l test/test-v3-core-modules.el \
		-l test/test-v3-ui-modules.el \
		-l test/test-v3-lang-modules.el \
		-l test/test-v3-lsp-module.el \
		-l test/test-v3-makefile.el \
		-f ert-run-tests-batch-and-exit

test-verbose:
	@echo Running tests (verbose)...
	$(EM) -Q --batch \
		-l test/test-v3-core-bootstrap.el \
		-l test/test-v3-core-lib.el \
		-l test/test-v3-core-modules.el \
		-l test/test-v3-ui-modules.el \
		-l test/test-v3-lang-modules.el \
		-l test/test-v3-lsp-module.el \
		-l test/test-v3-makefile.el \
		--eval "(ert-run-tests-batch-and-exit t)"

.PHONY: test test-verbose

# =============================================================================
# Git Submodule 管理
# =============================================================================

init-packages:
	@echo Initializing Git Submodules...
	git submodule update --init --recursive
	@echo [OK] Initialization complete!

init: init-packages

update-packages:
	@echo Updating all Git Submodules...
	@powershell -File scripts/Update-PackagesSafely.ps1

up: update-packages

status-packages:
	@echo Checking Git Submodules status...
	@powershell -File scripts/Get-PackageStatus.ps1

st: status-packages

lock-packages:
	@echo Locking package versions...
	@powershell -File scripts/Save-PackageVersions.ps1
	@echo [OK] Versions locked to site-lisp/packages-lock.el

restore-packages:
	@echo Restoring to locked versions...
	@powershell -File scripts/Restore-PackageVersions.ps1

.PHONY: init-packages init update-packages up status-packages st lock-packages restore-packages

# =============================================================================
# Help
# =============================================================================

help:
	@echo MCG Emacs Configuration Makefile
	@echo ================================
	@echo ""
	@echo Build:
	@echo "  make all      - Generate config from Org files (default)"
	@echo "  make compile  - Generate and byte-compile config"
	@echo "  make clean    - Clean generated config files"
	@echo "  make list     - List source and target files"
	@echo ""
	@echo Testing:
	@echo "  make test     - Run all tests"
	@echo ""
	@echo Package Management:
	@echo "  make init     - Initialize Git Submodules"
	@echo "  make st       - Check package status"
	@echo "  make up       - Update all packages"
	@echo ""
	@echo Quick shortcuts: init, st, up, h

h: help

.PHONY: help h

# 默认目标
.DEFAULT_GOAL := all
