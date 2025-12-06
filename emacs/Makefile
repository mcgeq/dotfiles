# 定义 EM 和 EE 变量
EM ?= emacs
EE ?= $(EM) -Q --batch --eval "(progn (require 'ob-tangle) (setq org-confirm-babel-evaluate nil))"


# 需要加载的目录
DS = core ui editor completion keybindings search input utils git org docs lang dev

# 编译生成后的*.el 文件存放位置
DF ?= site-lisp/config

# 自定义编译模板的函数 tangle_template 用于将 *.org 转为 *.el
define tangle_template
# 检查目录是否存在，不存在就创建
check_dir.$(1):
	@powershell -Command "if (-not (Test-Path 'site-lisp/config/$(1)')) { New-Item -ItemType Directory -Path 'site-lisp/config/$(1)' -Force | Out-Null }"

# 目录作为目标，指示新的编译目标
$(1): $(patsubst config-org/$(1)/%.org, site-lisp/config/$(1)/%.el, $(wildcard config-org/$(1)/*.org))

clean-$(1):
	-powershell -Command "if (Test-Path 'site-lisp/config/$(1)') { Remove-Item -Recurse -Force 'site-lisp/config/$(1)' }"

.PHONY: clean-$(1)

# 目标的编译方法
site-lisp/config/$(1)/%.el: config-org/$(1)/%.org
	$(EE) --eval '(org-babel-tangle-publish t "$$<" "$(DF)/$(1)/")'
endef

init.el: config-org/init.org
	$(EE) --eval '(org-babel-tangle-publish t "$<" "$(DF)/")'

# lang 子目录特殊处理
lang: $(patsubst config-org/lang/%.org, site-lisp/config/lang/%.el, $(wildcard config-org/lang/*.org)) \
      $(patsubst config-org/lang/backend/%.org, site-lisp/config/lang/backend/%.el, $(wildcard config-org/lang/backend/*.org)) \
      $(patsubst config-org/lang/frontend/%.org, site-lisp/config/lang/frontend/%.el, $(wildcard config-org/lang/frontend/*.org))

# lang 根目录的文件
site-lisp/config/lang/%.el: config-org/lang/%.org
	@powershell -Command "if (-not (Test-Path 'site-lisp/config/lang')) { New-Item -ItemType Directory -Path 'site-lisp/config/lang' -Force | Out-Null }"
	$(EE) --eval '(org-babel-tangle-publish t "$<" "$(DF)/lang/")'

# lang/backend 子目录
site-lisp/config/lang/backend/%.el: config-org/lang/backend/%.org
	@powershell -Command "if (-not (Test-Path 'site-lisp/config/lang/backend')) { New-Item -ItemType Directory -Path 'site-lisp/config/lang/backend' -Force | Out-Null }"
	$(EE) --eval '(org-babel-tangle-publish t "$<" "$(DF)/lang/backend/")'

# lang/frontend 子目录
site-lisp/config/lang/frontend/%.el: config-org/lang/frontend/%.org
	@powershell -Command "if (-not (Test-Path 'site-lisp/config/lang/frontend')) { New-Item -ItemType Directory -Path 'site-lisp/config/lang/frontend' -Force | Out-Null }"
	$(EE) --eval '(org-babel-tangle-publish t "$<" "$(DF)/lang/frontend/")'

$(foreach dir,$(filter-out lang,$(DS)),$(eval $(call tangle_template,$(dir))))

el: $(DS) init.el

# 字节码翻译 暂未使用
elc:
	$(EM) --batch -l ./init.el -L "site-lisp/config" --eval '(byte-recompile-directory "site-lisp/config/core" 0)'
	$(EM) --batch -l ./init.el -L "site-lisp/config" --eval '(byte-recompile-directory "site-lisp/config/ui" 0)'
	$(EM) --batch -l ./init.el -L "site-lisp/config" --eval '(byte-recompile-directory "site-lisp/config/editor" 0)'
	$(EM) --batch -l ./init.el -L "site-lisp/config" --eval '(byte-recompile-directory "site-lisp/config/completion" 0)'
	$(EM) --batch -l ./init.el -L "site-lisp/config" --eval '(byte-recompile-directory "site-lisp/config/tools" 0)'
	$(EM) --batch -l ./init.el -L "site-lisp/config" --eval '(byte-recompile-directory "site-lisp/config/etc" 0)'
	$(EM) --batch -l ./init.el -L "site-lisp/config" --eval '(byte-recompile-directory "site-lisp/config/org" 0)'
	$(EM) --batch -l ./init.el -L "site-lisp/config" --eval '(byte-recompile-directory "site-lisp/config/docs" 0)'
	$(EM) --batch -l ./init.el -L "site-lisp/config" --eval '(byte-recompile-directory "site-lisp/config/lang" 0)'


.PHONY: clean generate generate-elc init-packages update-packages update-package status-packages lock-packages restore-packages help init up upp st h

generate: el
generate-elc: el elc

clean:
	-powershell -Command "if (Test-Path 'site-lisp/config') { Remove-Item -Recurse -Force 'site-lisp/config' }"

# =============================================================================
# Git Submodule 管理
# =============================================================================

# Initialize all submodules
init-packages:
	@echo "Initializing Git Submodules..."
	git submodule update --init --recursive
	@echo "[OK] Initialization complete!"

# Short alias
init: init-packages

# Update all packages
update-packages:
	@echo "Updating all Git Submodules..."
	@powershell -File scripts/Update-PackagesSafely.ps1

# Short alias
up: update-packages

# Update specific package (requires PKG parameter)
# Usage: make update-package PKG=magit
update-package:
	@if [ -z "$(PKG)" ]; then \
		echo "❌ Error: Please specify package name"; \
		echo "Usage: make update-package PKG=name"; \
		echo "Example: make update-package PKG=magit"; \
		exit 1; \
	fi
	@echo "Updating package: $(PKG)..."
	@if [ -d "site-lisp/extensions/$(PKG)" ]; then \
		cd site-lisp/extensions/$(PKG) && git pull; \
		echo "[OK] $(PKG) updated!"; \
	else \
		echo "[X] Package not found: $(PKG)"; \
		exit 1; \
	fi

# Short alias
upp: update-package

# Check submodules status
status-packages:
	@echo "Checking Git Submodules status..."
	@powershell -File scripts/Get-PackageStatus.ps1

# Short alias
st: status-packages

# Lock current package versions
lock-packages:
	@echo "Locking package versions..."
	@powershell -File scripts/Save-PackageVersions.ps1
	@echo "[OK] Versions locked to site-lisp/packages-lock.el"

# Restore to locked versions
restore-packages:
	@echo "Restoring to locked versions..."
	@powershell -File scripts/Restore-PackageVersions.ps1

# Show help
help:
	@echo "Emacs Configuration Makefile Commands:"
	@echo ""
	@echo "Configuration:"
	@echo "  make generate        - Generate config from Org files"
	@echo "  make generate-elc    - Generate and byte-compile config"
	@echo "  make clean           - Clean generated config files"
	@echo ""
	@echo "Package Management:"
	@echo "  make init-packages (init)  - Initialize all Git Submodules"
	@echo "  make status-packages (st)  - Check package status"
	@echo "  make update-packages (up)  - Update all Git Submodules (safe)"
	@echo "  make update-package (upp) PKG=name - Update specific package"
	@echo "  make lock-packages         - Lock current package versions"
	@echo "  make restore-packages      - Restore to locked versions"
	@echo ""
	@echo "Other:"
	@echo "  make help (h)        - Show this help message"
	@echo ""
	@echo "Quick shortcuts: init, st, up, upp, h"

# 短命令别名
h: help
