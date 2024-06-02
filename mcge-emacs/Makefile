# 定义 EM 和 EE 变量
EM ?= emacs
EE ?= $(EM) -Q --batch --eval "(progn (require 'ob-tangle) (setq org-confirm-babel-evaluate nil))"


# 需要加载的目录，可能有 lisp site-lisp 等
DS = core etc lang

# 编译生成后的*.el 文件存放位置
DF ?= site-lisp/config

# 自定义编译模板的函数 tangle_template 用于将 *.org 转为 *.el
define tangle_template
# 检查目录是否存在，不存在就创建
check_dir.$(1):
	@mkdir -p site-lisp/config/$(1)

# 目录作为目标，指示新的编译目标
$(1): $(patsubst config-org/$(1)/%.org, site-lisp/config/$(1)/%.el, $(wildcard config-org/$(1)/*.org))

clean-$(1):
	rm -rf site-lisp/config/$(1)

.PHONY: clean-$(1)

# 目标的编译方法
site-lisp/config/$(1)/%.el: config-org/$(1)/%.org
	$(EE) --eval '(org-babel-tangle-publish t "$$<" "$(DF)/$(1)/")'
endef

init.el: config-org/init.org
	$(EE) --eval '(org-babel-tangle-publish t "$<" "$(DF)/")'

$(foreach dir,$(DS),$(eval $(call tangle_template,$(dir))))

el: $(DS) init.el

# 字节码翻译 暂未使用
elc:
	$(EM) --batch -l ./init.el -L "site-lisp/config" --eval '(byte-recompile-directory "site-lisp/config/etc" 0)'
	$(EM) --batch -l ./init.el -L "site-lisp/config" --eval '(byte-recompile-directory "site-lisp/config/lang" 0)'


.PHONY: clean generate generate-elc

generate: el
generate-elc: el elc

clean:
	rm -rf site-lisp/config
