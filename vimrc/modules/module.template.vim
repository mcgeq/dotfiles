vim9script
# ============================================================================
# 模块: Category / ModuleName
# 作者: mcge <mcgeq@outlook.com>
# 说明: 新模块模板，复制后请按实际插件和职责修改。
# ============================================================================

if g:MarkModuleLoaded('module_id')
  finish
endif

const module_id = 'module_id'
const module_name = 'ModuleName'

var config = {
  enabled: true,
}

# 如果自动命令或 :command 需要长期存在，优先调用 g:* wrapper，
# 避免脚本重载后留下失效的 <SNR> 引用。
def g:ModuleAction()
enddef

# 注册自动命令时，优先让 autocmd 直接调用 g:* wrapper。
def RegisterModuleAutocmds()
  augroup mcge_module_id
    autocmd!
    # autocmd User SomeEvent call g:ModuleAction()
  augroup END
enddef

# 用户命令同理，尽量把真正逻辑放到 g:* 入口。
def DefineModuleCommands()
  # command! -nargs=0 ModuleCommand call g:ModuleAction()
enddef

def g:InitModule(user_config: dict<any> = {})
  config = g:ResolveModuleConfig(module_id, config, user_config)

  if g:ModuleIsDisabled(config, module_name)
    return
  endif

  # 简单表结构用 g:ResolveModuleConfig()，嵌套配置改用
  # g:ResolveModuleConfigDeep()。
  # 纯插件全局变量转发优先用 g:ApplyGlobalVars()。

  RegisterModuleAutocmds()
  DefineModuleCommands()

  call g:ErrDebug($'{module_name} initialized')
enddef

def g:ModuleHealthCheck(): dict<any>
  return g:BuildManagedModuleHealth(module_id, module_name, config)
enddef

def g:GetModuleConfig(): dict<any>
  return config
enddef

call g:InitModule()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
