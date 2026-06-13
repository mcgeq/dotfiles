vim9script
# ============================================================================
# 组件: Core / Module
# 作者: mcge <mcgeq@outlook.com>
# 说明: 为 modules/* 提供共享的配置、守卫和 health helper。
# ============================================================================

# ----------------------------------------------------------------------------
# 配置合并
# ----------------------------------------------------------------------------

def g:MergeModuleConfig(defaults: dict<any>, user_config: dict<any> = {}): dict<any>
  var merged = copy(defaults)
  extend(merged, user_config)
  return merged
enddef

def g:MergeModuleConfigDeep(defaults: dict<any>, user_config: dict<any> = {}): dict<any>
  var merged = deepcopy(defaults)

  for [key, value] in items(user_config)
    if has_key(merged, key) && type(merged[key]) == v:t_dict && type(value) == v:t_dict
      merged[key] = g:MergeModuleConfigDeep(merged[key], value)
    else
      merged[key] = deepcopy(value)
    endif
  endfor

  return merged
enddef

def EnsureModuleOverrideRegistry()
  if type(get(g:, 'mcge_module_overrides', {})) != v:t_dict
    g:mcge_module_overrides = {}
  endif
enddef

def GetModuleOverrideStore(): dict<any>
  EnsureModuleOverrideRegistry()
  return get(g:, 'mcge_module_overrides', {})
enddef

def g:GetModuleOverride(module_id: string): dict<any>
  const override = get(GetModuleOverrideStore(), module_id, {})
  return type(override) == v:t_dict ? deepcopy(override) : {}
enddef

def g:GetModuleOverrideRegistry(): dict<any>
  return deepcopy(GetModuleOverrideStore())
enddef

def g:SetModuleOverride(module_id: string, override: dict<any>)
  EnsureModuleOverrideRegistry()
  g:mcge_module_overrides[module_id] = g:MergeModuleConfigDeep(g:GetModuleOverride(module_id), override)
enddef

def g:SetModuleOverrides(overrides: dict<any>)
  for [module_id, override] in items(overrides)
    if type(override) == v:t_dict
      g:SetModuleOverride(module_id, override)
    endif
  endfor
enddef

def g:ResolveModuleConfig(module_id: string, defaults: dict<any>, user_config: dict<any> = {}): dict<any>
  const resolved = g:MergeModuleConfig(defaults, g:GetModuleOverride(module_id))
  return g:MergeModuleConfig(resolved, user_config)
enddef

def g:ResolveModuleConfigDeep(module_id: string, defaults: dict<any>, user_config: dict<any> = {}): dict<any>
  const resolved = g:MergeModuleConfigDeep(defaults, g:GetModuleOverride(module_id))
  return g:MergeModuleConfigDeep(resolved, user_config)
enddef

# ----------------------------------------------------------------------------
# 运行时辅助
# ----------------------------------------------------------------------------

# 批量写入 g: 变量，避免各模块重复写一长串赋值语句。
def g:ApplyGlobalVars(values: dict<any>)
  for [name, value] in items(values)
    g:[name] = value
  endfor
enddef

def g:CommandExists(command_name: string): bool
  return exists(':' .. command_name) > 0
enddef

def g:FunctionExists(function_name: string): bool
  return exists('*' .. function_name) > 0
enddef

# ----------------------------------------------------------------------------
# 模块守卫
# ----------------------------------------------------------------------------

# 模块文件标准开头:
#   if g:MarkModuleLoaded('module_id')
#     finish
#   endif
def g:MarkModuleLoaded(module_id: string): bool
  const flag_name = $'mcge_{module_id}_loaded'
  if exists($'g:{flag_name}')
    g:ErrDebug($'Module {module_id} already loaded')
    return true
  endif

  g:[flag_name] = v:true
  return false
enddef

def g:ModuleIsDisabled(config: dict<any>, module_name: string): bool
  if !get(config, 'enabled', true)
    g:ErrDebug($'{module_name} is disabled')
    return true
  endif
  return false
enddef

def BuildManagedHealthExtras(module_id: string, config: dict<any>, extras: dict<any> = {}): dict<any>
  const override = g:GetModuleOverride(module_id)
  var result = {
    module_id: module_id,
    config_keys: sort(keys(config)),
    override_keys: sort(keys(override)),
    has_overrides: !empty(override),
  }
  extend(result, extras)
  return result
enddef

# ----------------------------------------------------------------------------
# Health 构建
# ----------------------------------------------------------------------------

def g:BuildModuleHealth(name: string, config: dict<any>, extras: dict<any> = {}): dict<any>
  var result = {
    name: name,
    enabled: get(config, 'enabled', true),
    status: get(config, 'enabled', true) ? 'running' : 'disabled',
    available: true,
  }
  extend(result, extras)
  return result
enddef

def g:BuildCommandModuleHealth(
  name: string,
  config: dict<any>,
  command_name: string,
  extras: dict<any> = {},
): dict<any>
  var result = {
    available: g:CommandExists(command_name),
    command: command_name,
  }
  extend(result, extras)
  return g:BuildModuleHealth(name, config, result)
enddef

def g:BuildFunctionModuleHealth(
  name: string,
  config: dict<any>,
  function_name: string,
  extras: dict<any> = {},
): dict<any>
  var result = {
    available: g:FunctionExists(function_name),
    function: function_name,
  }
  extend(result, extras)
  return g:BuildModuleHealth(name, config, result)
enddef

def g:BuildManagedModuleHealth(
  module_id: string,
  name: string,
  config: dict<any>,
  extras: dict<any> = {},
): dict<any>
  return g:BuildModuleHealth(name, config, BuildManagedHealthExtras(module_id, config, extras))
enddef

def g:BuildManagedCommandModuleHealth(
  module_id: string,
  name: string,
  config: dict<any>,
  command_name: string,
  extras: dict<any> = {},
): dict<any>
  return g:BuildCommandModuleHealth(name, config, command_name, BuildManagedHealthExtras(module_id, config, extras))
enddef

def g:BuildManagedFunctionModuleHealth(
  module_id: string,
  name: string,
  config: dict<any>,
  function_name: string,
  extras: dict<any> = {},
): dict<any>
  return g:BuildFunctionModuleHealth(name, config, function_name, BuildManagedHealthExtras(module_id, config, extras))
enddef

# vim: set ft=vim sw=2 ts=2 sts=2 et:
