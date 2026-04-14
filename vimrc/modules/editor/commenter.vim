vim9script
# ============================================================================
# 模块: Editor / Commenter
# 作者: mcge <mcgeq@outlook.com>
# 说明: 配置 NERDCommenter 注释行为。
# ============================================================================

# 防止重复加载
if g:MarkModuleLoaded('commenter')
  finish
endif

# 配置
var config = {
  enabled: true,
  create_default_mappings: 1,
  space_delims: 1,
  compact_sexy_coms: 1,
  default_align: "left",
  alt_delims_java: 1,
  custom_delimiters: { "c": { "left": "/**", "right": "*/" } },
  comment_empty_lines: 1,
  trim_trailing_whitespace: 1,
  toggle_check_all_lines: 1,
}

# 初始化 NERDCommenter
def g:InitNERDCommenter(user_config: dict<any> = {})
  config = g:ResolveModuleConfigDeep('commenter', config, user_config)

  if g:ModuleIsDisabled(config, 'NERDCommenter')
    return
  endif

  # 基础设置
  g:ApplyGlobalVars({
    NERDCreateDefaultMappings: config.create_default_mappings,
    NERDSpaceDelims: config.space_delims,
    NERDCompactSexyComs: config.compact_sexy_coms,
    NERDDefaultAlign: config.default_align,
    NERDAltDelims_java: config.alt_delims_java,
    NERDCustomDelimiters: config.custom_delimiters,
    NERDCommentEmptyLines: config.comment_empty_lines,
    NERDTrimTrailingWhitespace: config.trim_trailing_whitespace,
    NERDToggleCheckAllLines: config.toggle_check_all_lines,
  })

  call g:ErrDebug('NERDCommenter initialized')
enddef

# 健康检查
def g:NERDCommenterHealthCheck(): dict<any>
  return g:BuildManagedCommandModuleHealth('commenter', 'NERDCommenter', config, 'NERDCommenterToggle', {
    mappings_configured: config.create_default_mappings,
  })
enddef

# 获取配置
def g:GetNERDCommenterConfig(): dict<any>
  return config
enddef

# 立即初始化
call g:InitNERDCommenter()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
