vim9script
# ============================================================================
# 模块: Editor / Tags
# 作者: mcge <mcgeq@outlook.com>
# 说明: 配置 gutentags 自动生成 tags 行为。
# ============================================================================

# 防止重复加载
if g:MarkModuleLoaded('tags')
  finish
endif

# 配置
var config = {
  enabled: true,
  cache_dir: g:mcge_customvimrcdir .. '/cache/tags',
  project_root: ['.git', '.hg', '.svn', 'CMakeLists.txt', 'Makefile', 'package.json'],
  generate_on_new: 1,
  generate_on_missing: 1,
  generate_on_save: 1,
  generate_on_empty_buffer: 0,
  ctags_exclude: ['*.txt', '*.md', '*.json', '*.yaml', '*.toml', '*/tmp/*', '*/node_modules/*', '*/.git/*'],
  ctags_autotag: 1,
}

# 初始化 Gutentags
def g:InitGutentags(user_config: dict<any> = {})
  config = g:ResolveModuleConfig('tags', config, user_config)

  if g:ModuleIsDisabled(config, 'Gutentags')
    return
  endif

  # 设置缓存、生成行为和排除规则
  g:ApplyGlobalVars({
    gutentags_cache_dir: config.cache_dir,
    gutentags_project_root: config.project_root,
    gutentags_generate_on_new: config.generate_on_new,
    gutentags_generate_on_missing: config.generate_on_missing,
    gutentags_generate_on_save: config.generate_on_save,
    gutentags_generate_on_empty_buffer: config.generate_on_empty_buffer,
    gutentags_ctags_exclude: config.ctags_exclude,
    gutentags_ctags_autotag: config.ctags_autotag,
  })

  call g:ErrDebug('Gutentags initialized')
enddef

# 健康检查
def g:GutentagsHealthCheck(): dict<any>
  var cache_dir_exists = isdirectory(config.cache_dir)

  return g:BuildManagedCommandModuleHealth('tags', 'Gutentags', config, 'GutentagsCheckModules', {
    cache_dir: config.cache_dir,
    cache_dir_exists: cache_dir_exists,
    autotag_enabled: config.ctags_autotag,
  })
enddef

# 获取配置
def g:GetGutentagsConfig(): dict<any>
  return config
enddef

# 立即初始化
call g:InitGutentags()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
