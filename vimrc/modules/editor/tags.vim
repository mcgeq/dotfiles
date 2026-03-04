vim9script
# ============================================================================
# Editor 模块 - Gutentags 自动生成 tags
# 作者：mcge <mcgeq@outlook.com>
# ============================================================================

# 防止重复加载
if exists('g:mcge_tags_loaded')
  finish
endif
g:mcge_tags_loaded = true

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
  # 合并用户配置
  extend(config, user_config)

  if !config.enabled
    call g:ErrDebug('Gutentags is disabled')
    return
  endif

  # 设置缓存目录
  g:gutentags_cache_dir = config.cache_dir

  # 项目根目录标记
  g:gutentags_project_root = config.project_root

  # 生成行为
  g:gutentags_generate_on_new = config.generate_on_new
  g:gutentags_generate_on_missing = config.generate_on_missing
  g:gutentags_generate_on_save = config.generate_on_save
  g:gutentags_generate_on_empty_buffer = config.generate_on_empty_buffer

  # 排除配置
  g:gutentags_ctags_exclude = config.ctags_exclude

  # 自动 tag
  g:gutentags_ctags_autotag = config.ctags_autotag

  call g:ErrDebug('Gutentags initialized')
enddef

# 健康检查
def g:GutentagsHealthCheck(): dict<any>
  var cache_dir_exists = isdirectory(config.cache_dir)

  return {
    name: 'Gutentags',
    available: exists(':GutentagsCheckModules'),
    enabled: config.enabled,
    cache_dir: config.cache_dir,
    cache_dir_exists: cache_dir_exists,
    autotag_enabled: config.ctags_autotag,
    status: config.enabled ? 'running' : 'disabled',
  }
enddef

# 获取配置
def g:GetGutentagsConfig(): dict<any>
  return config
enddef

# 立即初始化
call g:InitGutentags()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
