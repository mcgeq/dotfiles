vim9script
# ============================================================================
# 组件: Local / ModuleOverrides
# 作者: mcge <mcgeq@outlook.com>
# 说明: 统一的模块配置覆盖入口示例；在模块加载前生效。
# ============================================================================

# 每个 key 都对应 modules/* 里的模块 ID，也就是 MarkModuleLoaded() 使用的值。
# 这里适合做“配置层覆盖”，不要在这里重新写插件初始化逻辑。

g:SetModuleOverrides({
  appearance: {
    cmdheight: 1,
  },
  clap: {
    layout: {
      width: '90%',
      height: '60%',
    },
  },
  colorscheme: {
    scheme: 'molokai',
  },
})

# 也可以逐个追加：
# g:SetModuleOverride('coc', {enabled: false})

# vim: set ft=vim sw=2 ts=2 sts=2 et:
