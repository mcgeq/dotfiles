---@class UserConfig
---@field colorscheme? "tokyonight" | "monkai" | "nord" | "onedark" | "gruvbox" | "nightfox" | "nordfox" | "duskfox" | "dracula" builtin colorscheme
---@field max_highlight_line_count? number disable code hightlight on big file for performance default 10000
---@field enable_imselect? boolean auto switch your input method, default false  ---@see https://github.com/daipeihust/im-select
---@field enable_very_magic_search? boolean enable regexp very magic mode ---@see https://www.youtube.com/watch?v=VjOcINs6QWs
---@field fix_windows_clipboard? boolean fix yank problem on windows WSL2 ---@see  https://stackoverflow.com/questions/44480829/how-to-copy-to-clipboard-in-vim-of-bash-on-windows
---@field keys? Commonkeys common keymappings
---@field plugins? PluginConfig

--- 插件配置
---@class PluginConfig
---@field s_windows? SWindowConfig enabled by default
---@field s_tab? STabConfig disabled by default
---@field cmp? CMPConfig Completion user config
---@field notify? NotifyConfig nvim-notify plugin user config
---@field nvimTree? NvimTreeConfig nvim-tree plugin user config
---@field bufferLine? BufferLineConfig bufferline.nvim plugin user config
---@field telescope? TelescopeConfig telescope.nvim plugin user config
---@field surround? SurroundConfig nvim-surround plugin user config
---@field venn? VENNConfig venn.nvim plugin user config
---@field zen? ZenConfig zen-mode.nvim plugin user config
---@field comment? CommentConfig Comment.nvim plugin user config
---@field toggleterm? ToggleTermConfig toggleterm.nvim plugin user config
---@field copilot_chat? CopilotChatConfig
---@field neotest? NeotestConfig neotest plugin user config

--- 开发环境配置
---@class DevConfig
---@field lsp? LSPConfig LSP common config
---@field dap? DAPConfig DAP common config
---@field frontend? FrontendConfig Frontend development user config
---@field clangd? ClangdConfig Clangd user config
---@field golang? GolangConfig Golang development user config
---@field lua? LuaConfig Lua development user config
---@field rust? RustConfig Rust development user config
---@field bash? BashConfig sh development user config
---@field python? PythonConfig python development user config
---@field ruby? RubyConfig ruby development user config
---@field json? JsonConfig Json user config
---@field markdown? MarkdownConfig
---@field toml? TomlConfig Toml user config
---@field yaml? YamlConfig Yaml user config
---@field docker? DockerConfig Docker user config
---@field solidity? SolidityConfig
---@field java? JavaConfig
---@field git? GitConfig git user config
---@field mirror? MirrorConfig mirror config

--- 快捷键
---@class Commonkeys
---@field leader_key string mapleader prefix
---@field n_save? string save keymap
---@field n_force_quit? string
---@field terminal_to_normal? string
---@field normal_mode? table<string, string>
---@field insert_mode? table<string, string>
---@field terminal_mode? table<string, string>
---@field visual_mode? table<string, string>

--- windows操作
---@class SWindowConfig
---@field enable boolean
---@field keys? SWindowConfigKeys

---@class SWindowConfigKeys
---@field w_close? string
---@field w_jump_left? string
---@field w_jump_right? string
---@field w_jump_up? string
---@field w_jump_dowm? string
---@field w_split_vertically? string
---@field w_split_horizontally? string
---@field w_width_decrease? string
---@field w_width_increase? string
---@field w_height_decrease? string
---@field w_height_increase? string
---@field w_size_equal? string

--- Tab操作
---@class STabConfig
---@field enable boolean
---@field keys? STabConfigKeys

---@class STabConfigKeys
---@field t_close? string
---@field t_first? string
---@field t_last? string
---@field t_next? string
---@field t_prev? string
---@field t_split? string

--- AI Completion
---@class CMPConfig
---@field enable boolean
---@field copilot? boolean
---@field codeium? boolean
---@field keys? CMPConfigKeys

---@class CMPConfigKeys
---@field c_abort? string
---@field c_complete? string
---@field c_confirm? string
---@field c_select_next_item? string
---@field c_select_prev_item? string
---@field c_scroll_doc_up? string
---@field c_scroll_doc_down? string
---@field c_snip_jump_next? string luasnip
---@field c_snip_jump_prev? string luasnip
---@field c_snip_choice_next? string luasnip
---@field c_snip_choice_prev? string luasnip

--- Notify Config
---@class NotifyConfig
---@field enable string
---@field timeout? number
---@field stages? 'fade' | 'static' | 'slide'
---@field render? 'default' | 'minimal' | 'simple' | 'compact' | 'wrapped-compact'

--- Nvim Tree
---@class NvimTreeConfig
---@field enable boolean
---@field keys? NvimTreeConfigKeys

---@class NvimTreeConfigKeys
---@field n_close? string
---@field n_cd? string
---@field n_cut? string
---@field n_copy? string
---@field n_copy_name? string
---@field n_copy_path? string
---@field n_copy_absolute? string
---@field n_create? string
---@field n_dir_up? string
---@field n_edit? string
---@field n_first_sibling? string
---@field n_last_sibling? string
---@field n_paste? string
---@field n_parent_node? string
---@field n_refresh? string
---@field n_remove? string
---@field n_rename? string
---@field n_split? string
---@field n_system_open? string
---@field n_tabnew? string
---@field n_toggle? string
---@field n_toggle_git_ignored? string
---@field n_toggle_dotfiles? string
---@field n_toggle_custom? string
---@field n_toggle_file_info? string
---@field n_vsplit? string

--- Telescope Config
---@class TelescopeConfig
---@field enable boolean
---@field keys? TelescopeConfigKeys

---@class TelescopeConfigKeys
---@field t_close? string
---@field t_cycle_history_next? string history
---@field t_cycle_history_prev? string history
---@field t_find_files? string Find files
---@field t_find_buffers? string Find buffers
---@field t_live_grep? string
---@field t_live_grep_args? string
---@field t_move_selection_next? string
---@field t_move_selection_prev? string
---@field t_preview_scrolling_up? string
---@field t_preview_scrolling_down? string
---@field t_recent_files? string
---@field t_telescope_help_tags? string
