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

--- 开发环境基础配置
---@class DevConfig
---@field enable boolean
---@field dev_lsp? string
---@field dev_formatter? string
---@field dev_linter? string
---@field dev_format_on_save? boolean
---@field dev_indent? number

---@class FrontendConfig: DevConfig Frontend development user config
---@field dev_f_cspell? boolean
---@field dev_f_tailwindcss? boolean
---@field dev_f_unocss? boolean
---@field dev_f_prisma? boolean
---@field dev_f_vue? boolean


---@class MarkdownConfig: DevConfig
---@field wrap? boolean
---@field theme? string
---@field keys? MarkdownConfigKeys

---@class MarkdownConfigKeys
---@field m_next_link? string
---@field m_prev_link? string
---@field m_next_heading? string
---@field m_prev_heading? string
---@field m_go_back? string
---@field m_follow_link? string
---@field m_toggle_item? string


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

--- nvim-surround
---@class SurroundConfig
---@field enable boolean
---@field keys? SurroundConfigKeys

---@class SurroundConfigKeys
---@field s_normal? string
---@field s_normal_cur? string
---@field s_delete? string
---@field s_change? string
---@field s_visual? string
---@field s_visual_line? string
---@field s_insert? boolean
---@field s_insert_line? boolean
---@field s_normal_line? boolean
---@field s_normal_cur_line? boolean

--- venn
---@class VENNConfig
---@field enable boolean
---@field keys? VENNConfigKeys

---@class VENNConfigKeys
---@field v_toggle? string
---@field v_up? string
---@field v_down? string
---@field v_left? string
---@field v_right? string
---@field v_draw_box? string

--- zen-mode
---@class ZenConfig
---@field enable boolean
---@field keys? ZenConfigKeys

---@class ZenConfigKeys
---@field z_toogle? string

--- comment
---@class CommentConfig
---@field enable boolean
---@field keys? CommentConfigKeys

---@class CommentConfigKeys
---@field c_n_toggle_line? string
---@field c_n_toggle_gbc? string
---@field c_v_toggle_line? string
---@field c_v_toggle_block? string

--- toggleterm
---@class ToggleTermConfig
---@field enable boolean
---@field keys? ToggleTermConfigKeys

---@class ToggleTermConfigKeys
---@field t_toggle_float_window? string
---@field t_toggle_float_window_command? string
---@field t_toggle_side_window? string
---@field t_toggle_side_window_command? string
---@field t_toggle_bottom_window? string
---@field t_toggle_bottom_window_command? string

--- copilot
---@class CopilotChatConfig
---@field enable boolean
---@field keys? CopilotChatConfigKeys

---@class CopilotChatConfigKeys
---@field c_quick_chat? string
---@field c_prompt_actions? string
---@field c_help_actions? string

--- LSP
---@class LSPConfig
---@field enable boolean
---@field keys? LSPConfigKeys

---@class LSPConfigKeys
---@field l_definition? string
---@field l_implementation? string
---@field l_references? string
---@field l_hover? string
---@field l_call_incoming? string
---@field l_call_outgoing? string
---@field l_rename? string
---@field l_code_action? string
---@field l_format? string
---@field l_diagnostics_open_float? string
---@field l_diagnostics_goto_next? string
---@field l_diagnostics_goto_prev? string

--- DAP
---@class DAPConfig
---@field enable boolean
---@field keys? DAPConfigKeys

---@class DAPConfigKeys
---@field d_continue? string
---@field d_terminate? string
---@field d_step_over? string
---@field d_step_into? string
---@field d_step_out? string
---@field d_toggle_breakpoint? string
---@field d_clear_breakpoint? string
---@field d_eval? string

--- neotest
---@class NeotestConfig
---@field enable boolean
---@field keys? NeotestConfigKeys

---@class NeotestConfigKeys
---@field t_toggle? string
---@field t_run? string
---@field t_run_file? string
---@field t_run_dap? string
---@field t_run_stop? string
---@field t_output_open? string

--- gitsigns
---@class GitConfig
---@field enable boolean
---@field code_actions? string
---@field signcolumn? boolean
---@field numhl? boolean
---@field linehl? boolean
---@field word_diff? boolean
---@field current_line_blame? boolean

