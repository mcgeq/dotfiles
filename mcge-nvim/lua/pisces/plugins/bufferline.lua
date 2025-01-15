local bufferline = pRequire("bufferline")
local cfg = require("pisces").config.bufferLine

if bufferline and cfg and cfg.enable then
	bufferline.setup({
		options = {
			close_command = "Bdelete! %d",
			right_mouse_command = "Bdelete! %d",
			buffer_close_icon = "󰅖",
			modified_icon = "● ",
			close_icon = " ",
			left_trunc_marker = " ",
			right_trunc_marker = " ",
			offsets = {
				{
					filetype = "NvimTree",
					text = "File Explorer",
					highlight = "Directory",
					text_align = "left",
				},
			},
			---@diagnostic disable-next-line: assign-type-mismatch
			diagnostics = "nvim_lsp",
			---@diagnostic disable-next-line: unused-local
			diagnostics_indicator = function(count, level, diagnostics_dict, context)
				local s = " "
				for e, n in pairs(diagnostics_dict) do
					local sym = e == "error" and " " or (e == "warning" and " " or "")
					s = s .. n .. sym
				end
				return s
			end,
			get_element_icon = function(element)
				-- element consists of {filetype: string, path: string, extension: string, directory: string}
				-- This can be used to change how bufferline fetches the icon
				-- for an element e.g. a buffer or a tab.
				-- e.g.
				local icon, hl =
					require("nvim-web-devicons").get_icon_by_filetype(element.filetype, { default = false })
				return icon, hl
				-- or
				-- local custom_map = {my_thing_ft: {icon = "my_thing_icon", hl}}
				-- return custom_map[element.filetype]
			end,
			show_buffer_icons = true,
			show_buffer_close_icons = true,
			show_close_icon = true,
			show_tab_indicators = true,
			show_duplicate_prefix = true,
		},
	})

	keymap("n", cfg.keys.prev, ":BufferLineCyclePrev<CR>")
	keymap("n", cfg.keys.next, ":BufferLineCycleNext<CR>")
	-- "moll/vim-bbye"
	keymap("n", cfg.keys.close, ":Bdelete!<CR>")
	keymap("n", cfg.keys.close_left, ":BufferLineCloseLeft<CR>")
	keymap("n", cfg.keys.close_right, ":BufferLineCloseRight<CR>")
	keymap("n", cfg.keys.close_others, ":BufferLineCloseRight<CR>:BufferLineCloseLeft<CR>")
	keymap("n", cfg.keys.close_pick, ":BufferLinePickClose<CR>")

	-- FIXME: reset colorscheme to make bufferline background color correct
	require("pisces.colorscheme").reset()
end
