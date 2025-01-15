local telescope = pRequire("telescope")
local cfg = require("pisces").config.telescope
local lga_actions = pRequire("telescope-live-grep-args.actions")
local telescope_builtin = require("telescope.builtin")

if telescope and lga_actions and cfg and cfg.enable then
	-- local actions = require("telescope.actions")
	telescope.setup({
		defaults = {
			initial_mode = "insert",
			-- vertical , center , cursor
			layout_strategy = "horizontal",
			file_ignore_patterns = { "node_modules", ".git" },
			mappings = {
				i = {
					-- move up and down
					[cfg.keys.move_selection_next] = "move_selection_next",
					[cfg.keys.move_selection_previous] = "move_selection_previous",
					-- history
					[cfg.keys.cycle_history_next] = "cycle_history_next",
					[cfg.keys.cycle_history_prev] = "cycle_history_prev",
					-- close window
					-- ["<esc>"] = actions.close,
					[cfg.keys.close] = "close",
					[cfg.keys.preview_scrolling_up] = "preview_scrolling_up",
					[cfg.keys.preview_scrolling_down] = "preview_scrolling_down",
				},
			},
		},
		pickers = {
			find_files = {
				-- theme = "dropdown", -- can be : dropdown, cursor, ivy
			},
		},
		extensions = {
			["ui-select"] = {
				require("telescope.themes").get_dropdown({
					-- even more opts
					initial_mode = "normal",
				}),
			},
			live_grep_args = {
				auto_quoting = false, -- enable/disable auto-quoting
				-- mappings = { -- extend mappings
				--   i = {
				--     ["<C-k>"] = lga_actions.quote_prompt(),
				--   },
				-- },
			},
		},
	})

	keymap("n", cfg.keys.find_files, telescope_builtin.find_files, { desc = "Telescope find file" })
	keymap("n", cfg.keys.live_grep, telescope_builtin.live_grep, { desc = "Telescope live grep" })
	keymap("n", cfg.keys.live_grep_args, telescope_builtin.live_grep_args, { desc = "Telescope live grep args" })
	keymap("n", cfg.keys.recent_file, telescope_builtin.oldfiles, { desc = "Telescope recent file" })
	keymap("n", cfg.keys.find_buffers, telescope_builtin.buffers, { desc = "Telescope buffers" })
	keymap("n", cfg.keys.find_commands, telescope_builtin.commands, { desc = "Telescope commands" })
	keymap("n", cfg.keys.telescope_help_tags, telescope_builtin.help_tags, { desc = "Telescope help tags" })
	-- keymap("n", cfg.keys.find_files, ":<CMD<Telescope find_files<CR>")
	-- keymap("n", cfg.keys.live_grep, ":<CMD>Telescope live_grep<CR>")
	-- keymap("n", cfg.keys.live_grep_args, ":lua require('telescope').extensions.live_grep_args.live_grep_args()<CR>")
	-- keymap("n", cfg.keys.recent_file, "<CMD>Telescope oldfiles<CR>")

	pcall(telescope.load_extension, "env")
	-- To get ui-select loaded and working with telescope, you need to call
	-- load_extension, somewhere after setup function:
	pcall(telescope.load_extension, "ui-select")
	pcall(telescope.load_extension, "live_grep_args")
end
