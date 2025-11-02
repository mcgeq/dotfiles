---Tab bar appearance configuration
local constants = require("config.constants")

return {
  -- Tab bar settings
  enable_tab_bar = constants.TAB_BAR.ENABLED,
  -- tab_bar_at_bottom = constants.TAB_BAR.AT_BOTTOM,
  -- hide_tab_bar_if_only_one_tab = constants.TAB_BAR.HIDE_IF_ONLY_ONE,
  use_fancy_tab_bar = constants.TAB_BAR.USE_FANCY,
  tab_max_width = constants.TAB_BAR.MAX_WIDTH,
  show_tab_index_in_tab_bar = constants.TAB_BAR.SHOW_INDEX,
  switch_to_last_active_tab_when_closing_tab = constants.TAB_BAR.SWITCH_TO_LAST_ACTIVE,
}

