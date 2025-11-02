-- This will run last in the setup process.
-- This is just pure lua so anything that doesn't
-- fit in the normal config locations above can go here

-- 自动更新时间戳功能
local auto_update_timestamp = require("config.auto_update_timestamp")
auto_update_timestamp.setup()