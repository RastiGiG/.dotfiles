--[[

=====================================================================
=========================== COLORSCHEME =============================
=====================================================================

--]]

local sel_colorscheme = "dracula"

local status_ok, _ = pcall(vim.cmd.colorscheme, sel_colorscheme)
if not status_ok then
   vim.notify("colorscheme " .. sel_colorscheme .. " not found!")
   return
end
