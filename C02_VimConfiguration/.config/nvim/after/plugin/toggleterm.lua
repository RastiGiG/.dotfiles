--[[

=====================================================================
======================== TOGGLETERM CONFIG ==========================
=====================================================================

--]]

-- [[ Configure toggleterm ]]

local status_ok, toggleterm = pcall(require, "toggleterm")
if not status_ok then
   return
end

toggleterm.setup({
  -- size can be a number or function which is passed the current terminal
  -- size = 20
  function(term)
    if term.direction == "horizontal" then
      return 15
    elseif term.direction == "vertical" then
      return vim.o.columns * 0.4
    elseif term.direction == "float" then
      return 20
    end
  end,
  open_mapping = [[<c-\>]],
  hide_numbers = true, -- hide the number column in toggleterm buffers
  shade_filetypes = {},
  shade_terminals = true, -- NOTE: this option takes priority over highlights specified so if you specify Normal highlights you should set this to false
  shading_factor = '2', -- the percentage by which to lighten terminal background, default: -30 (gets multiplied by -3 if background is light)
  insert_mappings = true, -- whether or not the open mapping applies in insert mode
  persist_size = true,
  direction = 'float', -- | 'horizontal' | 'tab' | 'vertical',
  close_on_exit = true, -- close the terminal window when the process exits
  start_in_insert = true,
  terminal_mappings = true, -- whether or not the open mapping applies in the opened terminals
  persist_mode = true, -- if set to true (default) the previous terminal mode will be remembered
  auto_scroll = true, -- automatically scroll to the bottom on terminal output
  autochdir = false, -- when neovim changes it current directory the terminal will change it's own when next it's opened


  -- on_create = fun(t: Terminal), -- function to run when the terminal is first created
  -- on_open = fun(t: Terminal), -- function to run when the terminal opens
  -- on_close = fun(t: Terminal), -- function to run when the terminal closes
  -- on_stdout = fun(t: Terminal, job: number, data: string[], name: string) -- callback for processing output on stdout
  -- on_stderr = fun(t: Terminal, job: number, data: string[], name: string) -- callback for processing output on stderr
  -- on_exit = fun(t: Terminal, job: number, exit_code: number, name: string) -- function to run when terminal process exits

  -- Change the default shell. Can be a string or a function returning a string
  shell = vim.o.shell,

  highlights = {
    -- highlights which map to a highlight group name and a table of it's values
    -- NOTE: this is only a subset of values, any group placed here will be set for the terminal window split
    -- Normal = {
    --   guibg = "<VALUE-HERE>",
    -- },
    -- NormalFloat = {
    --   link = 'Normal'
    -- },
    -- FloatBorder = {
    --   guifg = "<VALUE-HERE>",
    --   guibg = "<VALUE-HERE>",
    -- },
  },

  -- This field is only relevant if direction is set to 'float'
  float_opts = {

    -- The border key is *almost* the same as 'nvim_open_win'
    -- see :h nvim_open_win for details on borders however
    -- the 'curved' border is a custom border type
    -- not natively supported but implemented in this plugin.
    border = 'curved', -- | 'single' | 'double' | 'shadow' | ... other options supported by win open

    -- like `size`, width and height can be a number or function which is passed the current terminal
    winblend = 0,

    -- width = <value>,
    -- height = <value>,
    -- zindex = <value>,

    highlights = {
       border = "Normal",
       background = "Normal",
    },
  },

  winbar = {
    enabled = false,
    name_formatter = function(term) --  term: Terminal
      return term.name
    end
  },
})

function _G.set_terminal_keymaps()
   local opts = {noremap = true}
   vim.api.nvim_buf_set_keymap(0, 't', '<esc>', [[<C-\><C-n>]], opts)
   vim.api.nvim_buf_set_keymap(0, 't', 'jk', [[<C-\><C-n>]], opts)
   vim.api.nvim_buf_set_keymap(0, 't', '<C-h>', [[<C-\><C-n><C-W>h]], opts)
   vim.api.nvim_buf_set_keymap(0, 't', '<C-h>', [[<C-\><C-n><C-W>j]], opts)
   vim.api.nvim_buf_set_keymap(0, 't', '<C-h>', [[<C-\><C-n><C-W>k]], opts)
   vim.api.nvim_buf_set_keymap(0, 't', '<C-h>', [[<C-\><C-n><C-W>l]], opts)
end

vim.cmd('autocmd! TermOpen term://* lua set_terminal_keymaps()')

local Terminal = require("toggleterm.terminal").Terminal
local lazygit = Terminal:new({ cmd = "lazygit", hidden = true })

function _LAZYGIT_TOGGLE()
      lazygit:toggle()
end

local node = Terminal:new({ cmd = "node", hidden = true })

function _NODE_TOGGLE()
      node:toggle()
end

local ncdu = Terminal:new({ cmd = "ncdu", hidden = true })

function _NCDU_TOGGLE()
      ncdu:toggle()
end

local htop = Terminal:new({ cmd = "htop", hidden = true })

function _HTOP_TOGGLE()
      htop:toggle()
end

local python = Terminal:new({ cmd = "python", hidden = true })

function _PYTHON_TOGGLE()
      python:toggle()
end
