--[[

=====================================================================
============================== OPTIONS ==============================
=====================================================================

--]]

-- [[ Setting options ]]
-- See `:help vim.o`
local options = {
  hlsearch = false,                         -- highlight all matches on previous search pattern
  incsearch = true,                         -- Set incremental search

  mouse = 'a',                              -- Enable mouse mode

  -- Indenting and Tabs
  tabstop = 4,                              -- insert 2 spaces for a tab
  softtabstop = 4,
  shiftwidth = 4,                           -- the number of spaces inserted for each indentation
  expandtab = true,                         -- convert tabs to spaces

  smartindent = true,                       -- make indenting smarter again
  breakindent = true,                       -- Enable break indent
  wrap = true,                              -- display lines as one long line
  whichwrap = "bs<>[]hl",                   -- which "horizontal" keys are allowed to travel to prev/next line
  linebreak = true,                         -- companion to wrap, don't split words

  -- Sync clipboard between OS and Neovim.
  --  Remove this option if you want your OS clipboard to remain independent.
  --  See `:help 'clipboard'`
  clipboard = 'unnamedplus',

  guifont = "monospace:h17",                -- the font used in graphical neovim applications

  swapfile = false,                         -- Save undo history
  writebackup = false,                      -- if a file is being edited by another program (or was written to file while editing with another program), it is not allowed to be edited
  backupdir = os.getenv("HOME") .. "/.dotfiles/C02_VimConfiguration/backup",
  backup = false,
  undodir = os.getenv("HOME") .. "/.dotfiles/C02_VimConfiguration/undodir",
  undofile = true,

  ignorecase = true,                        -- Case-insensitive searching UNLESS \C or capital in search
  smartcase = true,

  scrolloff = 8,                            -- Stay focused on cursor, never go within 8 lines of borders
  sidescrolloff = 8,                        -- minimal number of screen columns either side of cursor if wrap is `false`

  colorcolumn = "90",                       -- Set colorcolumn - 90 chars    
  showtabline = 2,                          -- always show tabs
  cursorline = true,                        -- highlight the current line

  timeoutlen = 300,                         -- time to wait for a mapped sequence to complete (in milliseconds)
  updatetime = 300,                         -- faster completion (4000ms default)

  completeopt = { "menuone", "noselect" },  -- Set completeopt to have a better completion experience

  cmdheight = 2,                            -- more space in commandline for messaes

  conceallevel = 0,                         -- `` visible in makrdown

  fileencoding = 'utf-8',                   -- more space in commandline for messaes

  termguicolors = true,                     -- NOTE: You should make sure your terminal supports this, most do

  pumheight = 10,                           -- pop up menu height
  -- showmode = false,                         -- we don't need to see things like -- INSERT -- anymore

  splitbelow = true,                        -- force all horizontal splits to go below current window
  splitright = true,                        -- force all vertical splits to go to the right of current window
}

for k, v in pairs(options) do
  vim.opt[k] = v
end

vim.wo.number = true                                   -- Make line numbers default
vim.wo.numberwidth = 4                                 -- set number column width to 2 {default 4}

-- Make relative line numbers default
vim.wo.relativenumber = true

-- vim.opt.shortmess = "ilmnrx"                        -- flags to shorten vim messages, see :help 'shortmess'
vim.opt.shortmess:append "c"                           -- don't give |ins-completion-menu| messages
vim.opt.iskeyword:append "-"                           -- hyphenated words recognized by searches
vim.opt.formatoptions:remove({ "c", "r", "o" })        -- don't insert the current comment leader automatically for auto-wrapping comments using 'textwidth', hitting <Enter> in insert mode, or hitting 'o' or 'O' in normal mode.
vim.opt.runtimepath:remove("/usr/share/vim/vimfiles")  -- separate vim plugins from neovim in case vim still in use

-- Keep signcolumn on by default
vim.wo.signcolumn = 'yes'
