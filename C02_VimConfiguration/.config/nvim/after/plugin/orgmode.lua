--[[

=====================================================================
====================== GITSIGNS CONFIGURATION =======================
=====================================================================

--]]

require('orgmode').setup_ts_grammar()

-- Setup orgmode
require('orgmode').setup({
  org_agenda_files = {
     '~/Org/journal',
     '~/Org/personal-(tasks|mail|chores|contracts)-?[A-Za-z]*.org'
  },
  org_default_notes_file = '~/Projects/Notes/notes.org',
})
