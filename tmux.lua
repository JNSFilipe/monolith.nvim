-- Inspirations:
-- https://github.com/alexghergh/nvim-tmux-navigation

local M = {}

function M.move_left()
  if vim.fn.winnr('h') ~= vim.fn.winnr() then
    vim.cmd('wincmd h')
  else
    vim.cmd('silent !tmux select-pane -L')
  end
end

function M.move_down()
  if vim.fn.winnr('j') ~= vim.fn.winnr() then
    vim.cmd('wincmd j')
  else
    vim.cmd('silent !tmux select-pane -D')
  end
end

function M.move_up()
  if vim.fn.winnr('k') ~= vim.fn.winnr() then
    vim.cmd('wincmd k')
  else
    vim.cmd('silent !tmux select-pane -U')
  end
end

function M.move_right()
  if vim.fn.winnr('l') ~= vim.fn.winnr() then
    vim.cmd('wincmd l')
  else
    vim.cmd('silent !tmux select-pane -R')
  end
end

function M.list_and_select_tmux_terminals()
  local actions = require('telescope.actions')
  local state = require('telescope.actions.state')
  local finders = require('telescope.finders')
  local pickers = require('telescope.pickers')
  local conf = require('telescope.config').values

  -- Fetch list of all panes in the current tmux session
  local handle = io.popen("tmux list-panes -s -F '#{pane_id} #{pane_title}'")
  local result = handle:read("*a")
  handle:close()

  local terminals = {}
  for pane_id, pane_title in string.gmatch(result, "([%%#]%d+) ([^\n]+)") do
    table.insert(terminals, pane_title .. " (" .. pane_id .. ")")
  end

  -- Create a Telescope picker
  pickers.new({}, {
    prompt_title = 'Tmux Terminals',
    finder = finders.new_table({
      results = terminals,
      entry_maker = function(entry)
        return {
          value = entry,
          display = entry,
          ordinal = entry,
        }
      end,
    }),
    sorter = conf.generic_sorter({}),
    attach_mappings = function(prompt_bufnr, map)
      actions.select_default:replace(function()
        actions.close(prompt_bufnr)
        local selection = state.get_selected_entry()
        local pane_id = string.match(selection.value, "([%%#]%d+)")
        print("selection " .. selection.value)
        print("ID " .. pane_id)
        if pane_id then
          -- <++> Switch focus to selected terminal in tmux
          -- Get the window ID for the given pane ID
          local window_id = vim.fn.system("tmux display-message -p -t " .. pane_id .. " '#{window_id}'"):gsub("\n", "")

          -- Switch to the window
          vim.fn.system("tmux select-window -t " .. window_id)

          -- Switch to the pane
          vim.fn.system("tmux select-pane -t " .. pane_id)
        end
      end)
      return true
    end,
  }):find()
end

return M
