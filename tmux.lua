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
          -- Switch focus to selected terminal in tmux
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

function M.create_or_move_tmux_pane(opts)
  -- <++> Prompt for pane name if not provided
  -- TODO: Use telescope to select pane name and list existing panes
  -- TODO: Pass a command to run in the new pane
  if not opts.pane_name then
    vim.ui.input({ prompt = "Enter pane name: " }, function(input)
      opts.pane_name = input
      print(opts.pane_name)
      M.create_or_move_tmux_pane(opts)
    end)
    return
  end

  if not opts.split_direction then
    opts.split_direction = "h"
  end

  local pane_size = 30
  if opts.split_direction == "h" then
    pane_size = 30
  elseif opts.split_direction == "v" then
    pane_size = 30
  else
    print("Invalid split direction")
    return
  end

  if opts.pane_size then
    pane_size = opts.pane_size
  end

  -- Check if a pane with the given name already exists
  local handle = io.popen("tmux list-panes -s -F '#{pane_id} #{pane_title}'")
  local result = handle:read("*a")
  handle:close()

  for id, title in string.gmatch(result, "([%%#]%d+) ([^\n]+)") do
    print("ID " .. id)
    print("Title " .. title)
    if title == opts.pane_name then
      -- Move existing pane
      vim.fn.system("tmux join-pane -" .. opts.split_direction .. "b -t " .. id .. " -p " .. (100 - pane_size) .. " -d")
      return
    end
  end

  -- Create a new pane with the specified name
  -- vim.fn.system("tmux split-window -h -p 30 -d \\; select-pane -T '" .. pane_name .. "'") -- -d to prevent focus switch
  vim.fn.system("tmux split-window -" ..
    opts.split_direction .. " -p " .. pane_size .. " \\; select-pane -T '" .. opts.pane_name .. "'")
end

return M
