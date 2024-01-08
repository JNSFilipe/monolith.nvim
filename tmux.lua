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
return M
