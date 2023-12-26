#!/bin/bash

rm -rf ~/.config/nvim
rm -rf ~/.local/share/nvim
rm -rf ~/.cache/nvim

ln -sf $PWD $HOME/.config/nvim
