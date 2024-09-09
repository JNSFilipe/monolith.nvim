#ifndef CONFIG_H_
#define CONFIG_H_

#ifdef _WIN32
// TODO: Define config paths for windows
#else
// NEOVIM
#define NEOVIM_CONFIG_PATH "$XDG_CONFIG_HOME/nvim"
#define NEOVIM_CONFIG_DATA "$XDG_DATA_HOME/.local/share/nvim"
#define NEOVIM_CONFIG_CACHE "$XDG_CACHE_HOME/nvim"
// DOOM EMCAS
#define DEMACS_CONFIG_PATH "$XDG_CONFIG_HOME/doom"
#define DEMACS_CONFIG_DATA "$XDG_CONFIG_HOME/emacs"
#define DEMACS_CONFIG_CACHE "$HOME/.emacs_saves"
#define DEMACS_REPO "https://github.com/doomemacs/doomemacs"
// Vanilla EMACS
#define VEMACS_CONFIG_PATH "$HOME/.vemacs.d"
#endif

// EDITORS
// #define NEOVIM
// #define DEMACS
#define VEMACS

// OPTIONS
#define FORCE_INSTALL
#define SYNC

#endif // CONFIG_H_
