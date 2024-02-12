#ifndef CONFIG_H_
#define CONFIG_H_

#ifdef _WIN32
// TODO: Define config path for windows
#else
#define NEOVIM_CONFIG_PATH "$XDG_CONFIG_HOME/nvim"
#define NEOVIM_CONFIG_DATA "$XDG_DATA_HOME/.local/share/nvim"
#define NEOVIM_CONFIG_CACHE "$XDG_CACHE_HOME/nvim"
#endif

// #define FORCE_INSTALL

#endif // CONFIG_H_
