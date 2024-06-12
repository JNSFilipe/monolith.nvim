#include "monolith.h"
#include "config.h"

#define DEMACS_REPO "https://github.com/doomemacs/doomemacs"

int main() {
  char buffer[MAX_OUT_SZ];
  char buffer2[MAX_OUT_SZ];


#if defined(DEMACS) && defined(VEMACS)
  mono_log(MONO_ERROR, "DEMACS and VEMACS cannot be enabled at the same time, choose one emacs config");
  return -1;
#endif

  MONO_PRINT_SEP;

// Inform about enabled flags
#ifdef NEOVIM
  mono_log(MONO_WARNING, "NEOVIM ENABLED");
#endif
#ifdef DEMACS
  mono_log(MONO_WARNING, "DOOM EMACS (DEMACS) ENABLED");
#endif
#ifdef VEMACS
  mono_log(MONO_WARNING, "VANILLA EMACS (VEMACS) ENABLED");
#endif
#ifdef FORCE_INSTALL
  mono_log(MONO_WARNING, "FORCE DEPLOY ENABLED");
#endif
#ifdef SYNC
  mono_log(MONO_WARNING, "SYNCING ENABLED");
#endif

  MONO_PRINT_SEP;

// Deploy Neovim
#ifdef NEOVIM
  mono_log(MONO_INFO, "[NEOVIM] STARTING DEPLOYMENT");
  mono_log(MONO_INFO, "[NEOVIM] DELETING " NEOVIM_CONFIG_PATH);
  MONO_DEL(MONO_RESOLVE_ENV_VARS(NEOVIM_CONFIG_PATH));

#ifdef FORCE_INSTALL
  mono_log(MONO_INFO, "[NEOVIM] DELETING " NEOVIM_CONFIG_DATA);
  MONO_DEL(MONO_RESOLVE_ENV_VARS(NEOVIM_CONFIG_DATA));
  mono_log(MONO_INFO, "[NEOVIM] DELETING " NEOVIM_CONFIG_CACHE);
  MONO_DEL(MONO_RESOLVE_ENV_VARS(NEOVIM_CONFIG_CACHE));
#endif

  snprintf(buffer, sizeof(buffer), "%s/%s", MONO_PWD(), "neovim/");
  mono_log(MONO_INFO, "[NEOVIM] CREATE SYMLINK TO:");
  mono_log(MONO_INFO, buffer);
  MONO_SYMLINK(buffer, MONO_RESOLVE_ENV_VARS(NEOVIM_CONFIG_PATH));

#ifdef SYNC
  mono_log(MONO_INFO, "[NEOVIM] SYNCING PACKAGES");
  snprintf(buffer, sizeof(buffer), "nvim --headless \"+Lazy! sync\" +qa");
  mono_log(MONO_INFO, buffer);
  MONO_CMD(buffer);
#endif
#endif

  MONO_PRINT_SEP;

// Deploy Doom Emacs
#ifdef DEMACS
  mono_log(MONO_INFO, "[DEMACS] STARTING DEPLOYMENT");
  mono_log(MONO_INFO, "[DEMACS] DELETING " DEMACS_CONFIG_PATH);
  MONO_DEL(MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_PATH));
  MONO_DEL(MONO_RESOLVE_ENV_VARS(VEMACS_CONFIG_PATH));

#ifdef FORCE_INSTALL
  mono_log(MONO_INFO, "[DEMACS] DELETING " DEMACS_CONFIG_DATA);
  MONO_DEL(MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_DATA));
  mono_log(MONO_INFO, "[DEMACS] DELETING " DEMACS_CONFIG_CACHE);
  MONO_DEL(MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_CACHE));

  mono_log(MONO_INFO, "[DEMACS] CLONING DOOM EMACS REPO");
  snprintf(buffer, sizeof(buffer), "git clone --depth 1 %s %s", DEMACS_REPO,
           MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_DATA));
  mono_log(MONO_INFO, buffer);
  MONO_CMD(buffer);

  mono_log(MONO_INFO, "[DEMACS] INSTALLING DOOM");
  snprintf(buffer, sizeof(buffer), "%s/bin/doom install --force",
           MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_DATA)); // force for unattended
  mono_log(MONO_INFO, buffer);
  MONO_CMD(buffer);
#endif

  snprintf(buffer, sizeof(buffer), "%s/%s", MONO_PWD(), "demacs/");
  mono_log(MONO_INFO, "[DEMACS] CREATE SYMLINK TO:");
  mono_log(MONO_INFO, buffer);
  MONO_SYMLINK(buffer, MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_PATH));

#ifdef SYNC
  mono_log(MONO_INFO, "[DEMACS] PURGING OLD PACKAGES");
  snprintf(buffer, sizeof(buffer), "%s/bin/doom purge",
           MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_DATA));
  mono_log(MONO_INFO, buffer);
  MONO_CMD(buffer);

  mono_log(MONO_INFO, "[DEMACS] SYNCING PACKAGES");
  snprintf(buffer, sizeof(buffer), "%s/bin/doom sync",
           MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_DATA));
  mono_log(MONO_INFO, buffer);
  MONO_CMD(buffer);
#endif
#endif

  MONO_PRINT_SEP;

// Deploy Vanilla Emacs
#ifdef VEMACS
  mono_log(MONO_INFO, "[VEMACS] STARTING DEPLOYMENT");
  mono_log(MONO_INFO, "[VEMACS] DELETING (mv /tmp) " DEMACS_CONFIG_PATH);
  MONO_DEL(MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_PATH));
  MONO_DEL(MONO_RESOLVE_ENV_VARS(VEMACS_CONFIG_PATH));

#ifdef FORCE_INSTALL
  mono_log(MONO_INFO, "[VEMACS] DELETING " DEMACS_CONFIG_DATA);
  MONO_DEL(MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_DATA));
  mono_log(MONO_INFO, "[VEMACS] DELETING " DEMACS_CONFIG_CACHE);
  MONO_DEL(MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_CACHE));
#endif

  mono_log(MONO_INFO, "[VEMACS] CREATE CONFIG FOLDER:");
  mono_log(MONO_INFO, MONO_RESOLVE_ENV_VARS(VEMACS_CONFIG_PATH));
  MONO_MKDIR(MONO_RESOLVE_ENV_VARS(VEMACS_CONFIG_PATH));
  snprintf(buffer, sizeof(buffer), "%s/%s", MONO_PWD(), "vemacs/init.el");
  snprintf(buffer2, sizeof(buffer), "%s/%s", MONO_RESOLVE_ENV_VARS(VEMACS_CONFIG_PATH), "/init.el");
  mono_log(MONO_INFO, "[VEMACS] CREATE SYMLINK TO:");
  mono_log(MONO_INFO, buffer);
  int err = MONO_SYMLINK(buffer, buffer2);
  if(err != 0) mono_log(MONO_ERROR, "Failed to create symlink");

#endif

  MONO_PRINT_SEP;

  return 0;
}
