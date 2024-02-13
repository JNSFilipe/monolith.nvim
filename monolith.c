#include "monolith.h"
#include "config.h"

#define DEMACS_REPO "https://github.com/doomemacs/doomemacs"

int main() {
  char buffer[MAX_OUT_SZ];

  MONO_PRINT_SEP;

// Inform about enabled flags
#ifdef NEOVIM
  mono_log(MONO_WARNING, "NEOVIM ENABLED");
#endif
#ifdef DEMACS
  mono_log(MONO_WARNING, "DOOM EMACS (DEMACS) ENABLED");
#endif
#ifdef FORCE_INSTALL
  mono_log(MONO_WARNING, "FORCE DEPLOY ENABLED");
#endif

  MONO_PRINT_SEP;

// Deploy Neovim
#ifdef NEOVIM
  mono_log(MONO_INFO, "[NEOVIM] STARTING DEPLOYMENT");
  mono_log(MONO_INFO, "[NEOVIM] DELETING (mv /tmp) " NEOVIM_CONFIG_PATH);
  MONO_DEL_MV_TO_TMP(MONO_RESOLVE_ENV_VARS(NEOVIM_CONFIG_PATH));

#ifdef FORCE_INSTALL
  mono_log(MONO_INFO, "[NEOVIM] DELETING (mv /tmp) " NEOVIM_CONFIG_DATA);
  MONO_DEL_MV_TO_TMP(MONO_RESOLVE_ENV_VARS(NEOVIM_CONFIG_DATA));
  mono_log(MONO_INFO, "[NEOVIM] DELETING (mv /tmp) " NEOVIM_CONFIG_CACHE);
  MONO_DEL_MV_TO_TMP(MONO_RESOLVE_ENV_VARS(NEOVIM_CONFIG_CACHE));
#endif

  snprintf(buffer, sizeof(buffer), "%s/%s", MONO_PWD(), "neovim/");
  mono_log(MONO_INFO, "[NEOVIM] CREATE SYMLINK TO:");
  mono_log(MONO_INFO, buffer);
  MONO_SYMLINK(buffer, MONO_RESOLVE_ENV_VARS(NEOVIM_CONFIG_PATH));
#endif

// Deploy Doom Emacs
#ifdef DEMACS
  mono_log(MONO_INFO, "[DEMACS] STARTING DEPLOYMENT");
  mono_log(MONO_INFO, "[DEMACS] DELETING (mv /tmp) " DEMACS_CONFIG_PATH);
  MONO_DEL_MV_TO_TMP(MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_PATH));

#ifdef FORCE_INSTALL
  mono_log(MONO_INFO, "[DEMACS] DELETING (mv /tmp) " DEMACS_CONFIG_DATA);
  MONO_DEL_MV_TO_TMP(MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_DATA));
  mono_log(MONO_INFO, "[DEMACS] DELETING (mv /tmp) " DEMACS_CONFIG_CACHE);
  MONO_DEL_MV_TO_TMP(MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_CACHE));

  mono_log(MONO_INFO, "[DEMACS] CLONING DOOM EMACS REPO");
  snprintf(buffer, sizeof(buffer), "git clone --depth 1 %s %s", DEMACS_REPO,
           MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_DATA));
  mono_log(MONO_INFO, buffer);
  MONO_CMD(buffer);

  mono_log(MONO_INFO, "[DEMACS] INSTALLING DOOM");
  snprintf(buffer, sizeof(buffer), "%s/bin/doom install",
           MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_DATA));
  mono_log(MONO_INFO, buffer);
  MONO_CMD(buffer);
#endif

  snprintf(buffer, sizeof(buffer), "%s/%s", MONO_PWD(), "demacs/");
  mono_log(MONO_INFO, "[DEMACS] CREATE SYMLINK TO:");
  mono_log(MONO_INFO, buffer);
  MONO_SYMLINK(buffer, MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_PATH));

#endif

  return 0;
}
