#ifndef NEOVIM_H_
#define NEOVIM_H_

#include "config.h"
#include "monolith.h"

int deploy_neovim() {

  char buffer[MAX_OUT_SZ];

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
  int err = MONO_SYMLINK(buffer, MONO_RESOLVE_ENV_VARS(NEOVIM_CONFIG_PATH));
  if (err != 0)
    mono_log(MONO_ERROR, "Failed to create symlink");

#ifdef SYNC
  mono_log(MONO_INFO, "[NEOVIM] SYNCING PACKAGES");
  snprintf(buffer, sizeof(buffer), "nvim --headless \"+Lazy! sync\" +qa");
  mono_log(MONO_INFO, buffer);
  MONO_SYS(buffer);
#endif
  return 0;
}

#endif // NEOVIM_H_
