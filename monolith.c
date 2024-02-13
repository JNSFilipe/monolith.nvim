#include "monolith.h"
#include "config.h"

int main() {
  char buffer[MAX_OUT_SZ];

  MONO_PRINT_SEP;

// Inform about enabled flags
#ifdef NEOVIM
  mono_log(MONO_WARNING, "NEOVIM ENABLED");
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

  return 0;
}
