#include "monolith.h"
#include "config.h"

int main() {

#ifdef FORCE_INSTALL
  mono_log(MONO_WARNING, "FORCE INSTALL ENABLED");
  mono_log(MONO_INFO, "DELETING " NEOVIM_CONFIG_PATH);
  MONO_DEL_FILE_OR_DIR(NEOVIM_CONFIG_PATH);
  mono_log(MONO_INFO, "DELETING " NEOVIM_CONFIG_DATA);
  MONO_DEL_FILE_OR_DIR();
  mono_log(MONO_INFO, "DELETING " NEOVIM_CONFIG_CACHE);
  MONO_DEL_FILE_OR_DIR();
#endif

  mono_log(MONO_INFO, MONO_RESOLVE_ENV_VARS(NEOVIM_CONFIG_PATH));
  // mono_log(MONO_INFO, EVAL_ECHO_CMD("pwd"));
  // mono_log(MONO_INFO, EVAL_ECHO_CMD("ls"));

  char neovim[256];
  snprintf(neovim, sizeof(neovim), "%s/%s", MONO_PWD(), "neovim/");
  mono_log(MONO_INFO, neovim);

  // TO DEBUG TODO: Debug these functions
  MONO_MK_FULL_PATH("/tmp/nvim/cenas");
  MONO_MKDIR("/tmp/nvim/cenas");

  // WORKINIG
  // mono_log(MONO_INFO, MONO_RESOLVE_ENV_VARS(NEOVIM_CONFIG_PATH));
  // MONO_CREATE_SYMLINK(neovim, "/tmp/cenas");
  // MONO_DEL_FILE_OR_DIR("/tmp/cenas");

  return 0;
}
