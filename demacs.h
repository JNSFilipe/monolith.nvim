#ifndef DEMACS_H_
#define DEMACS_H_

#include "config.h"
#include "monolith.h"

int deploy_demacs() {

  char buffer[MAX_OUT_SZ];

#ifdef FORCE_INSTALL
  mono_log(MONO_INFO, "[DEMACS] DELETING " DEMACS_CONFIG_DATA);
  MONO_DEL(MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_DATA));
  mono_log(MONO_INFO, "[DEMACS] DELETING " DEMACS_CONFIG_CACHE);
  MONO_DEL(MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_CACHE));

  mono_log(MONO_INFO, "[DEMACS] CLONING DOOM EMACS REPO");
  snprintf(buffer, sizeof(buffer), "git clone --depth 1 %s %s", DEMACS_REPO,
           MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_DATA));
  mono_log(MONO_INFO, buffer);
  MONO_SYS(buffer);

  mono_log(MONO_INFO, "[DEMACS] INSTALLING DOOM");
  snprintf(buffer, sizeof(buffer), "%s/bin/doom install --force",
           MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_DATA)); // force for unattended
  mono_log(MONO_INFO, buffer);
  MONO_SYS(buffer);
#endif

  mono_log(MONO_INFO, "[DEMACS] STARTING DEPLOYMENT");
  mono_log(MONO_INFO, "[DEMACS] DELETING " DEMACS_CONFIG_PATH);
  MONO_DEL(MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_PATH));
  MONO_DEL(MONO_RESOLVE_ENV_VARS(VEMACS_CONFIG_PATH));

  snprintf(buffer, sizeof(buffer), "%s/%s", MONO_PWD(), "demacs/");
  mono_log(MONO_INFO, "[DEMACS] CREATE SYMLINK TO:");
  mono_log(MONO_INFO, buffer);
  int err = MONO_SYMLINK(buffer, MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_PATH));
  if (err != 0)
    mono_log(MONO_ERROR, "Failed to create symlink");

#ifdef SYNC
  mono_log(MONO_INFO, "[DEMACS] PURGING OLD PACKAGES");
  snprintf(buffer, sizeof(buffer), "%s/bin/doom purge",
           MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_DATA));
  mono_log(MONO_INFO, buffer);
  MONO_SYS(buffer);

  mono_log(MONO_INFO, "[DEMACS] SYNCING PACKAGES (TWICE, FOR GOOD MEASURE)");
  snprintf(buffer, sizeof(buffer), "%s/bin/doom sync",
           MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_DATA));
  snprintf(buffer, sizeof(buffer), "%s/bin/doom sync",
           MONO_RESOLVE_ENV_VARS(DEMACS_CONFIG_DATA));
  mono_log(MONO_INFO, buffer);
  MONO_SYS(buffer);
#endif

  return 0;
}

#endif // DEMACS_H_
