#ifndef VEMACS_H_
#define VEMACS_H_

#include "config.h"
#include "monolith.h"

int deploy_vemacs() {

  char buffer[MAX_OUT_SZ];
  char buffer2[MAX_OUT_SZ];

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
  snprintf(buffer2, sizeof(buffer), "%s/%s",
           MONO_RESOLVE_ENV_VARS(VEMACS_CONFIG_PATH), "init.el");
  mono_log(MONO_INFO, "[VEMACS] CREATE SYMLINK TO:");
  mono_log(MONO_INFO, buffer);
  int err = MONO_SYMLINK(buffer, buffer2);
  if (err != 0)
    mono_log(MONO_ERROR, "Failed to create symlink");

#ifdef SYNC
  mono_log(MONO_INFO, "[VEMACS] SYNCING PACKAGES");
  snprintf(buffer, sizeof(buffer), "emacs --batch --script %s/init.el",
           MONO_RESOLVE_ENV_VARS(VEMACS_CONFIG_PATH));
  MONO_SYS(buffer);
#endif

  return 0;
}

#endif // VEMACS_H_
