#ifndef VEMACS_H_
#define VEMACS_H_

#include "config.h"
#include "monolith.h"
#include <stdio.h>

int deploy_vemacs() {

  char buffer[MAX_OUT_SZ];
  char buffer2[MAX_OUT_SZ];
  char vemacs_cmd[MAX_OUT_SZ];

  snprintf(vemacs_cmd, sizeof(vemacs_cmd), "emacs --init-directory %s",
           MONO_RESOLVE_ENV_VARS(VEMACS_CONFIG_PATH));
    

  mono_log(MONO_INFO, "[VEMACS] STARTING DEPLOYMENT");
  mono_log(MONO_INFO, "[VEMACS] DELETING (mv /tmp) " VEMACS_CONFIG_PATH);
  MONO_DEL(MONO_RESOLVE_ENV_VARS(VEMACS_CONFIG_PATH));

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

  snprintf(buffer, sizeof(buffer),
           "%s/.local/share/applications/vemacs.desktop",
           MONO_RESOLVE_ENV_VARS("$HOME"));
  err = snprintf(buffer2, sizeof(buffer2), "%s %%F", vemacs_cmd);
  if (err != 0) 
    mono_log(MONO_ERROR, "Unnable to populate buffer2 variable");
  mono_log(MONO_INFO, "[VEMACS] CREATING .desktop FILE ");
  mono_log(MONO_INFO, buffer);
  mono_create_desktop_file(buffer, "vemacs", buffer2,
                           "Monolith Vanilla Emacs Config",
                           "");
  return 0;
}

#endif // VEMACS_H_
