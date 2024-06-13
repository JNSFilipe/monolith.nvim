#include "monolith.h"
#include "config.h"

#include "demacs.h"
#include "neovim.h"
#include "vemacs.h"

int main() {

#if defined(DEMACS) && defined(VEMACS)
  mono_log(MONO_ERROR, "DEMACS and VEMACS cannot be enabled at the same time, "
                       "choose one emacs config");
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
  deploy_neovim();
#endif

  MONO_PRINT_SEP;

// Deploy Doom Emacs
#ifdef DEMACS
  deploy_demacs();
#endif

  MONO_PRINT_SEP;

// Deploy Vanilla Emacs
#ifdef VEMACS
  deploy_vemacs();
#endif

  MONO_PRINT_SEP;

  return 0;
}
