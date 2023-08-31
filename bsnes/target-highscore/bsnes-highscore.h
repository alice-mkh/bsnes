#pragma once

#include <highscore/libhighscore.h>

G_BEGIN_DECLS

#define BSNES_TYPE_CORE (bsnes_core_get_type())

G_DECLARE_FINAL_TYPE (bsnesCore, bsnes_core, BSNES, CORE, HsCore)

G_MODULE_EXPORT GType hs_get_core_type (void);

G_END_DECLS
