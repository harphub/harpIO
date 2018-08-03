# Internal functions for default values for gross error checks on observations

get_min_obs_allowed <- function(parameter) {
  switch(parameter,
         "T2m"       = 223,
         "RH2m"      = 0,
         "Pmsl"      = 90000,
         "S10m"      = 0,
         "G10m"      = 0,
         "AccPcp1h"  = 0,
         "AccPcp3h"  = 0,
         "AccPcp6h"  = 0,
         "AccPcp12h" = 0,
         "AccPcp24h" = 0,
         "CCtot"     = 0,
         "CClow"     = 0,
         "CCmed"     = 0,
         "CChigh"    = 0,
         "Cbase"     = 0
  )
}
#
get_max_obs_allowed <- function(parameter) {
  switch(parameter,
         "T2m"       = 323,
         "RH2m"      = 100,
         "Pmsl"      = 110000,
         "S10m"      = 100,
         "G10m"      = 150,
         "AccPcp1h"  = 500,
         "AccPcp3h"  = 600,
         "AccPcp6h"  = 750,
         "AccPcp12h" = 1000,
         "AccPcp24h" = 1000,
         "CCtot"     = 8,
         "CClow"     = 8,
         "CCmed"     = 8,
         "CChigh"    = 8,
         "Cbase"     = 10000
  )
}
