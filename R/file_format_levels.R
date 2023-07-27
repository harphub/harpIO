define_grib_level_types <- function() {

  list(
    surface   = "surface",
    pressure  = "isobaricInhPa",
    sea       = "meanSea",
    msl       = "meanSea",
    height    = "heightAboveGround",
    asl       = "heightAboveSea",
    model     = "hybrid",
    isotherm  = "isotherm",
    isotherm0 = "isothermZero",
    unknown   = "unknown"
  )

}

define_nc_level_suffixes <- function() {
  list(
    pressure = "_pl",
    model    = "_ml",
    height   = ""
  )
}
