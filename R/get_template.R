#' Get File Name Template
#'
#' @param template The file type to generate the template for. Can be
#'   "harmoneps_grib", "harmeoneps_grib_fp", "harmoneps_grib_sfx", "meps_met",
#'   "meps_smhi", "meps_fmi", "arome_arctic", "harmonie_grib",
#'   "harmonie_grib_fp", "harmone_grib_sfx", "vfld", "vobs", or "fctable". If
#'   anything else is passed, it is returned unmodified. The function is case
#'   insensitive.
#'
#' @return A template that can be interpreted by \code{glue_data} from the
#'   \code{glue} package.
#' @export
#'
#' @examples
#' get_template("harmoneps_grib")
#' get_template("harmonie_grib_fp")
#'
get_template <- function(template) {
  template <- switch(tolower(template),

    "arome_arctic_extracted" =  file.path(
      "/lustre/storeB/immutable/archive/projects/metproduction/DNMI_AROME_ARCTIC",
      "{YYYY}", "{MM}", "{DD}",
      "arome_arctic_extracted_2_5km_{YYYY}{MM}{DD}T{HH}Z.nc"
    ),

    "arome_arctic_full" =  file.path(
      "/lustre/storeB/immutable/archive/projects/metproduction/DNMI_AROME_ARCTIC",
      "{YYYY}", "{MM}", "{DD}",
      "arome_arctic_full_2_5km_{YYYY}{MM}{DD}T{HH}Z.nc"
    ),

    "arome_arctic_sfx" =  file.path(
      "/lustre/storeB/immutable/archive/projects/metproduction/DNMI_AROME_ARCTIC",
      "{YYYY}", "{MM}", "{DD}",
      "arome_arctic_sfx_2_5km_{YYYY}{MM}{DD}T{HH}Z.nc"
    ),

    "fctable" = file.path(
      "{file_path}",
      "{eps_model}",
      "{YYYY}", "{MM}",
      "FCTABLE_{parameter}_{YYYY}{MM}_{HH}+{LDT3}.sqlite"
    ),

    "fctable_det" = file.path(
      "{file_path}",
      "{det_model}",
      "{YYYY}", "{MM}",
      "FCTABLE_{parameter}_{YYYY}{MM}_{HH}.sqlite"
    ),

    "fctable_eps" = file.path(
      "{file_path}",
      "{eps_model}",
      "{YYYY}", "{MM}",
      "FCTABLE_{parameter}_{YYYY}{MM}_{HH}+{LDT3}.sqlite"
    ),

    "glameps_grib" = file.path(
      "{file_path}",
      "{eps_model}",
      "{sub_model}",
      "{YYYY}", "{MM}", "{DD}", "{HH}",
      "mbr{MBR3}",
      "fc{YYYY}{MM}{DD}_{HH}+{LDT3}_grib"
    ),

    "harmoneps_grib" = file.path(
      "{file_path}",
      "{YYYY}", "{MM}", "{DD}", "{HH}",
      "mbr{MBR3}",
      "fc{YYYY}{MM}{DD}_{HH}+{LDT3}_grib"
    ),

    "harmoneps_grib_fp" = file.path(
      "{file_path}",
      "{YYYY}", "{MM}", "{DD}", "{HH}",
      "mbr{MBR3}",
      "fc{YYYY}{MM}{DD}_{HH}+{LDT3}_grib_fp"
    ),

    "harmoneps_grib_sfx" = file.path(
      "{file_path}",
      "{YYYY}", "{MM}", "{DD}", "{HH}",
      "mbr{MBR3}",
      "fc{YYYY}{MM}{DD}_{HH}+{LDT3}_grib_sfx"
    ),

    "harmonie_grib" = file.path(
      "{file_path}",
      "{YYYY}", "{MM}", "{DD}", "{HH}",
      "fc{YYYY}{MM}{DD}_{HH}+{LDT3}_grib"
    ),

    "harmonie_grib_fp" =file.path(
      "{file_path}",
      "{YYYY}", "{MM}", "{DD}", "{HH}",
      "fc{YYYY}{MM}{DD}_{HH}+{LDT3}_grib_fp"
    ),

    "harmonie_grib_sfx" = file.path(
      "{file_path}",
      "{YYYY}", "{MM}", "{DD}", "{HH}",
      "fc{YYYY}{MM}{DD}_{HH}+{LDT3}_grib_sfx"
    ),

    "meps_cntrl_extracted" =  file.path(
      "/lustre/storeB/immutable/archive/projects/metproduction/MEPS",
      "{YYYY}", "{MM}", "{DD}",
      "meps_mbr0_extracted_2_5km_{YYYY}{MM}{DD}T{HH}Z.nc"
    ),

    "meps_cntrl_sfx" =  file.path(
      "/lustre/storeB/immutable/archive/projects/metproduction/MEPS",
      "{YYYY}", "{MM}", "{DD}",
      "meps_mbr0_sfx_2_5km_{YYYY}{MM}{DD}T{HH}Z.nc"
    ),

    "meps_extracted" =  file.path(
      "/lustre/storeB/immutable/archive/projects/metproduction/MEPS",
      "{YYYY}", "{MM}", "{DD}",
      "meps_extracted_2_5km_{YYYY}{MM}{DD}T{HH}Z.nc"
    ),

    "meps_full" =  file.path(
      "/lustre/storeB/immutable/archive/projects/metproduction/MEPS",
      "{YYYY}", "{MM}", "{DD}",
      "meps_full_2_5km_{YYYY}{MM}{DD}T{HH}Z.nc"
    ),

    "meps_sfx" =  file.path(
      "/lustre/storeB/immutable/archive/projects/metproduction/MEPS",
      "{YYYY}", "{MM}", "{DD}",
      "meps_sfx_2_5km_{YYYY}{MM}{DD}T{HH}Z.nc"
    ),

    "obstable" = file.path(
      "{file_path}",
      "OBSTABLE_{YYYY}.sqlite"
    ),

    "vfld" = file.path(
      "{file_path}",
      "{sub_model}",
      "vfld{sub_model}mbr{MBR3}{YYYY}{MM}{DD}{HH}{LDT2}"
    ),

    "vobs" = file.path(
      "{file_path}",
      "vobs{YYYY}{MM}{DD}{HH}"
    ),

    file.path("{file_path}", template)
  )
  template
}
