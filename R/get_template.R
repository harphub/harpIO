# Get File Name Template
#
# @param .template The file type to generate the template for. Can be
#   "harmoneps_grib", "harmeoneps_grib_fp", "harmoneps_grib_sfx", "meps_met",
#   "meps_smhi", "meps_fmi", "arome_arctic", "harmonie_grib",
#   "harmonie_grib_fp", "harmone_grib_sfx", "vfld", "vobs", or "fctable". If
#   anything else is passed, it is returned unmodified. The function is case
#   insensitive.
#
# @return A template that can be interpreted by \code{glue_data} from the
#   \code{glue} package.
# Not exported as only for internal use.
#
# @examples
# get_template("harmoneps_grib")
# get_template("harmonie_grib_fp")
#
get_template <- function(.template) {

  template <- file_templates() %>%
    dplyr::filter(.data$template_name == .template) %>%
    dplyr::pull(.data$template)

  if (length(template) < 1) {
    if (grepl("\\{file_path\\}", .template)) {
      template <- .template
    } else {
      template <- file.path("{file_path}", .template)
    }
  }

  template

}

#' Show built in file templates
#'
#' @param template_row Some of the templates are too wide to show on the screen,
#'   so to show a template in full, pass the row number of the template you want
#'   to show in full in \code{template_row}
#'
#' @export
#'
#' @examples
#' show_file_templates()
#' show_file_templates(1)
#' show_file_templates(18)
show_file_templates <- function(template_row = NULL) {

  templates     <- dplyr::arrange(file_templates(), .data$template_name)
  num_templates <- nrow(templates)

  if (is.null(template_row)) {
    print(templates, n = nrow(templates))
  } else {
    if (template_row < 1 | template_row > num_templates) {
      stop("\"template_row\" must be between 1 and ", num_templates, call. = FALSE)
    }
    cat(
      "\ntemplate_name:\n", templates$template_name[template_row], "\n",
      "\ntemplate:\n", templates$template[template_row], "\n"
    )
  }

}

file_templates <- function() {
  tibble::tribble(

    ~template_name, ~template,

    "arome_arctic_extracted" ,  file.path(
      "/lustre/storeB/immutable/archive/projects/metproduction/DNMI_AROME_ARCTIC",
      "{YYYY}", "{MM}", "{DD}",
      "arome_arctic_extracted_2_5km_{YYYY}{MM}{DD}T{HH}Z.nc"
    ),

    "arome_arctic_full" ,  file.path(
      "/lustre/storeB/immutable/archive/projects/metproduction/DNMI_AROME_ARCTIC",
      "{YYYY}", "{MM}", "{DD}",
      "arome_arctic_full_2_5km_{YYYY}{MM}{DD}T{HH}Z.nc"
    ),

    "arome_arctic_sfx" ,  file.path(
      "/lustre/storeB/immutable/archive/projects/metproduction/DNMI_AROME_ARCTIC",
      "{YYYY}", "{MM}", "{DD}",
      "arome_arctic_sfx_2_5km_{YYYY}{MM}{DD}T{HH}Z.nc"
    ),

    "fctable" , file.path(
      "{file_path}",
      "{fcst_model}",
      "{YYYY}", "{MM}",
      "FCTABLE_{parameter}_{YYYY}{MM}_{HH}.sqlite"
    ),

    "fctable_det" , file.path(
      "{file_path}",
      "{det_model}",
      "{YYYY}", "{MM}",
      "FCTABLE_{parameter}_{YYYY}{MM}_{HH}.sqlite"
    ),

    "fctable_eps" , file.path(
      "{file_path}",
      "{eps_model}",
      "{YYYY}", "{MM}",
      "FCTABLE_{parameter}_{YYYY}{MM}_{HH}+{LDT3}.sqlite"
    ),

    "fctable_eps_all_cycles" , file.path(
      "{file_path}",
      "{eps_model}",
      "{YYYY}", "{MM}",
      "FCTABLE_{parameter}_{YYYY}{MM}+{LDT3}.sqlite"
    ),

    "fctable_eps_all_leads" , file.path(
      "{file_path}",
      "{eps_model}",
      "{YYYY}", "{MM}",
      "FCTABLE_{parameter}_{YYYY}{MM}_{HH}.sqlite"
    ),

    "glameps_grib" , file.path(
      "{file_path}",
      "{eps_model}",
      "{sub_model}",
      "{YYYY}", "{MM}", "{DD}", "{HH}",
      "mbr{MBR3}",
      "fc{YYYY}{MM}{DD}_{HH}+{LDT3}_grib"
    ),

    "harmoneps_grib" , file.path(
      "{file_path}",
      "{YYYY}", "{MM}", "{DD}", "{HH}",
      "mbr{MBR3}",
      "fc{YYYY}{MM}{DD}{HH}+{LDT3}grib"
    ),

    "harmoneps_grib_fp" , file.path(
      "{file_path}",
      "{YYYY}", "{MM}", "{DD}", "{HH}",
      "mbr{MBR3}",
      "fc{YYYY}{MM}{DD}{HH}+{LDT3}grib_fp"
    ),

    "harmoneps_grib_sfx" , file.path(
      "{file_path}",
      "{YYYY}", "{MM}", "{DD}", "{HH}",
      "mbr{MBR3}",
      "fc{YYYY}{MM}{DD}{HH}+{LDT3}grib_sfx"
    ),

    "harmonie_grib" , file.path(
      "{file_path}",
      "{YYYY}", "{MM}", "{DD}", "{HH}",
      "fc{YYYY}{MM}{DD}{HH}+{LDT3}grib"
    ),

    "harmonie_grib_fp" ,file.path(
      "{file_path}",
      "{YYYY}", "{MM}", "{DD}", "{HH}",
      "fc{YYYY}{MM}{DD}{HH}+{LDT3}grib_fp"
    ),

    "harmonie_grib_sfx" , file.path(
      "{file_path}",
      "{YYYY}", "{MM}", "{DD}", "{HH}",
      "fc{YYYY}{MM}{DD}{HH}+{LDT3}grib_sfx"
    ),

    "meps_cntrl_extracted" ,  file.path(
      "/lustre/storeB/immutable/archive/projects/metproduction/MEPS",
      "{YYYY}", "{MM}", "{DD}",
      "meps_mbr0_extracted_2_5km_{YYYY}{MM}{DD}T{HH}Z.nc"
    ),

    "meps_cntrl_sfx" ,  file.path(
      "/lustre/storeB/immutable/archive/projects/metproduction/MEPS",
      "{YYYY}", "{MM}", "{DD}",
      "meps_mbr0_sfx_2_5km_{YYYY}{MM}{DD}T{HH}Z.nc"
    ),

    "meps_det", file.path(
      "/lustre/storeB/immutable/archive/projects/metproduction/MEPS",
      "{YYYY}", "{MM}", "{DD}",
      "meps_det_2_5km_{YYYY}{MM}{DD}T{HH}Z.nc"
    ),

    "meps_extracted" ,  file.path(
      "/lustre/storeB/immutable/archive/projects/metproduction/MEPS",
      "{YYYY}", "{MM}", "{DD}",
      "meps_extracted_2_5km_{YYYY}{MM}{DD}T{HH}Z.nc"
    ),

    "meps_full" ,  file.path(
      "/lustre/storeB/immutable/archive/projects/metproduction/MEPS",
      "{YYYY}", "{MM}", "{DD}",
      "meps_full_2_5km_{YYYY}{MM}{DD}T{HH}Z.nc"
    ),

    "meps_lagged_6h_subset", file.path(
      "/lustre/storeB/immutable/archive/projects/metproduction/MEPS",
      "{YYYY}", "{MM}", "{DD}",
      "meps_lagged_6_h_subset_2_5km_{YYYY}{MM}{DD}T{HH}Z.nc"
    ),

    "meps_sfx" ,  file.path(
      "/lustre/storeB/immutable/archive/projects/metproduction/MEPS",
      "{YYYY}", "{MM}", "{DD}",
      "meps_sfx_2_5km_{YYYY}{MM}{DD}T{HH}Z.nc"
    ),

    "meps_subset" ,  file.path(
      "/lustre/storeB/immutable/archive/projects/metproduction/MEPS",
      "{YYYY}", "{MM}", "{DD}",
      "meps_subset_2_5km_{YYYY}{MM}{DD}T{HH}Z.nc"
    ),

    "obstable" , file.path(
      "{file_path}",
      "OBSTABLE_{YYYY}.sqlite"
    ),

    "vfld" , file.path(
      "{file_path}",
      "{fcst_model}",
      "vfld{fcst_model}{YYYY}{MM}{DD}{HH}{LDT2}"
    ),

    "vfld_eps" , file.path(
      "{file_path}",
      "{sub_model}",
      "vfld{sub_model}mbr{MBR3}{YYYY}{MM}{DD}{HH}{LDT2}"
    ),

    "vfld_multimodel" , file.path(
      "{file_path}",
      "{sub_model}",
      "vfld{sub_model}mbr{MBR3}{YYYY}{MM}{DD}{HH}{LDT2}"
    ),

    "vfld_det" , file.path(
      "{file_path}",
      "{det_model}",
      "vfld{det_model}{YYYY}{MM}{DD}{HH}{LDT2}"
    ),

    "vfld_noexp" , file.path(
      "{file_path}",
      "{fcst_model}",
      "vfldmbr{MBR3}{YYYY}{MM}{DD}{HH}{LDT2}"
    ),

    "vfld_eps_noexp" , file.path(
      "{file_path}",
      "{sub_model}",
      "vfldmbr{MBR3}{YYYY}{MM}{DD}{HH}{LDT2}"
    ),

    "vfld_multimodel_noexp" , file.path(
      "{file_path}",
      "{sub_model}",
      "vfldmbr{MBR3}{YYYY}{MM}{DD}{HH}{LDT2}"
    ),

    "vfld_det_noexp" , file.path(
      "{file_path}",
      "{det_model}",
      "vfld{YYYY}{MM}{DD}{HH}{LDT2}"
    ),

    "vobs" , file.path(
      "{file_path}",
      "vobs{YYYY}{MM}{DD}{HH}"
    )

  )

}
