#' Title
#'
#' @param tableData
#' @param fctable_path
#' @param fcstExp
#' @param parameter
#' @param filename_template
#'
#' @return
#' @export
#'
#' @examples
#'
#### I don't think this function is needed anymore... will double check...
write_fctable <- function(
  tableData,
  fctable_path,
  fcstExp,
  parameter,
  filename_template = "${fctable_path}/${fcstExp}/${YYYY}/${MM}/FCTABLE_${parameter}_${YYYY}${MM}_${HH}+${LDT3}.sqlite"
) {

  if (names(tableData) %>% stringr::str_detect("member") %>% any()) {
    tableData <- tableData %>%
      tidyr::spread(key = member, value = forecast)
  }

  filenames <- tableData %>%
    dplyr::filter(SID == .$SID[1]) %>%
    dplyr::transmute(
      fcdate,
      fcstExp      = fcstExp,
      YYYY         = fcdate %>% unix2datetime() %>% lubridate::year(),
      M            = fcdate %>% unix2datetime() %>% lubridate::month(),
      D            = fcdate %>% unix2datetime() %>% lubridate::day(),
      H            = fcdate %>% unix2datetime() %>% lubridate::hour(),
      MM           = fcdate %>% unix2datetime() %>% lubridate::month() %>% formatC(width = 2, flag = "0"),
      DD           = fcdate %>% unix2datetime() %>% lubridate::day() %>% formatC(width = 2, flag = "0"),
      HH           = fcdate %>% unix2datetime() %>% lubridate::hour() %>% formatC(width = 2, flag = "0"),
      LDT          = leadtime,
      LDT2         = leadtime %>% formatC(width = 2, flag = "0"),
      LDT3         = leadtime %>% formatC(width = 3, flag = "0"),
      parameter    = parameter,
      fctable_path = fctable_path
    )

  filename_strings <- filenames %>%
    purrrlyr::by_row( ~ as.list(.x)) %>%
    dplyr::transmute(.out) %>%
    as.list() %>%
    .[[1]] %>%
    purrr::map_chr(stringr::str_interp, string = filename_template)

  filenames <- filenames %>%
    dplyr::select(
      fcdate,
      leadtime = LDT
    ) %>%
    dplyr::mutate(
      filename = filename_strings
    )

  tableData <- tableData %>%
    dplyr::left_join(filenames, by = c("fcdate", "leadtime")) %>%
    split(.$filename) %>%
    purrr::map(~ dplyr::select(.x, -filename))

  purrr::walk2(tableData, names(tableData), ~ write_fctable_to_sqlite(.x, .y))
}
