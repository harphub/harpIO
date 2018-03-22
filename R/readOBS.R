readOBS <- function(obsFiles, 
                    startDate,
                    endDate,
                    parameter, 
                    grossErrorCheck = TRUE, 
                    minAllowed = NULL, 
                    maxAllowed = NULL) {
#
  require(tidyverse)
  require(lubridate)
#
  dateStart <- YMD2unix(startDate)
  dateEnd   <- YMD2unix(endDate)
#
  OBS <- list()
  listCounter <- 0
  for (inFile in obsFiles) {
    listCounter <- listCounter + 1
#
    obsDB <- DBI::dbConnect(RSQLite::SQLite(), (inFile))
#
    cat(inFile,":\n")
    cat("Reading", parameter, "OBS for", startDate, "-", endDate)
    obsParam <- quo(parameter)
    OBS[[listCounter]] <- tbl(obsDB, "SYNOP") %>%
                            dplyr::select(validdate, SID, !!obsParam) %>%
                            filter(between(validdate, dateStart, dateEnd)) %>%
                            collect(n = Inf) %>% 
                            drop_na()
    DBI::dbDisconnect(obsDB)
    cat(" ---> DONE \n")
  }
  OBS <- bind_rows(OBS)
#
  if (grossErrorCheck) {
    if (is.null(minAllowed)) minAllowed <- getMinObsAllowed(parameter)
    if (is.null(maxAllowed)) maxAllowed <- getMaxObsAllowed(parameter)
    OBSremoved <- OBS %>% dplyr::filter(!between(.data[[parameter]], minAllowed, maxAllowed))
    OBS        <- OBS %>% dplyr::filter(between(.data[[parameter]], minAllowed, maxAllowed))
  }
#
  list(goodOBS = OBS, badOBS = OBSremoved)
}

