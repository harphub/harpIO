###
# Internal function to read obsoul data
###

# Note this function only works for synop data. For other obseravtion types
# Some new tidying functions will need to be written


read_obsoul <- function(
  file_name,
  param_defs = get("harp_params"),
  ...
) {


  if (file.exists(file_name)) {
    message("Reading: ", file_name)
  } else {
    warning("File not found: ", file_name, call. = FALSE, immediate. = TRUE)
    return(list(synop = NULL))
  }

  # Get obsoul parameters from parameter definitions
  #param_defs <- param_defs[
  #  sapply(param_defs, function(x) is.element("obsoul", names(x)))
  #]

  maxcol <- max(count.fields(file_name))
  #print(param_defs)
  file_connection <- file(file_name, "r")
  on.exit(close(file_connection))

  #meta data
  #based on type, OBSOUL will be clasifict into TEMP or SYNOP (FOR NOW!!!)
  v_metadata <- scan(file_connection, nmax = 4, nlines = 2, quiet = TRUE)
  #num_columns <- v_metadata[3]
  obsoul_obstype <- v_metadata[4]
  print(obsoul_obstype)
  close(file_connection)

  # First line is date and time - time should by hms with leading zeroes
  # to make up a 6 character string
  file_connection <- file(file_name, "r")
  date_time_check    <- scan(file_connection, nlines = 1, quiet = TRUE)
  date_time_check[2] <- formatC(date_time_check[2], width = 6, flag = "0")
  date_time_check    <- paste(date_time_check, collapse = "")


  #if(num_rows <100){
  if(obsoul_obstype == 1){
  # Get obsoul parameters from parameter definitions
  param_defs <- list(

  t2m = list(description = 'Air temperature at 2m above the ground', min = 223, max = 333,
             obsoul = list(name=39, units="K", harp_name = "T2m")),

  tmax = list(description = 'Maximum temperature', min = 223, max = 333,
              obsoul = list(name=82, units="K", harp_name = "Tmax")),

  tmin = list(description = 'Minimum temperature', min = 223, max = 333,
              obsoul = list(name=81, units="K", harp_name = "Tmin")),

  accpcp1h = list(description = '1h accumulated precipitation', min = 0, max = 200,
              obsoul = list(name=79, units="mm", harp_name = "accpcp1h")),

  accpcp6h = list(description = '6h accumulated precipitation', min = 0, max = 600,
                  obsoul = list(name=80, units="mm", harp_name = "accpcp6h")),

  snow = list(description = 'Snow depth', min = 0, max = 700,
             obsoul = list(name=92, units="cm", harp_name = "sd")),

  cctot = list(description = 'cloudiness', min = 0, max = 100,
              obsoul = list(name=91, units="m", harp_name = "cctot")),

  q2m  = list(description = 'Specific humidity at 2m',
              obsoul = list(name=7, units="kg/kg", harp_name = "Q2m",common_name = "specific_humidity")),

  z  = list(description = 'Geopotencial',
            obsoul = list(name=1, units="m", harp_name = "Z")),

  rh2m = list(description = 'Relative humidity on 2m',
              obsoul = list(name=58, units="percent", harp_name = "RH2m")),

  bt = list(description = 'Brightness temperature',
            obsoul = list(name=119, units="K", harp_name = "BT")),

  s10m  = list(description = 'Wind speed 10m',
               obsoul = list(name=41, units="m/s", harp_name = "S10m",common_name="wind")),

  d10m  = list(description = 'Wind direction',
               obsoul = list(name=41, units="m/s", harp_name = "D10m",common_name="wind"))
)
  }

  #if(num_rows >100){
  if(obsoul_obstype == 5){
   #temp fix
   param_defs <- list(

   t2m = list(description = 'Air temperature at 2m above the ground', min = 223, max = 333,
             obsoul = list(name=39, units="K", harp_name = "T2m")),
   t  = list(description = 'Air temperature', min = 173, max = 333,
             obsoul = list(name=2, units="K", harp_name = "T")),
   q  = list(description = 'Specific humidity of air',
             obsoul = list(name=7, units="kg/kg", harp_name = "Q", common_name = "specific_humidity")),
   q2m  = list(description = 'Specific humidity at 2m',
             obsoul = list(name=7, units="kg/kg", harp_name = "Q2m",common_name = "specific_humidity")),
   z  = list(description = 'Geopotencial',
             obsoul = list(name=1, units="m", harp_name = "Z")),
   rh2m = list(description = 'Relative humidity on 2m',
             obsoul = list(name=58, units="percent", harp_name = "RH2m")),
   rh = list(description = 'Relative humidity of air',
             obsoul = list(name=29, units="percent", harp_name = "RH")),
   bt = list(description = 'Brightness temperature',
             obsoul = list(name=119, units="K", harp_name = "BT")),
   s  = list(description = 'Wind speed of air',
   #s  = list(description = 'Wind speed of air', min = 0, max=200,
             obsoul = list(name=3, units="m/s", harp_name = "S",common_name="wind_upper")),
   d  = list(description = 'Wind direction of air',
             obsoul = list(name=3, units="m/s", harp_name = "D",common_name="wind_upper")),
   s10m  = list(description = 'Wind speed 10m',
             obsoul = list(name=41, units="m/s", harp_name = "S10m",common_name="wind")),
   d10m  = list(description = 'Wind direction',
             obsoul = list(name=41, units="m/s", harp_name = "D10m",common_name="wind"))
  )

}

# Read the data and check for problems
  #obs_df <- try(read.table(file_connection, fill = TRUE), silent = TRUE)
  obs_df <- try(read.table(file_connection,col.names = paste0("V", seq_len(maxcol)), fill = TRUE), silent = TRUE)
  if (inherits(obs_df, "try-error")) {
    warning("Cannot read ", file_name, ".", call. = FALSE, immediate. = TRUE)
    return(list(synop = NULL))
  }

  if (ncol(obs_df) < 17) {
    warning("Cannot read ", file_name, ".", call. = FALSE, immediate. = TRUE)
    return(list(synop = NULL))
  }


  max_obs <- (ncol(obs_df) - 12) / 5

  # Add column names
  colnames(obs_df) <- obsoul_cols(max_obs)

  # Get the observation type based on the code
  obs_df <- dplyr::mutate(
    obs_df,
    type = dplyr::case_when(
      substr(.data[["xx"]],7,11) == 14  ~ "synop",
      substr(.data[["xx"]],7,11) == 24  ~ "ship",
      .data[["type"]] == 1   ~ "synop",
      .data[["type"]] == 2   ~ "airep",
      .data[["type"]] == 3   ~ "satob",
      .data[["type"]] == 4   ~ "dribu",
      .data[["type"]] == 5   ~ "temp",
      .data[["type"]] == 6   ~ "pilot",
      .data[["type"]] == 7   ~ "satem",
      .data[["type"]] == 8   ~ "paob",
      .data[["type"]] == 9   ~ "scatt",
      .data[["type"]] == 10  ~ "limb",
      .data[["type"]] == 13  ~ "radar",
      TRUE                   ~ "unknown"
    )
  )

  # Split the data by type
  obs_df <- split(obs_df, obs_df[["type"]])
  # Only works on synop data for now - will need other tidying functions
  # for other obs types

  if (!is.null(obs_df[["synop"]]) && obsoul_obstype == 1) {
    synop <- tidy_obsoul_synop(
      obs_df[["synop"]], param_defs, max_obs
    )
  } else {
    synop = list(synop = NULL)
  }
print(synop)
 # c(synop)

  if (!is.null(obs_df[["temp"]]) && obsoul_obstype == 5) {
    temp <- tidy_obsoul_temp(
      obs_df[["temp"]], param_defs, max_obs
    )
  } else {
    temp = list(temp = NULL)
  }
print(temp)
#for now,becouse we implement two type


if(obsoul_obstype == 1){
  c(synop)
}else{
#if(obsoul_obstype == 5){
  c(temp)}


#else{
#  list(synop = NULL, temp = NULL)
#}


}

#print(synop)
#print(temp)
###
# Function to tidy synop data
###
tidy_obsoul_synop <- function(synop_df, param_defs, max_obs) {
print("TIDY_OBSOUL_SYNOP")
  # Modify SID depending on country, set valid_dttm in unix time
  # and convert parameter codes to names
  synop_df <- dplyr::mutate(
    synop_df,
    SID  = modify_sid(.data[["SID"]]),
    valid_dttm = suppressMessages(
      harpCore::as_unixtime(
        paste0(
          .data[["date"]],
          formatC(as.integer(.data[["hms"]]), width = 6, flag = "0")
        )
      )
    ),
    dplyr::across(
      dplyr::contains("_code"), obsoul_param_code_to_name, param_defs
    )
  )

  # Gather all observations sections into common columns
  synop_df <- lapply(
    1:max_obs,
    function(x) dplyr::select(
      synop_df,
      !dplyr::matches("^obs[[:digit:]]+"),
      dplyr::starts_with(paste0("obs", x,"_"))
    ) %>%
      dplyr::rename_with(
        ~gsub("^obs[[:digit:]]+", "obs", .x)
      )
  ) %>%
    dplyr::bind_rows() %>%
    dplyr::select(
      .data[["SID"]],
      .data[["lat"]],
      .data[["lon"]],
      .data[["elev"]],
      .data[["valid_dttm"]],
      dplyr::starts_with("obs_")
    ) %>%
    dplyr::filter(!is.na(.data[["obs_code"]]))

  # Pull out the correct columns for the observations
  synop_df <- dplyr::bind_rows(
    dplyr::transmute(
      dplyr::filter(synop_df, .data[["obs_code"]] != "wind"),
      .data[["SID"]],
      .data[["lat"]],
      .data[["lon"]],
      .data[["elev"]],
      .data[["valid_dttm"]],
      param = .data[["obs_code"]],
      obs   = dplyr::case_when(
        .data[["param"]] == "Pmsl" ~ .data[["obs_1"]] * -1,
        TRUE                       ~ .data[["obs_3"]]
      )
    ),
    tidyr::pivot_longer(
      dplyr::rename(
        dplyr::filter(synop_df, .data[["obs_code"]] == "wind"),
        S10m = .data[["obs_2"]],
        D10m = .data[["obs_3"]]
      ),
      cols = c(dplyr::all_of(c("S10m", "D10m"))),
      names_to  = "param",
      values_to = "obs"
    ) %>%
      dplyr::select(!dplyr::starts_with("obs_"))
  )

  # The Pmsl is only Pmsl if it's positive - otherwise it's -surface
  # pressure and we don't want that for now... (probably)
  synop_df <- dplyr::filter(
    synop_df,
    !(.data[["param"]] == "Pmsl" & .data[["obs"]] < 0)
  )

  # Pivot the observations to their own columns and generate
  # the units data frame
  params <- unique(synop_df[["param"]])
  list(
    synop = tidyr::pivot_wider(
      synop_df,
      names_from  = .data[["param"]],
      values_from = .data[["obs"]]
    ),
    synop_params = obsoul_params(params, param_defs)
  )

}


##
# Function to tidy temp data
###
tidy_obsoul_temp <- function(temp_df, param_defs, max_obs) {
  # Modify SID depending on country, set valid_dttm in unix time
  # and convert parameter codes to names
  temp_df <- dplyr::mutate(
    temp_df,
    #SID  = modify_sid(.data[["SID"]]),
    SID  = as.character(.data[["SID"]]),
    valid_dttm = suppressMessages(
      harpCore::as_unixtime(
        paste0(
          .data[["date"]],
          formatC(as.integer(.data[["hms"]]), width = 6, flag = "0")
        )
      )
    ),
    dplyr::across(
      dplyr::contains("_code"), obsoul_param_code_to_name, param_defs
    )
  )

  # Gather all observations sections into common columns
  temp_df <- lapply(
    1:max_obs,
    function(x) dplyr::select(
      temp_df,
      !dplyr::matches("^obs[[:digit:]]+"),
      dplyr::starts_with(paste0("obs", x, "_"))
    ) %>%
      dplyr::rename_with(
        ~gsub("^obs[[:digit:]]+", "obs", .x)
      )
  ) %>%
    dplyr::bind_rows() %>%
    dplyr::select(
      .data[["SID"]],
      .data[["lat"]],
      .data[["lon"]],
      .data[["elev"]],
      .data[["valid_dttm"]],
      dplyr::starts_with("obs_")
    ) %>%
    dplyr::filter(!is.na(.data[["obs_code"]]))

  temp_df <- temp_df %>% distinct()
  #print(colnames(temp_df))
  #print(temp_df %>% dplyr::summarise(n = dplyr::n(), .by = c(SID,lat,lon,elev,valid_dttm,obs_code,obs_1,obs_2,obs_3,obs_end)) %>%
  #dplyr::filter(n > 1L))


  #prepare data TEMP
  wind_temp <- dplyr::bind_rows(
  tidyr::pivot_longer(
    dplyr::rename(
    p = .data[["obs_1"]],
    flag = .data[["obs_end"]],
    dplyr::filter(temp_df, .data[['obs_code']] == "wind"),
        S10m = .data[['obs_2']],
        D10m = .data[['obs_3']]),
    cols = c(all_of(c('S10m','D10m'))),
    names_to = "param",
    values_to = "obs"
  ),
  tidyr::pivot_longer(
    dplyr::rename(
    p = .data[["obs_1"]],
    flag = .data[["obs_end"]],
    dplyr::filter(temp_df, .data[['obs_code']] == "wind_upper"),
        S = .data[['obs_2']],
        D = .data[['obs_3']]),
    cols = c(all_of(c('S','D'))),
    names_to = "param",
    values_to = "obs"))%>%
        select(- obs_code)

  #wind_temp$SID <- sub("\\s+$", "", wind_temp$SID)

# Pull out the correct columns for the observations
  temp_df <- dplyr::transmute(
      dplyr::filter(temp_df, .data[["obs_code"]] != "wind" & .data[["obs_code"]] != "wind_upper"),
      .data[["SID"]],
      .data[["lat"]],
      .data[["lon"]],
      .data[["elev"]],
      .data[["valid_dttm"]],
      param = .data[["obs_code"]],
      flag  = .data[["obs_end"]],
      p     = .data[["obs_1"]],
      obs   = dplyr::case_when(
      .data[["param"]] == "Pmsl" ~ .data[["obs_1"]] * -1,
        TRUE                     ~ .data[["obs_3"]]
      ))%>%
      mutate(param = dplyr::case_when(
	.data[["flag"]] == 3680 & .data[["param"]] == "specific_humidity" ~ "Q2m",
        .data[["flag"]] != 3680 & .data[["param"]] == "specific_humidity" ~ "Q",
        TRUE ~ param
      ))%>%
      mutate(obs = dplyr::case_when(
	.data[["param"]] == "Z" ~ obs / 9.800665,
        TRUE ~ obs
      ))%>%
	dplyr::select(!dplyr::starts_with("obs_"))

  #merge converted wind into dataframe & filter on 3680[surface data] and 2560[significant levels] -->FOR NOW!
  temp_df <- full_join(temp_df, wind_temp, by = c("SID", "lat", "lon", "elev", "valid_dttm", "param", "flag", "p", "obs")) %>%
	filter(., flag %in% c(3680,2560))
	#filter(., flag %in% c(3680,10304))

#   temp_df <- bind_rows(temp_df, wind_temp)

	#filter(., p %in% c(100000,92500,85000,70000,60000,50000,40000,30000,25000,20000,10000,5000))
	#filter(., p %in% c(100000,92500,85000,70000,60000,50000,40000,30000))
  #print(temp_df %>% dplyr::summarise(n = dplyr::n(), .by = c(SID, lat, lon, elev, valid_dttm, p, param,obs,flag)) %>%
  #dplyr::filter(n > 1L))

  # The Pmsl is only Pmsl if it's positive - otherwise it's -surface
  # pressure and we don't want that for now... (probably)
  # remove flag columns
  # convert pressure Pa -> hPa
  temp_df <- dplyr::filter(
    temp_df,
    !(.data[["param"]] == "Pmsl" & .data[["obs"]] < 0)
  )%>%
  #  select(-flag) %>%
    mutate(across(p,~./100))
  #print(duplicated(temp_df))
  # Pivot the observations to their own columns and generate
  # the units data frame

  #clean,duplicates(can be found in TEMP file)!!!!
  #temp_df <- temp_df %>% distinct()
  temp_df$SID <- sub("\\s+$", "", temp_df$SID)

  temp_df <- temp_df %>%
	     group_by(SID, lat, lon, elev, valid_dttm, p, param, flag)  %>%
	     mutate(counts = row_number()) %>%
	     filter(., counts == 1) %>%
	     select(., - counts)

  temp_df <- ungroup(temp_df)

  temp_df <- temp_df %>% distinct()

 # print(temp_df %>% dplyr::summarise(n = dplyr::n(), .by = c(SID, lat, lon, elev, valid_dttm, p, param)) %>%
 # dplyr::filter(n > 1L))

  #select only Europe
#  lat_min = 35.0
#  lat_max = 71.0
#  lon_min = -16.0
#  lon_max = 45.0

#  temp_df <- temp_df %>%
#  	filter (lat >= lat_min & lat <= lat_max & lon >= lon_min & lon <= lon_max)

  print(temp_df %>% dplyr::summarise(n = dplyr::n(), .by = c(SID, lat, lon, elev, valid_dttm, p, param)) %>%
  	dplyr::filter(n > 1L))

  params <- unique(temp_df[["param"]])

  list(
    temp = tidyr::pivot_wider(
      temp_df,
      names_from  = .data[["param"]],
      values_from = .data[["obs"]]
    ),
    temp_params = obsoul_params(params, param_defs)
  )

}



###
# Function to set column names for obsoul data
###
obsoul_cols <- function(max_obs) {
  col_names <- c(
    "num_col",
    "type",
    "xx",
    "lat",
    "lon",
    "SID",
    "date",
    "hms",
    "elev",
    "num_obs",
    "xx1",
    "xx2"
  )

  obs_cols <- unlist(
    lapply(
      1:max_obs,
      function(x) paste0("obs", x, "_", c("code", "1", "2", "3", "end"))
    )
  )

  c(col_names, obs_cols)

}

###
# Function to get the parameter name from the parameter code
# Unknown codes get NA
###
obsoul_param_code_to_name <- function(x, param_defs) {
  param_df <- dplyr::distinct(
    tibble::tibble(
      name = purrr::map_chr(
        param_defs,
        ~ifelse(
          is.null(.x[["obsoul"]][["common_name"]]),
          .x[["obsoul"]][["harp_name"]],
          .x[["obsoul"]][["common_name"]]
        )
      ),
      code = sapply(
        param_defs,
        function(x) x[["obsoul"]][["name"]])
    )
  )

  dplyr::pull(
    dplyr::left_join(
      tibble::tibble(code = x),
      param_df,
      by = "code"
    ),
    .data[["name"]]
  )

}




###
# Function to add a country indicator to site IDs
# modify_sid <- function(x) {

  # x <- gsub("^AT","90",
       # gsub("^CR","91",
       # gsub("^CZ","92",
       # gsub("^HU","93",
       # gsub("^PL","94",
       # gsub("^RO","95",
       # gsub("^SI","96",
       # gsub("^SK","97", x ))))))))

       # as.numeric(x)

# }



modify_sid <- function(x) {
  # Define a named vector for country code mappings
  country_codes <- c(
    "AT" = "90",
    "CR" = "91",
    "CZ" = "92",
    "HU" = "93",
    "PL" = "94",
    "RO" = "95",
    "SI" = "96",
    "SK" = "97"
  )

  # Apply replacements for each country code
  for (code in names(country_codes)) {
    x <- gsub(paste0("^", code), country_codes[code], x)
  }

  # Convert to numeric where possible, leave others as is
  is_numeric <- suppressWarnings(!is.na(as.numeric(x)))
  x[is_numeric] <- as.numeric(x[is_numeric])

  # Return the modified vector
  x
}


###
# Function to generate the data frame for parameter units
###
obsoul_params <- function(params, param_defs) {

  param_names <-  sapply(
    param_defs,
    function(x) x[["obsoul"]][["harp_name"]],
    USE.NAMES = FALSE
  )

  param_units <- sapply(
    param_defs,
    function(x) x[["obsoul"]][["units"]],
    USE.NAMES = FALSE
  )

  param_df <- tibble::tibble(
    parameter   = param_names,
    accum_hours = rep(0, length(param_names)),
    units       = param_units
  )

  dplyr::filter(
    param_df,
    .data[["parameter"]] %in% params
  )

}

