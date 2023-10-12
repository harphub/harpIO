## JB functions for reading obs data from MET Norway's frost API to be adapted
## to harpIO - should be called by read_point_obs()

## Add support for polygons and nearest.



##   R functions for reading measurement data using the frost api/interface
##


miReadFrost <- function(sid = 18700, prm = "air_temperature", period = c("2019-01-01", "2019-01-05"),
  station_name = NULL, client_id = "30a8afce-0121-4ad9-9b40-05b12964c4c1",
  country_code = NULL, station_holder = NULL,
  quality = 0, print_url = FALSE, add_name = FALSE, sid_numeric = TRUE,
  time_format = "%Y-%m-%d %H:%M:%S", usetz = FALSE) {

  ##  find station IDs
  st <- list()
  if (!is.null(sid)) {
    for (p in seq_along(prm))
      st[[p]] <- paste("sn", sid, ":0", sep = "")
  } else {
    ##  station names
    if (!is.null(station_name)) {
      cat("find station IDs ... ")
      allst  <- jsonlite::fromJSON(
        paste("https://", client_id,
          "@frost.met.no/sources/v0.jsonld?types=SensorSystem",
          sep="")
      )$data
      for (p in seq_along(prm))
        st[[p]] <- allst$id[unlist(lapply(toupper(station_name), grep, allst$name))]
      cat(length(st[[1]]), "found\n")
    } else {
      if (!is.null(country_code)) {
        allst  <- jsonlite::fromJSON(
          paste("https://", client_id,
            "@frost.met.no/sources/v0.jsonld?types=SensorSystem",
            sep="")
        )$data
        for (p in seq_along(prm)) {
          k       <- allst$countryCode %in% country_code
          if (!is.null(station_holder))
            k <- k & sapply(allst$stationHolders, function(u)
              any(toupper(u) %in% toupper(station_holder)))
          st[[p]] <- allst$id[k]
        }
      }
    }
  }

  ##  replace space characters in parameter names
  for (p in seq_along(prm))
    prm[[p]]$name <- gsub(" ", "%20", prm[[p]]$name)

  ##  get data
  cat("get observations:\n")
  for (p in seq_along(prm)) {
    cat("  ", prm[[p]]$name, " ")
    obs   <- list()
    pname <- if (!is.null(names(prm))) names(prm)[p] else prm[[p]]$name
    for (s in st[[p]]) {
      cat(".")
      url0 <- paste("https://", client_id,
        "@api-staging-ext-b.frost.met.no/observations/v0.csv?",
        "sources=", s, "&referencetime=", period[1], "/", period[2],
        "&elements=", prm[[p]]$name,
        if (!is.null(prm[[p]]$lev[1])) "&levels=", prm[[p]]$lev[1],
        if (!is.null(prm[[p]]$res[1])) "&timeresolutions=", prm[[p]]$res[1],
        "&qualities=", quality,
        "&fields=sourceId,referencetime,value", sep = "")
      if (print_url)
        cat(url0, "\n")
      try({
        con      <- url(url0)
        obs[[s]] <- read.table(con, sep = ",", header = TRUE, stringsAsFactors = FALSE,
          row.names = NULL, col.names = c("sid", "time", pname))
        close(con)
      }, silent = TRUE)
    }
    cat(" ok\n")
    if (length(obs) > 0) {
      obs <- do.call("rbind", obs)
      row.names(obs) <- NULL
    }
    out <- if (p == 1) obs else merge(out, obs, all = TRUE)
  }

  ##  add column with station names
  if (add_name) {
    cat("add station names ... ")
    if (!exists("allst"))
      allst  <- jsonlite::fromJSON(
        paste("https://", client_id,
          "@frost.met.no/sources/v0.jsonld?types=SensorSystem",
          sep="")
      )$data
    rownames(allst) <- allst$id
    out <- cbind(sid = out$sid,
      name = allst[toupper(substring(out$sid, 1, nchar(out$sid)-2)), "name"],
      out[, -1], stringsAsFactors = FALSE)
    cat("ok\n")
  }

  ##  change timestamp format
  cat("set timestamp format ... ")
  tm       <- strptime(out$time, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  if (!is.character(time_format))
    out$time <- tm
  else
    out$time <- format(tm, format = time_format, usetz = usetz)
  cat("ok\n")

  if (sid_numeric) {
    cat("convert station IDs to numeric numbers ... ")
    out$sid <- as.numeric( substring(out$sid, 3, nchar(out$sid) - 2) )
    cat("ok\n")
  }

  return(out)
}



## tests
if (FALSE) {
  period <- c("2019-07-01", "2019-07-05")
  sites  <- c(18700, 50540)

  ##  hourly temperature 2m
  miReadFrost(sid = sites, period = period,
    prm = list(T=list(name="air_temperature", lev=2, res="PT1H")))

  ##  hourly temperature 2m and 10m  (does not work!)
  miReadFrost(sid = 18700, period = period,
    prm = list(T2=list(name="air_temperature", lev=2),
      T10=list(name="air_temperature", lev=10)))

  ##  relative humidty 2m
  miReadFrost(sid = sites, period = period,
    prm = list(UU=list(name="relative_humidity", lev=2)))

  ##  wind speed (only time resolution PT1H and PT10M works!)
  miReadFrost(sid = sites, period = period,
    prm = list(FF10 = list(name="wind_speed", lev=10, res="PT10M")))

  ##  wind direction
  miReadFrost(sid = sites, period = period,
    prm = list(DD10 = list(name = "wind_from_direction", lev = 10, res = "PT1H")))
  miReadFrost(sid = sites, period = period,
    prm = list(DD10 = list(name = "wind_from_direction", lev = 10, res = "PT10M")))

  ##  total cloud cover
  miReadFrost(sid = sites, period = period,
    prm = list(TCC = list(name = "cloud_area_fraction")))

  ##  height of cloud base
  miReadFrost(sid = sites, period = period,
    prm = list(HL=list(name="cloud_base_height")))

  ##  1-minute precipitation does not work
  miReadFrost(sid = sites, period = period,
    prm = list(RR=list(name="sum(precipitation_amount PT1M)", res="PT1M")))

  ##  10-minute precipitation
  miReadFrost(sid = sites, period = period,
    prm = list(RR=list(name="sum(precipitation_amount PT10M)", res="PT10M")))

  ##  hourly precipitation
  miReadFrost(sid = sites, period = period,
    prm = list(RR=list(name="sum(precipitation_amount PT1H)", res="PT1H")))

  ##  6-hour precipitation does not work!
  miReadFrost(sid = sites, period = period,
    prm = list(RR=list(name="sum(precipitation_amount PT6H)", res="PT6H")))

  ##  12-hour precipitation
  miReadFrost(sid = sites, period = period,
    prm = list(RR=list(name="sum(precipitation_amount PT12H)", res="PT12H")))

  ##  daily precipitation
  miReadFrost(sid = sites, period = period,
    prm = list(RR=list(name="sum(precipitation_amount P1D)")))

  ##  #minutes of precipitation last hour (does not work!)
  miReadFrost(sid = sites, period = period,
    prm = list(RR=list(name="sum(duration_of_precipitation P1H)", res="P1H")))

  ##  #minutes of precipitation last 24 hours (does not work!)
  miReadFrost(sid = sites, period = period,
    prm = list(RR=list(name="sum(duration_of_precipitation P1D)", res="P1D")))

  ##  max gust last hour
  miReadFrost(sid = sites, period = period,
    prm = list(FG=list(name="max(wind_speed_of_gust PT1H)", lev=10, res="PT1H")))

  ##  mean sea level pressure
  miReadFrost(sid = sites, period = period,
    prm = list(PR=list(name="air_pressure_at_sea_level", res="PT1H")))

  ##  sunshine duration (#minutes last hour)
  miReadFrost(sid = sites, period = period,
    prm = list(OT=list(name="sum(duration_of_sunshine PT1H)", res="PT1H")))

  ##  snow depth
  miReadFrost(sid = sites, period = c("2019-01-01", "2019-01-10"),
    prm = list(SA=list(name="surface_snow_thickness", res="PT1H")))

  ##  weather type
  miReadFrost(sid = sites, period = period, prm = list(WW=list(name="weather_type")))

  ##  several parameters
  miReadFrost(sid = sites, period = period,
    prm = list(
      FF10 = list(name="wind_speed", lev=10, res="PT1H"),
      T2   = list(name="air_temperature", lev=2, res="PT1H"),
      MSLP = list(name="air_pressure_at_sea_level", res="PT1H")))

  ##  stations specified by names
  miReadFrost(sid = NULL, period = period, station_name = c("Oslo", "BErgEN"),
    prm = list(RR1=list(name="sum(precipitation_amount PT1H)", res="PT1H")),
    add_name = TRUE)

  a <- miReadFrost(sid = NULL, period = period, station_name = c("Oslo", "BErgEN"),
    prm = list(T=list(name="air_temperature", res="PT1H")),
    add_name = TRUE)

  a <- miReadFrost(sid = NULL, period = period, station_name = c("Oslo", "BErgEN"),
    prm = list(T=list(name="air_temperature", res="PT1H"),
      RR1=list(name="sum(precipitation_amount PT1H)", res="PT1H")),
    add_name = TRUE)

}


miReadFrostStations <- function(prm = NULL) {
  if (is.null(prm)) {
    st  <- jsonlite::fromJSON(paste("https://", client_id,
      "@frost.met.no/sources/v0.jsonld?types=SensorSystem", sep=""))$data
    k   <- apply(is.na(st$geometry), 1, any)
    st  <- st[!k, ]
    st$longitude      <- unlist(sapply(st$geometry$coordinates, function(u) u[1]))
    st$latitude       <- unlist(sapply(st$geometry$coordinates, function(u) u[2]))
    st$stationHolders <- sapply(st$stationHolders, paste, collapse = ";")
    st$icaoCodes      <- sapply(st$icaoCodes, paste, collapse = ";")
    st$shipCodes      <- sapply(st$shipCodes, paste, collapse = ";")
    st$externalIds    <- sapply(st$externalIds, paste, collapse = ";")
    st                <- st[, -match(c("@type","geometry"), names(st))]
  } else {

  }
  return(st)
}
















