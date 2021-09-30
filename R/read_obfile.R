
read_obfile <- function(
  v_file_name,
  members       = NA,
  lead_time     = NA,
  v_type        = "obsoul",
  country = country,
  missing_value = -99.0
) {


v_type <- match.arg(v_type)

  data_type <- switch(
    v_type,
    "vfld" = "fcst",
    "obsoul" = "obs"
  )


v_metadata <- scan(v_file_name,nmax = 4)
names <- c(1:50)

obsoul_type <- v_metadata[4]

if (obsoul_type == 1){

	print("SYNOP")

}else if (obsoul_type == 2){

	print("AIREP")

}else if (obsoul_type == 3){

	print("SATOB")

}else{

	warning("Unable to read: ", v_file_name, "\nv version = ", v_version, "\n", call. = FALSE, immediate. = TRUE)
    close(file_connection)
	return(NULL)
}

params_synop <- data.frame(
      parameter        = character(),
      accum_hours      = integer(),
      units            = character(),
      stringsAsFactors = FALSE
    )

params_synop    <- data.frame(
        parameter        = v_default_names_obs("obsoul"),
        accum_hours      = 0,
        stringsAsFactors = FALSE
      )

params_synop <- dplyr::mutate(
      params_synop,
      parameter   = purrr::map(.data$parameter, parse_v_parameter_synop),
      units       = purrr::map_chr(.data$parameter, "param_units"),
      parameter   = purrr::map_chr(.data$parameter, "harp_param")
    )


df <- read.table(v_file_name, fill = TRUE, strip.white = TRUE, col.names = names)


df <- df[-1,]

df <- df %>% rename(SID=X6,lat=X4,lon=X5,date=X7,hour=X8,elev=X9,
                    var1_1=X13,var1_2=X14,var1_3=X15,var1_4=X16,var1_5=X17,
                    var2_1=X18,var2_2=X19,var2_3=X20,var2_4=X21,var2_5=X22, 
                    var3_1=X23,var3_2=X24,var3_3=X25,var3_4=X26,var3_5=X27,
                    var4_1=X28,var4_2=X29,var4_3=X30,var4_4=X31,var4_5=X32,
                    var5_1=X33,var5_2=X34,var5_3=X35,var5_4=X36,var5_5=X37,
                    var6_1=X38,var6_2=X39,var6_3=X40,var6_4=X41,var6_5=X42,
                    var7_1=X43,var7_2=X44,var7_3=X45,var7_4=X46,var7_5=X47)

df <- df %>% select(-contains("X"))

data <- df %>% select(

                     SID,lat,lon,elev,
                     var1_1,var1_2,var1_3,var1_4,var1_5,
                     var2_1,var2_2,var2_3,var2_4,var2_5,
                     var3_1,var3_2,var3_3,var3_4,var3_5,
                     var4_1,var4_2,var4_3,var4_4,var4_5,
                     var5_1,var5_2,var5_3,var5_4,var5_5,
                     var6_1,var6_2,var6_3,var6_4,var6_5,
                     var7_1,var7_2,var7_3,var7_4,var7_5 ) %>%

      mutate(T2m =  ifelse(var1_1 == 39,var1_4,
                    ifelse(var2_1 == 39,var2_4,
                    ifelse(var3_1 == 39,var3_4,
                    ifelse(var4_1 == 39,var4_4,
                    ifelse(var5_1 == 39,var5_4,
                    ifelse(var6_1 == 39,var6_4,
                    ifelse(var7_1 == 39,var7_4,"NA")))))))) %>%

      mutate(RH2m = ifelse(var1_1 == 58,var1_4,
                    ifelse(var2_1 == 58,var2_4,
                    ifelse(var3_1 == 58,var3_4,
                    ifelse(var4_1 == 58,var4_4,
                    ifelse(var5_1 == 58,var5_4,
                    ifelse(var6_1 == 58,var6_4,
                    ifelse(var7_1 == 58,var7_4,"NA")))))))) %>%
    
      mutate(Q2m =  ifelse(var1_1 == 7,var1_4,
                    ifelse(var2_1 == 7,var2_4,
                    ifelse(var3_1 == 7,var3_4,
                    ifelse(var4_1 == 7,var4_4,
                    ifelse(var5_1 == 7,var5_4,
                    ifelse(var6_1 == 7,var6_4,
                    ifelse(var7_1 == 7,var7_4,"NA")))))))) %>%
        
      mutate(S10m = ifelse(var1_1 == 41,var1_3,
                    ifelse(var2_1 == 41,var2_3,
                    ifelse(var3_1 == 41,var3_3,
                    ifelse(var4_1 == 41,var4_3,
                    ifelse(var5_1 == 41,var5_3,
                    ifelse(var6_1 == 41,var6_3,
                    ifelse(var7_1 == 41,var7_3,"NA")))))))) %>%  
    
      mutate(D10m =  ifelse(var1_1 == 41,var1_4,
                    ifelse(var2_1 == 41,var2_4,
                    ifelse(var3_1 == 41,var3_4,
                    ifelse(var4_1 == 41,var4_4,
                    ifelse(var5_1 == 41,var5_4,
                    ifelse(var6_1 == 41,var6_4,
                    ifelse(var7_1 == 41,var7_4,"NA")))))))) 


data <- data %>% select(SID,lat,lon,elev,T2m,RH2m,Q2m,D10m,S10m)

switch(country,

'hu' = {
    data$SID <- str_remove_all(data$SID,"[HU]")
    data$SID <- paste("93",data$SID,sep='')
    data$SID <- gsub(" ", "", data$SID, fixed = TRUE)
    
  
},

'at' = {
    data$SID <- str_remove_all(data$SID,"[AT]")
    data$SID <- paste("90",data$SID,sep='')
    data$SID <- gsub(" ", "", data$SID, fixed = TRUE)
    
     
},

'cz' = {
    data$SID <- str_remove_all(data$SID,"[CZ]")
    data$SID <- paste("92",data$SID,sep='')
    data$SID <- gsub(" ", "", data$SID, fixed = TRUE)
    
    
},

'cr' = {
    data$SID <- str_remove_all(data$SID,"[CR]")
    data$SID <- paste("91",data$SID,sep='')
    data$SID <- gsub(" ", "", data$SID, fixed = TRUE)
    
    
    
},

'pl' = {
    data$SID <- str_remove_all(data$SID,"[PL]")
    data$SID <- paste("94",data$SID,sep='')
    data$SID <- gsub(" ", "", data$SID, fixed = TRUE)
    
        
},

'ro' = {
    data$SID <- str_remove_all(data$SID,"[RO]")
    data$SID <- paste("95",data$SID,sep='')
    data$SID <- gsub(" ", "", data$SID, fixed = TRUE)
    
    
},

'si' = {
    data$SID <- str_remove_all(data$SID,"[SI]")
    data$SID <- paste("96",data$SID,sep='')
    data$SID <- gsub(" ", "", data$SID, fixed = TRUE)
    
},

'sk' = {
    data$SID <- str_remove_all(data$SID,"[SK]")
    data$SID <- paste("97",data$SID,sep='')
    data$SID <- gsub(" ", "", data$SID, fixed = TRUE)
    
}

)

data$SID <- as.numeric(as.character(data$SID))

list(synop = data, synop_params = params_synop)

}


v_default_names_obs <- function(data_type) {
  if(data_type == "obsoul"){
    c("TT","RH","QQ","DD","FF")
  } else {
    NA_character_
  }
}
