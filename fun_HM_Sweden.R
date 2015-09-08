################################################################################
#                                                                              #
#          Highmaps-function for drawing a map of Sweden with counts           #
#                                                                              #
################################################################################

HM_Sweden <- function(
  
  ############################ Obligatory parameters #############################
  df, # This parameter specifies the dataframe containing your data
  lkf, # This parameter specifies the variable containing the LKF-code. Acceptable range is 4-6 characters.
  region, # This parameter specifies the variable containing the regions name.
  
  ############################ Optionally parameters #############################
  title = "", # The title of the Highmap
  subtitle = "", # The subtitle of the Highmap
  tooltip = "Antal fall:", # The text before the value in the tooltip
  map_color = "#00b3f6", # The color of the regions/municipals while not highlighted
  hover_color = "#FFB117", # The color of the regions/municipals while highlighted
  show_value = FALSE, # Should the municipal/region value be showned in the chart?
  
  ###### Parameters for the path to the javascript and hardcoded HTML file #######
  script_municipal_code = "D:/R-Scripts/Väst/Oc5stafch/Highmaps/kommuner.csv", # Source path for the file containing the list of municipals name and code used to match the data from INCA
  script_part1_code = "D:/R-Scripts/Väst/Oc5stafch/Highmaps/del1.txt", # Source path for the Text-file containing the highmaps function as well as container call
  script_jquery = "/Public/Files/Gynregistret/Highmaps/Antal/js/jquery.min.js", # Source path for the jquery.js
  script_highmaps = "/Public/Files/Gynregistret/Highmaps/Antal/js/highmaps.js", # Source path for the highmaps.js
  script_drilldown = "/Public/Files/Gynregistret/Highmaps/Antal/js/drilldown.js", # Source path for the drilldown.js
  
  ################## Parameters for the path to the coordinates ##################
  script_maps_swe = "/Public/Files/Gynregistret/Highmaps/Antal/maps/swe.js", # Source path for the coordinates for the map of Sweden
  script_maps_vast = "/Public/Files/Gynregistret/Highmaps/Antal/maps/vast.js", # Source path for the coordinates for Region Väst
  script_maps_norr = "/Public/Files/Gynregistret/Highmaps/Antal/maps/norr.js", # Source path for the coordinates for Region Norr
  script_maps_syd = "/Public/Files/Gynregistret/Highmaps/Antal/maps/syd.js", # Source path for the coordinates for Region Syd
  script_maps_so = "/Public/Files/Gynregistret/Highmaps/Antal/maps/so.js", # Source path for the coordinates for Region Sydost
  script_maps_u = "/Public/Files/Gynregistret/Highmaps/Antal/maps/u.js", # Source path for the coordinates for Region Väst
  script_maps_sg = "/Public/Files/Gynregistret/Highmaps/Antal/maps/sg.js" # Source path for the coordinates for Region Stockholm/Gotland
){
  
  ########################## Loading required packages ###########################
  library(reshape2)
  library(jsonlite)
  
  ################# Set the variables defined by the parameters ##################
  if(!(missing(lkf))) df$lkf <- df[[lkf]] else stop('The variable for the lkf-code is not specified')
  if(is.null(df$lkf)) stop('The variable you have specified for the lkf-code is not valid')
  if(!(missing(region))) df$region <- df[[region]] else stop('The variable for the region is not specified')
  if(is.null(df$region)) stop('The variable you have specified for the region is not valid') 
  
  ################# Scan the neccessary files from the R-server ##################
  kommuner <- read.csv2(script_municipal_code, sep = ";", stringsAsFactors = FALSE,colClasses="character", fileEncoding = "UTF-8")
  del1 <- scan(script_part1_code, what="", sep="\n", quiet=TRUE, fileEncoding = "UTF-8")
  
  ##################### Adjustments to the LKF-code variable #####################
  df$lkf <- as.character(df$lkf)
  df$lkf[nchar(df$lkf)== 5] <- paste0("0", df$lkf)[nchar(df$lkf)== 5]
  df$lkf[nchar(df$lkf)== 3] <- paste0("0", df$lkf)[nchar(df$lkf)== 3]
  df$lkf <- substring(df$lkf,1,4)
  # Only keep valid format of LKF, i.e. nchar = 6
  df <- subset(df, lkf %in% kommuner$KNKOD)

  ################ Subset the data to only include valid regions #################
  df <- subset(df, region != "" & region != "Region Demo")
  

  
  ########################  Create optionally paramaeter #########################
  ## Title/Subtitle
  title <- paste0("var title = '", title, "';")
  subtitle <- paste0("var subtitle = '", subtitle, "';")
  ## Tooltip
  tooltip <- paste0("var tooltip = '", tooltip, "';")
  ## Colors
  map_color <- paste0("var map_color = '", map_color, "';")
  hover_color <- paste0("var hover_color = '", hover_color, "';")
  ## Value in datalabels
  if(!(show_value %in% c(TRUE,FALSE))) stop('The show_value parameter must be logical, i.e. TRUE or FALSE')
  if(show_value == TRUE) show_value <- paste0("var show_value = '{point.properties.name}: {point.value}';")
  if(show_value == FALSE) show_value <- paste0("var show_value = '{point.properties.name}';")
  
  
  
  
  ############# Creating an array with the value for the six regions #############
  dfswe <- subset(df,, c("region"))  
  dfswe$region <- droplevels(dfswe$region)
  dfswe <- dcast(dfswe, region ~ ..., drop = FALSE, fun.aggregate = length, value.var= "region")
  names(dfswe) <- c("code", "value")
  dfswe$code <- as.character(dfswe$code)
  dfswe$drilldown <- ""
  dfswe$drilldown[dfswe$code == "Region Norr"] <- "n"
  dfswe$drilldown[dfswe$code == "Region Sthlm/Gotland"] <- "sg"
  dfswe$drilldown[dfswe$code == "Region Syd"] <- "s"
  dfswe$drilldown[dfswe$code == "Region Sydöstra"] <- "so"
  dfswe$drilldown[dfswe$code == "Region Uppsala/Örebro"] <- "u"
  dfswe$drilldown[dfswe$code == "Region Väst"] <- "v"
  dfswe$code[dfswe$code == "Region Norr"] <- "Norr"
  dfswe$code[dfswe$code == "Region Sthlm/Gotland"] <- "Stockholm/Gotland"
  dfswe$code[dfswe$code == "Region Syd"] <- "Syd"
  dfswe$code[dfswe$code == "Region Sydöstra"] <- "Sydost"
  dfswe$code[dfswe$code == "Region Uppsala/Örebro"] <- "Uppsala/Örebro"
  dfswe$code[dfswe$code == "Region Väst"] <- "Väst"
  dfswe <- data.frame(drilldown = dfswe$drilldown, code = dfswe$code, value = dfswe$value)
  swedata <- paste("var","swedata","=",toJSON(dfswe))
  
  
  
  ################################################################################
  #                                                                              #
  #      Creating an array with values for Region väst on a municipal level      #
  #                                                                              #
  ################################################################################
  
  dfv <- subset(df, substring(df$lkf,1,2) == 14 | substring(df$lkf,1,4) %in% c(1382,1383,1384), c("lkf"))
  dfv <- dcast(dfv, lkf ~ ..., drop = FALSE, fun.aggregate = length, value.var = "lkf")
  names(dfv) <- c("lkf", "antal")
  
  #Ta fram delmängd
  dfv1 <- subset(kommuner, substr(KNKOD,1,2) == 14 | KNKOD %in% c(1382,1383,1384))
  dfv1$value <- 0
  dfv1$value[match(dfv$lkf, dfv1$KNKOD)] <- dfv$antal
  
  #skapa dataframe med variabler samt exporterar till java
  dfv1.id <- dfv1$KNNAMN
  dfv1 <- data.frame(code = dfv1.id, value = dfv1$value)
  vastdata <- paste("var","vastdata","=",toJSON(dfv1))
  
  
  
  ################################################################################
  #                                                                              #
  #      Creating an array with values for Region syd on a municipal level       #
  #                                                                              #
  ################################################################################
  
  dfv <- subset(df, substring(df$lkf,1,2) %in% c(12,13,10,"07") & !(substring(df$lkf,1,4) %in% c(1382,1383,1384)), c("lkf"))
  dfv <- dcast(dfv, lkf ~ ..., drop = FALSE, fun.aggregate = length, value.var = "lkf")
  names(dfv) <- c("lkf", "antal")
  
  #Ta fram delmängd
  dfv1 <- subset(kommuner, substr(KNKOD,1,2) %in% c(12,13,10,"07") & !(KNKOD %in% c(1382,1383,1384)))
  dfv1$value <- 0
  dfv1$value[match(dfv$lkf, dfv1$KNKOD)] <- dfv$antal
  
  #skapa dataframe med variabler samt exporterar till java
  dfv1.id <- dfv1$KNNAMN
  dfv1 <- data.frame(code = dfv1.id, value = dfv1$value)
  syddata <- paste("var","syddata","=",toJSON(dfv1))
  
  
  
  ################################################################################
  #                                                                              #
  #     Creating an array with values for Region sydost on a municipal level     #
  #                                                                              #
  ################################################################################
  
  dfv <- subset(df, substring(df$lkf,1,2) %in% c("05","06","08"), c("lkf"))
  dfv <- dcast(dfv, lkf ~ ..., drop = FALSE, fun.aggregate = length, value.var = "lkf")
  names(dfv) <- c("lkf", "antal")
  
  #Ta fram delmängd
  dfv1 <- subset(kommuner, substr(KNKOD,1,2) %in% c("05","06","08"))
  dfv1$value <- 0
  dfv1$value[match(dfv$lkf, dfv1$KNKOD)] <- dfv$antal
  
  #skapa dataframe med variabler samt exporterar till java
  dfv1.id <- dfv1$KNNAMN
  dfv1 <- data.frame(code = dfv1.id, value = dfv1$value)
  sodata <- paste("var","sodata","=",toJSON(dfv1))
  
  
  ################################################################################
  #                                                                              #
  # Creating an array with values for Region Uppsala/Örebro on a municipal level #
  #                                                                              #
  ################################################################################
  
  dfv <- subset(df, substring(df$lkf,1,2) %in% c("03","04",17,18,19,20,21), c("lkf"))
  dfv <- dcast(dfv, lkf ~ ..., drop = FALSE, fun.aggregate = length, value.var = "lkf")
  names(dfv) <- c("lkf", "antal")
  
  #Ta fram delmängd
  dfv1 <- subset(kommuner, substr(KNKOD,1,2) %in% c("03","04",17,18,19,20,21))
  dfv1$value <- 0
  dfv1$value[match(dfv$lkf, dfv1$KNKOD)] <- dfv$antal
  
  #skapa dataframe med variabler samt exporterar till java
  dfv1.id <- dfv1$KNNAMN
  dfv1 <- data.frame(code = dfv1.id, value = dfv1$value)
  udata <- paste("var","udata","=",toJSON(dfv1))
  
  
  
  
  ################################################################################
  #                                                                              #
  # Creating an array with values for Region Stockholm/Gotland on a municipal    #
  # level                                                                        #
  #                                                                              #
  ################################################################################
  
  dfv <- subset(df, substring(df$lkf,1,2) %in% c("01","09"), c("lkf"))
  dfv <- dcast(dfv, lkf ~ ..., drop = FALSE, fun.aggregate = length, value.var = "lkf")
  names(dfv) <- c("lkf", "antal")
  
  #Ta fram delmängd
  dfv1 <- subset(kommuner, substr(KNKOD,1,2) %in% c("01","09"))
  dfv1$value <- 0
  dfv1$value[match(dfv$lkf, dfv1$KNKOD)] <- dfv$antal
  
  #skapa dataframe med variabler samt exporterar till java
  dfv1.id <- dfv1$KNNAMN
  dfv1 <- data.frame(code = dfv1.id, value = dfv1$value)
  sgdata <- paste("var","sgdata","=",toJSON(dfv1))
  
  
  
  ################################################################################
  #                                                                              #
  #      Creating an array with values for Region Norr on a municipal level      #
  #                                                                              #
  ################################################################################
  
  dfv <- subset(df, substring(df$lkf,1,2) %in% c(22,23,24,25), c("lkf"))
  dfv <- dcast(dfv, lkf ~ ..., drop = FALSE, fun.aggregate = length, value.var = "lkf")
  names(dfv) <- c("lkf", "antal")
  
  #Ta fram delmängd
  dfv1 <- subset(kommuner, substr(KNKOD,1,2) %in% c(22,23,24,25))
  dfv1$value <- 0
  dfv1$value[match(dfv$lkf, dfv1$KNKOD)] <- dfv$antal
  
  #skapa dataframe med variabler samt exporterar till java
  dfv1.id <- dfv1$KNNAMN
  dfv1 <- data.frame(code = dfv1.id, value = dfv1$value)
  norrdata <- paste("var","norrdata","=",toJSON(dfv1))
  
  
  
  
  
  ################################################################################
  #                                                                              #
  #                 Merge the arrays and create the HTML object                  #
  #                                                                              #
  ################################################################################
  
  
  
  #################################### Intro #####################################
  
  intro <- paste0("
                  <!DOCTYPE HTML> 
                  <HTML> 
                  <HEAD> 
                  <meta http-equiv='content-type' content='text/html; charset=UTF-8'>"
  )
  
  middle_script <- paste0("
                          <script src=",script_jquery,"></script> 
                          <script src=",script_highmaps,"></script> 
                          <script src=",script_drilldown,"></script> 
                          <script src=",script_maps_swe,"></script> 
                          <script src=",script_maps_vast,"></script> 
                          <script src=",script_maps_norr,"></script> 
                          <script src=",script_maps_syd,"></script> 
                          <script src=",script_maps_so,"></script> 
                          <script src=",script_maps_u,"></script> 
                          <script src=",script_maps_sg,"></script>                        
                          "
  )
  
  middle_data <- paste0("\n",
                        title, "\n",
                        subtitle, "\n",
                        map_color, "\n",
                        tooltip, "\n",
                        hover_color, "\n",
                        show_value, "\n",
                        swedata, "\n",
                        vastdata, "\n",
                        syddata, "\n",
                        sodata, "\n",
                        sgdata, "\n",
                        udata, "\n",
                        norrdata, "\n"
  )
  
  
  outfile <- file("./output.html","w",encoding="UTF-8")
  cat(paste("\n",intro) ,file=outfile,append=TRUE)
  cat(paste("\n",middle_script) ,file=outfile,append=TRUE)
  cat(paste("\n",del1) ,file=outfile,append=TRUE)
  cat(paste("\n",middle_data) ,file=outfile,append=TRUE)
  cat(paste("\n","</script> </body> </html>") ,file=outfile,append=TRUE)
  close(outfile)
  
  
  
}