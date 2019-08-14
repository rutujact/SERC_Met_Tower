# First function of total two:
# ---
# title: "SERC_minute_data_compiled_annually"
# author: "Rutuja"
# Date: "6/19/2017"
# ---

# rm(list = ls())

## currently only set to get minute by year for the running year; because minute by year files for previous are already present
# year <- 2019
# from.copy.path <- "/Users/rutuja/Dropbox (Smithsonian)/Work_at_SERC/Data/SERC_Met_data/MET_TOWER/"
fun.get_minute_by_year <- function(year = year, from.copy.path = from.copy.path) {
  library(tidyverse)
  all.files0 <- list.files(path =
                             paste0(from.copy.path, year, "/Minute"),
                           pattern = (".txt"),
                           recursive = FALSE,
                           full.names = TRUE)
  ## creating local copy from MetTower folder where it is downloaded by Jess Shue from the Photobio lab shared folder

  to.path <- paste("data-raw/downloaded_data/", year, "/Min", sep = "")
  if (!dir.exists(to.path)) {dir.create(to.path)}
  file.copy(from = all.files0, to = to.path, overwrite = TRUE, recursive = TRUE)

  all.files <- list.files(path =
                            to.path,
                          pattern = (".txt"),
                          recursive = FALSE,
                          full.names = TRUE)
  str(all.files)

  total = length(all.files)
  # create progress bar
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  for (i in 1:length(all.files)) {
    new.file <- read.csv(
      file.path(all.files[i]),
      skip = 56,
      # has metadata
      na.strings = c("NA", ""),
      header = F,
      row.names = NULL,
      check.names = F
    )
    if (i == 1) {
      dat <- new.file
    } else {
      if(ncol(new.file) != ncol(dat)){
        df.NA <- setNames(data.frame(matrix(ncol = ncol(dat), nrow = nrow(new.file))), colnames(dat))
        new.file.mod <- cbind.data.frame(new.file, df.NA[, ((ncol(new.file)+1) : ncol(dat))])
        dat <- rbind.data.frame(dat, new.file.mod)
      } else {
        dat <- rbind.data.frame(dat, new.file)
      }
    }
    Sys.sleep(0.1)
    # update progress bar
    setTxtProgressBar(pb, i)
  }

  head(dat)
  tail(dat)
  write.csv(dat, paste0("data-raw/minute_by_year/Minute_data_", year, ".csv"), row.names = T)
  # row.names are present for older files, so not changing
  print(paste0("Last doy of the 'Minute_data_", year, ".csv' file : ", dat$V4[nrow(dat)]))
}

### Second function of total two:

# ---
# title: "SERC_minute_data_by_year_into_one"
# author: "Rutuja"
# Date: "7/19/2019"
# ---

fun.minute_by_year_into_one_file <- function(year = year){
  max.prev.years <- year - 1
  prev.years <- c(2003:max.prev.years)
  curr.years <- c(year)
  datayears <- c(prev.years, curr.years)
  ### previous years data need not be collated in each run, so collating and storing away--------------
  # #loading metdata and then converting into desired units and Variable names
  # total <- length(prev.years)
  # # create progress bar
  # pb <- txtProgressBar(min = 0, max = total, style = 3)
  # for (i in prev.years) {
  #   met.new1   <-
  #     read.csv(
  #       file.path(
  #         "data-raw/minute_by_year",
  #         paste("Minute_data_", i, ".csv", sep = "")
  #       ),
  #       na.strings = c("NA", ""),
  #       header = T,
  #       row.names = NULL,
  #       check.names = F
  #     )
  #   met.new2 <-
  #     subset(met.new1,
  #            select = c("V3",  # Year
  #                       "V4",  # Julian Day
  #                       "V5",  # Hour
  #                       "V18", # Temp forest floor
  #                       "V19", # RH forest floor
  #                       "V20", # Rs forest floor
  #                       "V12", # Precip
  #                       "V6",  # wind speed, at tower-top per Pat Neale
  #                       "V11", # Bp, at tower-top per Pat Neale
  #                       "V15", # Temp top of tower, forest floor sensor was bad and has incorrect measurements, apparently
  #                       "V16", # RH top of the tower. Forest floor also available, but would be unrepresentative for canopy evaporation
  #                       "V17"))# Rs top of the tower. Forest floor also available, but would be unrepresentative
  #   colnames(met.new2) = c(
  #     "Year",
  #     "Julian",
  #     "Hour",
  #     "Temp",
  #     "RH",
  #     "Rs",
  #     "Precip",
  #     "uz",
  #     "Bp",
  #     "Temp.tower",
  #     "RH.tower",
  #     "Rs.tower"
  #   )
  #   if (i == prev.years[1])
  #     met0 <- met.new2
  #   else
  #     met0 <- rbind(met0, met.new2)
  #   Sys.sleep(0.1)
  #   # update progress bar
  #   setTxtProgressBar(pb, i)
  # }
  #
  # save(met0, file = paste("data-raw/minute_data_", paste(min(prev.years),
  #                                                        max(prev.years), sep = "-"), ".Rda", sep = ""))
  # write.csv(met0,
  #         paste("data-raw/minute_data_", paste(min(prev.years), max(prev.years), sep = "-"), ".csv", sep = ""), row.names = F)


  ### Collating minute data for current years---------------------
  #loading metdata and then converting into desired units and Variable names
  total <- length(curr.years)
  # # create progress bar
  # pb <- txtProgressBar(min = 0, max = total, style = 3)
  for (i in curr.years) {
    met.new1   <-
      read.csv(
        file.path(
          "data-raw/minute_by_year",
          paste("Minute_data_", i, ".csv", sep = "")
        ),
        na.strings = c("NA", ""),
        header = T,
        row.names = NULL,
        check.names = F
      )
    met.new1 <- data.frame(apply(met.new1, 2, as.numeric))
    met.new2 <-
      met.new1 %>%
      select(c("V3",  # Year
               "V4",  # Julian Day
               "V5",  # Hour
               "V18", # Temp forest floor
               "V19", # RH forest floor
               "V20", # Rs forest floor
               "V12", # Precip
               "V6",  # wind speed, at tower-top per Pat Neale
               "V11", # Bp, at tower-top per Pat Neale
               "V15", # Temp top of tower, forest floor sensor was bad and has incorrect measurements, apparently
               "V16", # RH top of the tower. Forest floor also available, but would be unrepresentative for canopy evaporation
               "V17"))# Rs top of the tower. Forest floor also available, but would be unrepresentative
    colnames(met.new2) = c(
      "Year",
      "Julian",
      "Hour",
      "Temp",
      "RH",
      "Rs",
      "Precip",
      "uz",
      "Bp",
      "Temp.tower",
      "RH.tower",
      "Rs.tower"
    )
    if (i == curr.years[1])
      met0.1 <- met.new2
    else
      met0.1 <- bind_rows(met0.1, met.new2)
    Sys.sleep(0.1)
    # # update progress bar
    # setTxtProgressBar(pb, i)
  }
  ## binding with previous years minute data
  load(file = paste("data-raw/minute_data_", paste(min(prev.years),
                                                   max(prev.years), sep = "-"), ".Rda", sep = ""))
  met2 <- rbind(met1, met0.1)
  save(met2, file = paste("data-raw/minute_data_", paste(min(prev.years),
                                                         max(curr.years), sep = "-"), ".Rda", sep = ""))
 print(paste0("Last doy of the 'minute_data_", paste(min(prev.years),
                                                         max(curr.years), sep = "-"), ".Rda' file : ", met2$Julian[nrow(met2)]))
}
