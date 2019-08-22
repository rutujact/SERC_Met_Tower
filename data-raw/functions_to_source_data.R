# ---
# title: "SERC_minute_data_compiled_annually"
# author: "Rutuja"
# Date: "6/19/2017"
# ---

# rm(list = ls())
# -------------------------------
# First function of total two----
# -------------------------------
## currently only set to get minute by year for the running year; because minute by year files for previous are already present
# year <- 2019
# from.copy.path <- "/Users/rutuja/Dropbox (Smithsonian)/Work_at_SERC/Data/SERC_Met_data/MET_TOWER/"
fun.get_minute_by_year <- function(year = year, from.copy.path = from.copy.path, old.years = old.years) {
  if (!require("pacman")) install.packages("pacman"); library(pacman)
  pacman::p_load(tidyverse, data.table, rlist)

  if(old.years == TRUE) {
    years.on = c(2003:year)
    } else {
    years.on = year
    }
  ###------
  ## copying all minute files from MET_TOWER folder to data-raw---------
  ###------
  all.files <- list() # to store local minute files by year
  for (i in 1: length(years.on)) {
    # Minute data folder by year in the MET_TOWER data have all kinds of folder names: Min, Minute, min.
    # So renaming to Min
    old.dir1 <- paste0(from.copy.path, years.on[i], "/min")
    old.dir2 <- paste0(from.copy.path, years.on[i], "/Minute")
    if (dir.exists(old.dir1)) {
      file.rename(from = old.dir1, to = paste0(from.copy.path, years.on[i], "/Min"))
    } else
      if (dir.exists(old.dir2)) {
        file.rename(from = old.dir2, to = paste0(from.copy.path, years.on[i], "/Min"))
      }
    # list all minute files
    all.files0 <- list.files(path =
                               paste0(from.copy.path, years.on[i], "/Min"),
                             pattern = (".txt"),
                             recursive = FALSE,
                             full.names = TRUE)
     ## creating local copy from MetTower folder where it is downloaded by Jess Shue from the Photobio lab shared folder
    # create year folder
    to.path.year <- paste0("data-raw/downloaded_data/", years.on[i])
    if (!dir.exists(to.path.year)) {dir.create(to.path.year)}
    # create minute subfolder
    to.path.min <- paste0("data-raw/downloaded_data/", years.on[i], "/Min")
    if (!dir.exists(to.path.min)) {dir.create(to.path.min)}
    # copy files
    file.copy(from = all.files0, to = to.path.min, overwrite = TRUE, recursive = TRUE)

    all.files <- list.files(path =
                              to.path.min,
                            pattern = (".txt"),
                            recursive = FALSE,
                            full.names = TRUE)
  }

  ###------
  ## collating all minute data by year --------
  ###------
  ## as there are 24 columns to be read in
  cols.to.read <- 24
  for (i in 1: length(years.on)){
    if (length(years.on) == 1) {
      total <- length(all.files)
    } else {
      total <- length(all.files[[i]])
    }
  # create progress bar
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  dat <- setNames(data.frame(matrix(ncol = cols.to.read, nrow = 0)), paste0("V", 1:cols.to.read))
  for (j in 1:total) {
    if (total == 1) {
      all_content <- readLines(all.files[j])
    } else {
      all_content <- readLines(all.files[[i]][j])
    }
    ## those lines that start with "51,21" for 0.  	Campbell program line number (xxx) 1.  	Campbell Program ID  (xxx)
    ## all files data have these values in column 1 and 2 (or 0 and 1 as above)
    #keep <- all_content[c(grep("51,21,", all_content, fixed = TRUE))]
    keep <- all_content[c(stringi::stri_startswith_fixed(str = all_content, pattern = "51,21,"))]
    ## accept only those rows that have all cols.to.read
    all.cols <- which(sapply(strsplit(keep, ","), length) == cols.to.read)
    if(length(all.cols > 0)) {
      input <- read.table(textConnection(keep[all.cols]), sep = ",")
      dat <- rbind(dat, input)
    }
   Sys.sleep(0.1)
    # update progress bar
  setTxtProgressBar(pb, j)
  }
  write.table(dat, paste0("data-raw/minute_by_year/Minute_data_", year, ".txt"), row.names = T)
  # row.names are present for older files, so not changing
  print(paste0("Last doy of the 'Minute_data_", years.on[i], ".txt' file : ", dat$V4[nrow(dat)]))
  print(years.on[i])
  }
}

# -------------------------------
# Second function of total two---
# -------------------------------
fun.minute_by_year_into_one_file <- function(year = year, old.years = old.years){
  if (!require("pacman")) install.packages("pacman"); library(pacman)
  pacman::p_load(tidyverse)

  max.prev.years <- year - 1
  prev.years <- c(2003:max.prev.years)
  curr.years <- c(year)
  datayears <- c(prev.years, curr.years)
  # --------------------------------------------------------------
  ## Collating previous years data into one file when old.years is turned to TRUE------------------
  ## Previous years data need not be collated in each run, so collating and storing away--------------
  # --------------------------------------------------------------
  if (old.years == TRUE){
    #loading metdata and then converting into desired units and Variable names
    # total <- length(prev.years)
    # create progress bar
    # pb <- txtProgressBar(min = 0, max = total, style = 3)
    for (i in prev.years) {
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
      met.new2 <-
        subset(met.new1,
               select = c("V3",  # Year
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
      if (i == prev.years[1]){
        met0 <- met.new2
        } else {
        met0 <- rbind.data.frame(met0, met.new2)
        }
      # Sys.sleep(0.1)
      # # update progress bar
      # setTxtProgressBar(pb, i)
    }
    met0 <- met0 %>% mutate_all(as.numeric)
    save(met0, file = paste("data-raw/minute_data_", paste(min(prev.years),
                                                           max(prev.years), sep = "-"), ".Rda", sep = ""))
    write.table(met0,
              paste0("data-raw/minute_data_", paste(min(prev.years), max(prev.years), sep = "-"), ".txt"), row.names = F)
  }
  # --------------------------------------------------------------
  ### Collating minute data for current years---------------------
  # --------------------------------------------------------------

  #loading metdata and then converting into desired units and Variable names

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
    met.new2 <- met.new1 %>%
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
    if (i == curr.years[1]) {
        met1 <- met.new2
      } else {
        met1 <- rbind.data.frame(met1, met.new2)
      }
  }
  met1 <- met1 %>% mutate_all(as.numeric)
  write.table(met1, file = paste0("data-raw/minute_data_", paste0(curr.years), ".Rda"), row.names = FALSE)
  ## binding with previous years minute data
  load(file = paste("data-raw/minute_data_", paste(min(prev.years),
                                                   max(prev.years), sep = "-"), ".Rda", sep = ""))
  met2 <- rbind.data.frame(met0, met1)
  save(met2, file = paste("data-raw/minute_data_", paste(min(prev.years),
                                                         max(curr.years), sep = "-"), ".Rda", sep = ""))
  print(paste0("Last doy of the 'minute_data_", paste(min(prev.years),
                                                         max(curr.years), sep = "-"), ".Rda' file : ", met2$Julian[nrow(met2)]))
}
