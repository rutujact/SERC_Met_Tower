##----------------
## Title: SERC Met Tower minute data: Standardize units, clean, gap-fill, summarise daily_includes forest floor
## Author: Rutuja
## Date: March 29, 2018
##----------------
#
# rm(list = ls())
fun.mettower <- function(year = year, df_dict0 = df_dict0, met.Rs.QC = met.Rs.QC) {

  if (!require("pacman")) install.packages("pacman"); library(pacman)
  pacman::p_load(tidyverse, scales, janitor, lubridate, usethis, devtools, latticeExtra, neon)

  # graphics info
  tex <- element_text(size = 12, face = "plain") # , family = "gara"
  my.theme <-  theme(axis.text = tex, axis.title = tex,
                     title = tex, legend.title = tex, legend.text = tex, strip.text.y = tex, strip.text.x = tex)
  my.bg <- theme_bw() + theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "black", size = 0.25),
                              panel.grid.minor = element_blank())
  my.adjust <- theme(axis.title.y = element_text(vjust = 1), axis.title.x = element_text(vjust = -0.6), title = element_text(vjust = 2))

  ## Tidying up SERC met tower weather data
  # year <- 2019
  load(file = paste0("data-raw/minute_data_", paste(2003, year, sep = "-"), ".Rda"))
  datayears <- c(2003: year)
  head(met2); tail(met2)
  met2 <- met2
  #remove rows with all NAs
  nrow(met2)
  met2 <- met2[rowSums(is.na(met2)) != ncol(met2), ]
  nrow(met2)

  unique(met2$Year)
  ## all the non-i year data seems to be erroneously present, removing that. (typically just one day)
  # ## for year 2012
  # # row 216002 to 216050 had half the rows data overflown in to the next one.
  # # Locating that area
  ## Well, not fixing that right now. just removing those data
  met2 <- met2[met2$Year %in% datayears, ]
  nrow(met2)

  ## climatedata should contain variables as follows
  # Tmax - daily maximum temperature in degree Celcius,
  # Tmin - daily minimum temperature in degree Celcius,
  # RHmax - daily maximum relative humidity in percentage,
  # RHmin - daily minimum relative humidity in percentage,
  # Rs - daily incoming solar radiation in Megajoules per square metres per day
  # Precip - daily precipitation in millimeters,
  # u2 - subdaily wind speed measured at 2 meters from the ground surface in meters per second,
  # uz - subdaily wind speed in meters per second,
  # Bp - daily vapour pressure in h ectopascal,
  # met2$mon.day <-
  #   format(strptime(met2$Julian, format = "%j"), format = "%m-%d")
  ##------------------------------
  ## Setting correct date time
  ##------------------------------
  met2$Hour2 <- formatC(as.numeric(met2$Hour),  width = 4, flag = "0")
  met2$date.time <- strptime(paste(met2$Year, met2$Julian, met2$Hour2, sep = "-"), format = "%Y-%j-%H%M")
  # To see acceptable time zones OlsonNames()
  met2$date <- as.Date(met2$date.time)
  tail(met2$date)
  met2 <- select(met2, -Hour2)
  met2 <- met2[order(met2$date.time),]
  # dont see the need to get decimal hours
  # met2$Hour <- hour(met2$date.time) + minute(met2$date.time)/60
  # tail(unique(met2$Hour))

  met2 <- met2[!duplicated(met2$date.time),]
  head(met2);
  dim(met2)
  met2 <- met2[order(met2$date, met2$date.time),]
  met3 <- setNames(data.frame(matrix(ncol = ncol(met2), nrow = nrow(met2))), colnames(met2))
  met3$Year <- met2$Year; met3$Julian <- met2$Julian; met3$Hour <- met2$Hour;
  met3$date <- met2$date; met3$date.time <- as.POSIXct(met2$date.time)
  met3$uz <- met2$uz

  ##------------------------------
  ## Converting into desired units
  ##------------------------------

  #****Do not repeat the conversions***
  # Units in which data is collected is given in "literature/Column_Headers_SERC_weather_data" taken from folder "Data/SERC_Met_Data/MET_TOWER"
  str(met2);
  met3$Temp <- as.numeric(met2$Temp)
  met3$Temp <-
    met2$Temp * 100 - 40 # Volts to degree celcius
  met3$RH <- met2$RH * 100
  met3$Precip <- met2$Precip * 25.4 # inches to mm
  met3$Rs <-
    met2$Rs * 10 ^ 6 / 6  * 0.0864 #Volts to W/m-2 to MJ/m2/day (0.0864 multiplier does the last conversion) # 0.2 W/m2 should be normal per minute
  #met$uz is in meter/second
  met3$Bp <-
    met2$Bp*3.38639 # Looks like it is rather in inches Hg, and not pascals. So converting to kPa
  met3$Temp.tower <-
    met2$Temp.tower * 100 - 40 # Volts to degree celcius
  met3$RH.tower <- met2$RH.tower * 100
  met3$Rs.tower <-
    met2$Rs.tower * 10 ^ 6 / 6  * 0.0864 #Volts to W/m-2 to MJ/m2/day (0.0864 multiplier does the last conversion) # 0.2 W/m2should be normal per minute
  #----------------
  summary(met3)
  head(met3)
  ## removing rows with all NAs
  nrow(met3)
  met3 <- met3[rowSums(is.na(met3)) != ncol(met3), ]
  nrow(met3)

  ##  ****Tower temperature offset ****-------
  # Notes from data-raw/MetTower_T RH Record Documentation_Pat_Neale/Readme for Tower and Forest Floor Temperature and Relative Humidity data.doc
  # Tower measurements were shifted to a RM Young 41382 Fan Aspirated unit on 3/9/2017.
  # **** NOTE  **** The HMP45AC and RMY 41282 have different temperature offsets -40° and -50°, respectively
  # so need to remove 10 deg Cel to Temp.tower since 3/9/2017
  ##*** do not repeat this step***
  ##--------
  daily.Temp <- met3 %>% select(-date.time) %>%
    group_by(date) %>% summarise(
      Tmax = max(Temp, na.rm = TRUE),
      Tmax.tower = max(Temp.tower, na.rm = TRUE)
    )
  plot(daily.Temp$Tmax.tower ~ daily.Temp$date, main = "Before 10 deg C offset")
  met3 <- met3 %>% mutate(Temp.tower = if_else(date > as.Date("2017-03-09"),
                                               Temp.tower - 10, Temp.tower))
  # met3$Temp.tower[which(met3$date > as.Date("2017-03-09"))] <-
  #   met3$Temp.tower[which(met3$date > as.Date("2017-03-09"))] - 10;
  daily.Temp <- met3 %>% select(-date.time) %>%
    group_by(date) %>% summarise(
      Tmax = max(Temp, na.rm = TRUE),
      Tmax.tower = max(Temp.tower, na.rm = TRUE)
    )
  plot(daily.Temp$Tmax.tower ~ daily.Temp$date, main = "After 10 deg C offset")
  met3$date <- as.Date(met3$date)
  met3$Julian <-
    as.numeric(format(met3$date, format = "%j"))
  save(met3, file = paste0("data-raw/minute_data_all_years_metric_raw.Rda"))
  load(file = paste0("data-raw/minute_data_all_years_metric_raw.Rda"))

  ###--------------------------------------------
  ### Substituting clean Rs data from Pat Neale--
  ###--------------------------------------------

  met4 <- met3; rm(met3)

  # View(head(met4[which(met4$date %in% tower.bad.data.dates),]))
  ### from Pat Neale QA,QCd Met Rs data------------
  # met.Rs.QC.1 <- read.csv("data-raw/MetTower_Rs_data_from_Pat_Neale/TowerWoodsPSP02_17.csv",
  #                       na.strings = c("NA", "NaN", ""),
  #                       header = FALSE,
  #                       row.names = NULL,
  #                       check.names = F)
  # met.Rs.QC.2 <- read.csv("data-raw/MetTower_Rs_data_from_Pat_Neale/TowerWoodsPSP18.csv",
  #                       na.strings = c("NA", "NaN", ""),
  #                       header = FALSE,
  #                       row.names = NULL,
  #                       check.names = F)
  # met.Rs.QC <- bind_rows(met.Rs.QC.1, met.Rs.QC.2)
  # For the PSP data, the file has day number (excel format), time
  # as HHMM integer, then Rs W m-2 for tower and forest floor. NaN’s replace any
  # “bad”points, this includes all points with Rs < 0 for forest floor.

  colnames(met.Rs.QC) <- c("date", "time", "Rs.tower", "Rs")
  head(met.Rs.QC)
  met.Rs.QC$date <- excel_numeric_to_date(as.numeric(met.Rs.QC$date), include_time = FALSE)
  met.Rs.QC$Hour2 <- formatC(as.numeric(met.Rs.QC$time),  width = 4, flag = "0")
  met.Rs.QC$date.time <- as.POSIXct(strptime(paste(as.character(met.Rs.QC$date), met.Rs.QC$Hour2, sep = "-"),
                                             format = "%Y-%m-%d-%H%M"))
  str(met.Rs.QC)
  met.Rs.QC <- select(met.Rs.QC, -Hour2, -date, -time)
  met.Rs.QC$Rs.tower <- met.Rs.QC$Rs.tower * 0.0864  # in MJ m-2 day-1
  met.Rs.QC$Rs <- met.Rs.QC$Rs * 0.0864  # in MJ m-2 day-1

  ## making space
  rm(met2)
  ## making space


  met4 <- left_join(met4, met.Rs.QC, by = "date.time")
  met4 <- met4 %>% mutate(Rs = Rs.y,
                          Rs.tower = Rs.tower.y) %>%
    select(-Rs.x, -Rs.y, -Rs.tower.x, -Rs.tower.y)
  summary(met4)

  ##--------------------------------------
  ## Removing unacceptable/erroneous data
  ##--------------------------------------

  ##*****
  ## 1. Remove data within known, bad date range
  ##*****
  ## Doing this first since filter based on RH > 100

  ## See Notes from data-raw/MetTower_T RH Record Documentation_Pat_Neale/Readme for Tower and Forest Floor Temperature and Relative Humidity data.doc
  ## Met Tmax, Tmin, RHmin, RHmax (that is forest floor) readings problematic from August 1, 2013 until end of 2014
  ## Met Tmax.tower, Tmin.tower, RHmin.tower, RHmax.tower (that is tower-top) readings problematic in the range Jan 2015 - March 9, 2017 (new unit since 3/9/2017)

  FF.bad.data.dates <- seq(as.Date("2013-08-01"), as.Date("2014-12-31"), by = "1 day"); length(FF.bad.data.dates)
  met4$Temp[which(met4$date %in% FF.bad.data.dates)] <- NA; length(which(is.na(met4$Temp)))
  met4$RH[which(met4$date %in% FF.bad.data.dates)] <- NA; length(which(is.na(met4$RH)))

  # Tower: Unit was placed back in service on 11/13/15 and RH readings appeared to be within normal limits until 8/2/16.
  # Then high values until
  tower.bad.data.dates.range1 <- seq(as.Date("2015-01-01"), as.Date("2015-11-13"), by = "1 day"); length(tower.bad.data.dates.range1)
  tower.bad.data.dates.range2 <- seq(as.Date("2016-08-02"), as.Date("2017-03-09"), by = "1 day"); length(tower.bad.data.dates.range1)
  tower.bad.data.dates <- c(tower.bad.data.dates.range1,  tower.bad.data.dates.range2)

  # Tower: removing only those Temp data when RH >= 100 during bad dates
  ## RH removing only for range 2 and adding flag for range 1 for which RH is 5% lower that what it should be
  met4$Temp.tower[which(met4$date %in% tower.bad.data.dates & met4$RH.tower >= 100)] <- NA; length(which(is.na(met4$Temp.tower)))
  met4$RH.tower[which(met4$date %in% tower.bad.data.dates.range2)] <- NA; length(which(is.na(met4$RH.tower)))
  met4 <- met4 %>% mutate(tower.RH.Flag = if_else(date %in% tower.bad.data.dates.range1, 0, 1))
  #   #--------
  #   Adding VPD #--------
  #   #--------
  #   How to calculate air VPD: #https://getpulse.co/blog/vpd#calculate-air
  #   https://en.wikipedia.org/wiki/Vapour_pressure_of_water
  #   SVP = 0.61078 x exp(T / (T + 237.3) x 17.2694)); # The Tetens equation
  #   OR more accutate Buck's equation:
  #   SVP = 0.61121 x exp((18.678 - T/234.84) * (T/(257.14 + T))) # The Buck equation
  #   T is in degrees Celsius; SVP in kPa

   met4 <- met4 %>% mutate(SVP.tower = 0.61121 * exp((18.678 - Temp.tower/234.84) * (Temp.tower/(257.14 + T))),
                           SVP = 0.61121 * exp((18.678 - Temp/234.84) * (Temp/(257.14 + T))))

  # Cleaning SVP beyond acceptable range before calculating VPD

  ##*****
  ## 2. Remove data beyond acceptable range
  ##*****

  # to substitute outliers with NAs in a new dataframe
  # Gust winds could be upto 70 km/h i.e. 20 m/s
  # This is made variable in the Updater.Rmd
  # df_dict0 <-
  #   data.frame(
  #     variable = c("Temp", "Temp.tower", "RH", "RH.tower", "Rs", "Rs.tower", "Precip", "uz", "Bp", "SVP", "SVP.tower"),
  #     out_low = c(-25, -25, 0, 0, -20, -20, 0, 0, 90, 0, 0),
  #     out_high = c(50, 50, 100, 100, 20, 40, 300, 50, 106, 10, 10)
  #   )
  vars0 <- df_dict0$variable
  ## make all RH records greater than 100 to 100
  met4 <- met4 %>% mutate(RH = if_else(RH > 100, 100, RH),
                          RH.tower = if_else(RH.tower > 100, 100, RH.tower))
  total = length(vars0)
  # create progress bar
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  for (i in vars0) {
    met4[[i]][met4[[i]] < df_dict0[df_dict0$variable == i,]$out_low |
                met4[[i]] > df_dict0[df_dict0$variable == i,]$out_high] <- NA
    Sys.sleep(0.1)
    # update progress bar
    setTxtProgressBar(pb, i)
  }
  summary(met4)
  nrow(met4)
  met4 <- met4[rowSums(is.na(met4)) != ncol(met4), ]
  # Also seasonally:
  ## Tmax in summer below 4 looks unaccetable
  met4$Temp[met4$Julian > 150 & met4$Julian < 270 & met4$Temp < 4] <- NA
  met4$Temp.tower[met4$Julian > 150 & met4$Julian < 270 & met4$Temp.tower < 4] <- NA

  #   Calculate the VPD = SVP x (1 – RH/100) = VPD
  met4 <- met4 %>% mutate(VPD.tower = SVP.tower * (1 - RH.tower/100),
                          VPD = SVP * (1 - RH/100))

  mettower_minute <- met4
  write.table(mettower_minute,
            "data-raw/mettower_minute.txt",
            row.names = FALSE)
  usethis::use_data(mettower_minute, overwrite = TRUE, compress = 'xz')
  load("data/mettower_minute.Rda")

  head(mettower_minute)
  str(mettower_minute)

  mettower_minute$date <- as.character(mettower_minute$date)
  ##--------------------------------------
  ## Getting daily summaries
  ##--------------------------------------

  daily0 <- mettower_minute %>% select(-date.time) %>%
    group_by(date) %>% summarise(
      Year = mean(Year, na.rm = TRUE),
      # Month = mean(Month, na.rm = TRUE),
      # Day = mean(Day, na.rm = TRUE),
      Tmax = max(Temp, na.rm = TRUE),
      Tmin = min(Temp, na.rm = TRUE),
      RHmax = max(RH, na.rm = TRUE),
      RHmin = min(RH, na.rm = TRUE),
      Precip = sum(Precip, na.rm = TRUE),
      uz = mean(uz, na.rm = TRUE),
      Rs = mean(Rs, na.rm = TRUE),
      Bp = mean(Bp, na.rm = TRUE),
      Tmax.tower = max(Temp.tower, na.rm = TRUE),
      Tmin.tower = min(Temp.tower, na.rm = TRUE),
      RHmax.tower = max(RH.tower, na.rm = TRUE),
      RHmin.tower = min(RH.tower, na.rm = TRUE),
      Rs.tower = mean(Rs.tower, na.rm = TRUE),
      SVPmax = max(SVP, na.rm = TRUE),
      VPDmax = max(VPD, na.rm = TRUE),
      SVPmax.tower = max(SVP.tower, na.rm = TRUE),
      VPDmax.tower = max(VPD.tower, na.rm = TRUE),
      tower.RH.Flag = min(tower.RH.Flag, na.rm = TRUE) # taking mean so that one bad data (0) will make entire day's RH flagged as bad
    )
  # replacing Infinity by NAs
  daily1 <-
    do.call(data.frame, lapply(daily0, function(x)
      replace(x, is.infinite(x), NA)))
  summary(daily1)

  from <- as.Date(min(met4$date))
  to <- as.Date(daily1$date[length(daily1$date)])
  full <- data.frame(date = as.character(seq(from, to, by = '1 day')))
  daily2 <- merge(daily1, full, by = "date", all.y = T)
  daily2$date <- strptime(daily2$date, format = "%Y-%m-%d")

  daily2 <- daily2[order(daily2$date), ]
  missing <- function (x) {
    df <- data.frame(var = colnames(x), percent_missing = NA)
    for (i in 1:ncol(x)) {
      df[i, 2] <- round(length(which(is.na(x[[i]]))) / nrow(x) * 100, 2)
    }
    print(df)
  }
  daily2 <- daily2[!duplicated(daily2$date), ]
  missing(daily2)
  # var percent_missing
  # 1           date            0.00
  # 2           Year            2.48
  # 3           Tmax           10.98
  # 4           Tmin           10.98
  # 5          RHmax           11.86
  # 6          RHmin           11.86
  # 7         Precip            2.48
  # 8             uz            2.48
  # 9             Rs            2.56
  # 10            Bp            2.53
  # 11    Tmax.tower            4.08
  # 12    Tmin.tower            4.08
  # 13   RHmax.tower            6.19
  # 14   RHmin.tower            6.19
  # 15      Rs.tower            6.04
  # 16        SVPmax           10.98
  # 17        VPDmax           11.87
  # 18  SVPmax.tower            4.08
  # 19  VPDmax.tower            6.44
  # 20 tower.RH.Flag            2.48
  daily2$date <- as.Date(daily2$date)
  daily2$Julian <-
    as.numeric(format(daily2$date, format = "%j"))
  ## replacing seasonally unacceptable extremes
  ## RHmax in summer below 30 looks unacceptable
  daily2$RHmax[daily2$Julian >150 & daily2$Julian < 330 & daily2$RHmax < 30] <- NA
  daily2$RHmax.tower[daily2$Julian >150 & daily2$Julian < 330 & daily2$RHmax.tower < 30] <- NA
  ## RHmin in summer below 10 looks unaccetable
  daily2$RHmin[daily2$Julian > 150 & daily2$Julian < 330 & daily2$RHmin < 10] <- NA
  daily2$RHmin.tower[daily2$Julian > 150 & daily2$Julian < 330 & daily2$RHmin.tower < 10] <- NA

  ## Tmax in summer below 4 looks unaccetable
  daily2$Tmax[daily2$Julian > 150 & daily2$Julian < 270 & daily2$Tmax < 4] <- NA
  daily2$Tmax.tower[daily2$Julian > 150 & daily2$Julian < 270 & daily2$Tmax.tower < 4] <- NA
  ## Tmin in summer below 0 looks unaccetable
  daily2$Tmin[daily2$Julian > 150 & daily2$Julian < 270 & daily2$Tmin < 0] <- NA
  daily2$Tmin.tower[daily2$Julian > 150 & daily2$Julian < 270 & daily2$Tmin.tower < 0] <- NA
  summary(daily2)
  mettower <- daily2
  plot(mettower$Tmax.tower ~ mettower$date)
  write.table(mettower, "data-raw/mettower.txt", row.names = F)
  # mettower <- read.csv("data-raw/mettower.txt")
  usethis::use_data(mettower, overwrite = TRUE)
  load("data/mettower.Rda")

  mettower.p <- mettower %>% select(-Year, -Julian) %>% gather(key = "key", value = "value", -date)
  xyplot(value ~ date | key, data = mettower.p, scales=list(
    y=list(relation='free')),
         type = c("l"), main = "mettower_daily_gap") +
    layer_(panel.xblocks(x, is.na(y), col = "darkgray"))
  dev.copy(pdf,"figures/mettower_daily_gap.pdf",  height = 9, width = 12)
  while (!is.null(dev.list()))  dev.off()

  ##--------------------------------------
  ## Gap-filling
  ##--------------------------------------
  ## green generated by 2.0 SERC_greenhouse_weather_data.R in SERC_hydro-met_data project
  green <- readRDS("data-raw/greenhouse_weather_data_my_daily_aggregation_2011-2017.RDS")
  head(green)
  missing(green)
  str(green)

  # var percent_missing
  # 1    date            0.00
  # 2    Tmax            0.00
  # 3    Tmin            0.00
  # 4   RHmax            0.00
  # 5   RHmin            0.00
  # 6  Precip            0.00
  # 7    Epan            1.04
  # 8      u2            0.00
  # 9      Rs            6.70
  # 10    VPD            0.00
  # dates for which Precip data is missing and is available in dat.sub2


  met.green <- left_join(mettower, green, by = "date")
  head(met.green)
  # View(met.green)
  met.green$Precip.x[is.na(met.green$Precip.x)] <- met.green$Precip.y[is.na(met.green$Precip.x)]
  met.green$Tmax.x[is.na(met.green$Tmax.x)] <- met.green$Tmax.y[is.na(met.green$Tmax.x)]
  met.green$Tmin.x[is.na(met.green$Tmin.x)] <- met.green$Tmin.y[is.na(met.green$Tmin.x)]
  met.green$RHmax.x[is.na(met.green$RHmax.x)] <- met.green$RHmax.y[is.na(met.green$RHmax.x)]
  met.green$RHmin.x[is.na(met.green$RHmin.x)] <- met.green$RHmin.y[is.na(met.green$RHmin.x)]

  ## replacing seasonally unacceptable extremes
  ## RHmax in summer below 30 looks unaccetable
  met.green$RHmax.x[met.green$Day >150 & met.green$Day < 330 & met.green$RHmax.x < 30] <-
    met.green$RHmax.y[met.green$Day >150 & met.green$Day < 330 & met.green$RHmax.x < 30]
  ## RHmin in summer below 10 looks unaccetable
  met.green$RHmin.x[met.green$Day > 150 & met.green$Day < 330 & met.green$RHmin.x < 10] <-
    met.green$RHmin.y[met.green$Day >150 & met.green$Day < 330 & met.green$RHmin.x < 10]

  met.green$RHmin.x[met.green$date > as.Date("2013-05-01") & met.green$date < as.Date("2015-05-01") ] <-
    met.green$RHmin.y[met.green$date > as.Date("2013-05-01") & met.green$date < as.Date("2015-05-01") ]
  ## Tmin in summer below 4 looks unaccetable
  met.green$Tmax.x[met.green$Day > 150 & met.green$Day < 270 & met.green$Tmax.x < 4] <-
    met.green$Tmax.y[met.green$Day > 150 & met.green$Day < 270 & met.green$Tmax.x < 4]
  ## Tmin in summer below 0 looks unaccetable
  met.green$Tmin.x[met.green$Day > 150 & met.green$Day < 270 & met.green$Tmin.x < 0] <-
    met.green$Tmin.y[met.green$Day > 150 & met.green$Day < 270 & met.green$Tmin.x < 0]

  metbottom.clean <- met.green[, 1:ncol(daily2)]
  colnames(metbottom.clean) <- colnames(daily2)
  missing(metbottom.clean)
  metbottom.clean <- metbottom.clean[!duplicated(metbottom.clean$date), ]
  metbottom.clean$Tmax <- as.numeric(metbottom.clean$Tmax)
  metbottom.clean$Tmin <- as.numeric(metbottom.clean$Tmin)
  # var percent_missing
  # 1           date            0.00
  # 2           Year            2.48
  # 3           Tmax            2.33
  # 4           Tmin            2.33
  # 5          RHmax            2.86
  # 6          RHmin            2.91
  # 7         Precip            2.20
  # 8             uz            2.48
  # 9             Rs            2.56
  # 10            Bp            2.53
  # 11    Tmax.tower            4.31
  # 12    Tmin.tower            4.31
  # 13   RHmax.tower            6.56
  # 14   RHmin.tower            6.90
  # 15      Rs.tower            6.04
  # 16        SVPmax           10.98
  # 17        VPDmax           11.86
  # 18  SVPmax.tower            2.79
  # 19  VPDmax.tower            6.19
  # 20 tower.RH.Flag            2.48
  # 21        Julian            0.00

  write.table(metbottom.clean,
            "data-raw/metbottom.clean.txt",
            row.names = FALSE)
  usethis::use_data(metbottom.clean, overwrite = TRUE)
  load("data/metbottom.clean.Rda")

  # summarise_at(mydata, vars(Y2005, Y2006), funs(n(), mean, median))
  metbottom.clean$Y <-
    as.numeric(format(as.Date(metbottom.clean$date), format = "%Y"))
  meteo.year1 <- metbottom.clean %>%
    group_by(Y) %>%
    dplyr::summarise_at(vars(Tmax, Tmin, RHmax, RHmin, uz, Tmax.tower, Tmin.tower,
                             RHmax.tower, RHmin.tower, Rs.tower), mean, na.rm = TRUE)
  meteo.year2 <- metbottom.clean %>%
    group_by(Y) %>%
    summarise_at(vars(Precip, Rs), list(name = ~sum(., na.rm = TRUE)))

  meteo.year <- left_join(meteo.year1, meteo.year2, by = "Y")
  head(meteo.year)
  library(reshape2)
  meteo.year.long <- melt(meteo.year, id.vars = colnames(meteo.year)[1], variable.name = "variable", value.name = "value")
  str(meteo.year.long)

  summary(subset(meteo.year, Y %in% datayears))

  ggplot(subset(meteo.year.long, Y != max(datayears)), aes(x = Y, y = value)) +
    geom_line(aes(color = variable), show.legend = F) +
    geom_point(aes(color = variable), size = 1, show.legend = F) +
    facet_grid(variable ~ ., scales = "free_y") +
    ylab("") +
    xlab("year") + my.theme + my.bg + my.adjust + theme(axis.text.x = element_text(size = 10, face = "plain", angle = 90)) +
    scale_x_continuous(breaks = datayears) +
    ggtitle("SERC: Annual summary met data (Met Tower-Top and ForestFloor)")
  ggsave(file.path("figures/annual_met_data.pdf"), height = 12, width = 12, units='in')

  metbottom.clean <- select(metbottom.clean, -Year)
  head(metbottom.clean)
  daily3 <- reshape(metbottom.clean, varying = 2:ncol(metbottom.clean), idvar = "date", direction = "long", v.names = "Value",
                    timevar = "Variable", times = names(metbottom.clean)[-1])
  head(daily3)

  daily3$date <- as.Date(daily3$date)
  daily3$Year <-
    format(daily3$date, format = "%Y")
  daily3$Month <-
    format(daily3$date, format = "%m")
  daily3$Julian <-
    as.numeric(format(daily3$date, format = "%j"))
  str(daily3)

  select.var = c("Tmax", "Tmin", "RHmax","RHmin","Rs","Precip","uz","Bp")
  ggplot(subset(daily3, Variable %in% select.var), aes(x = Julian, y = Value, color = Year)) +
    # geom_point(size = 1) +
    geom_line(aes(x = Julian, y = Value, group = Year)) +
    facet_grid(Variable ~ ., scales = "free_y") +
    scale_x_continuous(breaks = seq(0, 366, by = 30)) +
    my.theme + my.bg + my.adjust + theme(panel.grid.major.x = element_blank()) +
    ggtitle("SERC: Daily Met Tower data - ForestFloor - by year")
  ggsave(file.path("figures/daily_met_data_by_year.pdf"), height = 12, width = 12, units='in')

  # plotting forest floor vs. top of the tower data
  daily2.1 <- subset(metbottom.clean, select = c("date","Tmax", "Tmin", "RHmax","RHmin","Rs"))
  daily4 <- reshape(daily2.1, varying = 2:ncol(daily2.1), idvar = "date", direction = "long", v.names = "Value",
                    timevar = "Variable", times = names(daily2.1)[2:ncol(daily2.1)])
  head(daily4)
  daily4$location <- "Forest Floor"
  daily2.6 <- subset(metbottom.clean, select = c("date","Tmax.tower", "Tmin.tower", "RHmax.tower", "RHmin.tower", "Rs.tower", "Precip", "uz"))
  colnames(daily2.6) <-  c("date","Tmax", "Tmin", "RHmax","RHmin","Rs", "Precip", "uz")
  head(daily2.6)
  daily5 <- reshape(daily2.6, varying = 2:ncol(daily2.6), idvar = "date", direction = "long", v.names = "Value",
                    timevar = "Variable", times = names(daily2.6)[2:ncol(daily2.6)])
  unique(daily5$Variable)
  head(daily5)
  daily5$location <- "TowerTop"
  daily4 <- subset(daily4, select = c("date", "Variable", "Value", "location"))
  daily5 <- subset(daily5, select = c("date", "Variable", "Value", "location"))
  daily6 <- rbind.data.frame(daily4, daily5)

  str(daily6)
  daily6$Julian <-
    as.numeric(format(daily6$date, format = "%j"))
  daily6$Year <-
    format(daily6$date, format = "%Y")
  daily6$year.location <- paste(daily6$Year, daily6$location, sep = ".")
  daily6 <- daily6[order(daily6$location, daily6$Year),]

  ggplot(daily6, aes(x = Julian, y = Value, color = location)) +
    geom_line(aes(x = Julian, y = Value, group = year.location)) +
    facet_grid(Variable ~ ., scales = "free_y") +
    scale_x_continuous(breaks = seq(0, 366, by = 30)) +
    my.theme + my.bg + my.adjust + theme(panel.grid.major.x = element_blank()) +
    ggtitle("SERC: Daily Met Tower data Tower Top vs. ForestFloor")
  ggsave(file.path("figures/Tower Top vs. Forest Floor.pdf"), height = 7, width = 12, units='in')

  ##--------------------------------------
  ## Calculate and plot climatic means by location
  ##--------------------------------------
  clim <-
    daily6 %>%
    group_by(location, Julian, Variable) %>%
    dplyr::summarise_at(vars(Value), list(mean = ~mean(., na.rm = TRUE),
                                          sd = ~sd(., na.rm = TRUE),
                                          n = ~sum(!is.na(.))))
  clim$se = clim$sd/sqrt(clim$n)
  #
  head(clim)
  ggplot(clim, aes(x = Julian, y = mean, color = location)) +
    geom_line() +
    geom_errorbar(aes(ymax = mean + se, ymin = mean - se)) +
    facet_grid(Variable ~ ., scales = "free_y") +
    scale_x_continuous(breaks = seq(0, 366, by = 30)) +
    my.theme + my.bg + my.adjust + theme(panel.grid.major.x = element_blank()) +
    ylab("Daily mean +- SE") +
    ggtitle("SERC: Met Tower data Tower Top vs. Forest Floor")
  ggsave(file.path("figures/Tower Top vs. Forest Floor_mean & SE.pdf"), height = 7, width = 12, units='in')

  write.table(clim,
            "data-raw/climatic_mean.txt",
            row.names = FALSE)
  usethis::use_data(clim, overwrite = TRUE)
  devtools::document()
  devtools::install()
}


