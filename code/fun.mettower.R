##----------------
## Title: SERC Met Tower minute data: Standardize units, clean, gap-fill, summarise daily_includes forest floor
## Author: Rutuja
## Date: March 29, 2018
##----------------
#
# rm(list = ls())
fun.mettower <- function(year = year, df_dict0 = df_dict0,
  met.Rs.QC.1 = met.Rs.QC.1) {

  if (!require("pacman")) install.packages("pacman"); library(pacman)
    pacman::p_load(tidyverse, scales, janitor, lubridate, usethis, devtools,
     latticeExtra, rhdf5)

  if (!requireNamespace("BiocManager", quietly = TRUE))
      install.packages("BiocManager")
  if (!requireNamespace("rhdf5", quietly = TRUE))
      BiocManager::install("rhdf5")


# Call the R HDF5 Library and establish two hdf files

  # h5createFile("data_to_publish/SERC_minute_data.h5")
  # h5createFile("data_to_publish/SERC_daily_data.h5")

  # graphics info
  tex <- element_text(size = 12, face = "plain") # , family = "gara"
  my.theme <-  theme(axis.text = tex, axis.title = tex,
   title = tex, legend.title = tex, legend.text = tex,
   strip.text = tex, strip.text.x = tex)
  my.bg <- theme_bw() + theme(panel.grid.major = element_blank(),
    panel.grid.major.x = element_line(colour = "black", size = 0.25),
    panel.grid.minor = element_blank())
  my.adjust <- theme(axis.title = element_text(vjust = 1),
    axis.title.x = element_text(vjust = -0.6), title = element_text(vjust = 2))

  ## Tidying up SERC met tower weather data
  load(file = paste0("data-raw/minute_data_", paste(2002, year, sep = "-"),
    ".Rda"))
  datayears <- c(2002: year)
  met2 <- met2
  #remove rows with all NAs
  met2 <- met2[rowSums(is.na(met2)) != ncol(met2), ]
  met2 <- met2[met2$Year %in% datayears, ]

  ## climatedata should contain variables as follows
  # Tmax - daily maximum temperature in degree Celsius,
  # Tmin - daily minimum temperature in degree Celsius,
  # RHmax - daily maximum relative humidity in percentage,
  # RHmin - daily minimum relative humidity in percentage,
  # Rs - daily incoming solar radiation in Megajoules per square metres per day
  # Precip - daily precipitation in millimeters,
  # u2 - subdaily wind speed measured at 2 meters from the ground surface in
  #           meters per second,
  # Ws - subdaily wind speed in meters per second,
  # Bp - daily vapour pressure in h ectopascal,
  # met2$mon.day <-
  #   format(strptime(met2$Julian, format = "%j"), format = "%m-%d")

  ##------------------------------
  ## Setting correct date time
  ##------------------------------
  met2$Hour2 <- formatC(as.numeric(met2$Hour),  width = 4, flag = "0")
  met2$date.time <- strptime(paste(met2$Year, met2$Julian, met2$Hour2, sep = "-"),
    format = "%Y-%j-%H%M")
  # To see acceptable time zones OlsonNames()
  met2$date <- as.Date(met2$date.time)
  tail(met2$date)
  met2 <- select(met2, -Hour2)
  met2 <- met2[order(met2$date.time),]

  met2 <- met2[!duplicated(met2$date.time),]
  met2 <- met2[order(met2$date, met2$date.time),]
  met3 <- setNames(data.frame(matrix(ncol = ncol(met2), nrow = nrow(met2))),
    colnames(met2))
  met3$Year <- met2$Year; met3$Julian <- met2$Julian; met3$Hour <- met2$Hour;
  met3$date <- met2$date;
  met3$date.time <- as.POSIXct(met2$date.time, tz = "EST")
  # the above takes in the given data as in "EST" format, but converts it to Sys timezone
  # so to convert back to "EST:
  attr(met3$date.time, "tzone") <- "EST"
  met3$Ws.tower <- met2$Ws.tower

  ##------------------------------
  ## Converting into desired units
  ##------------------------------

  met3$Temp.floor   <- as.numeric(met2$Temp.floor)
  met3$Temp.floor   <- met2$Temp.floor * 100 - 40 # Volts to degree celcius
  met3$RH.floor     <- met2$RH.floor * 100
  met3$Precip.tower <- met2$Precip.tower * 25.4 # inches to mm
  met3$Rs.floor     <- met2$Rs.floor / 6* 10^-6 # Volts to W/m2 # 0.2 W/m2
  #met$Ws is in meter/second
  met3$Bp.tower     <- met2$Bp.tower * 3.38639 #  converting to kPa
  met3$Temp.tower   <- met2$Temp.tower * 100 - 40 # Volts to degree celcius
  met3$RH.tower     <- met2$RH.tower * 100
  met3$Rs.tower     <- met2$Rs.tower / 6* 10^-6   # Volts to W/m2
  #----------------
  met3 <- met3[rowSums(is.na(met3)) != ncol(met3), ]

  ##  ****Tower temperature offset ****-------
  # Notes from data-raw/MetTower_T RH Record Documentation_Photobiology_Lab/Readme for Tower and Forest Floor Temperature and Relative Humidity data.doc
  # Tower measurements were shifted to a RM Young 41382 Fan Aspirated unit on 3/9/2017.
  # **** NOTE  **** The HMP45AC and RMY 41282 have different temperature offsets -40° and -50°, respectively
  # so need to remove 10 deg Cel to Temp.tower since 3/9/2017
  ##--------
  met3 <- met3 %>% mutate(Temp.tower.before = Temp.tower,
    Temp.tower = if_else(date > as.Date("2017-03-09"),
     Temp.tower.before - 10,
     Temp.tower.before))
  daily.Temp <- met3 %>% select(-date.time) %>%
  group_by(date) %>% summarise(
    Tmax.tower.before = max(Temp.tower.before, na.rm = TRUE),
    Tmax.tower.after = max(Temp.tower, na.rm = TRUE)) %>%
  gather(key = "variable", value = "value", -date) %>%
  mutate(offset = if_else(variable == "Tmax.tower.before",
    "Before offset correction", "After offset correction"),
  variable = "Tmax",
  offset = fct_relevel(offset, "After offset correction", after = 1))

  graph.1 <- ggplot(daily.Temp, aes(x = date, y = value)) +
  geom_point(size = 0.5, alpha = 0.7)+
  ylim(c(df_dict0$out_low[which(df_dict0$variable == "Temp.tower")],
   df_dict0$out_high[which(df_dict0$variable == "Temp.tower")])) +
  ylab("degree C") + xlab("Date") +
  facet_grid(offset ~ .) + my.theme + my.bg +
  ggtitle("Tower Top Temp offset changes by 10 deg C 2017-03-09 onwards")

  graph.1;
  ggsave(file.path("figures/graph.1_Tower Temp offset.png"),
    height = 5, width = 6, units='in')

  met3 <- met3 %>% select(-Temp.tower.before) %>%
  mutate(date = as.Date(date),
   Julian = as.numeric(format(date, format = "%j")),
   Month = as.numeric(format(date, format = "%m")),
   Hour = as.numeric(format(date, format = "%H"))) %>%
  select(Year, Month, Julian, Hour, date, date.time, Temp.floor:Rs.tower)
  met3 <- met3[!is.na(met3$date.time), ] # only one value

  save(met3, file = "results/minute_data_all_years_metric_raw.Rda")

  plot.datagaps.1 <- function(mydata) {
    mydata.p <- mydata %>% select(-Year, -Julian, -date.time) %>%
    group_by(date) %>%
    summarise_all(list(~mean(., na.rm = T))) %>%
    gather(key = "key", value = "value", -date)

    xyplot(value ~ date | key, data = mydata.p, scales = list(
      y = list(relation='free')),
    type = c("l"), main = "Mettower Raw Data Daily Means in SI units") +
    layer_(panel.xblocks(x, is.na(y), col = "darkgray"))
  }
  png("figures/Mettower_Raw_Daily_Means.png", height = 768,
    width = 1024, units ="px", res = 150)
  plot.datagaps.1(met3)
  while (!is.null(dev.list())) {dev.off()}

  ###--------------------------------------------
  ### Substituting clean Rs data from Photobiology Lab--
  ###--------------------------------------------
  load(file = "results/minute_data_all_years_metric_raw.Rda")

  met4 <- met3; rm(met3)

  met.Rs.QC.1.1 <- met.Rs.QC.1
  colnames(met.Rs.QC.1.1) <- c("Year", "Month", "Dy", "Hr", "Min", "Rs.tower", "Rs.floor")
  met.Rs.QC <- met.Rs.QC.1.1 %>%
    ## Need leading zeroes
  mutate_at(vars(Month, Dy, Hr, Min), formatC, width = 2, format = "d", flag = "0") %>%
  mutate_at(vars(Year), as.character) %>%
  mutate(date.time.raw = paste(Year, Month, Dy, Hr, Min, sep = "-"),
    date.time = as.POSIXct(strptime(date.time.raw, format = "%Y-%m-%d-%H-%M",
    tz = "EST")),
   date = as.Date(date.time)) %>%
  select(date, date.time, Rs.tower, Rs.floor)
  ## should not have any Nas for date.time, but has:
  summary(met.Rs.QC)

    # the above takes in the given data as in "EST" format, but converts it to Sys timezone
    # so to convert back to "EST:
  attr(met.Rs.QC$date.time, "tzone") <- "EST"
  met.Rs.QC$Julian <- as.numeric(format(met.Rs.QC$date, format = "%j"))
  head(met.Rs.QC)
  # str(met.Rs.QC)
  met.Rs.QC.p <- met.Rs.QC %>% select(date, Rs.tower, Rs.floor) %>%
    group_by(date) %>% summarise_all(list(~mean(., na.rm = T))) %>%
    gather(key = "key", value = "value", -date) %>%
    mutate(key = factor(key, levels = c("Rs.tower", "Rs.floor")))


  graph.2 <-  ggplot(met.Rs.QC.p, aes(x = date, y = value)) +
    geom_line() + ylab("W/m2") + xlab("Date") +
    facet_grid(key ~ ., scales = "free_y") +
    my.theme + my.bg + my.adjust + theme(panel.grid.major.x = element_blank()) +
    ggtitle("Daily Met Tower cleaned Rs data from PhotoBio Lab\nTower Top versus Forest Floor")
  graph.2;
  ggsave(file.path("figures/graph.2_Daily Rs  Top & Forest floor.png"),
    height = 5, width = 6, units='in')

  ## making space
  rm(met2)

  met4 <- left_join(met4, select(met.Rs.QC, date.time, Rs.floor, Rs.tower), by = "date.time")
  met4 <- met4 %>% mutate(Rs.floor = Rs.floor.y,
    Rs.tower = Rs.tower.y) %>%
    select(-Rs.floor.x, -Rs.floor.y, -Rs.tower.x, -Rs.tower.y)

  ###--------------------------------------------
  ### Substituting new RH floor fro 2018 on
  ###--------------------------------------------
  tmp.RH.floor <- read.csv(file = "data-raw/FF_T_RH_201710to201912.csv")
  names(tmp.RH.floor) <- c("Year", "Month", "Dy", "Hr", "Min", "Temp.C", "RH.floor")
  new.RH.floor <- subset(tmp.RH.floor, Year >= 2018)

  new.RH.floor2 <- new.RH.floor %>%
    ## Need leading zeroes
  mutate_at(vars(Month, Dy, Hr, Min), formatC, width = 2, format = "d", flag = "0") %>%
  mutate_at(vars(Year), as.character) %>%
  mutate(date.time.raw = paste(Year, Month, Dy, Hr, Min, sep = "-"),
    date.time = as.POSIXct(strptime(date.time.raw, format = "%Y-%m-%d-%H-%M",
    tz = "EST")),
   date = as.Date(date.time)) %>%
  select(date, date.time, RH.floor)
  ## should not have any Nas for date.time, but has:
  summary(new.RH.floor2)


  date.match <-  which(met4$date.time %in% new.RH.floor2$date.time)
  date.match2 <-  which(new.RH.floor2$date.time %in% met4$date.time)

  met4$RH.floor[date.match] <- new.RH.floor2$RH.floor[date.match2]
  ##--------------------------------------
  ## Removing unacceptable/erroneous data
  ##--------------------------------------

  ##****
  ## 1. Remove data within known, bad date range
  ##*****

  FF.bad.data.dates <- seq(as.Date("2013-08-01"), as.Date("2014-12-31"), by = "1 day"); length(FF.bad.data.dates)
  met4$Temp.floor[which(met4$date %in% FF.bad.data.dates)] <- NA; length(which(is.na(met4$Temp.floor)))
  met4$RH.floor[which(met4$date %in% FF.bad.data.dates)] <- NA; length(which(is.na(met4$RH.floor)))

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

  met4 <- met4 %>% mutate(SVP.tower = 0.61121 * exp((18.678 - Temp.tower/234.84) * (Temp.tower/(257.14 + Temp.tower))),
    SVP.floor = 0.61121 * exp((18.678 - Temp.floor/234.84) * (Temp.floor/(257.14 + Temp.floor))))

  ##*****
  ## 2. Remove data beyond acceptable range
  ##*****

  # to substitute outliers with NAs in a new dataframe
  # Gust winds could be upto 70 km/h i.e. 20 m/s
  # This is made variable in the Updater.Rmd
  # df_dict0 <-
  #   data.frame(
  #     variable = c("Precip.tower", "Ws.tower", "Bp.tower", "Rs.floor",
  #     "Rs.tower", "Temp.floor", "Temp.tower", "RH.floor", "RH.tower",
  #      "SVP.floor", "SVP.tower"),
  #     out_low = c(0, 0, 90, -10, -10, -25, -25, 0, 0, 0, 0),
  #     out_high = c(300, 50, 1500, 1500, 40, 50, 50, 100, 100, 10, 10)
  #   )
  vars0 <- as.character(df_dict0$variable)
  ## make all RH records greater than 100 to 100
  met4 <- met4 %>% mutate(RH.floor = if_else(RH.floor > 100, 100, RH.floor),
    RH.tower = if_else(RH.tower > 100, 100, RH.tower))
  total <- length(vars0)
  # create progress bar
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  for (i in vars0) {
    met4[[i]][met4[[i]] < df_dict0[df_dict0$variable == i,]$out_low |
    met4[[i]] > df_dict0[df_dict0$variable == i,]$out_high] <- NA
    Sys.sleep(0.1)
    # update progress bar
    setTxtProgressBar(pb, i)
  }
  met4 <- met4[rowSums(is.na(met4)) != ncol(met4), ]
  met5 <- met4
# Seasonal adjustments

# the parenthetical assignments print the output for integration into a table

  met5$Temp.floor[met5$Julian > 150 & met5$Julian < 270 & met5$Temp.floor < 4] <- NA
  met5$Temp.tower[met5$Julian > 150 & met5$Julian < 270 & met5$Temp.tower < 4] <- NA

  met5$Temp.floor[met5$Julian > 100 & met5$Julian < 150 & met5$Temp.floor < -10] <- NA
  met5$Temp.tower[met5$Julian > 100 & met5$Julian < 150 & met5$Temp.tower < -10] <- NA

  met5$Temp.floor[met5$Julian > 270 & met5$Julian < 330 & met5$Temp.floor < -10] <- NA
  met5$Temp.tower[met5$Julian > 270 & met5$Julian < 330 & met5$Temp.tower < -10] <- NA

  rm(met4)

  # Calculate the VPD = SVP x (1 – RH/100) = VPD
  met5 <- met5 %>% mutate(VPD.tower = SVP.tower * (1 - RH.tower/100),
    VPD.floor = SVP.floor * (1 - RH.floor/100))

  met_minute.discont <- met5 %>% arrange(date.time); rm(met5)
  ## But this met_minute has missing date.times
  from <- min(met_minute.discont$date.time, na.rm = TRUE)
  to <- max(met_minute.discont$date.time, na.rm = TRUE)
  full <- data.frame(date.time = seq(from, to, by = '1 min'))
  met_minute <- merge(met_minute.discont, full, by = "date.time", all = T)
  met_minute <- met_minute %>%
  mutate(date = as.Date(date.time),
   Year = as.numeric(format(date, format = "%Y")),
   Month = as.numeric(format(date, format = "%m")),
   Julian = as.numeric(format(date, format = "%j")),
   Hour = as.numeric(format(date, format = "%H")))
  write.table(met_minute,
    "results/met_minute.txt",
    row.names = FALSE)
  usethis::use_data(met_minute, overwrite = TRUE) # for smaller size use compress = 'xz'
  load("data/met_minute.Rda")

  # head((met_minute)
  # str(met_minute)
  missing <- function (x) {
    df <- data.frame(var = colnames(x), percent_missing = NA)
    for (i in 1:ncol(x)) {
      df[i, 2] <- round(length(which(is.na(x[[i]]))) /nrow(x) * 100, 2)
    }
    print(df)
  }
  missing(met_minute)
  # var percent_missing
  # 1      date.time            0.01
  # 2           Year            7.33
  # 3          Month            7.33
  # 4         Julian            7.33
  # 5           Hour            7.33
  # 6           date            7.33
  # 7     Temp.floor           15.43
  # 8       RH.floor           15.14
  # 9   Precip.tower            7.33
  # 10      Ws.tower            7.33
  # 11      Bp.tower            7.46
  # 12    Temp.tower           10.24
  # 13      RH.tower           10.83
  # 14      Rs.floor            7.82
  # 15      Rs.tower            7.48
  # 16 tower.RH.Flag            7.33
  # 17     SVP.tower            8.68
  # 18     SVP.floor           15.03
  # 19     VPD.tower           11.77
  # 20     VPD.floor           15.14

  missing_met_minute <- missing(met_minute)
  write.table(missing_met_minute,
    "results/missing_met_minute.txt",
    row.names = FALSE)

  ##--------------------------------------
  ## Getting daily summaries
  ##--------------------------------------
  met_minute$date <- as.character(met_minute$date)

  daily0 <- met_minute %>% select(-date.time) %>%
  group_by(date) %>% summarise(
      # Year = mean(Year, na.rm = TRUE),
      # Month = mean(Month, na.rm = TRUE),
      # Day = mean(Day, na.rm = TRUE),
    Precip.tower = sum(Precip.tower, na.rm = TRUE),
    Ws.tower = mean(Ws.tower, na.rm = TRUE),
    Bp.tower = mean(Bp.tower, na.rm = TRUE),
    Tmax.floor = max(Temp.floor, na.rm = TRUE),
    Tmin.floor = min(Temp.floor, na.rm = TRUE),
    RHmax.floor = max(RH.floor, na.rm = TRUE),
    RHmin.floor = min(RH.floor, na.rm = TRUE),
    Tmax.tower = max(Temp.tower, na.rm = TRUE),
    Tmin.tower = min(Temp.tower, na.rm = TRUE),
    RHmax.tower = max(RH.tower, na.rm = TRUE),
    RHmin.tower = min(RH.tower, na.rm = TRUE),
    Rs.floor = sum(Rs.floor, na.rm = TRUE)/1000, #from W/m2 to KW/m2
    Rs.tower = sum(Rs.tower, na.rm = TRUE)/1000, #from W/m2 to KW/m2
    SVPmax.floor = max(SVP.floor, na.rm = TRUE),
    VPDmax.floor = max(VPD.floor, na.rm = TRUE),
    SVPmax.tower = max(SVP.tower, na.rm = TRUE),
    VPDmax.tower = max(VPD.tower, na.rm = TRUE),
    tower.RH.Flag = min(tower.RH.Flag, na.rm = TRUE) # taking min so that one bad data (0) will make entire day's RH flagged as bad
    )
  # replacing Infinity by NAs
  daily1 <-
  do.call(data.frame, lapply(daily0, function(x)
    replace(x, is.infinite(x), NA)))
  # summary(daily1)

  from <- min(as.Date(daily1$date), na.rm = TRUE)
  to <- max(as.Date(daily1$date), na.rm = TRUE)# as.Date(daily1$date[length(daily1$date)])
  full <- data.frame(date = as.character(seq(from, to, by = '1 day')))
  daily2 <- merge(daily1, full, by = "date", all = T)
  daily2$date <- as.Date(strptime(daily2$date, format = "%Y-%m-%d"))
  daily2$Year <-
  as.numeric(format(daily2$date, format = "%Y"))

  daily2 <- daily2[order(daily2$date), ]
  daily2 <- daily2[!duplicated(daily2$date), ]
  daily2 <- daily2[!is.na(daily2$date), ]
  missing(daily2)
  # var percent_missing
  # 1           date            0.00
  # 2   Precip.tower            5.12
  # 3       Ws.tower            5.12
  # 4       Bp.tower            5.20
  # 5     Tmax.floor           13.32
  # 6     Tmin.floor           13.32
  # 7    RHmax.floor           13.18
  # 8    RHmin.floor           13.27
  # 9     Tmax.tower            6.88
  # 10    Tmin.tower            6.88
  # 11   RHmax.tower            8.82
  # 12   RHmin.tower            9.21
  # 13      Rs.floor            5.12
  # 14      Rs.tower            5.12
  # 15  SVPmax.floor           13.05
  # 16  VPDmax.floor           13.05
  # 17  SVPmax.tower            5.34
  # 18  VPDmax.tower            8.57
  # 19 tower.RH.Flag            5.12
  # 20          Year            0.00
  # 21        Julian            0.00

  daily2$Julian <-
  as.numeric(format(daily2$date, format = "%j"))
  ## replacing seasonally unacceptable extremes
  ## RHmax in summer below 30 looks unacceptable
  daily2$RHmax.floor[daily2$Julian >130 & daily2$Julian < 330 & daily2$RHmax.floor < 30] <- NA
  daily2$RHmax.tower[daily2$Julian >130 & daily2$Julian < 330 & daily2$RHmax.tower < 30] <- NA
  ## RHmin in summer below 10 looks unaccetable
  daily2$RHmin.floor[daily2$Julian > 130 & daily2$Julian < 330 & daily2$RHmin.floor < 10] <- NA
  daily2$RHmin.tower[daily2$Julian > 130 & daily2$Julian < 330 & daily2$RHmin.tower < 10] <- NA

  # summary(daily2)
  met_daily <- daily2
  write.table(met_daily, "results/met_daily.txt", row.names = F)
  usethis::use_data(met_daily, overwrite = TRUE)
  load("data/met_daily.Rda")

  plot.datagaps.2 <- function(mydata) {
    mydata.p <- mydata %>% select(-Year, -Julian) %>% gather(key = "key", value = "value", -date)
    xyplot(value ~ date | key, data = mydata.p, scales = list(
      y=list(relation='free')),
    type = c("l"), main = "Mettower Daily Data gaps") +
    layer_(panel.xblocks(x, is.na(y), col = "darkgray"))
  }
  #pdf("figures/mettower_daily_gap.pdf", height = 9, width = 12)
  png("figures/mettower_daily_gap.png", height = 768, width = 1344, units ="px", res = 150)
  plot.datagaps.2(met_daily)
  while (!is.null(dev.list())) {dev.off()}

  met_daily.p.2 <- met_daily %>% gather(key = "key", value = "value", -date, -Julian, -Year)
  select.var <- c("Tmax.tower", "Tmin.tower", "RHmax.tower", "RHmin.tower", "Rs.tower", "Precip.tower", "Ws.tower", "Bp.tower", "VPDmax.tower")
  graph.3 <-  ggplot(subset(met_daily.p.2, key %in% select.var), aes(x = Julian, y = value, color = as.factor(Year))) +
  geom_line(aes(x = Julian, y = value, group = Year)) +
  facet_grid(key ~ ., scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 366, by = 30)) +
  my.theme + my.bg + my.adjust + theme(panel.grid.major.x = element_blank()) +
  labs(colour = "Year") +
  ggtitle("Daily Met Tower Tower Top data - by year")
  graph.3;
  #ggsave(file.path("figures/daily_met_data_by_year_Tower-Top.pdf"), height = 8, width = 12, units='in')
  ggsave(file.path("figures/graph.3_daily_met_data_by_year_Tower-Top.png"), height = 7.5, width = 8, units='in', dpi = 400)

  missing(met_daily)
  missing_met_daily <- missing(met_daily) %>%
  subset(!var %in% c("Year", "Julian"))
  # var percent_missing
  # 1           date            0.00
  # 2   Precip.tower            5.12
  # 3       Ws.tower            5.12
  # 4       Bp.tower            5.20
  # 5     Tmax.floor           13.32
  # 6     Tmin.floor           13.32
  # 7    RHmax.floor           13.18
  # 8    RHmin.floor           13.27
  # 9     Tmax.tower            6.88
  # 10    Tmin.tower            6.88
  # 11   RHmax.tower            8.82
  # 12   RHmin.tower            9.21
  # 13      Rs.floor            5.12
  # 14      Rs.tower            5.12
  # 15  SVPmax.floor           13.05
  # 16  VPDmax.floor           13.05
  # 17  SVPmax.tower            5.34
  # 18  VPDmax.tower            8.57
  # 19 tower.RH.Flag            5.12
  # 20          Year            0.00
  # 21        Julian            0.00
  write.table(missing_met_daily,
    "results/missing_met_daily.txt",
    row.names = FALSE)
  ##--------------------------------------
  ## Gap-filling
  ##--------------------------------------
  ## green generated by 2.0 SERC_greenhouse_weather_data.R in SERC_hydro-met_data project
  green <- readRDS("data-raw/greenhouse_weather_data_my_daily_aggregation_2011-2017.RDS")
  # head((green)
  missing(green)
  # str(green)
  # > range(green$date)
  # [1] "2011-07-15" "2018-01-11"
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
  summary(green)
  met.green <- left_join(met_daily, green, by = "date")
  # head((met.green)
  # View(met.green)
  met.green$Precip.floor[is.na(met.green$Precip.floor)] <- met.green$Precip[is.na(met.green$Precip.floor)]
  met.green$Tmax.floor[is.na(met.green$Tmax.floor)] <- met.green$Tmax[is.na(met.green$Tmax.floor)]
  met.green$Tmin.floor[is.na(met.green$Tmin.floor)] <- met.green$Tmin[is.na(met.green$Tmin.floor)]
  met.green$RHmax.floor[is.na(met.green$RHmax.floor)] <- met.green$RHmax[is.na(met.green$RHmax.floor)]
  met.green$RHmin.floor[is.na(met.green$RHmin.floor)] <- met.green$RHmin[is.na(met.green$RHmin.floor)]
  # met.green$VPDmax.floor[is.na(met.green$VPDmax.floor)] <- met.green$VPDmax[is.na(met.green$VPDmax.floor)]
  ## Rs from greenhouse may not be a good substitute for forest floor Rs, as the latter is more shaded?

  ## replacing seasonally unacceptable extremes
  ## RHmax in summer below 30 looks unaccetable
  met.green$RHmax.floor[met.green$Day >150 & met.green$Day < 330 & met.green$RHmax.floor < 30] <-
  met.green$RHmax[met.green$Day >150 & met.green$Day < 330 & met.green$RHmax.floor < 30]
  ## RHmin in summer below 10 looks unaccetable
  met.green$RHmin.floor[met.green$Day > 150 & met.green$Day < 330 & met.green$RHmin.floor < 10] <-
  met.green$RHmin[met.green$Day >150 & met.green$Day < 330 & met.green$RHmin.floor < 10]

  met.green$RHmin.floor[met.green$date > as.Date("2013-05-01") & met.green$date < as.Date("2015-05-01") ] <-
  met.green$RHmin[met.green$date > as.Date("2013-05-01") & met.green$date < as.Date("2015-05-01") ]
  ## Tmin in summer below 4 looks unaccetable
  met.green$Tmax.floor[met.green$Day > 150 & met.green$Day < 270 & met.green$Tmax.floor < 4] <-
  met.green$Tmax[met.green$Day > 150 & met.green$Day < 270 & met.green$Tmax.floor < 4]
  ## Tmin in summer below 0 looks unaccetable
  met.green$Tmin.floor[met.green$Day > 150 & met.green$Day < 270 & met.green$Tmin.floor < 0] <-
  met.green$Tmin[met.green$Day > 150 & met.green$Day < 270 & met.green$Tmin.floor < 0]

  metbottom.filled <- met.green[, 1:ncol(daily2)]
  colnames(metbottom.filled) <- colnames(daily2)
  missing(metbottom.filled)
  metbottom.filled <- metbottom.filled[!duplicated(metbottom.filled$date), ]
  metbottom.filled$Tmax.floor <- as.numeric(metbottom.filled$Tmax.floor)
  metbottom.filled$Tmin.floor <- as.numeric(metbottom.filled$Tmin.floor)
  missing_met_filled_daily <- missing(metbottom.filled) %>%
  subset(!var %in% c("Year", "Julian"))

  write.table(missing_met_filled_daily,
    "results/missing_met_filled_daily.txt",
    row.names = FALSE)
  # var percent_missing
  # 1           date            0.00
  # 2   Precip.tower            5.12
  # 3       Ws.tower            5.12
  # 4       Bp.tower            5.20
  # 5     Tmax.floor            5.18
  # 6     Tmin.floor            5.18
  # 7    RHmax.floor            5.05
  # 8    RHmin.floor            5.14
  # 9     Tmax.tower            6.88
  # 10    Tmin.tower            6.88
  # 11   RHmax.tower            8.82
  # 12   RHmin.tower            9.21
  # 13      Rs.floor            5.12
  # 14      Rs.tower            5.12
  # 15  SVPmax.floor           13.05
  # 16  VPDmax.floor           13.05
  # 17  SVPmax.tower            5.34
  # 18  VPDmax.tower            8.57
  # 19 tower.RH.Flag            5.12
  metbottom.filled <- metbottom.filled %>%
  mutate(Month = format(date, format = "%m")) %>%
  select(Year, Month, Julian, date:tower.RH.Flag)

  write.table(metbottom.filled,
    "results/metbottom.filled.txt",
    row.names = FALSE)
  usethis::use_data(metbottom.filled, overwrite = TRUE)
  load("data/metbottom.filled.Rda")

  # summarise_at(mydata, vars(Y2005, Y2006), funs(n(), mean, median))
  metbottom.filled$Year <-
  as.numeric(format(as.Date(metbottom.filled$date), format = "%Y"))
  meteo.year1 <- metbottom.filled %>%
  group_by(Year) %>%
  dplyr::summarise_at(vars(Tmax.floor, Tmin.floor, RHmax.floor, RHmin.floor, Ws.tower, Tmax.tower, Tmin.tower,
   RHmax.tower, RHmin.tower, Rs.tower), mean, na.rm = TRUE)
  meteo.year2 <- metbottom.filled %>%
  group_by(Year) %>%
  summarise_at(vars(Precip.tower), list(~sum(., na.rm = TRUE)))

  meteo.year <- left_join(meteo.year1, meteo.year2, by = "Year")
  # head((meteo.year)
  library(reshape2)
  meteo.year.long <- melt(meteo.year, id.vars = colnames(meteo.year)[1], variable.name = "variable", value.name = "value")
  # str(meteo.year.long)

  # summary(subset(meteo.year, Y %in% datayears))

  graph.4 <-  ggplot(subset(meteo.year.long, Year != max(datayears)), aes(x = Year, y = value)) +
  geom_line(aes(color = variable), show.legend = F) +
  geom_point(aes(color = variable), size = 1, show.legend = F) +
  facet_grid(variable ~ ., scales = "free_y") +
  ylab("") +
  xlab("year") + my.theme + my.bg + my.adjust + theme(axis.text.x = element_text(size = 10, face = "plain", angle = 90)) +
  scale_x_continuous(breaks = datayears) +
  ggtitle("Met Tower Annual summary met data: Tower Top and Forest Floor")
  graph.4;
  #ggsave(file.path("figures/annual_met_data.pdf"), height = 12, width = 12, units='in')
  ggsave(file.path("figures/graph.4_annual_met_data.png"), height = 10, width = 8, units='in', dpi = 400)

  # metbottom.filled <- select(metbottom.filled, -Year)
  # head((metbottom.filled)
  daily3 <- reshape(metbottom.filled, varying = 5:ncol(metbottom.filled), idvar = "date", direction = "long", v.names = "Value",
    timevar = "Variable", times = names(metbottom.filled)[-(1:4)])
  # head((daily3)
  # str(daily3)

  select.var = c("Tmax.floor", "Tmin.floor", "RHmax.floor", "RHmin.floor", "Rs.floor", "VPDmax.floor")
  graph.5 <-  ggplot(subset(daily3, Variable %in% select.var), aes(x = Julian, y = Value, color = as.factor(Year))) +
  geom_line(aes(x = Julian, y = Value, group = Year)) +
  facet_grid(Variable ~ ., scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 366, by = 30)) +
  scale_color_discrete(name = "Year") +
  my.theme + my.bg + my.adjust + theme(panel.grid.major.x = element_blank()) +
  ggtitle("Daily Met Tower Forest Floor data - by year - after gap filling")
  graph.5;
  #ggsave(file.path("figures/daily_met_data_by_year_forestfloor.pdf"), height = 12, width = 12, units='in')
  ggsave(file.path("figures/graph.5_daily_met_data_by_year_forestfloor.png"), height = 6, width = 8, units='in', dpi = 400)

  # plotting forest floor vs. top of the tower data
  daily2.1 <- subset(metbottom.filled, select = c("date", "Tmax.floor", "Tmin.floor", "RHmax.floor", "RHmin.floor", "Rs.floor"))
  colnames(daily2.1) <-  c("date", "Tmax", "Tmin", "RHmax", "RHmin", "Rs")
  daily4 <- reshape(daily2.1, varying = 2:ncol(daily2.1), idvar = "date", direction = "long", v.names = "Value",
    timevar = "Variable", times = names(daily2.1)[2:ncol(daily2.1)])
  # head((daily4)
  daily4$location <- "Forest Floor"
  daily2.6 <- subset(metbottom.filled, select = c("date","Tmax.tower", "Tmin.tower", "RHmax.tower", "RHmin.tower", "Rs.tower", "Precip.tower", "Ws.tower"))
  colnames(daily2.6) <-  c("date","Tmax", "Tmin", "RHmax", "RHmin", "Rs", "Precip", "Ws")
  # head((daily2.6)
  daily5 <- reshape(daily2.6, varying = 2:ncol(daily2.6), idvar = "date", direction = "long", v.names = "Value",
    timevar = "Variable", times = names(daily2.6)[2:ncol(daily2.6)])
  unique(daily5$Variable)
  # head((daily5)
  daily5$location <- "Tower Top"
  daily4 <- subset(daily4, select = c("date", "Variable", "Value", "location"))
  daily5 <- subset(daily5, select = c("date", "Variable", "Value", "location"))
  daily6 <- rbind.data.frame(daily4, daily5)

  # str(daily6)
  daily6$Julian <-
  as.numeric(format(daily6$date, format = "%j"))
  daily6$Year <-
  format(daily6$date, format = "%Y")
  daily6$year.location <- paste(daily6$Year, daily6$location, sep = ".")
  daily6 <- daily6[order(daily6$location, daily6$Year),]

  graph.6 <-  ggplot(daily6, aes(x = Julian, y = Value, color = location)) +
  geom_line(aes(x = Julian, y = Value, group = year.location)) +
  facet_grid(Variable ~ ., scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 366, by = 30)) +
  my.theme + my.bg + my.adjust + theme(panel.grid.major.x = element_blank()) +
  ggtitle("Daily Met Tower data Tower Top vs. Forest Floor")
  graph.6;
  #ggsave(file.path("figures/Tower Top vs. Forest Floor.pdf"), height = 7, width = 12, units='in')
  ggsave(file.path("figures/graph.6_Tower Top vs. Forest Floor.png"), height = 6, width = 8, units='in', dpi = 400)

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
  # head((clim)
  graph.7 <- ggplot(clim, aes(x = Julian, y = mean, color = location)) +
  geom_line() +
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se)) +
  facet_grid(Variable ~ ., scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 366, by = 30)) +
  my.theme + my.bg + my.adjust + theme(panel.grid.major.x = element_blank()) +
  ylab("Daily mean +- SE") +
  ggtitle("Met Tower data Tower Top vs. Forest Floor")
  graph.7;
  #ggsave(file.path("figures/Tower Top vs. Forest Floor_mean & SE.pdf"), height = 7, width = 12, units='in')
  ggsave(file.path("figures/graph.7_Tower Top vs. Forest Floor_mean & SE.png"), height = 6, width = 8, units='in')

  write.table(clim,
    "results/climatic_mean.txt",
    row.names = FALSE)
  usethis::use_data(clim, overwrite = TRUE)
  load("data/clim.Rda")
  ##------------------------------------------------------------------
  ## Substitute mettower and metbottom.filled with climatic averages
  ##------------------------------------------------------------------
  ## Daily data gap-filling
  # Need spread form of climatic means
  ## Using metbottom.filled
  metbottom.filled$Julian <-
  as.numeric(format(as.Date(metbottom.filled$date), format = "%j"))
  clim.doy <-
  metbottom.filled %>% select(-date, -Year, -Month, -tower.RH.Flag) %>%
  group_by(Julian) %>%
  dplyr::summarise_all(list(mean = ~mean(., na.rm = TRUE)))

  SERC.daily.data <- dplyr::left_join(metbottom.filled, clim.doy, by = "Julian") %>%
  mutate(Precip.tower = if_else(is.na(Precip.tower), Precip.tower_mean, Precip.tower),
   Ws.tower = if_else(is.na(Ws.tower), Ws.tower_mean, Ws.tower),
   Bp.tower = if_else(is.na(Bp.tower), Bp.tower_mean, Bp.tower),
   Tmax.tower = if_else(is.na(Tmax.tower), Tmax.tower_mean, Tmax.tower),
   Tmin.tower = if_else(is.na(Tmin.tower), Tmin.tower_mean, Tmin.tower),
   RHmax.tower = if_else(is.na(RHmax.tower), RHmax.tower_mean, RHmax.tower),
   RHmin.tower = if_else(is.na(RHmin.tower), RHmin.tower_mean, RHmin.tower),
   Rs.tower = if_else(is.na(Rs.tower), Rs.tower_mean, Rs.tower),
   SVPmax.tower = if_else(is.na(SVPmax.tower), SVPmax.tower_mean, SVPmax.tower),
   VPDmax.tower = if_else(is.na(VPDmax.tower), VPDmax.tower_mean, VPDmax.tower),
   Tmax.floor = if_else(is.na(Tmax.floor), Tmax.floor_mean, Tmax.floor),
   Tmin.floor = if_else(is.na(Tmin.floor), Tmin.floor_mean, Tmin.floor),
   RHmax.floor = if_else(is.na(RHmax.floor), RHmax.floor_mean, RHmax.floor),
   RHmin.floor = if_else(is.na(RHmin.floor), RHmin.floor_mean, RHmin.floor),
   Rs.floor = if_else(is.na(Rs.floor), Rs.floor_mean, Rs.floor),
   SVPmax.floor = if_else(is.na(SVPmax.floor), SVPmax.floor_mean, SVPmax.floor),
   VPDmax.floor = if_else(is.na(VPDmax.floor), VPDmax.floor_mean, VPDmax.floor)
   ) %>% # remove clim.doy columns
  select(-c(colnames(clim.doy)[-1]))
  summary(SERC.daily.data)
  write.table(SERC.daily.data,
    "data_to_publish/SERC_daily_data.txt",
    row.names = FALSE)
  # h5write(SERC.daily.data, "data_to_publish/SERC_daily_data.h5", "SERC.daily.data")
  save(SERC.daily.data, file = "data_to_publish/SERC_daily_data.rda")
  usethis::use_data(SERC.daily.data, overwrite = TRUE)
  # load("data/SERC.daily.data.Rda")

  plot.datagaps.3 <- function(mydata) {
    mydata.p <- mydata %>% select(-Year, -Month, -Julian) %>% gather(key = "key", value = "value", -date)
    xyplot(value ~ date | key, data = mydata.p, scales = list(
      y=list(relation='free')),
    type = c("l"), main = "Mettower Daily Data Gap-filled With Climatic Means") +
    layer_(panel.xblocks(x, is.na(y), col = "darkgray"))
  }
  #pdf("figures/mettower_daily_gap.pdf", height = 9, width = 12)
  png("figures/mettower_daily_gap_filled_with_clim.png", height = 768, width = 1344, units ="px", res = 150)
  plot.datagaps.3(SERC.daily.data)
  while (!is.null(dev.list())) {dev.off()}

  ## Minute data gap-filling
  # Need spread form of climatic means
  met_minute$minute <-
  as.numeric(format(as.POSIXct(met_minute$date.time), format = "%M"))
  met_minute$doy.min <- paste(met_minute$Julian, met_minute$minute, sep = "-")
  clim.doy.min <-
  met_minute %>% select(-date.time, -date, -Year, -Month, -Julian, -Hour, -tower.RH.Flag) %>%
  group_by(doy.min) %>%
  dplyr::summarise_all(list(mean = ~mean(., na.rm = TRUE)))
  ## Using met_minute data
  SERC.minute.data <- dplyr::left_join(met_minute, clim.doy.min, by = "doy.min") %>%
  mutate(Precip.tower = if_else(is.na(Precip.tower), Precip.tower_mean, Precip.tower),
   Ws.tower = if_else(is.na(Ws.tower), Ws.tower_mean, Ws.tower),
   Bp.tower = if_else(is.na(Bp.tower), Bp.tower_mean, Bp.tower),
   Temp.tower = if_else(is.na(Temp.tower), Temp.tower_mean, Temp.tower),
   RH.tower = if_else(is.na(RH.tower), RH.tower_mean, RH.tower),
   Rs.tower = if_else(is.na(Rs.tower), Rs.tower_mean, Rs.tower),
   SVP.tower = if_else(is.na(SVP.tower), SVP.tower_mean, SVP.tower),
   VPD.tower = if_else(is.na(VPD.tower), VPD.tower_mean, VPD.tower),
   Temp.floor = if_else(is.na(Temp.floor), Temp.floor_mean, Temp.floor),
   RH.floor = if_else(is.na(RH.floor), RH.floor_mean, RH.floor),
   Rs.floor = if_else(is.na(Rs.floor), Rs.floor_mean, Rs.floor),
   SVP.floor = if_else(is.na(SVP.floor), SVP.floor_mean, SVP.floor),
   VPD.floor = if_else(is.na(VPD.floor), VPD.floor_mean, VPD.floor)
   ) %>% # remove clim.doy columns
  select(-c(colnames(clim.doy.min)[-1]), -minute, -doy.min)

  # h5write(SERC.minute.data, "data_to_publish/SERC_minute_data.h5", "SERC.minute.data")
  save(SERC.minute.data, file = "data_to_publish/SERC_minute_data.rda")
  write.table(SERC.minute.data,
              "data_to_publish/SERC_minute_data.txt",
              row.names = FALSE)
  usethis::use_data(SERC.minute.data, overwrite = TRUE)
  # load("data/SERC.minute.data.rda")

  available.daterange <- data.frame(date.time = range(SERC.minute.data$date.time, na.rm = TRUE),
    date = range(SERC.minute.data$date, na.rm = TRUE),
    year = range(SERC.minute.data$Year, na.rm = TRUE),
    month = range(SERC.minute.data$Month, na.rm = TRUE))
  row.names(available.daterange) <- c("lower", "upper")
  write.csv(available.daterange,
    "results/available.daterange.csv",
    row.names = TRUE)

  devtools::document()
  devtools::install()
}

#
# new.filter.minute <- function(met_minute  ) {
#   tmp <- subset(met_minute, Year == 2015)
#  plot(tmp$Julian, tmp$Temp.tower)
#  sp <- smooth.spline(tmp$Temp.tower[!is.na(tmp$Temp.tower)], df = 12)
#  plot(sp, type = "l", ylim = range(tmp$Temp.tower, na.rm = TRUE))
#  points(tmp$Temp.tower[!is.na(tmp$Temp.tower)], cex = 0.5, pch = 19)
#  new.temp <- tmp$Temp.tower[!is.na(tmp$Temp.tower)] - sp$y
#  sd(new.temp)
#  plot(sp$x, new.temp)
#  abline(h = 3*sd(new.temp))
#  abline(h = -3* sd(new.temp))
#
#
#  plot(density(new.temp))
#  }
#
# new.filter.minute(met_minute)

