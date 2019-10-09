##----------------
## Title: SERC Met Tower minute data: Standardize units, clean, gap-fill, summarise daily_includes forest floor
## Author: Rutuja
## Date: March 29, 2018
##----------------
#
# rm(list = ls())
fun.mettower <- function(year = year, df_dict0 = df_dict0, met.Rs.QC.3 = met.Rs.QC.3) {

  if (!require("pacman")) install.packages("pacman"); library(pacman)
  pacman::p_load(tidyverse, scales, janitor, lubridate, usethis, devtools,
                 latticeExtra, neon)

  # graphics info
  tex <- element_text(size = 12, face = "plain") # , family = "gara"
  my.theme <-  theme(axis.text = tex, axis.title = tex,
                     title = tex, legend.title = tex, legend.text = tex, strip.text = tex, strip.text.x = tex)
  my.bg <- theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.major.x = element_line(colour = "black", size = 0.25),
                              panel.grid.minor = element_blank())
  my.adjust <- theme(axis.title = element_text(vjust = 1), axis.title.x = element_text(vjust = -0.6), title = element_text(vjust = 2))

  ## Tidying up SERC met tower weather data
  # year <- 2019
  load(file = paste0("data-raw/minute_data_", paste(2003, year, sep = "-"), ".Rda"))
  datayears <- c(2003: year)
  # head((met2); tail(met2)
  met2 <- met2
  #remove rows with all NAs
  # nrow(met2)
  met2 <- met2[rowSums(is.na(met2)) != ncol(met2), ]
  # nrow(met2)

  unique(met2$Year)
  ## all the non-i year data seems to be erroneously present, removing that. (typically just one day)
  # ## for year 2012
  # # row 216002 to 216050 had half the rows data overflown in to the next one.
  # # Locating that area
  ## Well, not fixing that right now. just removing those data
  met2 <- met2[met2$Year %in% datayears, ]
  # nrow(met2)

  ## climatedata should contain variables as follows
  # Tmax - daily maximum temperature in degree Celcius,
  # Tmin - daily minimum temperature in degree Celcius,
  # RHmax - daily maximum relative humidity in percentage,
  # RHmin - daily minimum relative humidity in percentage,
  # Rs - daily incoming solar radiation in Megajoules per square metres per day
  # Precip - daily precipitation in millimeters,
  # u2 - subdaily wind speed measured at 2 meters from the ground surface in meters per second,
  # Ws - subdaily wind speed in meters per second,
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
  # head((met2);
  dim(met2)
  met2 <- met2[order(met2$date, met2$date.time),]
  met3 <- setNames(data.frame(matrix(ncol = ncol(met2), nrow = nrow(met2))), colnames(met2))
  met3$Year <- met2$Year; met3$Julian <- met2$Julian; met3$Hour <- met2$Hour;
  met3$date <- met2$date; met3$date.time <- as.POSIXct(met2$date.time)
  met3$Ws.tower <- met2$Ws.tower

  ##------------------------------
  ## Converting into desired units
  ##------------------------------

  #****Do not repeat the conversions***
  # Units in which data is collected is given in "literature/Column_# head(ers_SERC_weather_data" taken from folder "Data/SERC_Met_Data/MET_TOWER"
  # str(met2);
  met3$Temp.floor <- as.numeric(met2$Temp.floor)
  met3$Temp.floor <-
    met2$Temp.floor * 100 - 40 # Volts to degree celcius
  met3$RH.floor <- met2$RH.floor * 100
  met3$Precip.tower <- met2$Precip.tower * 25.4 # inches to mm
  met3$Rs.floor <-
    met2$Rs.floor * 10 ^ 6 / 6  * 0.0864 #Volts to W/m-2 to MJ/m2/day (0.0864 multiplier does the last conversion) # 0.2 W/m2 should be normal per minute
  #met$Ws is in meter/second
  met3$Bp.tower <- #
    met2$Bp.tower*3.38639 # Looks like it is rather in inches Hg, and not pascals. So converting to kPa
  met3$Temp.tower <-
    met2$Temp.tower * 100 - 40 # Volts to degree celcius
  met3$RH.tower <- met2$RH.tower * 100
  met3$Rs.tower <-
    met2$Rs.tower * 10 ^ 6 / 6  * 0.0864 #Volts to W/m-2 to MJ/m2/day (0.0864 multiplier does the last conversion) # 0.2 W/m2should be normal per minute
  #----------------
  # summary(met3)
  # head((met3)
  ## removing rows with all NAs
  # nrow(met3)
  met3 <- met3[rowSums(is.na(met3)) != ncol(met3), ]
  # nrow(met3)

  ##  ****Tower temperature offset ****-------
  # Notes from data-raw/MetTower_T RH Record Documentation_Pat_Neale/Readme for Tower and Forest Floor Temperature and Relative Humidity data.doc
  # Tower measurements were shifted to a RM Young 41382 Fan Aspirated unit on 3/9/2017.
  # **** NOTE  **** The HMP45AC and RMY 41282 have different temperature offsets -40° and -50°, respectively
  # so need to remove 10 deg Cel to Temp.tower since 3/9/2017
  ##--------
  met3 <- met3 %>% mutate(Temp.tower.before = Temp.tower,
                          Temp.tower = if_else(date > as.Date("2017-03-09"),
                                               Temp.tower.before - 10, Temp.tower.before))
  daily.Temp <- met3 %>% select(-date.time) %>%
    group_by(date) %>% summarise(
      Tmax.tower.before = max(Temp.tower.before, na.rm = TRUE),
      Tmax.tower.after = max(Temp.tower, na.rm = TRUE)) %>%
    gather(key = "variable", value = "value", -date) %>%
    mutate(offset = if_else(variable == "Tmax.tower.before",
                            "Before offset correction", "After offset correction"),
           variable = "Tmax",
           offset = fct_relevel(offset, "After offset correction", after = 1))
  # head((daily.Temp)
  graph.1 <- ggplot(daily.Temp, aes(x = date, y = value)) +
    geom_point(size = 0.5, alpha = 0.7)+
    ylim(c(df_dict0$out_low[which(df_dict0$variable == "Temp.tower")],
           df_dict0$out_high[which(df_dict0$variable == "Temp.tower")])) +
    ylab("degree C") + xlab("Date") +
    facet_grid(offset ~ .) + my.theme + my.bg +
    ggtitle("Tower Top Temp offset changes by 10 deg C 2017-03-09 onwards")

  graph.1;
  #ggsave(file.path("figures/graph.1_Tower Temp offset changes by 10 deg C 2017-03-09 onwards.pdf"), height = 12, width = 12, units='in')
  ggsave(file.path("figures/graph.1_Tower Temp offset changes by 10 deg C 2017-03-09 onwards.png"), height = 5, width = 6, units='in')

  met3 <- met3 %>% select(-Temp.tower.before) %>%
    mutate(date = as.Date(date),
           Julian =
             as.numeric(format(date, format = "%j")))
  save(met3, file = "data-raw/minute_data_all_years_metric_raw.Rda")
  # met3 %>% select(-Year, -Julian, Rs,floor, Rs.tower) %>%
  #   group_by(date) %>% summarise_all(list(~mean(., na.rm = T)))

  plot.datagaps.1 <- function(mydata) {
    mydata.p <- mydata %>% select(-Year, -Julian, -Hour, -date.time) %>%
      group_by(date) %>%
      summarise_all(list(~mean(., na.rm = T))) %>%
      gather(key = "key", value = "value", -date)

    xyplot(value ~ date | key, data = mydata.p, scales = list(
    y = list(relation='free')),
    type = c("l"), main = "Mettower Raw Data Daily Means in SI units") +
    layer_(panel.xblocks(x, is.na(y), col = "darkgray"))
  }
  png("figures/mettower Raw Data Daily Means in SI units.png", height = 768, width = 1024, units ="px", res = 150)
  plot.datagaps.1(met3)
  while (!is.null(dev.list()))  {dev.off()}

  ###--------------------------------------------
  ### Substituting clean Rs data from Pat Neale--
  ###--------------------------------------------
  load(file = "data-raw/minute_data_all_years_metric_raw.Rda")

  met4 <- met3; rm(met3)

  # View(# head((met4[which(met4$date %in% tower.bad.data.dates),]))
  ### from Pat Neale QA,QCd Met Rs data------------
  # met.Rs.QC.1 <- read.csv("data-raw/MetTower_Rs_data_from_Pat_Neale/TowerWoodsPSP02_17.csv",
  #                       na.strings = c("NA", "NaN", ""),
  #                       # head(er = FALSE,
  #                       row.names = NULL,
  #                       check.names = F)
  # met.Rs.QC.2 <- read.csv("data-raw/MetTower_Rs_data_from_Pat_Neale/TowerWoodsPSP18.csv",
  #                       na.strings = c("NA", "NaN", ""),
  #                       # head(er = FALSE,
  #                       row.names = NULL,
  #                       check.names = F)
  # met.Rs.QC <- bind_rows(met.Rs.QC.1, met.Rs.QC.2)
  # For the PSP data, the file has day number (excel format), time
  # as HHMM integer, then Rs W m-2 for tower and forest floor. NaN’s replace any
  # “bad”points, this includes all points with Rs < 0 for forest floor.
  met.Rs.QC <- met.Rs.QC.3
  colnames(met.Rs.QC) <- c("date", "time", "Rs.tower", "Rs.floor")
  # head((met.Rs.QC)
  met.Rs.QC$date <- excel_numeric_to_date(as.numeric(met.Rs.QC$date), include_time = FALSE)
  met.Rs.QC$Hour2 <- formatC(as.numeric(met.Rs.QC$time),  width = 4, flag = "0")
  met.Rs.QC$date.time <- as.POSIXct(strptime(paste(as.character(met.Rs.QC$date), met.Rs.QC$Hour2, sep = "-"),
                                             format = "%Y-%m-%d-%H%M"))
  # str(met.Rs.QC)
  met.Rs.QC <- select(met.Rs.QC, -Hour2, -time)
  met.Rs.QC$Rs.tower <- met.Rs.QC$Rs.tower * 0.0864  # from W/m2 to MJ m-2 day-1
  met.Rs.QC$Rs.floor <- met.Rs.QC$Rs.floor * 0.0864  # from W/m2 to MJ m-2 day-1
  met.Rs.QC$Year <-
    as.numeric(format(met.Rs.QC$date, format = "%Y"))
  met.Rs.QC$Julian <-
    as.numeric(format(met.Rs.QC$date, format = "%j"))
  met.Rs.QC %>% select(Year, Rs.floor, Rs.tower) %>%
    group_by(Year) %>% summarise_all(list(~mean(., na.rm = T)))
  met.Rs.QC.p <- met.Rs.QC %>% select(date, Rs.floor, Rs.tower) %>%
    group_by(date) %>% summarise_all(list(~mean(., na.rm = T))) %>%
    gather(key = "key", value = "value", -date)
   graph.2 <-  ggplot(met.Rs.QC.p, aes(x = date, y = value)) +
    geom_line() + ylab("MJ m-2 day-1") + xlab("Date") +
    facet_grid(key ~ ., scales = "free_y") +
    my.theme + my.bg + my.adjust + theme(panel.grid.major.x = element_blank()) +
    ggtitle("Daily Met Tower cleaned Rs data from Pat Neale\nTower Top versus Forest Floor")
   graph.2;
   #ggsave(file.path("figures/Daily Met Tower cleaned Rs data from Pat Neale Tower Top & Forest floor.pdf"), height = 12, width = 12, units='in')
   ggsave(file.path("figures/graph.2_Daily Met Tower cleaned Rs data from Pat Neale Tower Top & Forest floor.png"), height = 5, width = 6, units='in')

  ## making space
  rm(met2)
  ## making space


  met4 <- left_join(met4, select(met.Rs.QC, date.time, Rs.floor, Rs.tower), by = "date.time")
  met4 <- met4 %>% mutate(Rs.floor = Rs.floor.y,
                          Rs.tower = Rs.tower.y) %>%
    select(-Rs.floor.x, -Rs.floor.y, -Rs.tower.x, -Rs.tower.y)

  ##--------------------------------------
  ## Removing unacceptable/erroneous data
  ##--------------------------------------

  ##*****
  ## 1. Remove data within known, bad date range
  ##*****
  ## Doing this first since filter based on RH > 100

  ## See Notes from data-raw/MetTower_T RH Record Documentation_Pat_Neale/Readme for Tower and Forest Floor Temperature and Relative Humidity data.doc
  ## Met Tmax, Tmin, RHmin, RHmax (that is forest floor) readings problematic from August 1, 2013 until end of 2014
  ## Met Tmax.tower, Tmin.tower, RHmin.tower, RHmax.tower (that is tower top) readings problematic in the range Jan 2015 - March 9, 2017 (new unit since 3/9/2017)

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
  #   How to calculate air VPD: #https://getpulse.co/blog/vpd#calculate-air
  #   https://en.wikipedia.org/wiki/Vapour_pressure_of_water
  #   SVP = 0.61078 x exp(T / (T + 237.3) x 17.2694)); # The Tetens equation
  #   OR more accutate Buck's equation:
  #   SVP = 0.61121 x exp((18.678 - T/234.84) * (T/(257.14 + T))) # The Buck equation
  #   T is in degrees Celsius; SVP in kPa

  met4 <- met4 %>% mutate(SVP.tower = 0.61121 * exp((18.678 - Temp.tower/234.84) * (Temp.tower/(257.14 + Temp.tower))),
                          SVP.floor = 0.61121 * exp((18.678 - Temp.floor/234.84) * (Temp.floor/(257.14 + Temp.floor))))

  # Cleaning SVP beyond acceptable range before calculating VPD

  ##*****
  ## 2. Remove data beyond acceptable range
  ##*****

  # to substitute outliers with NAs in a new dataframe
  # Gust winds could be upto 70 km/h i.e. 20 m/s
  # This is made variable in the Updater.Rmd
  # df_dict0 <-
  #   data.frame(
  #     variable = c("Precip.tower", "Ws.tower", "Bp.tower", "Rs.floor", "Rs.tower", "Temp.floor", "Temp.tower", "RH.floor", "RH.tower", "SVP.floor", "SVP.tower"),
  #     out_low = c(0, 0, 90, 0, 0, -25, -25, 0, 0, 0, 0),
  #     out_high = c(300, 50, 104, 30, 40, 50, 50, 100, 100, 10, 10)
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
  # summary(met4)
  # nrow(met4)
  met4 <- met4[rowSums(is.na(met4)) != ncol(met4), ]
  # Also seasonally:
  ## Tmax in summer below 4 looks unaccetable
  met4$Temp.floor[met4$Julian > 150 & met4$Julian < 270 & met4$Temp.floor < 4] <- NA
  met4$Temp.tower[met4$Julian > 150 & met4$Julian < 270 & met4$Temp.tower < 4] <- NA

  #   Calculate the VPD = SVP x (1 – RH/100) = VPD
  met4 <- met4 %>% mutate(VPD.tower = SVP.tower * (1 - RH.tower/100),
                          VPD.floor = SVP.floor * (1 - RH.floor/100))

  mettower_minute <- met4; rm(met4)
  # write.table(mettower_minute,
  #           "data-raw/mettower_minute.txt",
  #           row.names = FALSE)
  usethis::use_data(mettower_minute, overwrite = TRUE) # for smaller size use compress = 'xz'
  load("data/mettower_minute.Rda")

  # head((mettower_minute)
  # str(mettower_minute)
  missing <- function (x) {
    df <- data.frame(var = colnames(x), percent_missing = NA)
    for (i in 1:ncol(x)) {
      df[i, 2] <- round(length(which(is.na(x[[i]]))) /nrow(x) * 100, 3)
    }
    print(df)
  }
  missing(mettower_minute)
  # var percent_missing
  # 4     Temp.floor           8.721
  # 5       RH.floor           8.631
  # 6   Precip.tower           0.000
  # 7       Ws.tower           0.000
  # 8       Bp.tower           0.154
  # 9     Temp.tower           3.210
  # 10      RH.tower           3.878
  # 11     date.time           0.012
  # 12          date           0.000
  # 13      Rs.floor          53.849
  # 14      Rs.tower          62.042
  # 15 tower.RH.Flag           0.000
  # 16     SVP.tower           1.520
  # 17     SVP.floor           8.630
  # 18     VPD.tower           4.936
  # 19     VPD.floor           8.631

  ##--------------------------------------
  ## A gap-filled record with NEON data using inter-relationships
  ##--------------------------------------
  neon.sub <- neon::neon_met_minute %>% select(-date, -throughfall, -RHmin.floor, -Tmin.floor, -Tmin.tower, -RHmin.tower,
                                               -RHmax.floor, -Tmax.floor, -Tmax.tower, -RHmax.tower)
  metmin.neon <- left_join(mettower_minute, neon.sub, by = "date.time")
  metmin.neon.sub <- metmin.neon %>% subset(date > min(neon.sub$date))
  # head((metmin.neon)

  metmin.filled <- metmin.neon
  vars.met <- c("Temp.tower", "RH.tower", "Rs.tower.x", "Temp.floor", "RH.floor", "Bp.tower.x")
  vars.neon <- c("Tmean.tower", "RHmean.tower", "Rs.tower.y", "Tmean.floor", "RHmean.floor", "Bp.tower.y") # NEON forest floor data missing for last download
  compare.plots <- list(); model.var <- list()
  var.na <- data.frame(var = vars.met); var.na$rsq <- var.na$intercept <- var.na$slope <- var.na$after.na <- var.na$before.na <- NA
  par(mfrow=c(2,3))

  for (i in 1 : length(vars.met)){
    var.na$rsq[i] <- round(cor(metmin.neon.sub[, vars.met[i]], metmin.neon.sub[, vars.neon[i]],
                         method = "spearman", use = "complete")^0.2, 3)
    # plot(metmin.neon.sub[, vars.met[i]] ~ metmin.neon.sub[, vars.neon[i]],
    #      ylab = vars.met[i],  xlab = vars.neon[i])
    m.var <- lm(metmin.neon.sub[, vars.met[i]] ~ metmin.neon.sub[, vars.neon[i]]) ## using Tmean as that is the mean of the minute
    model.var[[i]] <- m.var
    na.rows <- is.na(metmin.filled[, vars.met[i]])
    var.na$before.na[i] <- length(which(na.rows == TRUE))
    var.na$intercept[i] <- round(model.var[[i]]$coef[1], 2)
    var.na$slope[i] <- round(model.var[[i]]$coef[2], 2)

    # substitute only when Rsq greater than 0.9
    ## Rs is really bad (why?), Rh.floor poor
    if (var.na$rsq[i] > 0.9) {
      metmin.filled[na.rows, vars.met[i]] <-
      model.var[[i]]$coef[1] + model.var[[i]]$coef[2]*metmin.filled[na.rows, vars.neon[i]]
      var.na$after.na[i] <- length(which(is.na(metmin.filled[, vars.met[i]]) == TRUE))
    } else {
      var.na$after.na[i] <- var.na$before.na[i]
    }
  }
  par(mfrow=c(1,1))
  # Precip substitution by minute does not seem appropriate, so not done
  rm(m.var); rm(na.rows)
  var.na
  # var before.na after.na slope intercept   rsq
  # 1 Temp.tower    265402   265323  0.97      0.88 0.989
  # 2   RH.tower    320671   315197  0.80     14.71 0.946
  # 3 Rs.tower.x   5130395  5130395  0.05     11.89 0.578
  # 4 Temp.floor    721147   721146  0.85      0.46 0.956
  # 5   RH.floor    713697   713697  0.68     24.24 0.920
  # 6 Bp.tower.x     12701    12673  0.90      9.96 0.986

  # Effect sizes look dramatic though, so not sure if substitution should be done without further evaluation.

  metmin.filled <- metmin.filled %>% select(-Precip.tower.y, -Tmean.floor, -RHmean.floor,
                                          -Ws.floor, -Bp.tower.y, -Tmean.tower,
                                          -RHmean.tower, -Ws.tower.y, -Rs.tower.y) %>%
    rename(Precip.tower = Precip.tower.x, Ws.tower = Ws.tower.x, Bp.tower = Bp.tower.x, Rs.tower = Rs.tower.x)
  metmin.filled <- metmin.filled %>% mutate(SVP.tower = 0.61121 * exp((18.678 - Temp.tower/234.84) * (Temp.tower/(257.14 + Temp.tower))),
                          SVP.floor = 0.61121 * exp((18.678 - Temp.floor/234.84) * (Temp.floor/(257.14 + Temp.floor))))
  missing(metmin.filled)
  # 4     Temp.floor           8.721
  # 5       RH.floor           8.631
  # 6   Precip.tower           0.000
  # 7       Ws.tower           0.000
  # 8       Bp.tower           0.153
  # 9     Temp.tower           3.209
  # 10      RH.tower           3.812
  # 11     date.time           0.012
  # 12          date           0.000
  # 13      Rs.floor          53.849
  # 14      Rs.tower          62.043
  # 15 tower.RH.Flag           0.000
  # 16     SVP.tower           3.209
  # 17     SVP.floor           8.721
## dont understand why SVP missing values are increasing
  ## make all RH records greater than 100 to 100
  metmin.filled <- metmin.filled %>%
    mutate(RH.floor = if_else(RH.floor > 100, 100, RH.floor),
           RH.tower = if_else(RH.tower > 100, 100, RH.tower))

  vars0 <- df_dict0$variable
  total <- length(vars0)
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  for (i in vars0) {
    metmin.filled[[i]][metmin.filled[[i]] < df_dict0[df_dict0$variable == i,]$out_low |
                         metmin.filled[[i]] > df_dict0[df_dict0$variable == i,]$out_high] <- NA
    Sys.sleep(0.1)
    # update progress bar
    setTxtProgressBar(pb, i)
  }
  #   Calculate the VPD = SVP x (1 – RH/100) = VPD
  metmin.filled <- metmin.filled %>% mutate(VPD.tower = SVP.tower * (1 - RH.tower/100),
                          VPD.floor = SVP.floor * (1 - RH.floor/100))

  missing(metmin.filled)
  # 4     Temp.floor           8.721
  # 5       RH.floor           8.631
  # 6   Precip.tower           0.000
  # 7       Ws.tower           0.000
  # 8       Bp.tower           0.153
  # 9     Temp.tower           3.209
  # 10      RH.tower           3.878
  # 11     date.time           0.012
  # 12          date           0.000
  # 13      Rs.floor          53.849
  # 14      Rs.tower          62.043
  # 15 tower.RH.Flag           0.000
  # 16     SVP.tower           3.209
  # 17     SVP.floor           8.721
  # 18     VPD.tower           5.243
  # 19     VPD.floor           8.721
  # summary(metmin.filled)
  # nrow(metmin.filled)
  # usethis::use_data(metmin.filled, overwrite = TRUE)


  # load("data/metmin.neon.Rda")
  ##--------------------------------------
  ## Getting daily summaries
  ##--------------------------------------
  mettower_minute$date <- as.character(mettower_minute$date)

  daily0 <- mettower_minute %>% select(-date.time) %>%
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
      Rs.floor = mean(Rs.floor, na.rm = TRUE),
      Rs.tower = mean(Rs.tower, na.rm = TRUE),
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

  from <- min(as.Date(daily1$date))
  to <- as.Date(daily1$date[length(daily1$date)])
  full <- data.frame(date = as.character(seq(from, to, by = '1 day')))
  daily2 <- merge(daily1, full, by = "date", all = T)
  daily2$date <- strptime(daily2$date, format = "%Y-%m-%d")
  daily2$Year <-
    as.numeric(format(daily2$date, format = "%Y"))

  daily2 <- daily2[order(daily2$date), ]
  daily2 <- daily2[!duplicated(daily2$date), ]
  missing(daily2)
  # var percent_missing
  # 1           date           0.000
  # 2   Precip.tower           3.138
  # 3       Ws.tower           3.138
  # 4       Bp.tower           3.220
  # 5     Tmax.floor          11.561
  # 6     Tmin.floor          11.561
  # 7    RHmax.floor          11.561
  # 8    RHmin.floor          11.561
  # 9     Tmax.tower           4.905
  # 10    Tmin.tower           4.905
  # 11   RHmax.tower           6.672
  # 12   RHmin.tower           6.672
  # 13      Rs.floor          15.987
  # 14      Rs.tower           7.069
  # 15  SVPmax.floor          11.561
  # 16  VPDmax.floor          11.561
  # 17  SVPmax.tower           3.369
  # 18  VPDmax.tower           6.804
  # 19 tower.RH.Flag           3.138
  # 20          Year           0.000
  daily2$date <- as.Date(daily2$date)
  daily2$Julian <-
    as.numeric(format(daily2$date, format = "%j"))
  ## replacing seasonally unacceptable extremes
  ## RHmax in summer below 30 looks unacceptable
  daily2$RHmax.floor[daily2$Julian >150 & daily2$Julian < 330 & daily2$RHmax.floor < 30] <- NA
  daily2$RHmax.tower[daily2$Julian >150 & daily2$Julian < 330 & daily2$RHmax.tower < 30] <- NA
  ## RHmin in summer below 10 looks unaccetable
  daily2$RHmin.floor[daily2$Julian > 150 & daily2$Julian < 330 & daily2$RHmin.floor < 10] <- NA
  daily2$RHmin.tower[daily2$Julian > 150 & daily2$Julian < 330 & daily2$RHmin.tower < 10] <- NA

  ## Tmax in summer below 4 looks unaccetable
  daily2$Tmax.floor[daily2$Julian > 150 & daily2$Julian < 270 & daily2$Tmax.floor < 4] <- NA
  daily2$Tmax.tower[daily2$Julian > 150 & daily2$Julian < 270 & daily2$Tmax.tower < 4] <- NA
  ## Tmin in summer below 0 looks unaccetable
  daily2$Tmin.floor[daily2$Julian > 150 & daily2$Julian < 270 & daily2$Tmin.floor < 0] <- NA
  daily2$Tmin.tower[daily2$Julian > 150 & daily2$Julian < 270 & daily2$Tmin.tower < 0] <- NA
  # summary(daily2)
  mettower <- daily2
  write.table(mettower, "data-raw/mettower.txt", row.names = F)
  # mettower <- read.table("data-raw/mettower.txt", # head(er = TRUE)
  usethis::use_data(mettower, overwrite = TRUE)
  load("data/mettower.Rda")

  plot.datagaps.2 <- function(mydata) {
  mydata.p <- mydata %>% select(-Year, -Julian) %>% gather(key = "key", value = "value", -date)
  xyplot(value ~ date | key, data = mydata.p, scales = list(
    y=list(relation='free')),
    type = c("l"), main = "Mettower Daily Data gaps") +
    layer_(panel.xblocks(x, is.na(y), col = "darkgray"))
  }
  #pdf("figures/mettower_daily_gap.pdf", height = 9, width = 12)
  png("figures/mettower_daily_gap.png", height = 768, width = 1344, units ="px", res = 150)
  plot.datagaps.2(mettower)
  while (!is.null(dev.list())) {dev.off()}

  mettower.p.2 <- mettower %>% gather(key = "key", value = "value", -date, -Julian, -Year)
  select.var <- c("Tmax.tower", "Tmin.tower", "RHmax.tower", "RHmin.tower", "Rs.tower", "Precip.tower", "Ws.tower", "Bp.tower", "VPDmax.tower")
  graph.3 <-  ggplot(subset(mettower.p.2, key %in% select.var), aes(x = Julian, y = value, color = as.factor(Year))) +
    geom_line(aes(x = Julian, y = value, group = Year)) +
    facet_grid(key ~ ., scales = "free_y") +
    scale_x_continuous(breaks = seq(0, 366, by = 30)) +
    my.theme + my.bg + my.adjust + theme(panel.grid.major.x = element_blank()) +
    labs(colour = "Year") +
    ggtitle("Daily Met Tower Tower Top data - by year")
  graph.3;
  #ggsave(file.path("figures/daily_met_data_by_year_Tower-Top.pdf"), height = 8, width = 12, units='in')
  ggsave(file.path("figures/graph.3_daily_met_data_by_year_Tower-Top.png"), height = 6, width = 8, units='in', dpi = 400)


  missing(mettower)
  # var percent_missing
  # 1           date            0.00
  # 2   Precip.tower            3.14
  # 3       Ws.tower            3.14
  # 4       Bp.tower            3.22
  # 5     Tmax.floor           11.56
  # 6     Tmin.floor           11.56
  # 7    RHmax.floor           11.56
  # 8    RHmin.floor           11.61
  # 9     Tmax.tower            4.91
  # 10    Tmin.tower            4.91
  # 11   RHmax.tower            7.02
  # 12   RHmin.tower            7.37
  # 13      Rs.floor           15.99
  # 14      Rs.tower            7.07
  # 15  SVPmax.floor           11.56
  # 16  VPDmax.floor           11.56
  # 17  SVPmax.tower            3.37
  # 18  VPDmax.tower            6.80
  # 19 tower.RH.Flag            3.14
  # 20          Year            0.00
  # 21        Julian            0.00
  ##--------------------------------------
  ## Gap-filling
  ##--------------------------------------
  ## green generated by 2.0 SERC_greenhouse_weather_data.R in SERC_hydro-met_data project
  green <- readRDS("data-raw/greenhouse_weather_data_my_daily_aggregation_2011-2017.RDS")
  # head((green)
  missing(green)
  # str(green)

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


  # var percent_missing
  # 1           date            0.00
  # 2   Precip.tower            3.14
  # 3       Ws.tower            3.14
  # 4       Bp.tower            3.22
  # 5     Tmax.floor            2.91
  # 6     Tmin.floor            2.91
  # 7    RHmax.floor            2.91
  # 8    RHmin.floor            2.96
  # 9     Tmax.tower            4.91
  # 10    Tmin.tower            4.91
  # 11   RHmax.tower            7.02
  # 12   RHmin.tower            7.37
  # 13      Rs.floor           15.99
  # 14      Rs.tower            7.07
  # 15  SVPmax.floor           11.56
  # 16  VPDmax.floor           11.56
  # 17  SVPmax.tower            3.37
  # 18  VPDmax.tower            6.80
  # 19 tower.RH.Flag            3.14
  # 20          Year            0.00
  # 21        Julian            0.00

  write.table(metbottom.filled,
              "data-raw/metbottom.filled.txt",
              row.names = FALSE)
  usethis::use_data(metbottom.filled, overwrite = TRUE)
  load("data/metbottom.filled.Rda")

  # summarise_at(mydata, vars(Y2005, Y2006), funs(n(), mean, median))
  metbottom.filled$Y <-
    as.numeric(format(as.Date(metbottom.filled$date), format = "%Y"))
  meteo.year1 <- metbottom.filled %>%
    group_by(Y) %>%
    dplyr::summarise_at(vars(Tmax.floor, Tmin.floor, RHmax.floor, RHmin.floor, Ws.tower, Tmax.tower, Tmin.tower,
                             RHmax.tower, RHmin.tower, Rs.tower), mean, na.rm = TRUE)
  meteo.year2 <- metbottom.filled %>%
    group_by(Y) %>%
    summarise_at(vars(Precip.tower), list(~sum(., na.rm = TRUE)))

  meteo.year <- left_join(meteo.year1, meteo.year2, by = "Y")
  # head((meteo.year)
  library(reshape2)
  meteo.year.long <- melt(meteo.year, id.vars = colnames(meteo.year)[1], variable.name = "variable", value.name = "value")
  # str(meteo.year.long)

  # summary(subset(meteo.year, Y %in% datayears))

  graph.4 <-  ggplot(subset(meteo.year.long, Y != max(datayears)), aes(x = Y, y = value)) +
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

  metbottom.filled <- select(metbottom.filled, -Year)
  # head((metbottom.filled)
  daily3 <- reshape(metbottom.filled, varying = 2:ncol(metbottom.filled), idvar = "date", direction = "long", v.names = "Value",
                    timevar = "Variable", times = names(metbottom.filled)[-1])
  # head((daily3)

  daily3$date <- as.Date(daily3$date)
  daily3$Year <-
    format(daily3$date, format = "%Y")
  daily3$Month <-
    format(daily3$date, format = "%m")
  daily3$Julian <-
    as.numeric(format(daily3$date, format = "%j"))
  # str(daily3)

  select.var = c("Tmax.floor", "Tmin.floor", "RHmax.floor", "RHmin.floor", "Rs.floor", "VPDmax.floor")
   graph.5 <-  ggplot(subset(daily3, Variable %in% select.var), aes(x = Julian, y = Value, color = Year)) +
    geom_line(aes(x = Julian, y = Value, group = Year)) +
    facet_grid(Variable ~ ., scales = "free_y") +
    scale_x_continuous(breaks = seq(0, 366, by = 30)) +
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
              "data-raw/climatic_mean.txt",
              row.names = FALSE)
  usethis::use_data(clim, overwrite = TRUE)

  devtools::document()
  devtools::install()
}
