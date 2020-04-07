#' Met Tower daily data for Forest Floor and Tower Top
#'
#' This is a data package. It holds four datasets: 'mettower_minute', in which minute data are compiled into a single file. Two daily datasets--'mettower' and 'metbottom.clean', both are cleaned for erroneous data and latter one is substituted with some greenhouse data (see details). Finally clim.rda, a dataset with climatic mean and SE for all variables.
#'
#' Limits for erroneous or outlying minute data df_dict0 <- data.frame(variable =  c("Temp","RH","Rs","Precip","uz", "Bp","Temp.tower","RH.tower","Rs.tower"),out_low = c(-25, 0, -20, 0, 0, 25, -25, 0, -20),out_high = c(60, 100, 50, 300, 50, 35, 60, 100, 50))
#'
#' Besides that limits for unacceptable daily summary data in summer (days 150-270): RHmax < 30, RHmin < 10, Tmax < 4, Tmin < 0.
#'
#' Rs data are replaced by QAQCd data given by Pat Neale.
#'
#' Major data gaps exist: Jan 2015 - March 9, 2017 for Tower-Top. August 1, 2013 until end of 2014 for Forest Floor.
#'
#'In metbottom.clean (& substituted) version of the data, unacceptable seasonal data for forestfloor Temp, RH are substituted with greenhouse station data available on those days.
#'
#'Check Readme.html for full details.
#'
#'To update package: Knit Updater.rmd. It will also install the package. If documentaion is to be updated in data.R, run devtools::document() before Build & Install.

#' This is the minute data version, precursor to mettower
"met_minute"

#' This is the daily summary dataset. Cleaned for data beyond unacceptable range.
"met_daily"

#' Daily summary data 'mettower' is substituted with SERC greenhouse station data. Unacceptable seasonal data for Temp, RH of forest floor are also removed. Daily summaries for Tower Top are retained here, but are not gap-filled.
"metbottom.filled"

#' This has climatic means by location
"clim"

#' This is daily summary data gap-filled with climatic means for the DOY
"SERC.daily.data"

#' This is minute data gap-filled with climatic means for DOY-minute
"SERC.minute.data"
