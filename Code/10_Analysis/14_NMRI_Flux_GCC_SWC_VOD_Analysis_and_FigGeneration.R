library(tidyverse)
library(pracma)
library(xts)
library(dplyr)
library(data.table)
library(ggplot2)
library(ggpubr)
library(ggh4x)
library(ggpmisc)
library(cowplot)
library(amerifluxr)

# ------------ Set directory filepaths
code.fp = getwd()
setwd('../../')
gitdata.fp = paste(getwd(), 'Data', sep = '/')
figs.fp = paste(getwd(), 'Figures', sep = '/')

# ------------ Set data filepaths
lpdr.files = list.files(paste0(gitdata.fp,'/Timeseries_CSVs/LPDRv3'), pattern = '.csv', full.names = TRUE, recursive = FALSE)
lprm.files = list.files(paste0(gitdata.fp,'/Timeseries_CSVs/LPRM'), pattern = '.csv', full.names = TRUE, recursive = FALSE)
smap.files = list.files(paste0(gitdata.fp,'/Timeseries_CSVs/SMAP_09km'), pattern = '.csv', full.names = TRUE, recursive = FALSE)
fluxnet.files = list.files(paste0(gitdata.fp,'/FLUXNET2015'), full.names = TRUE, recursive = FALSE)
amflux.base.files = list.files(paste0(gitdata.fp,'/Ameriflux_BASE'), full.names = TRUE, recursive = FALSE)
gnss.files = list.files(paste0(gitdata.fp, '/GNSS_GPS'), full.names = TRUE, recursive = FALSE)
phenocam.files = list.files(paste0(gitdata.fp, '/Phenocamr'), full.names = TRUE, recursive = FALSE)

# ------------ Set date parameters
# Set dates for full-record datefill
start.date = as.Date('2015-01-01')
end.date = as.Date('2023-01-01')

# Set date sequence corresponding to NMRI collection
date.seq = seq(as.Date('2021-06-30'),as.Date('2021-10-26'),1)

# Read in function to fill in rows for missing dates
source(paste(code.fp, '10_Timeseries_DateFill_Function.R', sep = '/'))

# ------------ Read daily values from timeseries CSV files
# Get GNSS
wkg.gnss = read.csv(gnss.files, header = TRUE)

# Get LPDR VOD
lpdr.wkg = read.csv(lpdr.files, header = TRUE)

# Get LPRM VOD
lprm.wkg = read.csv(lprm.files, header = TRUE)

# Get SMAP VOD
smap.wkg = read.csv(smap.files[1], header = TRUE)
smap.wkg$Dates = as.Date(smap.wkg$Dates)
smap.lpdr.wkg = read.csv(smap.files[2], header = TRUE)
smap.lpdr.wkg$Dates = as.Date(smap.lpdr.wkg$Dates)
smap.lpdr.wkg.mean = smap.lpdr.wkg[smap.lpdr.wkg$AggStat == 'Mean',]
smap.lpdr.wkg.sd = smap.lpdr.wkg[smap.lpdr.wkg$AggStat == 'SD',]
smap.lprm.wkg = read.csv(smap.files[3], header = TRUE)
smap.lprm.wkg$Dates = as.Date(smap.lprm.wkg$Dates)
smap.lprm.wkg.mean = smap.lprm.wkg[smap.lprm.wkg$AggStat == 'Mean',]
smap.lprm.wkg.sd = smap.lprm.wkg[smap.lprm.wkg$AggStat == 'SD',]

# Get PhenocamNetwork GCC
phenocam.gr.wkg = read.csv(phenocam.files[1], skip = 24, header = TRUE) # Grass ROI
phenocam.gr.wkg$date = as.Date(phenocam.gr.wkg$date)
phenocam.sh.wkg = read.csv(phenocam.files[2], skip = 24, header = TRUE) # Shrub ROI
phenocam.sh.wkg$date = as.Date(phenocam.sh.wkg$date)

# Get FLUXNET2015 and convert Dates and DateTime fields to Date/POSIXct classes
fluxnet.wkg = read.csv(fluxnet.files, header = TRUE) # Hourly
fluxnet.wkg$Dates = as.Date(fluxnet.wkg$Dates)
fluxnet.wkg$DateTime = as.POSIXct(fluxnet.wkg$DateTime, format = '%Y-%m-%d %H:00:00')

# Get Ameriflux BASE
amflux.wkg = amf_read_base(amflux.base.files, unzip = TRUE, parse_timestamp = TRUE)

wkg.amflux.dates = as.Date(as.character(amflux.wkg$TIMESTAMP), format = '%Y-%m-%d')
wkg.amflux.datetimes = as.POSIXct(paste('2021', amflux.wkg$MONTH, amflux.wkg$DAY, amflux.wkg$HOUR, sep = '-'), format = '%Y-%m-%d-%H')
wkg.amflux.swc.1 = amflux.wkg$SWC_1_1_1
wkg.amflux.swc.2 = amflux.wkg$SWC_1_2_1
wkg.amflux.swc.3 = amflux.wkg$SWC_1_3_1
wkg.amflux.swc.1.pi = amflux.wkg$SWC_PI_2_1_A
wkg.amflux.swc.2.pi = amflux.wkg$SWC_PI_2_2_A
wkg.amflux.swc.3.pi = amflux.wkg$SWC_PI_2_3_A
wkg.amflux.le = amflux.wkg$LE
wkg.amflux.ta.1 = amflux.wkg$TA_1_1_1
wkg.amflux.ta.2 = amflux.wkg$TA_1_2_1

amflux.wkg = data.frame('Dates' = wkg.amflux.dates,
                        'DateTime' = wkg.amflux.datetimes,
                        'Year' = amflux.wkg$YEAR,
                        'Month' = amflux.wkg$MONTH,
                        'Day' = amflux.wkg$DAY,
                        'LocalHour' = amflux.wkg$HOUR,
                        'LE' = amflux.wkg$LE,
                        'TA_1' = wkg.amflux.ta.1,
                        'TA_2' = wkg.amflux.ta.2,
                        'SWC_1' = wkg.amflux.swc.1,
                        'SWC_2' = wkg.amflux.swc.2,
                        'SWC_3' = wkg.amflux.swc.3,
                        'SWC_1_PI' = wkg.amflux.swc.1.pi,
                        'SWC_2_PI' = wkg.amflux.swc.2.pi,
                        'SWC_3_PI' = wkg.amflux.swc.3.pi)

amflux.wkg$SWC_mean = rowMeans(amflux.wkg[,c('SWC_1','SWC_1_PI')], na.rm = TRUE)
amflux.wkg$SWC_1_mean = rowMeans(amflux.wkg[,c('SWC_1','SWC_1_PI')], na.rm = TRUE)
amflux.wkg$SWC_2_mean = rowMeans(amflux.wkg[,c('SWC_2','SWC_2_PI')], na.rm = TRUE)
amflux.wkg$SWC_3_mean = rowMeans(amflux.wkg[,c('SWC_3','SWC_3_PI')], na.rm = TRUE)
amflux.wkg$TA_mean = rowMeans(amflux.wkg[,c('TA_1','TA_2')], na.rm = TRUE)

amflux.wkg = amflux.wkg[amflux.wkg$Dates %in% date.seq,]

amflux.le = amflux.wkg$LE * 0.036
amflux.ta = amflux.wkg$TA_mean 
amflux.lh.vap = 2.501 - (2.361 * (10^-3)) * amflux.ta 
amflux.et = amflux.le / amflux.lh.vap

amflux.wkg$ET = amflux.et

# ---------------------------------------------------------------------------------
# Create function to filter values on days when rain occurred
# ---------------------------------------------------------------------------------

precip.filter.fun = function(df.in, var.name) {
  
  df.out = df.in
  flux.df = fluxnet.wkg[fluxnet.wkg$Dates %in% date.seq,]
  precip = coredata(apply.daily(xts(fluxnet.wkg$Precip, fluxnet.wkg$DateTime),
                                FUN = function(x) sum(x, na.rm = TRUE)))
  precip.sig = precip 
  precip.sig[precip.sig < 5] = NA
  df.out$Precip = precip.sig
  
  for (i in 1 : nrow(df.out)) {
    
    if (isTRUE(df.out$Precip[i] > 0)) {
      
      if (var.name == 'NMRI') {
        df.out$NMRI_PeriodMean[i] = NA
        df.out$NMRI_PeriodMedian[i] = NA
      }
      
      if (var.name == 'VOD') {
        df.out$VOD[i] = NA
      }
      
      if (var.name == 'SWC') {
        df.out$SWC_1_PeriodMean[i] = NA
        df.out$SWC_1_PeriodMedian[i] = NA
        df.out$SWC_1_PI_PeriodMean[i] = NA
        df.out$SWC_1_PI_PeriodMedian[i] = NA
        df.out$SWC_2_PeriodMean[i] = NA
        df.out$SWC_2_PI_PeriodMean[i] = NA
        df.out$SWC_3_PeriodMean[i] = NA
        df.out$SWC_3_PI_PeriodMean[i] = NA
        df.out$SWC_mean_PeriodMean[i] = NA
        df.out$SWC_mean_PeriodMedian[i] = NA
        df.out$SWC_1_mean_PeriodMean[i] = NA
        df.out$SWC_2_mean_PeriodMean[i] = NA
        df.out$SWC_3_mean_PeriodMean[i] = NA
      }
      
      if (var.name == 'Value') {
        df.out$Value[i] = NA
      }
      
      if (var.name == 'GPP') {
        df.out$GPP_day_PeriodMean[i] = NA
        df.out$GPP_day_PeriodMedian[i] = NA
      }
      
      if (var.name == 'NEE') {
        df.out$NEE_PeriodMean[i] = NA
        df.out$NEE_PeriodMedian[i] = NA
      }
      
      if (var.name == 'ET_fluxnet') {
        df.out$ET[i] = NA
      }
      
      if (var.name == 'ET_ameriflux') {
        df.out$ET_PeriodMean[i] = NA
        df.out$ET_PeriodMedian[i] = NA
      }
      
    }
  }
  #df.out = subset(df.out, select = -Precip)
  return(df.out)
}

# ---------------------------------------------------------------------------------
# Filter NMRI outliers (NMRI = 1 or 0)
# ---------------------------------------------------------------------------------

nmri.outlier.filter = function(df.in) {
  
  df.out = df.in
  
  # Filter 0 values
  outlier.0 = which(df.in$Value %in% 0)
  
  for (i in 1 : length(outlier.0)) {
    df.out[outlier.0[i],][['Value']] = NA
  }
  
  # Filter 1 values
  outlier.1 = which(df.in$Value %in% 1)
  
  for (j in 1 : length(outlier.1)) {
    df.out[outlier.1[j],][['Value']] = NA
  }
  # Return ouutlier-filtered data frame
  return(df.out)
}

# ------------------------- Get and clean NMRI

wkg.nmri = datefill.fun(data.frame('Dates' = as.Date(paste('2021', wkg.gnss$month, wkg.gnss$day, sep = '-'), format = '%Y-%m-%d'),
                                   'Year' = '2021',
                                   'Month' = wkg.gnss$month,
                                   'Day' = wkg.gnss$day,
                                   'LocalHour' = wkg.gnss$localHour,
                                   'Value' = wkg.gnss$NMRI,
                                   'Dataset' = 'NMRI',
                                   'Site' = 'WKG'))

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# For diurnal period aggregation and analysis, raw hourly NMRI values are used instead of the smoothed values
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

wkg.nmri = wkg.nmri[wkg.nmri$Dates %in% date.seq,]
wkg.nmri$DateTime = as.POSIXct(paste('2021', wkg.nmri$Month, wkg.nmri$Day, wkg.nmri$LocalHour, sep = '-'), format = '%Y-%m-%d-%H')
wkg.nmri = nmri.outlier.filter(wkg.nmri)

wkg.nmri.smoothed = wkg.nmri
wkg.nmri.smoothed  = wkg.nmri.smoothed[wkg.nmri.smoothed$Dates %in% date.seq,]
wkg.nmri.smoothed$DateTime = as.POSIXct(paste('2021', wkg.nmri.smoothed$Month, wkg.nmri.smoothed$Day, wkg.nmri.smoothed$LocalHour, sep = '-'), format = '%Y-%m-%d-%H')
wkg.nmri.smoothed$Value_smoothed = stats::filter(wkg.nmri.smoothed$Value, filter = rep(1/24,24), method = 'convolution', sides = 1)

wkg.nmri.daily.mean = wkg.nmri %>%
  group_by(Dates) %>%
  summarize(Year = first(Year),
            Month = first(Month),
            Day = first(Day),
            Value = mean(Value, na.rm = TRUE)) %>%
  as.data.frame() %>%
  select(c('Dates','Year','Month','Day','Value'))

# -------------------------------------------------------------------------------------------------------------
# Period (predawn, afternoon, evening) and diurnal (daytime/nighttime) aggregations for NMRI, GPP, ET, and SWC
# -------------------------------------------------------------------------------------------------------------

# NMRI
wkg.nmri.predawn = wkg.nmri[wkg.nmri$LocalHour %in% seq(0,6,1),] # 12:00 AM to 6:00 AM
wkg.nmri.predawn$Period = 'Predawn'
wkg.nmri.afternoon = wkg.nmri[wkg.nmri$LocalHour %in% seq(10,14,1),] # 10:00 AM to 2:00 PM
wkg.nmri.afternoon$Period = 'Afternoon'
wkg.nmri.evening = wkg.nmri[wkg.nmri$LocalHour %in% seq(19,24,1),] # 7:00 PM to 11:00 PM
wkg.nmri.evening$Period = 'Evening'

# SWC
wkg.swc.predawn = amflux.wkg[amflux.wkg$LocalHour %in% seq(0,6,1),] # 12:00 AM to 6:00 AM
wkg.swc.predawn$Period = 'Predawn'
wkg.swc.afternoon = amflux.wkg[amflux.wkg$LocalHour %in% seq(10,14,1),] # 10:00 AM to 2:00 PM
wkg.swc.afternoon$Period = 'Afternoon'
wkg.swc.evening = amflux.wkg[amflux.wkg$LocalHour %in% seq(19,24,1),] # 7:00 PM to 11:00 PM
wkg.swc.evening$Period = 'Evening'

# ET (FLUXNET HOURLY)
wkg.et.predawn = fluxnet.wkg[fluxnet.wkg$LocalHour %in% seq(0,6,1),]
wkg.et.predawn$Period = 'Predawn'
wkg.et.afternoon = fluxnet.wkg[fluxnet.wkg$LocalHour %in% seq(10,14,1),]
wkg.et.afternoon$Period = 'Afternoon'
wkg.et.evening = fluxnet.wkg[fluxnet.wkg$LocalHour %in% seq(19,24,1),]
wkg.et.evening$Period = 'Evening'

# GPP (FLUXNET HOURLY)
wkg.gpp.predawn = fluxnet.wkg[fluxnet.wkg$LocalHour %in% seq(0,6,1),]
wkg.gpp.predawn$Period = 'Predawn'
wkg.gpp.afternoon = fluxnet.wkg[fluxnet.wkg$LocalHour %in% seq(10,14,1),]
wkg.gpp.afternoon$Period = 'Afternoon'
wkg.gpp.evening = fluxnet.wkg[fluxnet.wkg$LocalHour %in% seq(19,24,1),]
wkg.gpp.evening$Period = 'Evening'

# Combine NMRI period subsets and aggregate by day (mean and median)
wkg.nmri.period = rbind(wkg.nmri.predawn, wkg.nmri.afternoon, wkg.nmri.evening) %>%
  group_by(Dates, Year, Month, Day, Period) %>%
  summarize(NMRI_PeriodMean = mean(Value, na.rm = TRUE),
            NMRI_PeriodMedian = median(Value, na.rm = TRUE),
            DateTime = last(DateTime)) %>%
  as.data.frame()

# Combine Amerflux SWC period subsets and aggregate by day (mean and median)
wkg.swc.period = rbind(wkg.swc.predawn, wkg.swc.afternoon, wkg.swc.evening) %>%
  group_by(Dates, Year, Month, Day, Period) %>%
  summarize(SWC_1_PeriodMean = mean(SWC_1, na.rm = TRUE),
            SWC_1_PeriodMedian = median(SWC_1, na.rm = TRUE),
            SWC_1_PI_PeriodMean = mean(SWC_1_PI, na.rm = TRUE),
            SWC_1_PI_PeriodMedian = median(SWC_1_PI, na.rm = TRUE),
            SWC_2_PeriodMean = mean(SWC_2, na.rm = TRUE),
            SWC_2_PeriodMedian = median(SWC_2, na.rm = TRUE),
            SWC_2_PI_PeriodMean = mean(SWC_2_PI, na.rm = TRUE),
            SWC_2_PI_PeriodMedian = median(SWC_2_PI, na.rm = TRUE),
            SWC_3_PeriodMean = mean(SWC_3, na.rm = TRUE),
            SWC_3_PeriodMedian = median(SWC_3, na.rm = TRUE),
            SWC_3_PI_PeriodMean = mean(SWC_3_PI, na.rm = TRUE),
            SWC_3_PI_PeriodMedian = median(SWC_3_PI, na.rm = TRUE),
            SWC_mean_PeriodMean = mean(SWC_mean, na.rm = TRUE),
            SWC_mean_PeriodMedian = median(SWC_mean, na.rm = TRUE),
            SWC_1_mean_PeriodMean = mean(SWC_1_mean, na.rm = TRUE),
            SWC_2_mean_PeriodMean = mean(SWC_2_mean, na.rm = TRUE),
            SWC_3_mean_PeriodMean = mean(SWC_3_mean, na.rm = TRUE),
            DateTime = last(DateTime)) %>%
  as.data.frame()

# Combine Fluxnet ET period subsets and aggregate by day (mean and median)
wkg.et.period = rbind(wkg.et.predawn, wkg.et.afternoon, wkg.et.evening) %>%
  group_by(Dates, Period) %>%
  summarize(ET_PeriodMean = mean(ET, na.rm = TRUE),
            ET_PeriodMedian = median(ET, na.rm = TRUE),
            DateTime = last(DateTime)) %>%
  as.data.frame()

# Combine Fluxnet GPP period subsets and aggregate by day (mean and median)
wkg.gpp.period = rbind(wkg.gpp.predawn, wkg.gpp.afternoon, wkg.gpp.evening) %>%
  group_by(Dates, Period) %>%
  summarize(GPP_day_PeriodMean = mean(GPP_day, na.rm = TRUE),
            GPP_day_PeriodMedian = median(GPP_day, na.rm = TRUE),
            DateTime = last(DateTime)) %>%
  as.data.frame()


# --------------------- Convert period aggregations into day/night diurnal periods
day.night.fun = function(data.in, prod.name) {
  
  data = data.in
  
  # Daytime
  day = data[data$Period == 'Afternoon',]
  day$Period = 'Daytime'
  day$DateTime = as.POSIXct(paste(day$Dates, '13:00', sep = '-'), format = '%Y-%m-%d-%H')
  
  # Nighttime
  night = data[data$Period %in% c('Predawn','Evening'),]
  night.agg = night[2,]
  night.agg$Period = 'Nighttime'
  night.agg$DateTime = as.POSIXct(paste(night.agg$Dates, '01:00', sep = '-'), format = '%Y-%m-%d-%H')
  
  if (prod.name == 'NMRI') {
    
    for (i in 2 : 16) {
      
      sub.i = night[night$Dates %in% date.seq[c(i,i-1)],]
      evening.i = sub.i[sub.i$Dates == date.seq[i-1] & sub.i$Period == 'Evening',]
      predawn.i = sub.i[sub.i$Dates == date.seq[i] & sub.i$Period == 'Predawn',]
      predawn.i$NMRI_PeriodMean = mean(c(evening.i$NMRI_PeriodMean, predawn.i$NMRI_PeriodMean), na.rm = TRUE)
      predawn.i$NMRI_PeriodMedian = mean(c(evening.i$NMRI_PeriodMedian, predawn.i$NMRI_PeriodMedian), na.rm = TRUE)
      predawn.i$Period = 'Nighttime'
      predawn.i$DateTime = as.POSIXct(paste(predawn.i$Dates, '01:00', sep = '-'), format = '%Y-%m-%d-%H')
      night.i = predawn.i
      night.agg = rbind(night.agg, night.i)
    }
    
    for (i in 30) {
      sub.i = night[night$Dates %in% date.seq[c(i,i-1)],]
      evening.i = sub.i[sub.i$Dates == date.seq[i-1] & sub.i$Period == 'Evening',]
      predawn.i = sub.i[sub.i$Dates == date.seq[i] & sub.i$Period == 'Predawn',]
      predawn.i$NMRI_PeriodMean = mean(c(evening.i$NMRI_PeriodMean, predawn.i$NMRI_PeriodMean), na.rm = TRUE)
      predawn.i$NMRI_PeriodMedian = mean(c(evening.i$NMRI_PeriodMedian, predawn.i$NMRI_PeriodMedian), na.rm = TRUE)
      predawn.i$Period = 'Nighttime'
      predawn.i$DateTime = as.POSIXct(paste(predawn.i$Dates, '01:00', sep = '-'), format = '%Y-%m-%d-%H')
      night.i = predawn.i
      night.agg = rbind(night.agg, night.i)
    }
    
    for (i in 33:60) {
      sub.i = night[night$Dates %in% date.seq[c(i,i-1)],]
      evening.i = sub.i[sub.i$Dates == date.seq[i-1] & sub.i$Period == 'Evening',]
      predawn.i = sub.i[sub.i$Dates == date.seq[i] & sub.i$Period == 'Predawn',]
      predawn.i$NMRI_PeriodMean = mean(c(evening.i$NMRI_PeriodMean, predawn.i$NMRI_PeriodMean), na.rm = TRUE)
      predawn.i$NMRI_PeriodMedian = mean(c(evening.i$NMRI_PeriodMedian, predawn.i$NMRI_PeriodMedian), na.rm = TRUE)
      predawn.i$Period = 'Nighttime'
      predawn.i$DateTime = as.POSIXct(paste(predawn.i$Dates, '01:00', sep = '-'), format = '%Y-%m-%d-%H')
      night.i = predawn.i
      night.agg = rbind(night.agg, night.i)
    }
    
    for (i in 73:74) {
      sub.i = night[night$Dates %in% date.seq[c(i,i-1)],]
      evening.i = sub.i[sub.i$Dates == date.seq[i-1] & sub.i$Period == 'Evening',]
      predawn.i = sub.i[sub.i$Dates == date.seq[i] & sub.i$Period == 'Predawn',]
      predawn.i$NMRI_PeriodMean = mean(c(evening.i$NMRI_PeriodMean, predawn.i$NMRI_PeriodMean), na.rm = TRUE)
      predawn.i$NMRI_PeriodMedian = mean(c(evening.i$NMRI_PeriodMedian, predawn.i$NMRI_PeriodMedian), na.rm = TRUE)
      predawn.i$Period = 'Nighttime'
      predawn.i$DateTime = as.POSIXct(paste(predawn.i$Dates, '01:00', sep = '-'), format = '%Y-%m-%d-%H')
      night.i = predawn.i
      night.agg = rbind(night.agg, night.i)
    }
    
    for (i in 76:93) {
      sub.i = night[night$Dates %in% date.seq[c(i,i-1)],]
      evening.i = sub.i[sub.i$Dates == date.seq[i-1] & sub.i$Period == 'Evening',]
      predawn.i = sub.i[sub.i$Dates == date.seq[i] & sub.i$Period == 'Predawn',]
      predawn.i$NMRI_PeriodMean = mean(c(evening.i$NMRI_PeriodMean, predawn.i$NMRI_PeriodMean), na.rm = TRUE)
      predawn.i$NMRI_PeriodMedian = mean(c(evening.i$NMRI_PeriodMedian, predawn.i$NMRI_PeriodMedian), na.rm = TRUE)
      predawn.i$Period = 'Nighttime'
      predawn.i$DateTime = as.POSIXct(paste(predawn.i$Dates, '01:00', sep = '-'), format = '%Y-%m-%d-%H')
      night.i = predawn.i
      night.agg = rbind(night.agg, night.i)
    }
    
    for (i in 95:119) {
      sub.i = night[night$Dates %in% date.seq[c(i,i-1)],]
      evening.i = sub.i[sub.i$Dates == date.seq[i-1] & sub.i$Period == 'Evening',]
      predawn.i = sub.i[sub.i$Dates == date.seq[i] & sub.i$Period == 'Predawn',]
      predawn.i$NMRI_PeriodMean = mean(c(evening.i$NMRI_PeriodMean, predawn.i$NMRI_PeriodMean), na.rm = TRUE)
      predawn.i$NMRI_PeriodMedian = mean(c(evening.i$NMRI_PeriodMedian, predawn.i$NMRI_PeriodMedian), na.rm = TRUE)
      predawn.i$Period = 'Nighttime'
      predawn.i$DateTime = as.POSIXct(paste(predawn.i$Dates, '01:00', sep = '-'), format = '%Y-%m-%d-%H')
      night.i = predawn.i
      night.agg = rbind(night.agg, night.i)
    }
  }
  
  if (prod.name == 'SWC') {
    
    for (i in 2 : 119) {
      
      sub.i = night[night$Dates %in% date.seq[c(i,i-1)],]
      evening.i = sub.i[sub.i$Dates == date.seq[i-1] & sub.i$Period == 'Evening',]
      predawn.i = sub.i[sub.i$Dates == date.seq[i] & sub.i$Period == 'Predawn',]
      
      predawn.i$SWC_1_PeriodMean = mean(c(evening.i$SWC_1_PeriodMean, predawn.i$SWC_1_PeriodMean), na.rm = TRUE)
      predawn.i$SWC_1_PI_PeriodMean = mean(c(evening.i$SWC_1_PI_PeriodMean, predawn.i$SWC_1_PI_PeriodMean), na.rm = TRUE)
      
      predawn.i$SWC_2_PeriodMean = mean(c(evening.i$SWC_2_PeriodMean, predawn.i$SWC_2_PeriodMean), na.rm = TRUE)
      predawn.i$SWC_2_PI_PeriodMean = mean(c(evening.i$SWC_2_PI_PeriodMean, predawn.i$SWC_2_PI_PeriodMean), na.rm = TRUE)
      
      predawn.i$SWC_3_PeriodMean = mean(c(evening.i$SWC_3_PeriodMean, predawn.i$SWC_3_PeriodMean), na.rm = TRUE)
      predawn.i$SWC_3_PI_PeriodMean = mean(c(evening.i$SWC_3_PI_PeriodMean, predawn.i$SWC_3_PI_PeriodMean), na.rm = TRUE)
      
      predawn.i$SWC_mean_PeriodMean = mean(c(evening.i$SWC_mean_PeriodMean, predawn.i$SWC_mean_PeriodMean), na.rm = TRUE)
      predawn.i$SWC_1_mean_PeriodMean = mean(c(evening.i$SWC_1_mean_PeriodMean, predawn.i$SWC_1_mean_PeriodMean), na.rm = TRUE)
      predawn.i$SWC_2_mean_PeriodMean = mean(c(evening.i$SWC_2_mean_PeriodMean, predawn.i$SWC_2_mean_PeriodMean), na.rm = TRUE)
      predawn.i$SWC_3_mean_PeriodMean = mean(c(evening.i$SWC_3_mean_PeriodMean, predawn.i$SWC_3_mean_PeriodMean), na.rm = TRUE)
      
      predawn.i$Period = 'Nighttime'
      predawn.i$DateTime = as.POSIXct(paste(predawn.i$Dates, '01:00', sep = '-'), format = '%Y-%m-%d-%H')
      
      night.i = predawn.i
      night.agg = rbind(night.agg, night.i)
    }
  }
  
  if (prod.name == 'ET') {
    
    for (i in 2 : 119) {
      
      sub.i = night[night$Dates %in% date.seq[c(i,i-1)],]
      evening.i = sub.i[sub.i$Dates == date.seq[i-1] & sub.i$Period == 'Evening',]
      predawn.i = sub.i[sub.i$Dates == date.seq[i] & sub.i$Period == 'Predawn',]
      predawn.i$ET_PeriodMean = mean(c(evening.i$ET_PeriodMean, predawn.i$ET_PeriodMean), na.rm = TRUE)
      predawn.i$ET_PeriodMedian = mean(c(evening.i$ET_PeriodMedian, predawn.i$ET_PeriodMedian), na.rm = TRUE)
      predawn.i$Period = 'Nighttime'
      predawn.i$DateTime = as.POSIXct(paste(predawn.i$Dates, '01:00', sep = '-'), format = '%Y-%m-%d-%H')
      night.i = predawn.i
      night.agg = rbind(night.agg, night.i)
    }
  }
  
  if (prod.name == 'GPP') {
    
    for (i in 2 : 119) {
      
      sub.i = night[night$Dates %in% date.seq[c(i,i-1)],]
      evening.i = sub.i[sub.i$Dates == date.seq[i-1] & sub.i$Period == 'Evening',]
      predawn.i = sub.i[sub.i$Dates == date.seq[i] & sub.i$Period == 'Predawn',]
      predawn.i$GPP_day_PeriodMean = mean(c(evening.i$GPP_day_PeriodMean, predawn.i$GPP_day_PeriodMean), na.rm = TRUE)
      predawn.i$GPP_day_PeriodMedian = mean(c(evening.i$GPP_day_PeriodMedian, predawn.i$GPP_day_PeriodMedian), na.rm = TRUE)
      predawn.i$Period = 'Nighttime'
      predawn.i$DateTime = as.POSIXct(paste(predawn.i$Dates, '01:00', sep = '-'), format = '%Y-%m-%d-%H')
      night.i = predawn.i
      night.agg = rbind(night.agg, night.i)
    }
  }
  
  day = padr::pad(day, start_val = as.Date('2021-06-30'), end_val = as.Date('2021-10-26'), interval = 'day', by = 'Dates')
  day$Period = 'Daytime'
  day$Year = '2021'
  day$Month = as.numeric(format(date.seq, '%m'))
  day$Day = as.numeric(format(date.seq, '%d'))
  day$DateTime = as.POSIXct(paste(date.seq, '13:00', sep = '-'), format = '%Y-%m-%d-%H')
  
  night.agg = padr::pad(night.agg, start_val = as.Date('2021-06-30'), end_val = as.Date('2021-10-26'), interval = 'day', by = 'Dates')
  night.agg$Period = 'Nighttime'
  night.agg$Year = '2021'
  night.agg$Month = as.numeric(format(date.seq, '%m'))
  night.agg$Day = as.numeric(format(date.seq, '%d'))
  night.agg$DateTime = as.POSIXct(paste(date.seq, '01:00', sep = '-'), format = '%Y-%m-%d-%H')
  
  day.night = rbind(day,night.agg)
  
  return(rbind(day.night))
}

# Summarize NMRI, SWC, ET, and GPP by day/night
wkg.nmri.day.night = day.night.fun(wkg.nmri.period, 'NMRI')
wkg.swc.day.night = day.night.fun(wkg.swc.period, 'SWC')
wkg.et.day.night = day.night.fun(wkg.et.period, 'ET')
wkg.gpp.day.night = day.night.fun(wkg.gpp.period, 'GPP')

# Factor period values
wkg.nmri.day.night$Period = factor(wkg.nmri.day.night$Period, levels = c('Nighttime','Daytime'))
wkg.swc.day.night$Period = factor(wkg.swc.day.night$Period, levels = c('Nighttime','Daytime'))
wkg.et.day.night$Period = factor(wkg.et.day.night$Period, levels = c('Nighttime','Daytime'))
wkg.gpp.day.night$Period = factor(wkg.gpp.day.night$Period, levels = c('Nighttime','Daytime'))

# -----------------------------------------------------------------
# Subset VOD datasets
# -----------------------------------------------------------------

# LPDR VOD
wkg.lpdr.asc = datefill.fun(lpdr.wkg[lpdr.wkg$Dataset == 'LPDRv3_VOD_ASCENDING_DAY',])
wkg.lpdr.asc$Dataset = 'LPDRv3_X_VOD_ASCENDING_DAY'
wkg.lpdr.asc = precip.filter.fun(wkg.lpdr.asc[wkg.lpdr.asc$Dates %in% date.seq,],'Value')
wkg.lpdr.desc = datefill.fun(lpdr.wkg[lpdr.wkg$Dataset == 'LPDRv3_VOD_DESCENDING_NIGHT',])
wkg.lpdr.desc$Dataset = 'LPDRv3_X_VOD_DESCENDING_NIGHT'
wkg.lpdr.desc = precip.filter.fun(wkg.lpdr.desc[wkg.lpdr.desc$Dates %in% date.seq,],'Value')

# LPRM VOD
wkg.lprm.x.asc = datefill.fun(lprm.wkg[lprm.wkg$Dataset == 'LPRM_X_VOD_ASCENDING_DAY',])
wkg.lprm.x.asc = precip.filter.fun(wkg.lprm.x.asc[wkg.lprm.x.asc$Dates %in% date.seq,],'Value')
wkg.lprm.x.desc = datefill.fun(lprm.wkg[lprm.wkg$Dataset == 'LPRM_X_VOD_DESCENDING_NIGHT',])
wkg.lprm.x.desc = precip.filter.fun(wkg.lprm.x.desc[wkg.lprm.x.desc$Dates %in% date.seq,],'Value')
wkg.lprm.c1.asc = datefill.fun(lprm.wkg[lprm.wkg$Dataset == 'LPRM_C1_VOD_ASCENDING_DAY',])
wkg.lprm.c1.asc = precip.filter.fun(wkg.lprm.c1.asc[wkg.lprm.c1.asc$Dates %in% date.seq,],'Value')
wkg.lprm.c1.desc = datefill.fun(lprm.wkg[lprm.wkg$Dataset == 'LPRM_C1_VOD_DESCENDING_NIGHT',])
wkg.lprm.c1.desc = precip.filter.fun(wkg.lprm.c1.desc[wkg.lprm.c1.desc$Dates %in% date.seq,],'Value')

# SMAP VOD
wkg.smap.vod.am = precip.filter.fun(smap.wkg[smap.wkg$Var == 'VOD' & smap.wkg$Period == 'Daytime',],'Value')
wkg.smap.vod.pm = precip.filter.fun(smap.wkg[smap.wkg$Var == 'VOD' & smap.wkg$Period == 'Nighttime',],'Value')
wkg.smap.dca.am = precip.filter.fun(smap.wkg[smap.wkg$Var == 'DCA' & smap.wkg$Period == 'Daytime',],'Value')
wkg.smap.dca.pm = precip.filter.fun(smap.wkg[smap.wkg$Var == 'DCA' & smap.wkg$Period == 'Nighttime',],'Value')
wkg.smap.sch.am = precip.filter.fun(smap.wkg[smap.wkg$Var == 'SCH' & smap.wkg$Period == 'Daytime',],'Value')
wkg.smap.sch.pm = precip.filter.fun(smap.wkg[smap.wkg$Var == 'SCH' & smap.wkg$Period == 'Nighttime',],'Value')
wkg.smap.scv.am = precip.filter.fun(smap.wkg[smap.wkg$Var == 'SCV' & smap.wkg$Period == 'Daytime',],'Value')
wkg.smap.scv.pm = precip.filter.fun(smap.wkg[smap.wkg$Var == 'SCV' & smap.wkg$Period == 'Nighttime',],'Value')
wkg.smap.vwc.am = precip.filter.fun(smap.wkg[smap.wkg$Var == 'VWC' & smap.wkg$Period == 'Daytime',],'Value')
wkg.smap.vwc.pm = precip.filter.fun(smap.wkg[smap.wkg$Var == 'VWC' & smap.wkg$Period == 'Nighttime',],'Value')

wkg.smap.lpdr.vod.am = precip.filter.fun(smap.lpdr.wkg.mean[smap.lpdr.wkg.mean$Var == 'VOD' & smap.lpdr.wkg.mean$Period == 'Daytime',],'Value')
wkg.smap.lpdr.vod.pm = precip.filter.fun(smap.lpdr.wkg.mean[smap.lpdr.wkg.mean$Var == 'VOD' & smap.lpdr.wkg.mean$Period == 'Nighttime',],'Value')
wkg.smap.lpdr.vwc.am = precip.filter.fun(smap.lpdr.wkg.mean[smap.lpdr.wkg.mean$Var == 'VWC' & smap.lpdr.wkg.mean$Period == 'Daytime',],'Value')
wkg.smap.lpdr.vwc.pm = precip.filter.fun(smap.lpdr.wkg.mean[smap.lpdr.wkg.mean$Var == 'VWC' & smap.lpdr.wkg.mean$Period == 'Nighttime',],'Value')

# -------------------------------------------------------------------------
# Create function to compute Loess models and pad dates in output
# -------------------------------------------------------------------------

loess.datefill = function(data.in, span){
  data.in$Index = as.numeric(date.seq)
  loess.in = loess(Value ~ Index, data = data.in, span = span, degree = 2)
  loess.df = data.frame('Dates' = data.in[data.in$Index %in% loess.in$x,1],
                        'y' = unname(loess.in$fitted))
  loess.out = padr::pad(loess.df, 
                        start_val = as.Date('2021-06-30'), 
                        end_val = as.Date('2021-10-26'), 
                        interval = 'day', 
                        by = 'Dates')
  return(loess.out)
}

loess.span = 2/5

# LPRM
wkg.lprm.x.asc.loess = loess.datefill(wkg.lprm.x.asc, loess.span)
wkg.lprm.x.desc.loess = loess.datefill(wkg.lprm.x.desc, loess.span)
wkg.lprm.c1.asc.loess = loess.datefill(wkg.lprm.c1.asc, loess.span)
wkg.lprm.c1.desc.loess = loess.datefill(wkg.lprm.c1.desc, loess.span)

# SMAP
wkg.smap.vod.am.loess = loess.datefill(wkg.smap.vod.am, loess.span)
wkg.smap.vod.pm.loess = loess.datefill(wkg.smap.vod.pm, loess.span)
wkg.smap.dca.am.loess = loess.datefill(wkg.smap.dca.am, loess.span)
wkg.smap.dca.pm.loess = loess.datefill(wkg.smap.dca.pm, loess.span)
wkg.smap.sch.am.loess = loess.datefill(wkg.smap.sch.am, loess.span)
wkg.smap.sch.pm.loess = loess.datefill(wkg.smap.sch.pm, loess.span)
wkg.smap.scv.am.loess = loess.datefill(wkg.smap.scv.am, loess.span)
wkg.smap.scv.pm.loess = loess.datefill(wkg.smap.scv.pm, loess.span)
wkg.smap.vwc.am.loess = loess.datefill(wkg.smap.vwc.am, loess.span)
wkg.smap.vwc.pm.loess = loess.datefill(wkg.smap.vwc.pm, loess.span)

# -------------------------------------------------------------------------
# Set greening and browning dates
# -------------------------------------------------------------------------

greening.dates = seq(as.Date('2021-06-30'),as.Date('2021-08-23'),'day')
browning.dates = seq(as.Date('2021-08-24'),as.Date('2021-10-26'),'day')

# --------------------------------------------------------------------------------
# Set dates for pulse events
# --------------------------------------------------------------------------------

p1.dates = seq(as.Date('2021-07-10'),as.Date('2021-07-15'),'day')
p2.dates = seq(as.Date('2021-08-14'),as.Date('2021-08-22'),'day')
p3.dates = seq(as.Date('2021-09-29'),as.Date('2021-10-7'),'day')

# --------------------------------------------------------------------------------------------
# Fig. 5
# Function to plot timeseries of daytime/nighttime NMRI for 3 rainfall pulses
# --------------------------------------------------------------------------------------------

day.night.pulse.nmri = function() {

   # ---- Plot paramters
   axisText = element_text(size = 13)
   axisTitleY = element_text(size = 13)
   plotTitle = element_text(size = 16, face = 'bold')

   # ---- NMRI
   # Day/night values for pulse events
   nmri = wkg.nmri.day.night
   nmri$MonsoonPhase = ''
   nmri[nmri$Dates %in% greening.dates,9] = 'Greening'
   nmri[nmri$Dates %in% browning.dates,9] = 'Browning'
   nmri$MonsoonPhase = factor(nmri$MonsoonPhase, levels = c('Greening','Browning'))

   # Subset pulse events
   nmri.p1 = nmri[nmri$Dates %in% c(p1.dates,seq(as.Date('2021-07-16'),as.Date('2021-07-18'),by='day')),]
   nmri.p1$Pulse = 'P1'
   nmri.p2 = nmri[nmri$Dates %in% p2.dates,]
   nmri.p2$Pulse = 'P2'
   nmri.p3 = nmri[nmri$Dates %in% p3.dates,]
   nmri.p3$Pulse = 'P3'
   nmri.pulses = rbind(nmri.p1, nmri.p2, nmri.p3)

   nmri.pulses$Period = factor(nmri.pulses$Period, levels = c('Nighttime','Daytime'))
   nmri.pulses$Pulse = factor(nmri.pulses$Pulse, levels = c('P1','P2','P3'))

   # Pulse datetimes
   p1.datetime = nmri.p1$DateTime[1]
   p2.datetime = nmri.p2$DateTime[1]
   p3.datetime = nmri.p3$DateTime[1]

   # Pulse labels
   pulse.labs = data.frame(x = c(p1.datetime,p2.datetime,p3.datetime),
                           y = c(0.73,0.46,0.73),
                           label = c('Pulse 1\n07/10',
                                     'Pulse 2\n08/14',
                                     'Pulse 3\n09/29'))

   # Pulse slopes (Mann-Kendall Sen's Slope)
   mannkendall.slope.fun = function(data.in) {
     mk = wql::mannKen(data.in)
     # Slope
     mk.slope = round(mk$sen.slope,4)
     mk.slope = sprintf('%7.4f', mk.slope)
     # Significance
     mk.p = mk$p.value
     if (mk.p > 0.001){ mk.p = round(mk.p,4)}
     if (mk.p < 0.001){ mk.p = '<0.001'}
     #mk.p = sprintf('%7.4f', mk.p)
     return(c(mk.slope,mk.p))
   }

   # Daytime
   nmri.p1.dt.slope = mannkendall.slope.fun(nmri.p1[nmri.p1$Period == 'Daytime',6])
   nmri.p2.dt.slope = mannkendall.slope.fun(nmri.p2[nmri.p2$Period == 'Daytime',6])
   nmri.p3.dt.slope = mannkendall.slope.fun(nmri.p3[nmri.p3$Period == 'Daytime',6])

   # Nighttime
   nmri.p1.nt.slope = mannkendall.slope.fun(nmri.p1[nmri.p1$Period == 'Nighttime',6])
   nmri.p2.nt.slope = mannkendall.slope.fun(nmri.p2[nmri.p2$Period == 'Nighttime',6])
   nmri.p3.nt.slope = mannkendall.slope.fun(nmri.p3[nmri.p3$Period == 'Nighttime',6])

   # Labels for time series plots
   nmri.pulse.slope.labs = rbind(data.frame('x' = c(rep(nmri.pulses$DateTime[9],2),rep(nmri.pulses$DateTime[27],2),rep(nmri.pulses$DateTime[45],2)),
                                            'y' = c(0.75,0.72,0.55,0.52,0.75,0.72),
                                            'Pulse' = c(rep('P1',2),rep('P2',2),rep('P3',2)),
                                            'Period' = 'Daytime',
                                            'labs' = c(paste('MK Slope =', nmri.p1.dt.slope[1]),
                                                       paste('p =', nmri.p1.dt.slope[2]),
                                                       paste('MK Slope =', nmri.p2.dt.slope[1]),
                                                       paste('p =', nmri.p2.dt.slope[2]),
                                                       paste('MK Slope =', nmri.p3.dt.slope[1]),
                                                       paste('p =', nmri.p3.dt.slope[2]))),
                                 data.frame('x' = c(rep(nmri.pulses$DateTime[9],2),rep(nmri.pulses$DateTime[27],2),rep(nmri.pulses$DateTime[45],2)),
                                            'y' = c(0.68,0.65,0.48,0.45,0.68,0.65),
                                            'Pulse' = c(rep('P1',2),rep('P2',2),rep('P3',2)),
                                            'Period' = 'Nighttime',
                                            'labs' = c(paste('MK Slope =', nmri.p1.nt.slope[1]),
                                                       paste('p =', nmri.p1.nt.slope[2]),
                                                       paste('MK Slope =', nmri.p2.nt.slope[1]),
                                                       paste('p =', nmri.p2.nt.slope[2]),
                                                       paste('MK Slope =', nmri.p3.nt.slope[1]),
                                                       paste('p =', nmri.p3.nt.slope[2]))))

   nmri.pulse.slope.labs$Period = factor(nmri.pulse.slope.labs$Period, levels = c('Nighttime','Daytime'))
   nmri.pulse.slope.labs$Pulse = factor(nmri.pulse.slope.labs$Pulse, levels = c('P1','P2','P3'))

   # Full season timeseries
   nmri.full.ts = ggplot(data = nmri,
                         aes(x = DateTime,
                             y = NMRI_PeriodMean,
                             col = Period)) +
     geom_point(size = 2, pch = 19, alpha = 0.75) +
     geom_line(alpha = 0.5, linewidth = 1) +
     scale_color_manual(values = c('blue','red')) +
     xlab('') +
     ylab('NMRI [ ]') +
     ggtitle('Day & night mean NMRI (full season)') +
     annotate(x = head(nmri.p1$DateTime,1), xend = head(nmri.p1$DateTime,1),
              y = -Inf, yend = Inf,
              geom = 'segment', linewidth = 2,
              color = 'blue', alpha = 0.25) +
     annotate(x = head(nmri.p2$DateTime,1), xend = head(nmri.p2$DateTime,1),
              y = -Inf, yend = Inf,
              geom = 'segment', linewidth = 2,
              color = 'blue', alpha = 0.25) +
     annotate(x = head(nmri.p3$DateTime,1), xend = head(nmri.p3$DateTime,1),
              y = -Inf, yend = Inf,
              geom = 'segment', linewidth = 2,
              color = 'blue', alpha = 0.25) +
     geom_text(data = pulse.labs,
               aes(x = x, y = y, label = label),
               inherit.aes = FALSE,
               size = 4.5, fontface = 'bold') +
     theme_bw() +
     theme(axis.text = axisText,
           axis.title.y = axisTitleY,
           plot.title = plotTitle,
           legend.title = element_blank(),
           legend.text = element_text(size = 15),
           legend.position = 'none')

   nmri.full.ts
   
   
   # Pulse timeseries
   nmri.pulse.ts = ggplot(data = nmri.pulses,
                          aes(x = DateTime,
                              y = NMRI_PeriodMean,
                              col = Period)) +
     geom_line(data = nmri.pulses,
               aes(x = DateTime, y = NMRI_PeriodMean),
               inherit.aes = FALSE,
               linewidth = 0.75, alpha = 0.75) +
     geom_point(size = 4, pch = 19, alpha = 0.75) +
     geom_smooth(formula = y ~ x,
                 na.rm = TRUE,
                 se = FALSE,
                 method = 'lm',
                 linewidth = 1,
                 linetype = 'dashed',
                 alpha = 0.25,
                 show.legend = TRUE) +
     xlab('') +
     ylab('NMRI [ ]') +
     #ggtitle('NMRI for rainfall pulse events') +
     scale_color_manual(values = c('blue','red'),
                        labels = c('Nighttime (06:00 PM - 06:00 AM)',
                                   'Daytime   (10:00 AM - 02:00 PM)'),
                        name = 'Diurnal period aggregates (mean)') +
     geom_vline(data = filter(nmri.pulses, Pulse == 'P1'),
                aes(xintercept = p1.datetime),
                linewidth = 2,
                color = 'blue', alpha = 0.25) +
     geom_vline(data = filter(nmri.pulses, Pulse == 'P2'),
                aes(xintercept = p2.datetime),
                linewidth = 2,
                color = 'blue', alpha = 0.25) +
     geom_vline(data = filter(nmri.pulses, Pulse == 'P3'),
                aes(xintercept = p3.datetime),
                linewidth = 2,
                color = 'blue', alpha = 0.25) +
     # Slope labels
     geom_text(data = nmri.pulse.slope.labs,
               aes(x = x, y = y, label = labs, col = Period),
               size = 6, show.legend = FALSE, hjust = 1) +
     facet_wrap(Pulse~., ncol = 3, scales = 'free_x',
                labeller = as_labeller(c(P1 = 'Pulse 1 (07/10)\nearly greening phase',
                                         P2 = 'Pulse 2 (08/14)\nlate greening phase',
                                         P3 = 'Pulse 3 (09/29)\nbrowning phase'))) +
     theme_bw() +
     theme(axis.text = axisText,
           axis.title.y = axisTitleY,
           legend.position = 'bottom',
           legend.direction = 'vertical',
           legend.title = element_text(size = 17, face = 'bold', hjust = 0),
           legend.text = element_text(size = 17),
           #legend.text.align = 0,
           legend.box.margin = margin(-20,0,0,0),
           #legend.justification = 0,
           #plot.title = plotTitle,
           panel.border = element_rect(color = 'black', fill = NA, linewidth = 1),
           strip.background = element_rect(fill = 'white', color = 'white'),
           strip.text = element_text(size = 20, face = 'bold'))
   
   # nmri.pulse.ts.legend = get_plot_component(nmri.pulse.ts + theme(legend.position = 'bottom',
   #                                                                 legend.justification = 'bottom',
   #                                                                 legend.direction = 'horizontal',
   #                                                                 legend.margin = margin(-5,0,2,0, unit = 'mm')),
   #                                           'guide-box-bottom', return_all = TRUE)

   nmri.pulse.ts = ggdraw(nmri.pulse.ts) +
     draw_label('a)', x = 0.035, y = 0.88, fontface = 'bold', size = 19, hjust = -0.5) +
     draw_label('b)', x = 0.355, y = 0.88, fontface = 'bold', size = 19, hjust = -0.5) +
     draw_label('c)', x = 0.675, y = 0.88, fontface = 'bold', size = 19, hjust = -0.5)

   ggsave(filename = paste(figs.fp, 'Fig05_WKG_Diurnal_NMRI_Pulses_ONLY.png', sep = '/'),
          nmri.pulse.ts,
          height = 5, width = 15,
          bg = 'white')
}

day.night.pulse.nmri()

# ------------------------------------------------------------------------------------------------------------
# Prepare data to plot 1-day-lagged relationships between daytime/nighttime NMRI and GPP
# ------------------------------------------------------------------------------------------------------------

wkg.gpp = rbind(precip.filter.fun(wkg.gpp.day.night[wkg.gpp.day.night$Period == 'Daytime',],'GPP'),
                precip.filter.fun(wkg.gpp.day.night[wkg.gpp.day.night$Period == 'Nighttime',],'GPP'))

wkg.nmri.day.night = rbind(precip.filter.fun(wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Daytime',],'NMRI'),
                           precip.filter.fun(wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Nighttime',],'NMRI'))

wkg.nmri.gpp.day.night = rbind(data.frame('Dates' = wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Daytime',1],
                                          'GPP' = wkg.gpp[wkg.gpp$Period == 'Daytime',3],
                                          'NMRI_PeriodMean' = wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Daytime',][['NMRI_PeriodMean']],
                                          'NMRI_PeriodMedian' = wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Daytime',][['NMRI_PeriodMedian']],
                                          'Period' = 'Daytime'),
                               data.frame('Dates' = wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Nighttime',1],
                                          'GPP' = wkg.gpp[wkg.gpp$Period == 'Nighttime',3],
                                          'NMRI_PeriodMean' = wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Nighttime',][['NMRI_PeriodMean']],
                                          'NMRI_PeriodMedian' = wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Nighttime',][['NMRI_PeriodMedian']],
                                          'Period' = 'Nighttime'))

wkg.nmri.gpp.day.night$Period = factor(wkg.nmri.gpp.day.night$Period,
                                       levels = c('Nighttime','Daytime'))

# For day/night NMRI-GPP scatterplot: mask NMRI values that peak after maximum GPP (AUG 24-28)
wkg.nmri.gpp.day.night[wkg.nmri.gpp.day.night$Period == 'Daytime',3][56:60] = NA
wkg.nmri.gpp.day.night[wkg.nmri.gpp.day.night$Period == 'Daytime',4][56:60] = NA
wkg.nmri.gpp.day.night[wkg.nmri.gpp.day.night$Period == 'Nighttime',3][56:60] = NA
wkg.nmri.gpp.day.night[wkg.nmri.gpp.day.night$Period == 'Nighttime',4][56:60] = NA

# ------------------------------------------------------------------------------------------------------------
# Prepare data to plot 1-day-lagged relationships between daytime/nighttime NMRI and ET
# ------------------------------------------------------------------------------------------------------------

wkg.et.daytime = wkg.et.day.night[wkg.et.day.night$Period == 'Daytime',]
wkg.et.nighttime = wkg.et.day.night[wkg.et.day.night$Period == 'Nighttime',]

wkg.et = rbind(precip.filter.fun(wkg.et.daytime, 'ET_ameriflux'),
               precip.filter.fun(wkg.et.nighttime, 'ET_ameriflux'))

wkg.nmri.et.day.night = rbind(data.frame('Dates' = wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Daytime',1],
                                          'ET' = wkg.et[wkg.et$Period == 'Daytime',3],
                                          'NMRI_PeriodMean' = wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Daytime',][['NMRI_PeriodMean']],
                                          'NMRI_PeriodMedian' = wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Daytime',][['NMRI_PeriodMedian']],
                                          'Period' = 'Daytime'),
                               data.frame('Dates' = wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Nighttime',1],
                                          'ET' = wkg.et[wkg.et$Period == 'Nighttime',3],
                                          'NMRI_PeriodMean' = wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Nighttime',][['NMRI_PeriodMean']],
                                          'NMRI_PeriodMedian' = wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Nighttime',][['NMRI_PeriodMedian']],
                                          'Period' = 'Nighttime'))

wkg.nmri.et.day.night$Period = factor(wkg.nmri.et.day.night$Period,
                                      levels = c('Nighttime','Daytime'))

# For day/night NMRI-ET scatterplot: mask NMRI values that peak after maximum ET (AUG 24-28)
wkg.nmri.et.day.night[wkg.nmri.et.day.night$Period == 'Daytime',3][56:60] = NA
wkg.nmri.et.day.night[wkg.nmri.et.day.night$Period == 'Nighttime',3][56:60] = NA

# --------------------------------------------------------------------------------------------------------------------
# Prepare data to plot relationships between daytime/nighttime NMRI and SWC
# --------------------------------------------------------------------------------------------------------------------

wkg.swc.day.night = rbind(wkg.swc.day.night[wkg.swc.day.night$Period == 'Daytime',],
                          wkg.swc.day.night[wkg.swc.day.night$Period == 'Nighttime',])

wkg.nmri.swc.day.night = rbind(data.frame('Dates' = wkg.swc.day.night[wkg.swc.day.night$Dates %in% wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Daytime',1],1],
                                          'SWC_PeriodMean' = wkg.swc.day.night[wkg.swc.day.night$Dates %in% wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Daytime',1],][['SWC_mean_PeriodMean']],
                                          'SWC_1_mean_PeriodMean' = wkg.swc.day.night[wkg.swc.day.night$Dates %in% wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Daytime',1],][['SWC_1_mean_PeriodMean']],
                                          'SWC_2_mean_PeriodMean' = wkg.swc.day.night[wkg.swc.day.night$Dates %in% wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Daytime',1],][['SWC_2_mean_PeriodMean']],
                                          'SWC_3_mean_PeriodMean' = wkg.swc.day.night[wkg.swc.day.night$Dates %in% wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Daytime',1],][['SWC_3_mean_PeriodMean']],
                                          'NMRI_PeriodMean' = wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Daytime',][['NMRI_PeriodMean']],
                                          'Period' = 'Daytime'),
                               data.frame('Dates' = wkg.swc.day.night[wkg.swc.day.night$Dates %in% wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Nighttime',1],1],
                                          'SWC_PeriodMean' = wkg.swc.day.night[wkg.swc.day.night$Dates %in% wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Nighttime',1],][['SWC_mean_PeriodMean']],
                                          'SWC_1_mean_PeriodMean' = wkg.swc.day.night[wkg.swc.day.night$Dates %in% wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Nighttime',1],][['SWC_1_mean_PeriodMean']],
                                          'SWC_2_mean_PeriodMean' = wkg.swc.day.night[wkg.swc.day.night$Dates %in% wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Nighttime',1],][['SWC_2_mean_PeriodMean']],
                                          'SWC_3_mean_PeriodMean' = wkg.swc.day.night[wkg.swc.day.night$Dates %in% wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Nighttime',1],][['SWC_3_mean_PeriodMean']],
                                          'NMRI_PeriodMean' = wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Nighttime',][['NMRI_PeriodMean']],
                                          'Period' = 'Nighttime'))

wkg.nmri.swc.day.night$Period = factor(wkg.nmri.swc.day.night$Period, levels = c('Nighttime','Daytime'))

# --------------------------------------------------------------------------------------------------------
# Prepare data for day/night NMRI and next-day GPP/ET
# --------------------------------------------------------------------------------------------------------

wkg.nmri.dailymean.smoothed = coredata(apply.daily(xts(wkg.nmri.smoothed$Value, wkg.nmri.smoothed$Dates), FUN = function(x) mean(x, na.rm = TRUE)))
wkg.gpp.day.lag = precip.filter.fun(wkg.gpp.day.night[wkg.gpp.day.night$Period == 'Daytime',],'GPP')[['GPP_day_PeriodMean']][2:119]
wkg.et.day.lag = precip.filter.fun(wkg.et.day.night[wkg.et.day.night$Period == 'Daytime',],'ET')[['ET_PeriodMean']][2:119]

wkg.nmri.daytime = wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Daytime',]
wkg.nmri.nighttime = wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Nighttime',]

# Create data frame with day/night diff. and ratio, nighttime NMRI, and day-lag GPP and ET
wkg.nmri.gpp.et.day.night.lag = data.frame('NMRI_Dates' = date.seq[1:118],
                                           'NMRI_Night' = wkg.nmri.nighttime[,6][1:118],
                                           'Flux_Dates' = date.seq[2:119],
                                           'GPP' = wkg.gpp.day.lag,
                                           'ET' = wkg.et.day.lag)

# Mask peak NMRI for dates following peak greenness (AUG 25-28)
wkg.nmri.gpp.et.day.night.lag[56:60,2:4] = NA

# Assign monsoon phase
wkg.nmri.gpp.et.day.night.lag$MonsoonPhase = ''
wkg.nmri.gpp.et.day.night.lag$MonsoonPhase[1:54] = 'Greening'
wkg.nmri.gpp.et.day.night.lag$MonsoonPhase[55:118] = 'Browning'
wkg.nmri.gpp.et.day.night.lag$MonsoonPhase = factor(wkg.nmri.gpp.et.day.night.lag$MonsoonPhase,
                                                        levels = c('Greening','Browning'))

# --------------------------------------------------------------------------------------------------------------------------------
# Fig. 2 and Fig. 3
# Function for plotting precip (top panel) and NMRI, GPP, ET, and GCC timeseries together (bottom panel) 
# Also plotting NMRI vs. GPP/ET/GCC scatterplots (combined on single panel, saved separately)
# --------------------------------------------------------------------------------------------------------------------------------

wkg.nmri.flux.ts.sp.fun = function() {
  
  mannkendall.slope.fun = function(data.in) {
    mk = wql::mannKen(data.in)
    mk.slope = round(mk$sen.slope,3)
    mk.p = mk$p.value
    return(c(mk.slope,mk.p))
  }
  
  # NMRI
  nmri.nt = wkg.nmri.day.night[wkg.nmri.day.night$Period == 'Nighttime',]
  colnames(nmri.nt)[6] = 'Value'
  # nmri.nt = wkg.nmri.gpp.day.night[wkg.nmri.gpp.day.night$Period == 'Nighttime',][,c(1,3)]
  # colnames(nmri.nt)[2] = 'Value'
  nmri.nt.loess = loess.datefill(nmri.nt, loess.span)
  nmri.nt.loess = na.omit(nmri.nt.loess)
  nmri.col = 'blue'
  nmri.lab = expression(bold('NMRI [ ] (nighttime)'))
  
  # GPP
  gpp = wkg.gpp.day.night[wkg.gpp.day.night$Period == 'Daytime',c(1,3)]
  #gpp = wkg.nmri.gpp.day.night[wkg.nmri.gpp.day.night$Period == 'Daytime',][,1:2]
  gpp.ylab = expression(bold(GPP~gC~m^-2~d^-1~'(daytime)'))
  gpp.col = 'darkolivegreen'
  gpp.ylim = c(0,15)
  gpp.tick.labs = pretty(c(-2, range(gpp[,2], na.rm = TRUE)[2]+2))
  
  # ET
  et = wkg.et.day.night[wkg.et.day.night$Period == 'Daytime',c(1,3)]
  #et = wkg.nmri.et.day.night[wkg.nmri.et.day.night$Period == 'Daytime',][,1:2]
  et.ylab = expression(bold(ET~mm~d^-1~'(daytime)'))
  et.col = 'salmon'
  et.ylim = c(0,6)
  
  # GCC
  gcc = data.frame('Dates' = phenocam.gr.wkg$date, 'GCC' = phenocam.gr.wkg$gcc_90)
  gcc = gcc[gcc$Dates %in% date.seq,]
  gcc.ylab = expression(bold('GCC (90th Percentile) [ ] (daytime)'))
  gcc.col = 'green3'
  gcc.ylim = c(0.34,0.42)
  gcc.tick.labs = pretty(c(0.32, range(gcc[,2], na.rm = TRUE)[2]))
  
  # ---------------------------------------------------------- Create timeseries plot
  nmri.flux.ts.fun = function() {
    
    # Subset greening/browning and compute MK slope for all variables
    nmri.nt.g = nmri.nt[nmri.nt$Dates %in% greening.dates,]$Value
    nmri.nt.b = nmri.nt[nmri.nt$Dates %in% browning.dates,]$Value
    nmri.nt.slope.g = mannkendall.slope.fun(nmri.nt.g)
    nmri.nt.slope.b = mannkendall.slope.fun(nmri.nt.b)
    
    gpp.g = gpp[gpp$Dates %in% greening.dates,]$GPP
    gpp.b = gpp[gpp$Dates %in% browning.dates,]$GPP
    gpp.slope.g = mannkendall.slope.fun(gpp.g)
    gpp.slope.b = mannkendall.slope.fun(gpp.b)
    
    et.g = et[et$Dates %in% greening.dates,]$ET
    et.b = et[et$Dates %in% browning.dates,]$ET
    et.slope.g = mannkendall.slope.fun(et.g)
    et.slope.b = mannkendall.slope.fun(et.b)
    
    gcc.g = gcc[gpp$Dates %in% greening.dates,]$GCC
    gcc.b = gcc[gpp$Dates %in% browning.dates,]$GCC
    gcc.slope.g = mannkendall.slope.fun(gcc.g)
    gcc.slope.b = mannkendall.slope.fun(gcc.b)
    
    # Legend graphical parameters
    slope.legend.xjust.1 = -0.1
    slope.legend.xjust.2 = -0.2
    slope.legend.yjust = 2.25
    gpp.yax2.lab = gpp.ylab
    et.yax2.lab = et.ylab
    gcc.yax2.lab = gcc.ylab
    ts.legend.width = 25
    ts.legend.ypos = 0.95
    ts.legend.b.xpos = 85
    ts.legend.yspace = 1.25
    
    # ----------------------------- Plot
    # NMRI, GPP, ET, GCC
    # NMRI (y-axis 1)
    par(mar = c(3,4.5,0.75,15),
        oma = c(0,0,0,0))
    plot(x = nmri.nt$Dates,
         y = nmri.nt$Value,
         ylim = c(0.4,0.8),
         col = 'white', pch = 1, cex = 1.25, 
         xlab = '', ylab = nmri.lab, 
         cex.lab = 1.5, cex.axis = 1.5, las = 1,
         col.lab = nmri.col, yaxt = 'n', xaxt = 'n', font.lab = 2)
    abline(v = as.Date('2021-08-23'), lwd = 3, col = rgb(0,0,0,0.25))
    box(lwd = 2)
    # GPP (y-axis 2)
    par(new = TRUE)
    plot(x = date.seq,
         y = gpp[,2],
         ylim = c(first(gpp.tick.labs),last(gpp.tick.labs)),
         axes = FALSE, xlab = '', ylab = '',
         col = gpp.col, type = 'l', lwd = 3)
    axis(side = 4, cex.axis = 1.5, 
         lwd = 2,
         at = gpp.tick.labs,
         labels = c(NA,gpp.tick.labs[2:length(gpp.tick.labs)]),
         col.axis = gpp.col, col.ticks = gpp.col, col = gpp.col, font = 2)
    mtext(gpp.yax2.lab, cex = 1.5, side = 4, line = 3, col = gpp.col)
    axis(side = 1, cex.axis = 1.5,
         lwd = 2, 
         at = c(c(NA,as.Date('2021-07-01'),rep(NA,30)),
                c(as.Date('2021-08-01'),rep(NA,30)),
                c(as.Date('2021-09-01'),rep(NA,29)),
                c(as.Date('2021-10-01'),rep(NA,25))),
         labels = c(c(NA,'Jul-2021',rep(NA,30)),
                    c('Aug-2021',rep(NA,30)),
                    c('Sep-2021',rep(NA,29)),
                    c('Oct-2021',rep(NA,25))))
    # ET (y-axis 3)
    par(new = TRUE)
    plot(x = date.seq,
         y = et[,2],
         axes = FALSE, xlab = '', ylab = '',
         col = et.col, type = 'l', lwd = 3)
    axis(side = 4, cex.axis = 1.5, 
         line = 5, lwd = 2,
         at = pretty(range(et[,2], na.rm = TRUE)),
         labels = pretty(range(et[,2], na.rm = TRUE)),
         col.axis = et.col, col.ticks = et.col, col = et.col, font = 2)
    mtext(et.yax2.lab, cex = 1.5, side = 4, line = 8, col = et.col)
    # GCC (y-axis 4)
    par(new = TRUE)
    plot(x = date.seq,
         y = gcc[,2],
         ylim = c(first(gcc.tick.labs),last(gcc.tick.labs)),
         axes = FALSE, xlab = '', ylab = '',
         col = gcc.col, type = 'l', lwd = 3)
    axis(side = 4, cex.axis = 1.5, 
         line = 10, , lwd = 2,
         at = gcc.tick.labs,
         labels = gcc.tick.labs,
         col.axis = gcc.col, col.ticks = gcc.col, col = gcc.col, font = 2)
    mtext(gcc.yax2.lab, cex = 1.5, side = 4, line = 13, col = gcc.col)
    # Plot NMRI overtop of GPP, ET, and GCC (for clarity)
    par(new = TRUE)
    plot(x = nmri.nt$Dates,
         y = nmri.nt$Value,
         ylim = c(0.4,0.8),
         col = 'blue', pch = 19, cex = 1.5, 
         xlab = '', ylab = '', cex.lab = 1.5, cex.axis = 0.0001,
         las = 1, axes = FALSE, yaxt = 'n')
    lines(x = nmri.nt.loess$Dates,
          y = nmri.nt.loess$y,
          lwd = 2,
          col = 'blue')
    axis(side = 2, cex.axis = 1.5,
         lwd = 2,
         at = pretty(range(nmri.nt$Value, na.rm = TRUE)),
         labels = pretty(range(nmri.nt$Value, na.rm = TRUE)),
         col.axis = nmri.col, col.ticks = nmri.col, col = nmri.col, font = 2)
    # Add legends
    legend(x = date.seq[1]-5,
           y = ts.legend.ypos,
           inset = 0.05,
           title = as.expression(bquote(bold("MK Sen's Slope (greening)"))),
           legend = c(paste('NMRI:',nmri.nt.slope.g[1]),
                      paste0('GPP: ',gpp.slope.g[1]),
                      paste0('ET: ',et.slope.g[1]),
                      paste0('GCC: ',gcc.slope.g[1])),
           title.col = 'black',
           text.col = c(nmri.col,gpp.col,et.col,gcc.col),
           text.font = 2,
           xjust = slope.legend.xjust.1,
           yjust = slope.legend.yjust,
           y.intersp = ts.legend.yspace,
           bty = 'n',
           cex = 1.5)
    legend(x = date.seq[ts.legend.b.xpos],
           y = ts.legend.ypos,
           inset = 0.05,
           title = as.expression(bquote(bold("MK Sen's Slope (browning)"))),
           legend = c(paste('NMRI:',nmri.nt.slope.b[1]),
                      paste0('GPP: ',gpp.slope.b[1]),
                      paste0('ET: ',et.slope.b[1]),
                      paste0('GCC: ',gcc.slope.b[1])),
           title.col = 'black',
           text.col = c(nmri.col,gpp.col,et.col,gcc.col),
           text.font = 2,
           xjust = slope.legend.xjust.2,
           yjust = slope.legend.yjust,
           y.intersp = ts.legend.yspace,
           bty = 'n',
           cex = 1.5)
    text(x = as.Date('2021-07-25'), y = 0.4, font = 2, cex = 1.75,
         label = 'Greening', col = 'forestgreen')
    text(x = as.Date('2021-09-25'), y = 0.4, font = 2, cex = 1.75,
         label = 'Browning', col = 'darkorange4')
    text(x = as.Date('2021-08-23'), y = 0.475, font = 2, cex = 1.5,
         label = 'Date of peak\n greenness\n (AUG 23)')
    
    ts.plot = recordPlot()
    return(as_grob(ts.plot))
  }
  
  nmri.flux.ts = nmri.flux.ts.fun()  
  
  ggsave(filename = paste(figs.fp, 'Fig02b_NMRI_GPP_ET_GCC_TS.png', sep = '/'),
         nmri.flux.ts,
         width = 16, height = 8,
         bg = 'white')
  
  # Precip data
  precip = fluxnet.wkg[,c(1,2,11)]
  precip = precip %>%
    group_by(Dates) %>%
    summarise(Precip = sum(Precip, na.rm = TRUE)) %>%
    as.data.frame()
  
  precip.sig = precip$Precip
  precip.sig[precip.sig < 5] = NA
  precip$Precip_sig = precip.sig
  
  precip.sig.for.zscore.ts = precip[,c(1,3)]
  precip.sig.for.zscore.ts = na.omit(precip.sig.for.zscore.ts)
  
  precip.nonsig.for.zscore.ts = precip$Precip
  precip.nonsig.for.zscore.ts[precip.nonsig.for.zscore.ts >= 5] = NA
  precip.nonsig.for.zscore.ts[precip.nonsig.for.zscore.ts == 0] = NA
  precip.nonsig.for.zscore.ts = data.frame('Dates' = precip$Dates, 'Precip_nonsig' = precip.nonsig.for.zscore.ts)
  precip.nonsig.for.zscore.ts = na.omit(precip.nonsig.for.zscore.ts)
  
  # Precipitation timeseries
  precip.ts = ggplot(data = precip,
                     aes(x = Dates)) +
    geom_vline(xintercept = as.Date('2021-07-10'), linewidth = 1, alpha = 0.5, linetype = 'dashed') +
    geom_vline(xintercept = as.Date('2021-08-14'), linewidth = 1, alpha = 0.5, linetype = 'dashed') +
    geom_vline(xintercept = as.Date('2021-09-29'), linewidth = 1, alpha = 0.5, linetype = 'dashed') +
    geom_bar(aes(y = Precip, fill = 'lightblue'),
             stat = 'identity') +
    geom_bar(aes(y = Precip_sig, fill = 'blue'), stat = 'identity') +
    scale_fill_manual(values = c('blue','lightblue'),
                      labels = c('>= 5 mm (significant)', '< 5 mm'),
                      name = 'P') +
    guides(fill = guide_legend(position = 'inside', title = '', ncol = 1)) +
    scale_y_continuous(limits = c(0,45), expand = c(0,0)) +
    scale_x_date(breaks = seq(as.Date('2021-07-01'),
                              as.Date('2021-10-01'),
                              by = 'month'),
                 date_labels = '%b-%Y') +
    geom_text(data = data.frame('x' = as.Date('2021-07-12'), 'y' = 42, 'lab' = 'Pulse 1 (07/10)'),
              aes(x = x, y = y, label = lab),
              size = 6, fontface = 'bold') +
    geom_text(data = data.frame('x' = as.Date('2021-08-16'), 'y' = 27, 'lab' = 'Pulse 2 (08/14)'),
              aes(x = x, y = y, label = lab),
              size = 6, fontface = 'bold') +
    geom_text(data = data.frame('x' = as.Date('2021-10-01'), 'y' = 20, 'lab' = 'Pulse 3 (09/29)'),
              aes(x = x, y = y, label = lab),
              size = 6, fontface = 'bold') +
    xlab('') +
    ylab(expression(Precip.~'['~mm~d^-1~']')) +
    theme_bw() +
    theme(axis.text.y = element_text(size = 15),
          axis.text.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          legend.title = element_blank(),
          legend.box.background = element_rect(fill = 'transparent', color = 'transparent'),
          legend.key = element_rect(color = 'transparent', fill = 'transparent'),
          legend.text = element_text(size = 16, face = 'bold'),
          #legend.position.inside = c(0.575,0.87),
          legend.position.inside = c(0.87,0.87),
          panel.border = element_rect(color = 'black', 
                                      fill = NA, 
                                      linewidth = 1))
  
  # Combine precip and NMRI/GPP/ET/GCC timeseries
  nmri.flux.ts.fig = magick::image_read(paste(figs.fp, 'Fig02b_NMRI_GPP_ET_GCC_TS.png', sep = '/')) %>%
    magick::image_background('white') %>%
    magick::image_ggplot()
  
  nmri.flux.precip.ts.combo = plot_grid(plotlist = list(plot_grid(NULL,precip.ts,NULL, rel_widths = c(0.02,1,0.24), ncol = 3),
                                                        nmri.flux.ts.fig),
                                        ncol = 1,
                                        align = 'v',
                                        rel_heights = c(0.5,1),
                                        labels = c('a)','b)'),
                                        label_size = 19)
  
  ggsave(filename = paste(figs.fp, 'Fig02_NMRI_GPP_ET_GCC_TS_Precip.png', sep = '/'),
         nmri.flux.precip.ts.combo,
         height = 11, width = 15,
         bg = 'white')
  
  # ---------------------------------------------------------- Create scatterplots
  nmri.flux.sp.fun = function() {
    
    # ------------------------------------------- Create dataframes and assign greening/browning 
    # nmri.gpp.df = data.frame('Dates' = date.seq,
    #                          'NMRI' = c(wkg.nmri.gpp.et.day.night.lag$NMRI_Night,NA),
    #                          'GPP' = c(NA,wkg.nmri.gpp.et.day.night.lag$GPP))
    
    nmri.gpp.df = data.frame('Dates' = wkg.nmri.gpp.et.day.night.lag$NMRI_Dates,
                             'NMRI' = wkg.nmri.gpp.et.day.night.lag$NMRI_Night,
                             'GPP' = wkg.nmri.gpp.et.day.night.lag$GPP)
    
    # nmri.et.df = data.frame('Dates' = date.seq,
    #                         'NMRI' = c(wkg.nmri.gpp.et.day.night.lag$NMRI_Night,NA),
    #                         'ET' = c(NA,wkg.nmri.gpp.et.day.night.lag$ET))
    
    nmri.et.df = data.frame('Dates' = wkg.nmri.gpp.et.day.night.lag$NMRI_Dates,
                            'NMRI' = wkg.nmri.gpp.et.day.night.lag$NMRI_Night,
                            'ET' = wkg.nmri.gpp.et.day.night.lag$ET)
    
    nmri.gcc.df = data.frame('Dates' = date.seq[1:118],
                             'NMRI' =wkg.nmri.gpp.et.day.night.lag$NMRI_Night,
                             'GCC' = gcc$GCC[2:119])
    
    nmri.gpp.df$MonsoonPhase = ''
    nmri.gpp.df[nmri.gpp.df$Dates %in% greening.dates,]$MonsoonPhase = 'Greening'
    nmri.gpp.df[nmri.gpp.df$Dates %in% browning.dates,]$MonsoonPhase = 'Browning'
    nmri.gpp.df$MonsoonPhase = factor(nmri.gpp.df$MonsoonPhase, levels = c('Greening','Browning'))
    
    nmri.et.df$MonsoonPhase = ''
    nmri.et.df[nmri.et.df$Dates %in% greening.dates,]$MonsoonPhase = 'Greening'
    nmri.et.df[nmri.et.df$Dates %in% browning.dates,]$MonsoonPhase = 'Browning'
    nmri.et.df$MonsoonPhase = factor(nmri.et.df$MonsoonPhase, levels = c('Greening','Browning'))
    
    nmri.gcc.df$MonsoonPhase = ''
    nmri.gcc.df[nmri.gcc.df$Dates %in% greening.dates,]$MonsoonPhase = 'Greening'
    nmri.gcc.df[nmri.gcc.df$Dates %in% browning.dates,]$MonsoonPhase = 'Browning'
    nmri.gcc.df$MonsoonPhase = factor(nmri.gcc.df$MonsoonPhase, levels = c('Greening','Browning'))
    
    # Subset greening/browning vars
    nmri.g = nmri.gpp.df[nmri.gpp.df$MonsoonPhase == 'Greening',]$NMRI
    nmri.b = nmri.gpp.df[nmri.gpp.df$MonsoonPhase == 'Browning',]$NMRI
    
    gpp.g = nmri.gpp.df[nmri.gpp.df$MonsoonPhase == 'Greening',]$GPP
    gpp.b = nmri.gpp.df[nmri.gpp.df$MonsoonPhase == 'Browning',]$GPP
    
    et.g = nmri.et.df[nmri.et.df$MonsoonPhase == 'Greening',]$ET
    et.b = nmri.et.df[nmri.et.df$MonsoonPhase == 'Browning',]$ET
    
    gcc.g = nmri.gcc.df[nmri.gcc.df$MonsoonPhase == 'Greening',]$GCC
    gcc.b = nmri.gcc.df[nmri.gcc.df$MonsoonPhase == 'Browning',]$GCC
    
    # Compute linear models and statistics
    nmri.gpp.lm.g = lm(gpp.g ~ nmri.g, na.action = na.omit)
    nmri.gpp.lm.b = lm(gpp.b ~ nmri.b, na.action = na.omit)
    nmri.gpp.r2.g = round(summary(nmri.gpp.lm.g)$r.squared,2)
    nmri.gpp.r2.b = round(summary(nmri.gpp.lm.b)$r.squared,2)
    nmri.gpp.slope.g = round(coef(nmri.gpp.lm.g)[2],2)
    nmri.gpp.slope.b = round(coef(nmri.gpp.lm.b)[2],2)
    
    nmri.et.lm.g = lm(et.g ~ nmri.g, na.action = na.omit)
    nmri.et.lm.b = lm(et.b ~ nmri.b, na.action = na.omit)
    nmri.et.r2.g = round(summary(nmri.et.lm.g)$r.squared,2)
    nmri.et.r2.b = round(summary(nmri.et.lm.b)$r.squared,2)
    nmri.et.slope.g = round(coef(nmri.et.lm.g)[2],2)
    nmri.et.slope.b = round(coef(nmri.et.lm.b)[2],2)
    
    nmri.gcc.lm.g = lm(gcc.g ~ nmri.g, na.action = na.omit)
    nmri.gcc.lm.b = lm(gcc.b ~ nmri.b, na.action = na.omit)
    nmri.gcc.r2.g = round(summary(nmri.gcc.lm.g)$r.squared,2)
    nmri.gcc.r2.b = round(summary(nmri.gcc.lm.b)$r.squared,2)
    nmri.gcc.slope.g = round(coef(nmri.gcc.lm.g)[2],2)
    nmri.gcc.slope.b = round(coef(nmri.gcc.lm.b)[2],2)
    
    # Create dataframes for linear stats labels
    nmri.gpp.r2.lab.g = data.frame('label' = paste('R\u00b2 =', nmri.gpp.r2.g))
    nmri.gpp.r2.lab.b = data.frame('label' = paste('R\u00b2 =', nmri.gpp.r2.b))
    nmri.gpp.slope.lab.g = data.frame('label' = paste('Slope =', unname(nmri.gpp.slope.g)))
    nmri.gpp.slope.lab.b = data.frame('label' = paste('Slope =', unname(nmri.gpp.slope.b)))
    
    nmri.et.r2.lab.g = data.frame('label' = paste('R\u00b2 =', nmri.et.r2.g))
    nmri.et.r2.lab.b = data.frame('label' = paste('R\u00b2 =', nmri.et.r2.b))
    nmri.et.slope.lab.g = data.frame('label' = paste('Slope =', unname(nmri.et.slope.g)))
    nmri.et.slope.lab.b = data.frame('label' = paste('Slope =', unname(nmri.et.slope.b)))
    
    nmri.gcc.r2.lab.g = data.frame('label' = paste('R\u00b2 =', nmri.gcc.r2.g))
    nmri.gcc.r2.lab.b = data.frame('label' = paste('R\u00b2 =', nmri.gcc.r2.b))
    nmri.gcc.slope.lab.g = data.frame('label' = paste('Slope =', unname(nmri.gcc.slope.g)))
    nmri.gcc.slope.lab.b = data.frame('label' = paste('Slope =', unname(nmri.gcc.slope.b)))
    
    # Set general graphical parameters
    sp.pointsize = 4
    axis.labsize = element_text(size = 18)
    axis.titlesize = element_text(size = 18)
    stats.textsize = 8
    slope.lab.xjust = 0.12
    sp.title = element_text(size = 17, face = 'bold', hjust = 0.5)
    legend.textsize = element_text(size = 19)
    label_x = 0.43
    
    # ------------------------------------------------ Plot
    # NMRI vs. GPP
    nmri.gpp.sp = ggplot(data = nmri.gpp.df,
                         aes(x = NMRI, y = GPP, color = MonsoonPhase)) +
      geom_point(size = 3, pch = 19, alpha = 0.75) +
      # ------ Add regression lines (greening, and drying)
      geom_smooth(method = 'lm' , se = TRUE , aes(x = NMRI, y = GPP, color = MonsoonPhase, fill = MonsoonPhase),
                  linewidth = 1.25, na.rm = TRUE, fullrange = TRUE, show.legend = FALSE) +
      scale_color_manual(values = c('forestgreen','darkorange4'), aesthetics = c('color','fill'),
                         labels = c('Greening (06/30 - 08/23)', 'Browning (08/24 - 10/26)')) +
      # ------ Add regression R2 value (greening, and drying)
      geom_text(data = nmri.gpp.r2.lab.g, aes(label = label),
                inherit.aes = FALSE,
                color = 'forestgreen',
                x = label_x,
                hjust = 0,
                y = 15,
                size = stats.textsize,
                fontface = 'bold') +
      geom_text(data = nmri.gpp.r2.lab.b, aes(label = label),
                inherit.aes = FALSE,
                color = 'darkorange4',
                x = label_x,
                hjust = 0,
                y = 14,
                size = stats.textsize,
                fontface = 'bold') +
      # ------ Add regression slope value (greening, and drying)
      geom_text(data = nmri.gpp.slope.lab.g, aes(label = label),
                inherit.aes = FALSE,
                color = 'forestgreen',
                x = label_x + slope.lab.xjust,
                hjust = 0,
                y = 15,
                size = stats.textsize,
                fontface = 'bold') +
      geom_text(data = nmri.gpp.slope.lab.b, aes(label = label),
                inherit.aes = FALSE,
                color = 'darkorange4',
                x = label_x + slope.lab.xjust,
                hjust = 0,
                y = 14,
                size = stats.textsize,
                fontface = 'bold') +
      xlab(nmri.lab) +
      ylab(gpp.ylab) +
      xlim(c(0.44,0.76)) +
      ylim(gpp.ylim) +
      theme_bw() +
      theme(legend.position = 'bottom',
            legend.title = element_blank(),
            legend.text = legend.textsize,
            plot.title = sp.title,
            axis.title = axis.titlesize,
            axis.text = axis.labsize,
            panel.border = element_rect(color = 'black', fill = NA, linewidth = 1))

    
    # NMRI vs. ET
    nmri.et.sp = ggplot(data = nmri.et.df,
                        aes(x = NMRI, y = ET, color = MonsoonPhase, fill = MonsoonPhase)) +
      geom_point(size = 3, pch = 19, alpha = 0.75) +
      # ------ Add regression lines (greening, and drying)
      geom_smooth(method = 'lm' , se = TRUE , aes(color = MonsoonPhase),
                  linewidth = 1.25, na.rm = TRUE, fullrange = TRUE, show.legend = FALSE) +
      scale_color_manual(values = c('forestgreen','darkorange4'), aesthetics = c('color','fill'),
                         labels = c('Greening (06/30 - 08/23)', 'Browning (08/24 - 10/26)')) +
      # ------ Add regression R2 value (greening, and drying)
      geom_text(data = nmri.et.r2.lab.g, aes(label = label),
                inherit.aes = FALSE,
                color = 'forestgreen',
                x = label_x,
                hjust = 0,
                y = 6,
                size = stats.textsize,
                fontface = 'bold') +
      geom_text(data = nmri.et.r2.lab.b, aes(label = label),
                inherit.aes = FALSE,
                color = 'darkorange4',
                x = label_x,
                hjust = 0,
                y = 5.6,
                size = stats.textsize,
                fontface = 'bold') +
      # ------ Add regression slope value (greening, and drying)
      geom_text(data = nmri.et.slope.lab.g, aes(label = label),
                inherit.aes = FALSE,
                color = 'forestgreen',
                x = label_x + slope.lab.xjust,
                hjust = 0,
                y = 6,
                size = stats.textsize,
                fontface = 'bold') +
      geom_text(data = nmri.et.slope.lab.b, aes(label = label),
                inherit.aes = FALSE,
                color = 'darkorange4',
                x = label_x + slope.lab.xjust,
                hjust = 0,
                y = 5.6,
                size = stats.textsize,
                fontface = 'bold') +
      xlab(nmri.lab) +
      ylab(et.ylab) +
      xlim(c(0.44,0.76)) +
      ylim(et.ylim) +
      theme_bw() +
      theme(legend.position = 'bottom',
            legend.title = element_blank(),
            legend.text = legend.textsize,
            plot.title = sp.title,
            axis.title = axis.titlesize,
            axis.text = axis.labsize,
            panel.border = element_rect(color = 'black', fill = NA, linewidth = 1))
    
    
    # NMRI vs. GCC
    nmri.gcc.sp = ggplot(data = nmri.gcc.df,
                         aes(x = NMRI, y = GCC, color = MonsoonPhase, fill = MonsoonPhase)) +
      geom_point(size = 3, pch = 19, alpha = 0.75) +
      # ------ Add regression lines (greening, and drying)
      geom_smooth(method = 'lm' , se = TRUE , aes(color = MonsoonPhase),
                  linewidth = 1.25, na.rm = TRUE, fullrange = TRUE, show.legend = FALSE) +
      scale_color_manual(values = c('forestgreen','darkorange4'), aesthetics = c('color','fill'),
                         labels = c('Greening (06/30 - 08/23)', 'Browning (08/24 - 10/26)')) +
      # ------ Add regression R2 value (greening, and drying)
      geom_text(data = nmri.gcc.r2.lab.g, aes(label = label),
                inherit.aes = FALSE,
                color = 'forestgreen',
                x = label_x,
                hjust = 0,
                y = 0.42,
                size = stats.textsize,
                fontface = 'bold') +
      geom_text(data = nmri.gcc.r2.lab.b, aes(label = label),
                inherit.aes = FALSE,
                color = 'darkorange4',
                x = label_x,
                hjust = 0,
                y = 0.414,
                size = stats.textsize,
                fontface = 'bold') +
      # ------ Add regression slope value (greening, and drying)
      geom_text(data = nmri.gcc.slope.lab.g, aes(label = label),
                inherit.aes = FALSE,
                color = 'forestgreen',
                x = label_x + slope.lab.xjust,
                hjust = 0,
                y = 0.42,
                size = stats.textsize,
                fontface = 'bold') +
      geom_text(data = nmri.gcc.slope.lab.b, aes(label = label),
                inherit.aes = FALSE,
                color = 'darkorange4',
                x = label_x + slope.lab.xjust,
                hjust = 0,
                y = 0.414,
                size = stats.textsize,
                fontface = 'bold') +
      xlab(nmri.lab) +
      ylab(gcc.ylab) +
      xlim(c(0.44,0.76)) +
      ylim(gcc.ylim) +
      theme_bw() +
      theme(legend.position = 'bottom',
            legend.title = element_blank(),
            legend.text = legend.textsize,
            plot.title = sp.title,
            axis.title = axis.titlesize,
            axis.text = axis.labsize,
            panel.border = element_rect(color = 'black', fill = NA, linewidth = 1))
    
    
    # ------------------------------------------------ Combine scatterplots
    
    # Combine
    sp.combo = plot_grid(plotlist = list(nmri.gpp.sp + theme(legend.position = 'none', axis.title.x = element_blank()),
                                         nmri.et.sp + theme(legend.position = 'none'),
                                         nmri.gcc.sp + theme(legend.position = 'none', axis.title.x = element_blank())),
                         ncol = 3,
                         align = 'h',
                         axis = 'l',
                         labels = c('a)','b)','c)'),
                         label_size = 22,
                         hjust = 0.01)
    
    # Get legend and add to combo plot
    sp.combo.legend = get_plot_component(nmri.gpp.sp, 'guide-box-bottom', return_all = TRUE)
    sp.combo = plot_grid(sp.combo, sp.combo.legend, ncol = 1, rel_heights = c(1,0.05))
    
    ggsave(filename = paste(figs.fp, 'Fig03_NMRI_GPP_ET_GCC_SP_Combo.png', sep = '/'),
           sp.combo,
           width = 17, height = 6,
           bg = 'white')
    
    return(sp.combo)
  }
  
  nmri.flux.sp = nmri.flux.sp.fun()  

  # # ------------------------------------------------ Combine timeseries and scatterplots
  # 
  # ts.sp.combo = plot_grid(plotlist = list(nmri.flux.ts, nmri.flux.sp),
  #                         ncol = 1,
  #                         rel_heights = c(1,0.75),
  #                         labels = c('a)',''),
  #                         label_size = 20,
  #                         hjust = 0.01)
  # 
  # ggsave(filename = paste(figs.fp, 'zFinal', 'NMRI_GPP_ET_GCC_TS_SP_Combo.png', sep = '/'),
  #        ts.sp.combo,
  #        width = 16, height = 13,
  #        bg = 'white')
}

wkg.nmri.flux.ts.sp.fun()

# -------------------------------------------------------------------------------------------------------------------------------------
# Fig. S3
# Function for plotting timeseris, scatterplots, box/whisker plots, and bar plots for actual GPP and GPP predicted from nighttime NMRI
# -------------------------------------------------------------------------------------------------------------------------------------

gpp.from.nmri.fun = function(nmri.dataset) {
  
  # Subset NMRI and GPP
  df.gpp = wkg.nmri.gpp.et.day.night.lag[c(nmri.dataset,'GPP','MonsoonPhase','NMRI_Dates','Flux_Dates')]
  colnames(df.gpp)[1:2] = c('x','y') # x = NMRI, y = GPP
  
  # Subset greening and browning
  df.gpp.greening = df.gpp[df.gpp$MonsoonPhase == 'Greening',]
  df.gpp.browning = df.gpp[df.gpp$MonsoonPhase == 'Browning',]
  
  # Create linear models
  gpp.nmri.full.lm = lm(df.gpp$y ~ df.gpp$x + I(df.gpp$x^2), na.action = na.omit)
  # gpp.nmri.greening.lm = lm(df.gpp.greening$y ~ df.gpp.greening$x, na.action = na.omit)
  # gpp.nmri.browning.lm = lm(df.gpp.browning$y ~ df.gpp.browning$x, na.action = na.omit)
  
  # Extract model coefficients
  coeffs.full = gpp.nmri.full.lm$coefficients
  # coeffs.greening = gpp.nmri.greening.lm$coefficients
  # coeffs.browning = gpp.nmri.browning.lm$coefficients
  
  # Estimate GPP from NMRI using extracted model coefficients
  gpp.from.nmri.full = coeffs.full[1] + (coeffs.full[2] * df.gpp$x) + (coeffs.full[3] * (df.gpp$x^2))
  # gpp.from.nmri.greening = coeffs.greening[1] + (coeffs.greening[2] * df.gpp.greening$x)
  # gpp.from.nmri.browning = coeffs.browning[1] + (coeffs.browning[2] * df.gpp.browning$x)
  
  # Normalize dates for actual and estimated GPP, combine data frames for plotting
  gpp.from.nmri.df = rbind(data.frame('GPP_est' = gpp.from.nmri.full,
                                      'Dates' = df.gpp$NMRI_Dates),
                           data.frame('GPP_est' = NA,
                                      'Dates' = as.Date('2021-10-26')))
  
  gpp.orig.df = rbind(data.frame('GPP_act' = NA,
                                 'Dates' = as.Date('2021-06-30')),
                      data.frame('GPP_act' = df.gpp$y,
                                 'Dates' = df.gpp$Flux_Dates))
  
  nmri.gpp.df = data.frame('GPP_est' = gpp.from.nmri.df$GPP_est,
                           'GPP_act' = gpp.orig.df$GPP_act,
                           'Dates' = date.seq)
  
  nmri.gpp.df$MonsoonPhase = ''
  nmri.gpp.df[nmri.gpp.df$Dates %in% greening.dates,4] = 'Greening'
  nmri.gpp.df[nmri.gpp.df$Dates %in% browning.dates,4] = 'Browning'
  nmri.gpp.df$MonsoonPhase = factor(nmri.gpp.df$MonsoonPhase, levels = c('Greening','Browning'))
  
  nmri.gpp.df.for.ts = nmri.gpp.df
  nmri.gpp.df.for.ts = reshape2::melt(nmri.gpp.df.for.ts,
                                      id.vars = c('Dates','MonsoonPhase'),
                                      variable.name = 'GPP_var',
                                      value.name = 'GPP')
  
  nmri.gpp.df.for.bp = rbind(data.frame('MonsoonPhase' = 'Full',
                                        'GPP_est' = nmri.gpp.df$GPP_est,
                                        'GPP_act' = nmri.gpp.df$GPP_act),
                             data.frame('MonsoonPhase' = 'Greening',
                                        'GPP_est' = nmri.gpp.df[nmri.gpp.df$MonsoonPhase == 'Greening',]$GPP_est,
                                        'GPP_act' = nmri.gpp.df[nmri.gpp.df$MonsoonPhase == 'Greening',]$GPP_act),
                             data.frame('MonsoonPhase' = 'Browning',
                                        'GPP_est' = nmri.gpp.df[nmri.gpp.df$MonsoonPhase == 'Browning',]$GPP_est,
                                        'GPP_act' = nmri.gpp.df[nmri.gpp.df$MonsoonPhase == 'Browning',]$GPP_act))
  
  nmri.gpp.df.for.bp = nmri.gpp.df.for.bp[complete.cases(nmri.gpp.df.for.bp[2:3]),]
  nmri.gpp.df.for.bp$MonsoonPhase = factor(nmri.gpp.df.for.bp$MonsoonPhase, levels = c('Full','Greening','Browning'))
  nmri.gpp.df.for.bp = reshape2::melt(nmri.gpp.df.for.bp,
                                      id.vars = 'MonsoonPhase',
                                      variable.name = 'GPP_var',
                                      value.name = 'GPP')
  
  colnames(nmri.gpp.df)[1:2] = c('x','y') # x = GPP_est, y = GPP_act
  
  # Sub-seasonal temporal subsets
  gpp.from.nmri.greening.df = nmri.gpp.df[nmri.gpp.df$MonsoonPhase == 'Greening',]
  gpp.from.nmri.browning.df = nmri.gpp.df[nmri.gpp.df$MonsoonPhase == 'Browning',]
  
  # Linear model fit of estimated to actual GPP (full season and greening/browning phases)
  gpp.from.nmri.full.lm = lm(nmri.gpp.df$y ~ nmri.gpp.df$x, na.action = na.omit)
  gpp.from.nmri.greening.lm = lm(gpp.from.nmri.greening.df$y ~ gpp.from.nmri.greening.df$x, na.action = na.omit)
  gpp.from.nmri.browning.lm = lm(gpp.from.nmri.browning.df$y ~ gpp.from.nmri.browning.df$x, na.action = na.omit)
  
  # Extract slope values and create data frames for labelling on plot
  gpp.from.nmri.full.slope = round(coef(gpp.from.nmri.full.lm)[2],2)
  gpp.from.nmri.greening.slope = round(coef(gpp.from.nmri.greening.lm)[2],2)
  gpp.from.nmri.browning.slope = round(coef(gpp.from.nmri.browning.lm)[2],2)
  gpp.from.nmri.full.slope.lab = data.frame(label = paste('Slope =', unname(gpp.from.nmri.full.slope)))
  gpp.from.nmri.greening.slope.lab = data.frame(label = paste('Slope =', unname(gpp.from.nmri.greening.slope)))
  gpp.from.nmri.browning.slope.lab = data.frame(label = paste('Slope =', unname(gpp.from.nmri.browning.slope)))
  
  # Extract R^2 values and create data frames for labelling on plot
  gpp.from.nmri.full.r2 = round(summary(gpp.from.nmri.full.lm)$r.squared,2)
  gpp.from.nmri.greening.r2 = round(summary(gpp.from.nmri.greening.lm)$r.squared,2)
  gpp.from.nmri.browning.r2 = round(summary(gpp.from.nmri.browning.lm)$r.squared,2)
  gpp.from.nmri.full.r2.lab = data.frame(label = paste('R\u00b2 =', gpp.from.nmri.full.r2))
  gpp.from.nmri.greening.r2.lab = data.frame(label = paste('R\u00b2 =', gpp.from.nmri.greening.r2))
  gpp.from.nmri.browning.r2.lab = data.frame(label = paste('R\u00b2 =', gpp.from.nmri.browning.r2))
  
  # Set equation for predicting GPP from NMRI
  coeff.1 = round(unname(coeffs.full[1],3))
  coeff.2 = round(unname(coeffs.full[2],3))
  coeff.3 = round(unname(coeffs.full[3],3))
  
  gpp.from.nmri.equation = substitute(italic(GPP[ESTIMATED]) == a + b %.% italic(NMRI)* + c %.% italic(NMRI^2),
                                      list(a = coeff.1,
                                           b = coeff.2,
                                           c = coeff.3))
  
  gpp.from.nmri.equation.lab = as.character(as.expression(gpp.from.nmri.equation))
  
  # --- Compute stats for display
  # RMSE
  rmse.fun = function(obs, est) {
    diff = obs - est
    diff.square = diff^2
    mean.diff.square = mean(diff.square, na.rm = TRUE)
    rmse = sqrt(mean.diff.square)
    return(format(round(rmse,2), nsmall = 2))
  }
  
  gpp.from.nmri.full.rmse = data.frame(x = as.Date('2021-10-01'),
                                       y = 14,
                                       label = paste('RMSE =',rmse.fun(nmri.gpp.df$y, nmri.gpp.df$x)))
  
  gpp.from.nmri.phase.rmse = data.frame(x = rep(as.Date('2021-10-01'),2),
                                        y = c(13,12),
                                        label = paste('RMSE =', 
                                                      c(rmse.fun(gpp.from.nmri.greening.df$y, gpp.from.nmri.greening.df$x),
                                                        rmse.fun(gpp.from.nmri.browning.df$y, gpp.from.nmri.browning.df$x))),
                                        MonsoonPhase = factor(c('Greening','Browning'), levels = c('Greening','Browning')))
  
  # Mean bias error (MBE)
  mbe.fun = function(obs, est) {
    mbe = mean(est - obs, na.rm = TRUE)
    return(format(round(mbe,2), nsmall = 2))
  }
  
  gpp.from.nmri.full.mbe = data.frame(x = as.Date('2021-10-01'),
                                      y = 10.5,
                                      label = paste('MBE =',mbe.fun(nmri.gpp.df$y, nmri.gpp.df$x)))
  
  gpp.from.nmri.phase.mbe = data.frame(x = rep(as.Date('2021-10-01'),2),
                                       y = c(9.5,8.5),
                                       label = paste('MBE =', 
                                                     c(mbe.fun(gpp.from.nmri.greening.df$y, gpp.from.nmri.greening.df$x),
                                                       mbe.fun(gpp.from.nmri.browning.df$y, gpp.from.nmri.browning.df$x))),
                                       MonsoonPhase = factor(c('Greening','Browning'),levels = c('Greening','Browning')))
  
  # Set general graphical parameters
  sp.pointsize = 4
  ts.pointsize = 3
  axis.labsize = element_text(size = 19)
  axis.titlesize = element_text(size = 17)
  stats.textsize = 7
  legend.textsize = element_text(size = 21)
  
  # ------------------------------------------ Create timeseries plot
  ts = ggplot(data = nmri.gpp.df.for.ts,
              aes(x = Dates, y = GPP, color = MonsoonPhase, shape = GPP_var)) +
    geom_point(size = 3, stroke = 1.25, alpha = 0.75) +
    scale_color_manual(values = c('forestgreen','darkorange4'),
                       labels = c('Greening (06/30 - 08/23)', 'Browning (08/24 - 10/26)'),
                       guide = 'none') +
    scale_shape_manual(values = c(0,8),
                       labels = c('ESTIMATED','ACTUAL')) +
    xlab('') +
    ylab(expression(GPP~' ['~gC~m^-2~d^-1~']')) +
    ylim(c(0,16.5)) +
    theme_bw() +
    # Add equation for GPP (estimated)
    annotate('text', 
             x = as.Date('2021-08-23'),
             y = 16.5,
             label = gpp.from.nmri.equation.lab,
             parse = TRUE, size = 7) +
    # Add RMSE values
    geom_text(data = gpp.from.nmri.full.rmse,
              aes(x = x, y = y, label = label),
              inherit.aes = FALSE, size = stats.textsize) +
    geom_text(data = gpp.from.nmri.phase.rmse,
              aes(x = x, y = y, label = label),
              color = c('forestgreen','darkorange4'),
              inherit.aes = FALSE, size = stats.textsize) +
    # Add MBE values
    geom_text(data = gpp.from.nmri.full.mbe,
              aes(x = x, y = y, label = label),
              inherit.aes = FALSE, size = stats.textsize) +
    geom_text(data = gpp.from.nmri.phase.mbe,
              aes(x = x, y = y, label = label),
              color = c('forestgreen','darkorange4'),
              inherit.aes = FALSE, size = stats.textsize) +
    guides(shape = guide_legend(ncol = 1, position = 'inside'),
           color = guide_legend(ncol = 2, position = 'bottom')) +
    theme(legend.position.inside = c(0.15,0.7),
          legend.title = element_blank(),
          legend.text = legend.textsize,
          legend.margin = margin(-25,0,0,0,'pt'),
          axis.text = axis.labsize,
          axis.title = element_text(size = 20),
          panel.border = element_rect(color = 'black', fill = NA, linewidth = 1))
  
  # ------------------------------------------ Create scatterplot
  sp = ggplot(data = nmri.gpp.df,
              aes(x = x, y = y, color = MonsoonPhase)) +
    geom_point(size = 4, shape = 19, alpha = 0.6) +
    scale_color_manual(values = c('forestgreen','darkorange4'),
                       labels = c('Greening (06/30 - 08/23)', 'Browning (08/24 - 10/26)')) +
    # ------ Add regression lines (full set, greening, and drying)
    # Full set
    geom_smooth(method = 'lm' , se = FALSE , color = 'black', 
                linewidth = 1.25, na.rm = TRUE, show.legend = FALSE) +
    # Greening/browning phases
    geom_smooth(method = 'lm' , se = FALSE , aes(color = MonsoonPhase), 
                linewidth = 1.25, na.rm = TRUE, show.legend = FALSE) +
    # ------ Add correlation stats (full set, greening, and drying)
    # Full set
    stat_cor(aes(x = x, y = y),
             inherit.aes = FALSE,
             method = 'pearson',
             size = stats.textsize, 
             na.rm = TRUE, show.legend = FALSE,
             label.y = 17,
             label.x = 0.1) +
    # Greening/browning phases
    stat_cor(aes(x = x, y = y),
             method = 'pearson', 
             size = stats.textsize,
             na.rm = TRUE, show.legend = FALSE,
             label.y = c(16,15),
             label.x = 0.1) +
    # ------ Add regression slope value (full set, greening, and drying)
    # Full set
    geom_text(data = gpp.from.nmri.full.slope.lab, aes(label = label),
              inherit.aes = FALSE,
              x = 9.2,
              hjust = 0,
              y = 6,
              size = stats.textsize) +
    # Greening/browning phases
    geom_text(data = gpp.from.nmri.greening.slope.lab, aes(label = label),
              inherit.aes = FALSE,
              color = 'forestgreen',
              x = 9.2,
              hjust = 0,
              y = 5,
              size = stats.textsize) +
    geom_text(data = gpp.from.nmri.browning.slope.lab, aes(label = label),
              inherit.aes = FALSE,
              color = 'darkorange4',
              x = 9.2,
              hjust = 0,
              y = 4,
              size = stats.textsize) +
    # ------ Add regression R2 value (full set, greening, and drying)
    # Full set
    geom_text(data = gpp.from.nmri.full.r2.lab, aes(label = label),
              inherit.aes = FALSE,
              x = 9.2,
              hjust = 0,
              y = 2.5,
              size = stats.textsize) +
    # Greening/browning phases
    geom_text(data = gpp.from.nmri.greening.r2.lab, aes(label = label),
              inherit.aes = FALSE,
              color = 'forestgreen',
              x = 9.2,
              hjust = 0,
              y = 1.5,
              size = stats.textsize) +
    geom_text(data = gpp.from.nmri.browning.r2.lab, aes(label = label),
              inherit.aes = FALSE,
              color = 'darkorange4',
              x = 9.2,
              hjust = 0,
              y = 0.5,
              size = stats.textsize) +
    xlab(expression(GPP[ESTIMATED]~' ['~gC~m^-2~d^-1~']')) +
    ylab(expression(GPP[ACTUAL]~' ['~gC~m^-2~d^-1~']')) +
    theme_bw() +
    theme(legend.position = 'bottom',
          legend.title = element_blank(),
          legend.text = legend.textsize,
          axis.text = axis.labsize,
          axis.title = element_text(size = 20),
          panel.border = element_rect(color = 'black', fill = NA, linewidth = 1))

  # ------------------------------------------ Create box-whisker plots
  bp.strip.cols = strip_themed(background_x = elem_list_rect(fill = c('black','forestgreen','darkorange4')))
  
  bp = ggplot(data = nmri.gpp.df.for.bp,
              aes(x = GPP_var, y = GPP)) +
    geom_boxplot(fill = 'transparent', size = 1) +
    scale_x_discrete(labels = factor(c('ESTIMATED','ACTUAL'))) +
    stat_summary(fun = mean,
                 aes(shape = 'Mean'),
                 geom = 'point',
                 size = 5, shape = 18, color = 'red') +
    theme_bw() +
    xlab('') +
    ylab(expression(GPP~' ['~gC~m^-2~d^-1~']')) +
    facet_wrap2(~MonsoonPhase,
                nrow = 1,
                strip = bp.strip.cols) +
    theme(legend.position = 'none',
          axis.text = axis.labsize,
          axis.title = axis.titlesize,
          strip.text = element_text(size = 20, color = 'white', face = 'bold'),
          strip.placement = 'outside',
          panel.border = element_rect(color = 'black', fill = NA, linewidth = 1))
  
  # ------------------------------------------ Create bar plots for total GPP
  barplot = ggplot(data = nmri.gpp.df.for.bp,
                   aes(x = GPP_var, y = GPP, fill = MonsoonPhase, color = MonsoonPhase)) +
    geom_bar(stat = 'identity', na.rm = TRUE) +
    scale_fill_manual(values = c('black','forestgreen','darkorange4'),
                      guide = 'none') +
    scale_color_manual(values = c('black','forestgreen','darkorange4'),
                       guide = 'none') +
    scale_x_discrete(labels = factor(c('ESTIMATED','ACTUAL'))) +
    xlab('') +
    ylab(expression('Tot.'~GPP~' ['~gC~m^-2~d^-1~']')) +
    theme_bw() +
    facet_wrap2(~MonsoonPhase,
                nrow = 1,
                #strip = bp.strip.cols,
                ) +
    theme(legend.position = 'none',
          axis.text = axis.labsize,
          axis.title = axis.titlesize,
          #strip.text = element_text(size = 20, color = 'white', face = 'bold'),
          strip.text = element_blank(),
          strip.background = element_blank(),
          #strip.placement = 'outside',
          panel.border = element_rect(color = 'black', fill = NA, linewidth = 1))
  
  # ------------------------------------------ Combine plots
  ts.sp.combo = plot_grid(ts, 
                          sp + theme(legend.position = 'none'),
                          ncol = 2,
                          rel_widths = c(1.5,1),
                          align = 'h',
                          axis = 'l',
                          labels = c('a)','b)'),
                          label_size = 22)
  
  # Get legend from scatterplot and add to combined TS/SP plot
  ts.sp.combo.legend = get_plot_component(sp + theme(legend.position = c(0.325,-0.01),
                                                     legend.margin = margin(-30,0,0,0, unit = 'mm'),
                                                     legend.direction = 'horizontal',
                                                     legend.background = element_rect(fill = 'transparent'),
                                                     legend.text = element_text(size = 20)),
                                          'guide-box-bottom', return_all = TRUE)
  
  ts.sp.combo = plot_grid(ts.sp.combo, ts.sp.combo.legend, ncol = 1, rel_heights = c(1,0.05))
  
  bp.combo = plot_grid(plotlist = list(bp + theme(axis.text.x = element_blank()), 
                                       barplot + theme(plot.margin = unit(c(-5,0,0,0), 'mm'))),
                       ncol = 1,
                       rel_heights = c(1.1,1),
                       align = 'v',
                       axis = 't',
                       labels = c('c)','d)'),
                       label_size = 22,
                       vjust = c(0.75,-1.25))
  
  ts.sp.bp.combo = plot_grid(plotlist = list(ts.sp.combo, bp.combo + theme(plot.margin = unit(c(-5,0,0,0), 'mm'))),
                             ncol = 1,
                             rel_heights = c(1.1,1),
                             align = 'v',
                             axis = 't')
  
  ggsave(filename = paste(figs.fp, 'FigS3_GPP_ACT_EST_TS_SP_BoxPlot_BarPlot.png', sep = '/'),
         ts.sp.bp.combo,
         width = 16, height = 14,
         bg = 'white')
  
}

gpp.from.nmri.fun('NMRI_Night')

# --------------------------------------------------------------------------------------------------------------------------------
# Prepare data for diurnal NMRI/VOD analysis
# --------------------------------------------------------------------------------------------------------------------------------

wkg.lpdr.desc$MonsoonPhase = ''
wkg.lpdr.desc[wkg.lpdr.desc$Dates %in% greening.dates,7] = 'Greening'
wkg.lpdr.desc[wkg.lpdr.desc$Dates %in% browning.dates,7] = 'Browning'

wkg.lpdr.asc$MonsoonPhase = ''
wkg.lpdr.asc[wkg.lpdr.asc$Dates %in% greening.dates,7] = 'Greening'
wkg.lpdr.asc[wkg.lpdr.asc$Dates %in% browning.dates,7] = 'Browning'

wkg.lprm.x.desc$MonsoonPhase = ''
wkg.lprm.x.desc[wkg.lprm.x.desc$Dates %in% greening.dates,8] = 'Greening'
wkg.lprm.x.desc[wkg.lprm.x.desc$Dates %in% browning.dates,8] = 'Browning'

wkg.lprm.x.asc$MonsoonPhase = ''
wkg.lprm.x.asc[wkg.lprm.x.asc$Dates %in% greening.dates,8] = 'Greening'
wkg.lprm.x.asc[wkg.lprm.x.asc$Dates %in% browning.dates,8] = 'Browning'

wkg.smap.dca.am$MonsoonPhase = ''
wkg.smap.dca.am[wkg.smap.dca.am$Dates %in% greening.dates,9] = 'Greening'
wkg.smap.dca.am[wkg.smap.dca.am$Dates %in% browning.dates,9] = 'Browning'

wkg.smap.dca.pm$MonsoonPhase = ''
wkg.smap.dca.pm[wkg.smap.dca.pm$Dates %in% greening.dates,9] = 'Greening'
wkg.smap.dca.pm[wkg.smap.dca.pm$Dates %in% browning.dates,9] = 'Browning'

wkg.nmri.vod.day.night = rbind(data.frame('Dates' = date.seq,
                                          'NMRI' = wkg.nmri.daytime[wkg.nmri.daytime$Period == 'Daytime',6],
                                          'LPDR_Xband_25km' = wkg.lpdr.asc$Value,
                                          'LPRM_Xband_10km' = wkg.lprm.x.asc$Value,
                                          'SMAP_DCA_Lband_09km' = wkg.smap.dca.am$Value,
                                          'Period' = 'Daytime'),
                               data.frame('Dates' = date.seq,
                                          'NMRI' = wkg.nmri.nighttime[wkg.nmri.nighttime$Period == 'Nighttime',6],
                                          'LPDR_Xband_25km' = wkg.lpdr.desc$Value,
                                          'LPRM_Xband_10km' = wkg.lprm.x.desc$Value,
                                          'SMAP_DCA_Lband_09km' = wkg.smap.dca.pm$Value,
                                          'Period' = 'Nighttime'))

wkg.nmri.vod.day.night$MonsoonPhase = ''
wkg.nmri.vod.day.night[wkg.nmri.vod.day.night$Dates %in% greening.dates,7] = 'Greening'
wkg.nmri.vod.day.night[wkg.nmri.vod.day.night$Dates %in% browning.dates,7] = 'Browning'
wkg.nmri.vod.day.night$Period = factor(wkg.nmri.vod.day.night$Period, levels = c('Nighttime','Daytime'))
wkg.nmri.vod.day.night$MonsoonPhase = factor(wkg.nmri.vod.day.night$MonsoonPhase, levels = c('Greening','Browning'))

# Create function to compute temporal mean of NMRI and VOD datasets
temporal.mean = function(data.in, data.type, data.name, period, window) {
  
  # Create XTS object
  if (data.type == 'NMRI') {data.xts = xts(data.in$NMRI_PeriodMean, data.in$Dates)}
  if (data.type == 'VOD') {data.xts = xts(data.in$Value, data.in$Dates)}
  
  # Compute mean over specified temporal window
  data.mean = rollapply(data.xts,
                        by = window,
                        width = window,
                        align = 'center',
                        FUN = function(x) mean(x, na.rm = TRUE),
                        fill = NA)
  
  # Create and return output data frame
  data.mean.df = data.frame('Dates' = index(data.mean),
                            'Period' = period,
                            'Value' = coredata(data.mean),
                            'Dataset' = data.name)
  return(data.mean.df)
}

# Set value for temporal averaging window
t.window = 3

# Compute temporal means
wkg.nmri.daytime.tmean = temporal.mean(wkg.nmri.daytime, 'NMRI', 'NMRI', 'Daytime', t.window)
wkg.nmri.nighttime.tmean = temporal.mean(wkg.nmri.nighttime, 'NMRI', 'NMRI', 'Nighttime', t.window)
wkg.lpdr.asc.tmean = temporal.mean(wkg.lpdr.asc, 'VOD', 'LPDR_Xband_25km', 'Daytime', t.window)
wkg.lpdr.desc.tmean = temporal.mean(wkg.lpdr.desc, 'VOD', 'LPDR_Xband_25km', 'Nighttime', t.window)
wkg.lprm.x.asc.tmean = temporal.mean(wkg.lprm.x.asc, 'VOD', 'LPRM_Xband_10km', 'Daytime', t.window)
wkg.lprm.x.desc.tmean = temporal.mean(wkg.lprm.x.desc, 'VOD', 'LPRM_Xband_10km', 'Nighttime', t.window)
wkg.smap.dca.am.tmean = temporal.mean(wkg.smap.dca.am, 'VOD', 'SMAP_DCA_Lband_09km', 'Daytime', t.window)
wkg.smap.dca.pm.tmean = temporal.mean(wkg.smap.dca.pm, 'VOD', 'SMAP_DCA_Lband_09km', 'Nighttime', t.window)

wkg.nmri.vod.day.night.diff.ratio = rbind(data.frame('Dates' = date.seq,
                                                     'Rel' = 'Diff',
                                                     'Relationship' = 'daytime - nighttime (diff.)',
                                                     'NMRI' = wkg.nmri.daytime.tmean$Value - wkg.nmri.nighttime.tmean$Value,
                                                     'VOD' = wkg.lpdr.asc.tmean$Value - wkg.lpdr.desc.tmean$Value,
                                                     'VOD_Product' = 'LPDR_Xband_25km'),
                                          data.frame('Dates' = date.seq,
                                                     'Rel' = 'Diff',
                                                     'Relationship' = 'daytime - nighttime (diff.)',
                                                     'NMRI' = wkg.nmri.daytime.tmean$Value - wkg.nmri.nighttime.tmean$Value,
                                                     'VOD' = wkg.lprm.x.asc.tmean$Value - wkg.lprm.x.desc.tmean$Value,
                                                     'VOD_Product' = 'LPRM_Xband_10km'),
                                          data.frame('Dates' = date.seq,
                                                     'Rel' = 'Diff',
                                                     'Relationship' = 'daytime - nighttime (diff.)',
                                                     'NMRI' = wkg.nmri.daytime.tmean$Value - wkg.nmri.nighttime.tmean$Value,
                                                     'VOD' = wkg.smap.dca.am.tmean$Value - wkg.smap.dca.pm.tmean$Value,
                                                     'VOD_Product' = 'SMAP_DCA_Lband_09km'),
                                          data.frame('Dates' = date.seq,
                                                     'Rel' = 'Ratio',
                                                     'Relationship' = 'daytime / nighttime (ratio)',
                                                     'NMRI' = wkg.nmri.daytime.tmean$Value / wkg.nmri.nighttime.tmean$Value,
                                                     'VOD' = wkg.lpdr.asc.tmean$Value / wkg.lpdr.desc.tmean$Value,
                                                     'VOD_Product' = 'LPDR_Xband_25km'),
                                          data.frame('Dates' = date.seq,
                                                     'Rel' = 'Ratio',
                                                     'Relationship' = 'daytime / nighttime (ratio)',
                                                     'NMRI' = wkg.nmri.daytime.tmean$Value / wkg.nmri.nighttime.tmean$Value,
                                                     'VOD' = wkg.lprm.x.asc.tmean$Value / wkg.lprm.x.desc.tmean$Value,
                                                     'VOD_Product' = 'LPRM_Xband_10km'),
                                          data.frame('Dates' = date.seq,
                                                     'Rel' = 'Ratio',
                                                     'Relationship' = 'daytime / nighttime (diff.)',
                                                     'NMRI' = wkg.nmri.daytime.tmean$Value / wkg.nmri.nighttime.tmean$Value,
                                                     'VOD' = wkg.smap.dca.am.tmean$Value / wkg.smap.dca.pm.tmean$Value,
                                                     'VOD_Product' = 'SMAP_DCA_Lband_09km'))

wkg.nmri.vod.day.night.diff.ratio$MonsoonPhase = ''
wkg.nmri.vod.day.night.diff.ratio[wkg.nmri.vod.day.night.diff.ratio$Dates %in% greening.dates,7] = 'Greening'
wkg.nmri.vod.day.night.diff.ratio[wkg.nmri.vod.day.night.diff.ratio$Dates %in% browning.dates,7] = 'Browning'
wkg.nmri.vod.day.night.diff.ratio$MonsoonPhase = factor(wkg.nmri.vod.day.night.diff.ratio$MonsoonPhase, levels = c('Greening','Browning'))


# Create function to compute standardized anomalies (Z-scores) for daytime/nighttime NMRI and VOD
zscore.fun = function(data.in, var.name) {
  dates = data.in$Dates
  data.mean = mean(data.in[[var.name]], na.rm = TRUE)
  data.sd = sd(data.in[[var.name]], na.rm = TRUE)
  zscore = (data.in[[var.name]] - data.mean) / data.sd
  zscore.df = data.frame('Dates' = dates,
                         'Value' = zscore)
  return(zscore.df)
}

# Compute Z-scores
wkg.swc.daytime = wkg.swc.day.night[wkg.swc.day.night$Period == 'Daytime',]
wkg.swc.nighttime = wkg.swc.day.night[wkg.swc.day.night$Period == 'Nighttime',]

nmri.smoothed.zscore = zscore.fun(wkg.nmri.smoothed, 'Value')
nmri.day.zscore = zscore.fun(wkg.nmri.daytime, 'NMRI_PeriodMean')
nmri.night.zscore = zscore.fun(wkg.nmri.nighttime, 'NMRI_PeriodMean')
lpdr.asc.zscore = zscore.fun(wkg.lpdr.asc, 'Value')
lpdr.desc.zscore = zscore.fun(wkg.lpdr.desc, 'Value')
lprm.asc.zscore = zscore.fun(wkg.lprm.x.asc, 'Value')
lprm.desc.zscore = zscore.fun(wkg.lprm.x.desc, 'Value')
smap.am.zscore = zscore.fun(wkg.smap.dca.am, 'Value')
smap.pm.zscore = zscore.fun(wkg.smap.dca.pm, 'Value')
swc.day.zscore = zscore.fun(wkg.swc.daytime, 'SWC_mean_PeriodMean')
swc.night.zscore = zscore.fun(wkg.swc.nighttime, 'SWC_mean_PeriodMean')
swc.1.day.zscore = zscore.fun(wkg.swc.daytime, 'SWC_1_mean_PeriodMean')
swc.1.night.zscore = zscore.fun(wkg.swc.nighttime, 'SWC_1_mean_PeriodMean')
swc.2.day.zscore = zscore.fun(wkg.swc.daytime, 'SWC_2_mean_PeriodMean')
swc.2.night.zscore = zscore.fun(wkg.swc.nighttime, 'SWC_2_mean_PeriodMean')
swc.3.day.zscore = zscore.fun(wkg.swc.daytime, 'SWC_3_mean_PeriodMean')
swc.3.night.zscore = zscore.fun(wkg.swc.nighttime, 'SWC_3_mean_PeriodMean')

wkg.nmri.vod.day.night.zscores = rbind(data.frame('Dates' = date.seq,
                                                  'NMRI' = nmri.day.zscore$Value,
                                                  'LPDR_Xband_25km' = lpdr.asc.zscore$Value,
                                                  'LPRM_Xband_10km' = lprm.asc.zscore$Value,
                                                  'SMAP_DCA_Lband_09km' = smap.am.zscore$Value,
                                                  'Period' = 'Daytime'),
                                       data.frame('Dates' = date.seq,
                                                  'NMRI' = nmri.night.zscore$Value,
                                                  'LPDR_Xband_25km' = lpdr.desc.zscore$Value,
                                                  'LPRM_Xband_10km' = lprm.desc.zscore$Value,
                                                  'SMAP_DCA_Lband_09km' = smap.pm.zscore$Value,
                                                  'Period' = 'Nighttime'))

wkg.nmri.vod.day.night.zscores$MonsoonPhase = ''
wkg.nmri.vod.day.night.zscores[wkg.nmri.vod.day.night.zscores$Dates %in% greening.dates,7] = 'Greening'
wkg.nmri.vod.day.night.zscores[wkg.nmri.vod.day.night.zscores$Dates %in% browning.dates,7] = 'Browning'
wkg.nmri.vod.day.night.zscores$Period = factor(wkg.nmri.vod.day.night.zscores$Period, levels = c('Nighttime','Daytime'))
wkg.nmri.vod.day.night.zscores$MonsoonPhase = factor(wkg.nmri.vod.day.night.zscores$MonsoonPhase, levels = c('Greening','Browning'))

# Compute loess curves for z-scores
#loess.span = 0.2
nmri.day.zscore.loess = loess.datefill(nmri.day.zscore, loess.span)
nmri.night.zscore.loess = loess.datefill(nmri.night.zscore, loess.span)
lprm.asc.zscore.loess = loess.datefill(lprm.asc.zscore, loess.span)
lprm.desc.zscore.loess = loess.datefill(lprm.desc.zscore, loess.span)
smap.am.zscore.loess = loess.datefill(smap.am.zscore, loess.span)
smap.pm.zscore.loess = loess.datefill(smap.pm.zscore, loess.span)

wkg.nmri.vod.day.night.zscores.loess = rbind(data.frame('Dates' = date.seq,
                                                        'NMRI' = nmri.day.zscore.loess$y,
                                                        'LPDR_Xband_25km' = lpdr.asc.zscore$Value,
                                                        'LPRM_Xband_10km' = lprm.asc.zscore.loess$y,
                                                        'SMAP_DCA_Lband_09km' = smap.am.zscore.loess$y,
                                                        'Period' = 'Daytime'),
                                             data.frame('Dates' = date.seq,
                                                        'NMRI' = nmri.night.zscore.loess$y,
                                                        'LPDR_Xband_25km' = lpdr.desc.zscore$Value,
                                                        'LPRM_Xband_10km' = lprm.desc.zscore.loess$y,
                                                        'SMAP_DCA_Lband_09km' = smap.pm.zscore.loess$y,
                                                        'Period' = 'Nighttime'))

wkg.nmri.vod.day.night.zscores.loess$MonsoonPhase = ''
wkg.nmri.vod.day.night.zscores.loess[wkg.nmri.vod.day.night.zscores.loess$Dates %in% greening.dates,7] = 'Greening'
wkg.nmri.vod.day.night.zscores.loess[wkg.nmri.vod.day.night.zscores.loess$Dates %in% browning.dates,7] = 'Browning'
wkg.nmri.vod.day.night.zscores.loess$Period = factor(wkg.nmri.vod.day.night.zscores.loess$Period, levels = c('Nighttime','Daytime'))
wkg.nmri.vod.day.night.zscores.loess$MonsoonPhase = factor(wkg.nmri.vod.day.night.zscores.loess$MonsoonPhase, levels = c('Greening','Browning'))

# ------------------------------------------------------------------------------
# Fig. 6
# Function for plotting combo NMRI/VOD z-score timeseries and scatterplots
# ------------------------------------------------------------------------------

nmri.vod.ts.sp.day.night.zscore.fun = function() {

  # Graphical parameters
  axisText = element_text(size = 16)
  axisTitle = element_text(size = 16)
  axisTitleY = element_text(vjust = -1.5)
  stripText.y = element_text(size = 18, face = 'bold', angle = 90)
  stripText.x = element_text(size = 20, face = 'bold', vjust = -0.15)
  ts.pointSize = 3
  sp.pointSize = 3.25

  # Get NMRI and VOD zscores and fitted loess curve values
  nmri.vod = wkg.nmri.vod.day.night.zscores
  nmri.vod.loess = wkg.nmri.vod.day.night.zscores.loess

  # Reshape data frames
  nmri.vod = reshape2::melt(nmri.vod,
                            id.vars = c('Dates','NMRI','Period','MonsoonPhase'),
                            variable.name = 'VOD_Product',
                            value.name = 'VOD')

  nmri.vod.loess = reshape2::melt(nmri.vod.loess,
                                  id.vars = c('Dates','NMRI','Period','MonsoonPhase'),
                                  variable.name = 'VOD_Product',
                                  value.name = 'VOD')

  # Set VOD facet labels
  vod.facet.labs = c(expression(bold(atop('LPDR VOD','X-band'~~'25km'))),
                     expression(bold(atop('LPRM VOD','X-band'~~'10km'))),
                     expression(bold(atop('SMAP-DCA VOD','L-band'~~'09km'))))

  # Factor categories
  nmri.vod$VOD_Product = factor(nmri.vod$VOD_Product,
                                levels = c('LPDR_Xband_25km','LPRM_Xband_10km','SMAP_DCA_Lband_09km'),
                                labels = vod.facet.labs)

  nmri.vod.loess$VOD_Product = factor(nmri.vod.loess$VOD_Product,
                                      levels = c('LPDR_Xband_25km','LPRM_Xband_10km','SMAP_DCA_Lband_09km'),
                                      labels = vod.facet.labs)

  # Subset nighttime NMRI/VOD
  nmri.vod.nighttime = nmri.vod[nmri.vod$Period == 'Nighttime',]
  nmri.vod.loess.nighttime = nmri.vod.loess[nmri.vod.loess$Period == 'Nighttime',]

  # Get data subsets and create linear models for nighttime VOD~NMRI relationships (full, greening, and browning)
  zscores.nighttime = wkg.nmri.vod.day.night.zscores[wkg.nmri.vod.day.night.zscores$Period == 'Nighttime',]
  nmri.full = zscores.nighttime$NMRI
  nmri.greening = zscores.nighttime[zscores.nighttime$MonsoonPhase == 'Greening',][['NMRI']]
  nmri.browning = zscores.nighttime[zscores.nighttime$MonsoonPhase == 'Browning',][['NMRI']]

  lpdr.full = zscores.nighttime$LPDR_Xband_25km
  lpdr.greening = zscores.nighttime[zscores.nighttime$MonsoonPhase == 'Greening',][['LPDR_Xband_25km']]
  lpdr.browning = zscores.nighttime[zscores.nighttime$MonsoonPhase == 'Browning',][['LPDR_Xband_25km']]

  lprm.full = zscores.nighttime$LPRM_Xband_10km
  lprm.greening = zscores.nighttime[zscores.nighttime$MonsoonPhase == 'Greening',][['LPRM_Xband_10km']]
  lprm.browning = zscores.nighttime[zscores.nighttime$MonsoonPhase == 'Browning',][['LPRM_Xband_10km']]

  smap.full = zscores.nighttime$SMAP_DCA_Lband_09km
  smap.greening = zscores.nighttime[zscores.nighttime$MonsoonPhase == 'Greening',][['SMAP_DCA_Lband_09km']]
  smap.browning = zscores.nighttime[zscores.nighttime$MonsoonPhase == 'Browning',][['SMAP_DCA_Lband_09km']]

  nmri.lpdr.full.lm = lm(lpdr.full ~ nmri.full, na.action = na.omit)
  nmri.lpdr.greening.lm = lm(lpdr.greening ~ nmri.greening, na.action = na.omit)
  nmri.lpdr.browning.lm = lm(lpdr.browning ~ nmri.browning, na.action = na.omit)

  nmri.lprm.full.lm = lm(lprm.full ~ nmri.full, na.action = na.omit)
  nmri.lprm.greening.lm = lm(lprm.greening ~ nmri.greening, na.action = na.omit)
  nmri.lprm.browning.lm = lm(lprm.browning ~ nmri.browning, na.action = na.omit)

  nmri.smap.full.lm = lm(smap.full ~ nmri.full, na.action = na.omit)
  nmri.smap.greening.lm = lm(smap.greening ~ nmri.greening, na.action = na.omit)
  nmri.smap.browning.lm = lm(smap.browning ~ nmri.browning, na.action = na.omit)

  # Create R2 labels
  nmri.lpdr.full.r2.lab = data.frame(label = paste('R\u00b2 =', round(summary(nmri.lpdr.full.lm)$r.squared,2)))
  nmri.lpdr.greening.r2.lab = data.frame(label = paste('R\u00b2 =', round(summary(nmri.lpdr.greening.lm)$r.squared,2)))
  nmri.lpdr.browning.r2.lab = data.frame(label = paste('R\u00b2 =', round(summary(nmri.lpdr.browning.lm)$r.squared,2)))

  nmri.lprm.full.r2.lab = data.frame(label = paste('R\u00b2 =', round(summary(nmri.lprm.full.lm)$r.squared,2)))
  nmri.lprm.greening.r2.lab = data.frame(label = paste('R\u00b2 =', round(summary(nmri.lprm.greening.lm)$r.squared,2)))
  nmri.lprm.browning.r2.lab = data.frame(label = paste('R\u00b2 =', round(summary(nmri.lprm.browning.lm)$r.squared,2)))

  nmri.smap.full.r2.lab = data.frame(label = paste('R\u00b2 =', round(summary(nmri.smap.full.lm)$r.squared,2)))
  nmri.smap.greening.r2.lab = data.frame(label = paste('R\u00b2 =', round(summary(nmri.smap.greening.lm)$r.squared,2)))
  nmri.smap.browning.r2.lab = data.frame(label = paste('R\u00b2 =', round(summary(nmri.smap.browning.lm)$r.squared,2)))

  full.r2.labs = rbind(nmri.lpdr.full.r2.lab, nmri.lprm.full.r2.lab, nmri.smap.full.r2.lab)
  full.r2.labs$VOD_Product = unique(nmri.vod.nighttime$VOD_Product)

  greening.r2.labs = rbind(nmri.lpdr.greening.r2.lab, nmri.lprm.greening.r2.lab, nmri.smap.greening.r2.lab)
  greening.r2.labs$VOD_Product = unique(nmri.vod.nighttime$VOD_Product)

  browning.r2.labs = rbind(nmri.lpdr.browning.r2.lab, nmri.lprm.browning.r2.lab, nmri.smap.browning.r2.lab)
  browning.r2.labs$VOD_Product = unique(nmri.vod.nighttime$VOD_Product)

  # Set statistic label parameters for scatterplot
  statcor.y.full = 0.95
  statcor.x.full = 0
  statcor.hjust.full = 0.05
  statcor.vjust.full = 0
  statcor.y.phases = 0.95
  statcor.x.phases = 0
  statcor.hjust.phases = 0.05
  statcor.vjust.phases = c(c(0,1.3),c(0,1.3),c(0,1.35))
  statcor.textSize = 6.25
  yax.lim = c(-3,3)
  xax.lim = c(-3,3)

  # --------------- Timeseries plot
  ts.strip.cols = strip_themed(background_y = elem_list_rect(fill = 'white', color = 'white'))

  ts = ggplot(data = nmri.vod.nighttime,
              aes(x = Dates, color = MonsoonPhase)) +
    # Plot NMRI
    geom_point(aes(y = NMRI,
                   shape = 1),
               size = ts.pointSize, stroke = 1) +
    # geom_smooth(data = nmri.vod.loess.nighttime,
    #             aes(x = Dates, y = NMRI, lty = 'solid'),
    #             col = 'black',
    #             #linetype = 'solid',
    #             se = FALSE,) +
    # Plot VOD
    geom_point(aes(y = VOD,
                   shape = 15),
               size = ts.pointSize, alpha = 0.6) +
    # geom_smooth(data = nmri.vod.loess.nighttime,
    #             aes(x = Dates, y = VOD, lty = 'dashed'),
    #             col = 'black',
    #             #linetype = 'dashed'
    #             se = FALSE) +
    scale_color_manual(values = c('Greening' = 'forestgreen',
                                  'Browning' = 'darkorange4'),
                       labels = c('Greening (06/30 - 08/23)',
                                  'Browning (08/24 - 10/26)')) +
    scale_shape_identity(labels = c('VOD','NMRI'),
                         breaks = c(15,1),
                         guide = 'legend') +
    # scale_linetype_discrete(labels = c('VOD loess','NMRI loess'),
    #                         guide = 'legend') +
    # ggtitle(label = 'Nighttime NMRI and VOD (Z-scores)',
    #         subtitle = 'Nighttime NMRI = mean 06:00 PM - 06:00 AM || Nighttime VOD overpass = 01:00 AM') +
    xlab('') +
    ylab('Z-scores') +
    ylim(c(-2.5,2.5)) +
    geom_hline(yintercept = 0, linewidth = 1.25, alpha = 0.5) +
    theme_bw() +
    facet_grid2(VOD_Product~.,
                labeller = label_parsed,
                strip = ts.strip.cols,
                scales = 'fixed') +
    theme(axis.title.x = element_blank(),
          axis.title.y = axisTitle,
          axis.text = axisText,
          plot.title = element_text(size = 17),
          plot.subtitle = element_text(size = 13),
          legend.position = 'bottom',
          legend.direction = 'vertical',
          legend.title = element_blank(),
          legend.text = element_text(size = 18),
          legend.box.margin = margin(-10,0,0,0),
          #strip.text.x = stripText.x,
          strip.text.y = stripText.y,
          #strip.background.y = element_rect(fill = 'white'),
          strip.placement = 'outside',
          panel.border = element_rect())

  ts

  # --------------- Scatterplot

  sp = ggplot(data = nmri.vod.nighttime,
              aes(x = NMRI,
                  y = VOD,
                  color = MonsoonPhase)) +
    geom_hline(yintercept = 0, linewidth = 1, alpha = 0.15) +
    geom_vline(xintercept = 0, linewidth = 1, alpha = 0.15) +
    geom_point(size = sp.pointSize, shape = 19, alpha = 0.6) +
    scale_color_manual(values = c('forestgreen','darkorange4'),
                       labels = c('Greening (06/30 - 08/23)', 'Browning (08/24 - 10/26)')) +
    geom_abline(intercept = 0, slope = 1, linewidth = 0.5, linetype = 'dashed') +
    # ------ Add regression lines (full set, greening, and drying)
    # Full set
    # geom_smooth(method = 'lm' , se = FALSE , color = 'black',
    #             linewidth = 1.25, na.rm = TRUE, show.legend = FALSE) +
    # Greening/browning phases
    geom_smooth(method = 'lm' , se = FALSE , aes(color = MonsoonPhase),
                linewidth = 1.25, na.rm = TRUE, show.legend = FALSE) +
    # ------ Add correlation stats (full set, greening, and drying)
    # Full set
    # stat_cor(aes(x = NMRI, y = VOD),
    #          inherit.aes = FALSE,
    #          method = 'pearson',
    #          size = statcor.textSize,
    #          na.rm = TRUE, show.legend = FALSE,
    #          label.y.npc = statcor.y.full,
    #          label.x.npc = statcor.x.full,
    #          hjust = statcor.hjust.full,
    #          vjust = statcor.vjust.full,
    # ) +
    # Greening/browning phases
    stat_cor(aes(x = NMRI, y = VOD),
             method = 'pearson',
             size = statcor.textSize,
             na.rm = TRUE, show.legend = FALSE,
             label.y.npc = statcor.y.phases,
             label.x.npc = statcor.x.phases,
             hjust = statcor.hjust.phases,
             vjust = statcor.vjust.phases,
    ) +
    # ------ Add regression stats (full set, greening, and drying)
    # Full set
    # geom_text(data = full.r2.labs, aes(label = label),
    #           inherit.aes = FALSE,
    #           x = 1.5,
    #           hjust = 0,
    #           y = -1,
    #           size = statcor.textSize) +
    # Greening phase
    geom_text(data = greening.r2.labs, aes(label = label),
              inherit.aes = FALSE,
              x = 1.5,
              hjust = 0,
              y = -1.75,
              color = 'forestgreen',
              size = statcor.textSize) +
    # Browning phase
    geom_text(data = browning.r2.labs, aes(label = label),
              inherit.aes = FALSE,
              x = 1.5,
              hjust = 0,
              y = -2.5,
              color = 'darkorange4',
              size = statcor.textSize) +
    ylab('VOD [ ]') +
    xlab('NMRI [ ]') +
    ylim(yax.lim) +
    xlim(xax.lim) +
    # ggtitle(label = plot.title,
    #         subtitle = plot.subtitle) +
    theme_bw() +
    facet_grid2(VOD_Product~.,
                scales = 'free_y',
                labeller = label_parsed,
                switch = 'y') +
    theme(axis.title = axisTitle,
          axis.title.y = axisTitleY,
          axis.text = element_text(size = 15),
          plot.title = element_text(size = 17),
          plot.subtitle = element_text(size = 12),
          legend.position = 'bottom',
          legend.title = element_blank(),
          legend.box.margin = margin(-10,0,0,0),
          legend.text = element_text(size = 15),
          strip.text.y = element_blank(),
          strip.background.y = element_rect(color = 'white'),
          strip.placement = 'outside',
          panel.border = element_rect())

  sp

  # ------ Combine timeseries and scatterplots
  
  ts.sp.combo = plot_grid(plotlist = list(ts + theme(legend.position = 'none'),
                                          sp + theme(legend.position = 'none')),
                          ncol = 2,
                          rel_widths = c(1.5,1),
                          align = 'h',
                          axis = 'l')
  
  ts.sp.combo.legend = get_plot_component(ts + theme(legend.position = 'bottom',
                                                     legend.background = element_rect(fill = 'transparent')),
                                          'guide-box-bottom', return_all = TRUE)
  
  ts.sp.combo = plot_grid(ts.sp.combo, ts.sp.combo.legend, ncol = 1, rel_heights = c(1,0.05))
  ts.sp.combo = ggdraw(ts.sp.combo) +
    draw_label('a)', x = 0.02, y = 0.98, fontface = 'bold', size = 19, hjust = 0) +
    draw_label('c)', x = 0.02, y = 0.68, fontface = 'bold', size = 19, hjust = 0) +
    draw_label('e)', x = 0.02, y = 0.38, fontface = 'bold', size = 19, hjust = 0) +
    draw_label('b)', x = 0.62, y = 0.98, fontface = 'bold', size = 19, hjust = 0) +
    draw_label('d)', x = 0.62, y = 0.68, fontface = 'bold', size = 19, hjust = 0) +
    draw_label('f)', x = 0.62, y = 0.38, fontface = 'bold', size = 19, hjust = 0)
  
  ggsave(paste(figs.fp, 'Fig06_NMRI_LPDR_LPRM_SMAP_VOD_TS_SP.png', sep = '/'),
         ts.sp.combo,
         width = 12, height = 11,
         bg = 'white')
}

nmri.vod.ts.sp.day.night.zscore.fun()

# -----------------------------------------------------------------------------------------
# Fig. 4
# Function for plotting daytime SWC (all three depths) and NMRI z-scores and scatterplots
# -----------------------------------------------------------------------------------------

nmri.swc.zscores.ts.sp = function(n.panels) {
  
  zscore.daily.mean.fun = function(var) {
    if (var == 'NMRI') {
      nmri = as.data.frame(matrix(NA, nrow = length(date.seq), ncol = 2))
      colnames(nmri) = c('day','night')
      nmri$day = nmri.day.zscore$Value
      nmri$night = nmri.night.zscore$Value
      nmri = rowMeans(nmri)
      return(nmri)
    }
    if (var == 'SWC_1') {
      swc.1 = as.data.frame(matrix(NA, nrow = length(date.seq), ncol = 2))
      colnames(swc.1) = c('day','night')
      swc.1$day = swc.1.day.zscore$Value
      swc.1$night = swc.1.night.zscore$Value
      swc.1 = rowMeans(swc.1)
      swc.1 = data.frame('Dates' = date.seq, 'Value' = swc.1)
      return(swc.1)
    }
    if (var == 'SWC_2') {
      swc.2 = as.data.frame(matrix(NA, nrow = length(date.seq), ncol = 2))
      colnames(swc.2) = c('day','night')
      swc.2$day = swc.2.day.zscore$Value
      swc.2$night = swc.2.night.zscore$Value
      swc.2 = rowMeans(swc.2)
      swc.2 = data.frame('Dates' = date.seq, 'Value' = swc.2)
      return(swc.2)
    }
    if (var == 'SWC_3') {
      swc.3 = as.data.frame(matrix(NA, nrow = length(date.seq), ncol = 2))
      colnames(swc.3) = c('day','night')
      swc.3$day = swc.3.day.zscore$Value
      swc.3$night = swc.3.night.zscore$Value
      swc.3 = rowMeans(swc.3)
      swc.3 = data.frame('Dates' = date.seq, 'Value' = swc.3)
      return(swc.3)
    }
  }
  
  nmri = zscore.daily.mean.fun('NMRI')
  swc.1.zscore = zscore.daily.mean.fun('SWC_1')
  swc.2.zscore = zscore.daily.mean.fun('SWC_2')
  swc.3.zscore = zscore.daily.mean.fun('SWC_3')
  
  # nmri = nmri.day.zscore$Value
  # swc.1.zscore = swc.1.day.zscore
  # swc.2.zscore = swc.2.day.zscore
  # swc.3.zscore = swc.3.day.zscore
  
  # NMRI and SWC z-scores
  df = rbind(data.frame('Dates' = date.seq,
                        'Value' = swc.1.zscore$Value,
                        'Var' = 'SWC_1'),
             data.frame('Dates' = date.seq,
                        'Value' = swc.2.zscore$Value,
                        'Var' = 'SWC_2'),
             data.frame('Dates' = date.seq,
                        'Value' = swc.3.zscore$Value,
                        'Var' = 'SWC_3'),
             data.frame('Dates' = date.seq,
                        'Value' = nmri,
                        'Var' = 'NMRI'))
  
  df$Var = factor(df$Var, levels = c('SWC_1','SWC_2','SWC_3','NMRI'))
  
  # Precip data
  precip = fluxnet.wkg[,c(1,2,11)]
  precip = precip %>%
    group_by(Dates) %>%
    summarise(Precip = sum(Precip, na.rm = TRUE)) %>%
    as.data.frame()
     
  precip.sig = precip$Precip
  precip.sig[precip.sig < 5] = NA
  precip$Precip_sig = precip.sig
  
  precip.sig.for.zscore.ts = precip[,c(1,3)]
  precip.sig.for.zscore.ts = na.omit(precip.sig.for.zscore.ts)
  
  precip.nonsig.for.zscore.ts = precip$Precip
  precip.nonsig.for.zscore.ts[precip.nonsig.for.zscore.ts >= 5] = NA
  precip.nonsig.for.zscore.ts[precip.nonsig.for.zscore.ts == 0] = NA
  precip.nonsig.for.zscore.ts = data.frame('Dates' = precip$Dates, 'Precip_nonsig' = precip.nonsig.for.zscore.ts)
  precip.nonsig.for.zscore.ts = na.omit(precip.nonsig.for.zscore.ts)
  
  # Set axes scales
  y.scale = scale_y_continuous(breaks = seq(-2,3,1), 
                               labels = seq(-2,3,1), 
                               limits = c(-2,3))
  
  x.scale = scale_x_continuous(breaks = seq(-2,3,1), 
                               labels = seq(-2,3,1), 
                               limits = c(-2,3))
  
  # Precipitation timeseries
  precip.ts = ggplot(data = precip,
                     aes(x = Dates)) +
    geom_bar(aes(y = Precip, fill = 'lightblue'),
                         stat = 'identity') +
    geom_bar(aes(y = Precip_sig, fill = 'blue'), stat = 'identity') +
    scale_fill_manual(values = c('blue','lightblue'),
                      labels = c('>= 5 mm (significant)', '< 5 mm'),
                      name = 'P') +
    guides(fill = guide_legend(position = 'inside', title = '', ncol = 1)) +
    scale_y_continuous(limits = c(0,42), expand = c(0,0)) +
    scale_x_date(breaks = seq(as.Date('2021-07-01'),
                              as.Date('2021-10-01'),
                              by = 'month'),
                 date_labels = '%b-%Y') +
    xlab('') +
    ylab(expression(Precip.~'['~mm~d^-1~']')) +
    theme_bw() +
    theme(axis.text = element_text(size = 15),
          axis.title.y = element_text(size = 16),
          legend.title = element_blank(),
          legend.box.background = element_rect(fill = 'transparent', color = 'transparent'),
          legend.key = element_rect(color = 'transparent', fill = 'transparent'),
          legend.text = element_text(size = 13, face = 'bold'),
          legend.position.inside = c(0.85,0.82),
          panel.border = element_rect(color = 'black', 
                                      fill = NA, 
                                      linewidth = 1))
  
  # NMRI and SWC z-score timeseries
  ts = ggplot() +
    geom_hline(yintercept = 0, linewidth = 1.25, alpha = 0.4) +
    # geom_vline(data = precip.sig.for.zscore.ts,
    #            aes(xintercept = Dates),
    #            linewidth = 1, col = 'blue', alpha = 0.4) +
    # geom_vline(data = precip.nonsig.for.zscore.ts,
    #            aes(xintercept = Dates),
    #            linetype = 1,
    #            linewidth = 1, col = 'lightblue') +
    geom_vline(xintercept = as.Date('2021-08-23'), linewidth = 1.25, alpha = 0.5) +
    geom_text(data = data.frame('x' = as.Date('2021-08-23'),
                                'y' = -1.5,
                                'lab' = 'Date of peak\n greenness\n (AUG 23)'),
              aes(x = x, y = y, label = lab),
              size = 6,
              fontface = 'bold') +
    geom_text(data = data.frame('x' = as.Date('2021-07-25'),
                                'y' = -2,
                                'lab' = 'Greening'),
              aes(x = x, y = y, label = lab),
              size = 7,
              color = 'forestgreen',
              fontface = 'bold') +
    geom_text(data = data.frame('x' = as.Date('2021-10-01'),
                                'y' = -2,
                                'lab' = 'Browning'),
              aes(x = x, y = y, label = lab),
              size = 7,
              color = 'darkorange4',
              fontface = 'bold') +
    geom_line(data = df[df$Var %like% c('SWC_1|SWC_2|SWC_3'),],
              aes(x = Dates, y = Value, color = Var),
              linewidth = 1.25) +
    # geom_point(data = df[df$Var %like% c('SWC_1|SWC_2|SWC_3'),],
    #            aes(x = Dates, y = Value, color = Var),
    #            shape = 19, size = 2.25) +
    geom_line(data = df[df$Var == 'NMRI',],
              aes(x = Dates, y = Value, color = Var),
              linewidth = 1.25) +
    geom_point(data = df[df$Var == 'NMRI',],
               aes(x = Dates, y = Value, color = Var),
               shape = 15, size = 2.25) +
    scale_color_manual(values = c('SWC_1' = 'lightblue3',
                                  'SWC_2' = 'steelblue1',
                                  'SWC_3' = 'mediumblue',
                                  'NMRI' = 'black'),
                       labels = c('SWC (-5 cm) [%]',
                                  'SWC (-15 cm) [%]',
                                  'SWC (-30 cm) [%]',
                                  'NMRI [ ]'),
                       name = '',
                       guide = 'legend') +
    scale_y_continuous(breaks = seq(-2,3,1), 
                       labels = seq(-2,3,1), 
                       limits = c(-2,3)) +
    scale_x_date(breaks = seq(as.Date('2021-07-01'),
                              as.Date('2021-10-01'),
                              by = 'month'),
                 date_labels = '%b-%Y') +
    xlab('') +
    ylab('Z-score') +
    guides(color = guide_legend(override.aes = list(linewidth = 3))) +
    theme_bw() +
    theme(axis.text.y = element_text(size = 16),
          axis.text.x = element_text(size = 16),
          axis.title.y = element_text(size = 17),
          legend.text = element_text(size = 18),
          legend.position = 'top',
          legend.direction = 'horizontal',
          legend.background = element_rect(fill = 'transparent'),
          legend.box.margin =  margin(0,0,-15,0),
          panel.border = element_rect(color = 'black', 
                                      fill = NA, 
                                      linewidth = 1))
  
  # Scatterplots
  # swc.1.zscore.pfilt = zscore.fun(precip.filter.fun(wkg.swc.day.night[wkg.swc.day.night$Period == 'Nighttime',],'SWC'), 'SWC_1_mean_PeriodMean')
  # swc.2.zscore.pfilt = zscore.fun(precip.filter.fun(wkg.swc.day.night[wkg.swc.day.night$Period == 'Nighttime',],'SWC'), 'SWC_2_mean_PeriodMean')
  # swc.3.zscore.pfilt = zscore.fun(precip.filter.fun(wkg.swc.day.night[wkg.swc.day.night$Period == 'Nighttime',],'SWC'), 'SWC_3_mean_PeriodMean')
  
  swc.1.zscore.pfilt = precip.filter.fun(swc.1.zscore, 'Value')
  swc.2.zscore.pfilt = precip.filter.fun(swc.2.zscore, 'Value')
  swc.3.zscore.pfilt = precip.filter.fun(swc.3.zscore, 'Value')
  
  # sp.df = rbind(data.frame('Dates' = date.seq[2:length(date.seq)],
  #                          'NMRI' = nmri[2:length(date.seq)],
  #                          'SWC_Value' = swc.1.day.zscore.pfilt[1:(length(date.seq)-1),]$Value,
  #                          'SWC_Var' = 'SWC_1',
  #                          'XOS' = NA),
  #               data.frame('Dates' = date.seq[2:length(date.seq)],
  #                          'NMRI' = nmri[2:length(date.seq)],
  #                          'SWC_Value' = swc.2.day.zscore.pfilt[1:(length(date.seq)-1),]$Value,
  #                          'SWC_Var' = 'SWC_2',
  #                          'XOS' = NA),
  #               data.frame('Dates' = date.seq[2:length(date.seq)],
  #                          'NMRI' = nmri[2:length(date.seq)],
  #                          'SWC_Value' = swc.3.day.zscore.pfilt[1:(length(date.seq)-1),]$Value,
  #                          'SWC_Var' = 'SWC_3',
  #                          'XOS' = NA))
  
  sp.df = rbind(data.frame('Dates' = date.seq,
                           'NMRI' = nmri,
                           'SWC_Value' = swc.1.zscore.pfilt$Value,
                           'SWC_Var' = 'SWC_1',
                           'XOS' = NA),
                data.frame('Dates' = date.seq,
                           'NMRI' = nmri,
                           'SWC_Value' = swc.2.zscore.pfilt$Value,
                           'SWC_Var' = 'SWC_2',
                           'XOS' = NA),
                data.frame('Dates' = date.seq,
                           'NMRI' = nmri,
                           'SWC_Value' = swc.3.zscore.pfilt$Value,
                           'SWC_Var' = 'SWC_3',
                           'XOS' = NA))
    
  sp.df[sp.df$Dates %in% greening.dates,]$XOS = 'Greening'
  sp.df[sp.df$Dates %in% browning.dates,]$XOS = 'Browning'
  
  sp.df$XOS = factor(sp.df$XOS, levels = c('Greening','Browning'))
  xos.labs = c('Greening (06/30 - 08/23)','Browning (08/24 - 10/26)')
  
  
  sp = ggplot(data = sp.df, aes(x = NMRI, y = SWC_Value, group = XOS)) +
    geom_hline(yintercept = 0, linewidth = 0.5, linetype = 'dashed', alpha = 0.5) +
    geom_vline(xintercept = 0, linewidth = 0.5, linetype = 'dashed', alpha = 0.5) +
    geom_point(aes(col = XOS), pch = 19, size = 2) +
    scale_color_manual(values = c('forestgreen','darkorange4'),
                       labels = xos.labs) +
    geom_smooth(data = filter(sp.df, XOS == 'Greening'),
                method = lm, color = 'forestgreen', fill = 'forestgreen', na.rm = TRUE, se = TRUE, alpha = 0.25) +
    geom_smooth(data = filter(sp.df, XOS == 'Browning'),
                method = lm, color = 'darkorange4', fill = 'darkorange4', na.rm = TRUE, se = TRUE, alpha = 0.25) +
    stat_poly_eq(data = filter(sp.df, XOS == 'Greening'),
                 mapping = use_label(c('R2','P','eq')), na.rm = TRUE, size = 5,
                 label.y = c(0.98), col = 'forestgreen') +
    stat_poly_eq(data = filter(sp.df, XOS == 'Browning'),
                 mapping = use_label(c('R2','P','eq')), na.rm = TRUE, size = 5,
                 label.y = c(0.9), col = 'darkorange4') +
    x.scale +
    y.scale +
    xlab('NMRI Z-score [ ]') +
    ylab('SWC Z-score [ % ]') +
    facet_wrap(SWC_Var~., ncol = 3, labeller = as_labeller(c('SWC_1' = 'SWC depth: -5 cm',
                                                             'SWC_2' = 'SWC depth: -15 cm',
                                                             'SWC_3' = 'SWC depth: -30 cm'))) +
    guides(color = guide_legend(override.aes = list(size = 4))) +
    theme_bw() +
    theme(axis.text.y = element_text(size = 15),
          axis.text.x = element_text(size = 15),
          axis.title = element_text(size = 16),
          legend.title = element_blank(),
          legend.position = 'bottom',
          legend.margin = margin(-5,0,0,0,unit = 'pt'),
          legend.text = element_text(size = 17),
          strip.text = element_text(size = 16, color = 'black', face = 'bold'),
          strip.background = element_rect(fill = 'white', color = 'black'))
  
  # --- Combine plots
  # If combining precip/z-score timeseries and scatterplots
  
  if (n.panels == 3) {
    top.panel = plot_grid(plotlist = list(precip.ts, ts),
                          ncol = 1,
                          rel_heights = c(0.7,1),
                          align = 'v',
                          axis = 'lt',
                          labels = c('a)','b)'),
                          label_size = 16)
    
    combo.plot = plot_grid(plotlist = list(top.panel, sp),
                           ncol = 1,
                           rel_heights = c(1,0.6),
                           #align = 'v',
                           axis = 'l',
                           labels = c('','c)'),
                           label_size = 16)
    
    ggsave(paste(figs.fp, 'Fig04_NMRI_SWC_depths_Precip_zscore_TS_SP.png', sep = '/'),
           combo.plot,
           width = 13, height = 12,
           bg = 'white')
  }
  
  # If combing only z-score timeseries and scatterplots
  if (n.panels == 2) {
    combo.plot = plot_grid(plotlist = list(ts, sp),
                           ncol = 1,
                           rel_heights = c(1,0.8),
                           align = 'v',
                           axis = 'lt',
                           labels = c('a)','b)'),
                           label_size = 16)
    
    ggsave(paste(figs.fp, 'Fig04_NMRI_SWC_depths_zscore_TS_SP.png', sep = '/'),
           combo.plot,
           width = 13, height = 11,
           bg = 'white')
  }
}

nmri.swc.zscores.ts.sp(n.panels = 2)

# --------------------------------------------------------------------------------------------
# Fig. S1d
# Function to plot timeseries of GCC (Grass ROI) contrasting 2020 and 2021 seasons
# --------------------------------------------------------------------------------------------

wkg.gcc.2020.2021.ts.fun = function() {
  
  date.seq.2020.2021 = seq(as.Date('2020-06-30'),as.Date('2021-10-26'),1)
  
  wkg.gr.gcc = data.frame('Dates' = phenocam.gr.wkg$date,
                          'GCCmean' = phenocam.gr.wkg$gcc_mean,
                          'GCC90' = phenocam.gr.wkg$gcc_90,
                          'VegCover' = 'Grass')
  
  wkg.gr.gcc.full = wkg.gr.gcc
  wkg.gr.gcc.2020.2021 = wkg.gr.gcc[wkg.gr.gcc$Dates %in% date.seq.2020.2021,]
  wkg.gr.gcc = wkg.gr.gcc[wkg.gr.gcc$Dates %in% date.seq,]
  
  date.seq.2020 = seq(as.Date('2020-06-30'),as.Date('2020-10-26'),1)
  date.seq.2021 = date.seq
  
  wkg.gr.gcc.2020 = wkg.gr.gcc.full[wkg.gr.gcc.full$Dates %in% date.seq.2020,]
  wkg.gr.gcc.2021 = wkg.gr.gcc
  
  date.seq.month.day = format(date.seq, '%m-%d')
  date.seq.month = seq(date.seq[1], last(date.seq), by = '1 month')
  date.seq.month.numeric = lubridate::yday(date.seq.month) / 365 * 52 + 1
  date.seq.month.label = lubridate::month(date.seq.month, label = TRUE)
  
  # --- Combine data frames
  # GCC 90
  wkg.gcc.2020.2021 = rbind(data.frame('Dates' = date.seq.2020,
                                       'DatesIndex' = seq(1,119,1),
                                       'DatesMD' = date.seq.month.day,
                                       'DOY' = yday(date.seq.2020),
                                       'Year' = '2020',
                                       'Value' = wkg.gr.gcc.2020$GCC90,
                                       'Dataset' = 'GCC90'),
                            data.frame('Dates' = date.seq.2021,
                                       'DatesIndex' = seq(1,119,1),
                                       'DatesMD' = date.seq.month.day,
                                       'DOY' = yday(date.seq.2021),
                                       'Year' = '2021',
                                       'Value' = wkg.gr.gcc.2021$GCC90,
                                       'Dataset' = 'GCC90')) %>%
    group_by(DatesMD) %>%
    mutate(Dates = first(Dates))
  
  # --- Plot timeseries 
  # GCC 90
  wkg.gcc.2020.2021.ts = ggplot(data = wkg.gcc.2020.2021,
                                aes(x = Dates, 
                                    y = Value, 
                                    color = factor(Year),
                                    group = factor(Year))) +
    geom_line(linewidth = 2) +
    scale_color_manual(values = c('2020' = 'orange3',
                                  '2021' = 'limegreen')) +
    scale_x_date(date_breaks = '1 month', date_labels = '%b') +
    ylim(c(0.34,0.41)) +
    annotate('text', x = as.Date('2020-06-30'), y = 0.346, label = 'a)', fontface = 2, size = 5) +
    annotate('text', x = as.Date('2020-08-23'), y = 0.407, label = 'b)', fontface = 2, size = 5) +
    annotate('text', x = as.Date('2020-10-26'), y = 0.351, label = 'c)', fontface = 2, size = 5) +
    ggtitle('90th Percentile GCC [ ] summer 2020 vs. summer 2021') +
    guides(color = guide_legend(position = 'inside', title = '', ncol = 1)) +
    theme_bw() +
    theme(axis.text = element_text(size = 15),
          axis.title = element_blank(),
          plot.title = element_text(size = 17),
          legend.position.inside = c(0.9,0.8),
          legend.title = element_blank(),
          legend.text = element_text(size = 17))
  
  wkg.gcc.2020.2021.ts
  
  # Add figure label and save plot
  wkg.gcc.2020.2021.ts = plot_grid(plotlist = list(wkg.gcc.2020.2021.ts),
                                   ncol = 1,
                                   align = 'v',
                                   axis = 't',
                                   labels = 'd)',
                                   label_size = 15)
  
  ggsave(paste(figs.fp, 'FigS1d_WKG_GCC_2020_2021_GCC90.png', sep = '/'),
         wkg.gcc.2020.2021.ts,
         width = 8, height = 3)
}

wkg.gcc.2020.2021.ts.fun()

# --------------------------------------------------------------------------------------------
# Fig. S2
# Function to plot timeseries of daily mean NMRI and 90th pct. GCC (Grass and Shrub ROIs)
# --------------------------------------------------------------------------------------------
wkg.nmri.gr.sh.gcc.2021.ts.fun = function() {
  
  # Subset GCC
  gcc.gr.21 = phenocam.gr.wkg[phenocam.gr.wkg$date %in% date.seq,]$gcc_90
  gcc.sh.21 = phenocam.sh.wkg[phenocam.sh.wkg$date %in% date.seq,]$gcc_90
  
  # Plot
  par(mar = c(2.5,4.5,5,5))
  plot(#x = wkg.nmri.smoothed$Dates, y = wkg.nmri.smoothed$Value_smoothed,
       x = wkg.nmri.daily.mean$Dates, y = wkg.nmri.daily.mean$Value,
       pch = 19, cex = 1.25,
       main = 'Walnut Gulch Experimental Watershed - Kendall Grasslands\n2021 Study Period',
       xlab = '', ylab = 'NMRI [ ]',
       cex.main = 2, cex.axis = 1.4, cex.lab = 1.5)
  lines(x = wkg.nmri.daily.mean$Dates, y = wkg.nmri.daily.mean$Value, lwd = 3)
  par(new = TRUE)
  plot(x = date.seq, y = gcc.gr.21,
       ylim = c(min(c(gcc.gr.21, gcc.sh.21)), max(c(gcc.gr.21, gcc.sh.21))),
       axes = FALSE, xlab = '', ylab = '',
       col = 'springgreen', type = 'l', lwd = 4)
  axis(side = 4, cex.axis = 1.4,
       at = pretty(range(c(gcc.gr.21, gcc.sh.21)), n = 6),
       labels = pretty(range(c(gcc.gr.21, gcc.sh.21)), n = 6))
  mtext('GCC (90th percentile) [ ]', side = 4, cex = 1.5, line = 3.25)
  par(new = TRUE)
  plot(x = date.seq, y = gcc.sh.21,
       ylim = c(min(c(gcc.gr.21, gcc.sh.21)), max(c(gcc.gr.21, gcc.sh.21))),
       axes = FALSE, xlab = '', ylab = '',
       col = 'forestgreen', type = 'l', lwd = 4)
  legend('topright',
         legend = c('NMRI','GCC: Grass ROI','GCC: Shrub ROI'),
         col = c('black','springgreen','forestgreen'),
         lwd = 3,
         pch = c(19,NA,NA),
         cex = 1.5)
  box()
  
  ts.plot = as_grob(recordPlot())
  
  ggsave(filename = paste(figs.fp, 'FigS2_NMRI_GCC_Grass_Shrub_TS.png', sep = '/'),
         ts.plot,
         width = 12, height = 7, bg = 'white')
}

wkg.nmri.gr.sh.gcc.2021.ts.fun()

