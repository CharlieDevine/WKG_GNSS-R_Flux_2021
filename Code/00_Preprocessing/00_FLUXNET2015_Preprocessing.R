library(dplyr)

setwd('../../')
git.fp = getwd()
git.data.fp = paste(git.fp, 'Data', sep = '/')
setwd('../../')
raw.data.fp = paste(getwd(), 'Data', sep = '/')
fluxnet.fp = paste(raw.data.fp, 'FLUXNET2015', sep = '/')

# Set date sequence corresponding to NMRI collection
date.seq = seq(as.Date('2021-06-30'),as.Date('2021-10-26'), 1)

# Set WKG FLUXNET2015 filepath
fluxnet.file = list.files(fluxnet.fp, pattern = '.csv', full.names = TRUE)

# Read half-hourly FLUXNET2015 file
fluxnet.wkg = read.csv(fluxnet.file, header = TRUE)

# Extract desired variables
wkg.fluxnet.datetimes = as.POSIXct(as.character(fluxnet.wkg$TIMESTAMP_START), format = '%Y%m%d%H')
wkg.fluxnet.dates = as.Date(substr(wkg.fluxnet.datetimes, start = 1, stop = 10))
wkg.fluxnet.local.hour = as.numeric(substr(wkg.fluxnet.datetimes, start = 12, stop = 13))
wkg.gpp.mean.day = fluxnet.wkg$GPP_DT_VUT_MEAN # gC m-2 d-1
wkg.gpp.mean.night = fluxnet.wkg$GPP_NT_VUT_MEAN
wkg.nee = fluxnet.wkg$NEE_VUT_MEAN
wkg.reco.mean.day = fluxnet.wkg$RECO_DT_VUT_MEAN
wkg.reco.mean.night = fluxnet.wkg$RECO_NT_VUT_MEAN
wkg.vpd = fluxnet.wkg$VPD_F_MDS # hPa
wkg.le = fluxnet.wkg$LE_F_MDS # W m-2
wkg.h = fluxnet.wkg$H_F_MDS # W m-2
wkg.g = fluxnet.wkg$G_F_MDS # W m-2
wkg.netrad = fluxnet.wkg$NETRAD # W m-2
wkg.precip = fluxnet.wkg$P_F # mm d-1
wkg.ta = fluxnet.wkg$TA_F_MDS
wkg.ts.1 = fluxnet.wkg$TS_F_MDS_1 # deg C
wkg.ts.2 = fluxnet.wkg$TS_F_MDS_2
wkg.ts.3 = fluxnet.wkg$TS_F_MDS_3
wkg.swc.1 = fluxnet.wkg$SWC_F_MDS_1 # %
wkg.swc.1[wkg.swc.1 == -9999] = NA
wkg.swc.2 = fluxnet.wkg$SWC_F_MDS_2 # %
wkg.swc.2[wkg.swc.2 == -9999] = NA
wkg.swc.3 = fluxnet.wkg$SWC_F_MDS_3 # %
wkg.swc.3[wkg.swc.3 == -9999] = NA
wkg.swc = matrix(data = NA, nrow = 3, ncol = length(wkg.fluxnet.dates))
wkg.swc[1,] = wkg.swc.1
wkg.swc[2,] = wkg.swc.2
wkg.swc[3,] = wkg.swc.3
wkg.swc.mean = colMeans(wkg.swc, na.rm = TRUE)
wkg.swc.mean[is.nan(wkg.swc.mean)] = NA

# Combine within new data frame
wkg.flux.vars = data.frame('Dates' = wkg.fluxnet.dates,
                           'DateTime' = wkg.fluxnet.datetimes,
                           'LocalHour' = wkg.fluxnet.local.hour,
                           'GPP_day' = wkg.gpp.mean.day,
                           'NEE' = wkg.nee,
                           'VPD' = wkg.vpd,
                           'LE' = wkg.le,
                           'H' = wkg.h,
                           'G' = wkg.g,
                           'NetRad' = wkg.netrad,
                           'Precip' = wkg.precip,
                           'AirTemp' = wkg.ta,
                           'SWC_1' = wkg.swc.1,
                           'SWC_2' = wkg.swc.2,
                           'SWC_3' = wkg.swc.3,
                           'SWC' = wkg.swc.mean)

# Convert half-hourly to hourly mean (!Precip) and sum (Precip) values
wkg.flux.vars = wkg.flux.vars %>% 
  group_by(DateTime) %>%
  summarize(Dates = first(Dates),
            LocalHour = first(LocalHour),
            GPP_day = mean(GPP_day, na.rm = TRUE),
            NEE = mean(NEE, na.rm = TRUE),
            VPD = mean(VPD, na.rm = TRUE),
            LE = mean(LE, na.rm = TRUE),
            H = mean(H, na.rm = TRUE),
            G = mean(G, na.rm = TRUE),
            NetRad = mean(NetRad, na.rm = TRUE),
            Precip = sum(Precip, na.rm = TRUE),
            AirTemp = mean(AirTemp, na.rm = TRUE),
            SWC_1 = mean(SWC_1, na.rm = TRUE),
            SWC_2 = mean(SWC_2, na.rm = TRUE),
            SWC_3 = mean(SWC_3, na.rm = TRUE),
            SWC = mean(SWC, na.rm = TRUE)) %>%
  as.data.frame()

# Compute evapotranspiration (ET) 
# https://earthscience.stackexchange.com/questions/20733/fluxnet15-how-to-convert-latent-heat-flux-to-actual-evapotranspiration
le = wkg.flux.vars$LE * 0.036              # W m-2 hr-1 ---> MJ m-2 hr-1
ta = wkg.flux.vars$AirTemp                 # deg C (hourly)
lh.vap = 2.501 - (2.361 * (10^-3)) * ta   # MJ kg-1
et = le / lh.vap                          # kg m2-1 t-1 equal to mm t-1

# Add ET to data frame
wkg.flux.vars$ET = et

# Compute absolute energy balance residual (REB inst.)
wkg.flux.vars$REBinst = wkg.flux.vars$NetRad - wkg.flux.vars$G - wkg.flux.vars$LE - wkg.flux.vars$H

# Subset to timeframe set above
wkg.flux.vars = wkg.flux.vars[wkg.flux.vars$Dates %in% date.seq,]

# Reformat Dates and DateTime fields
wkg.flux.vars$DateTime = format(wkg.flux.vars$DateTime, '%Y-%m-%d %H:00:00')
wkg.flux.vars$Dates = format(wkg.flux.vars$Dates, '%Y-%m-%d')

# Write to GitHub data directory
write.csv(wkg.flux.vars,
          file = paste(git.data.fp, 'FLUXNET2015', 'WKG_FLUXNET2015_Hourly_Summer2021_Subset.csv', sep = '/'),
          row.names = FALSE)
