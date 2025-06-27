library(R.matlab)
library(ncdf4)
library(raster)
library(xts)
library(dplyr)

# --------------------------------------------------------------
# Reference files for pixel centerpoints (9, 10, 25, and 36 km)
# --------------------------------------------------------------

setwd('../../')
git.fp = getwd()
gitdata.fp = paste(git.fp, 'Data', sep = '/')
coords.fp = paste(gitdata.fp, 'GridCenterCoordinates', sep = '/')
setwd('../../')
data.fp = paste(getwd(), 'Data', sep = '/')

# Get site geometry shapefiles
shp.files = list.files(paste(gitdata.fp, 'Shapefiles', 'Site_Geometries', sep = '/'), pattern = '.shp', full.names = TRUE)

lpdr.pix.areas = shapefile(shp.files[1])
lprm.pix.areas = shapefile(shp.files[2])
mtdca.pix.areas = shapefile(shp.files[3])

# Set EASE2 grid CRS variable and reproject pixel area shapefiles
ease2.crs = '+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0, 0 +units=m +no_defs'

wkg.lpdr.pix.ease2 = spTransform(lpdr.pix.areas[lpdr.pix.areas$SITE_ID == 'WKG',], ease2.crs)
wkg.lprm.pix.ease2 = spTransform(lprm.pix.areas[lprm.pix.areas$SITE_ID == 'WKG',], ease2.crs)
wkg.mtdca.pix.ease2 = spTransform(mtdca.pix.areas[mtdca.pix.areas$SITE_ID == 'WKG',], ease2.crs)

coords.36km = readMat(paste(coords.fp, 'SMAPCenterCoordinates36km_v6.mat', sep = '/'))
coords.25km = nc_open(paste(coords.fp, 'NSIDC0772_LatLon_EASE2_M25km_v1.0.nc', sep = '/'))
coords.09km = readMat(paste(coords.fp, 'SMAPCenterCoordinates9km_v6.mat', sep = '/'))

lat.36km = coords.36km$SMAPCenterLatitudes[,1]
lon.36km = coords.36km$SMAPCenterLongitudes[1,]

lat.25km = ncvar_get(coords.25km, 'latitude')[1,]
lon.25km = ncvar_get(coords.25km, 'longitude')[,1]

lat.10km = read.csv(paste(coords.fp, 'LPRM_LAT_GridCenterPoints_10km.csv', sep = '/'), header = TRUE)[,1]
lon.10km = read.csv(paste(coords.fp, 'LPRM_LON_GridCenterPoints_10km.csv', sep = '/'), header = TRUE)[,1]

lat.09km = coords.09km$SMAPCenterLatitudes[,1]
lon.09km = coords.09km$SMAPCenterLongitudes[1,]

# Create function to find the row/column indices of the closest matching lat/lon coordinates for each resolution
timeseries.subset = function(lat.in, lon.in, spatial.res){
  
  if (spatial.res == '09km') {
    lat.index = which(abs(lat.09km - lat.in) == min(abs(lat.09km - lat.in)))
    lon.index = which(abs(lon.09km - lon.in) == min(abs(lon.09km - lon.in)))
  }
  
  if (spatial.res == '10km') {
    lat.index = which(abs(lat.10km - lat.in) == min(abs(lat.10km - lat.in)))
    lon.index = which(abs(lon.10km - lon.in) == min(abs(lon.10km - lon.in)))
  }
  
  if (spatial.res == '25km') {
    lat.index = which(abs(lat.25km - lat.in) == min(abs(lat.25km - lat.in)))
    lon.index = which(abs(lon.25km - lon.in) == min(abs(lon.25km - lon.in)))
  }
  
  if (spatial.res == '36km') {
    lat.index = which(abs(lat.36km - lat.in) == min(abs(lat.36km - lat.in)))
    lon.index = which(abs(lon.36km - lon.in) == min(abs(lon.36km - lon.in)))
  }
  
  return(c(lat.index, lon.index))
}

# Walnut Gulch - Kendal Grassland (Tombstone, AZ) [Ameriflux/USDA ARS] // grass/shrub
wkg.lat = 31.736627
wkg.lon = -109.942636

wkg.09km.indices = timeseries.subset(wkg.lat, wkg.lon, '09km')
wkg.10km.indices = timeseries.subset(wkg.lat, wkg.lon, '10km')
wkg.25km.indices = timeseries.subset(wkg.lat, wkg.lon, '25km')
wkg.36km.indices = timeseries.subset(wkg.lat, wkg.lon, '36km')

# ---------------------------------------------------------------------------
# Create function to extract single-pixel subsets from VOD and SM stacks
# ---------------------------------------------------------------------------

extract.timeseries = function(data.list, indices) {
  data.out = vector(mode = 'numeric', length = length(data.list))
  for (i in 1 : length(data.list)) {
    data.ras = raster(data.list[i])
    data.array = as.array(data.ras)
    data.out[i] = data.array[indices[1],indices[2],]
    pct = round((i / length(data.list) * 100),2)
    print(paste0(pct, '% complete'))
  }
  return(data.out)
}

# ----------------------------------------
# MTDCA VOD and Soil Moisture (SM) data
# ----------------------------------------

mtdca.fp = 'U:/charlie/NASA_Drylands/Data/MTDCA/GeoTIFF/Global/EASE2'
# ---- 36km
vod.36km.files = list.files(paste(mtdca.fp, '36km', sep = '/'), pattern = 'VOD', full.names = TRUE, recursive = FALSE)
sm.36km.files = list.files(paste(mtdca.fp, '36km', sep = '/'), pattern = 'SM', full.names = TRUE, recursive = FALSE)
# ---- 09km
vod.09km.files = list.files(paste(mtdca.fp, '09km', sep = '/'), pattern = 'VOD', full.names = TRUE, recursive = FALSE)
sm.09km.files = list.files(paste(mtdca.fp, '09km', sep = '/'), pattern = 'SM', full.names = TRUE, recursive = FALSE)
vod.09km.dates = as.Date(gsub('_', '-', stringi::stri_sub(vod.09km.files, -14, -5)))
sm.09km.dates = as.Date(gsub('_', '-', stringi::stri_sub(sm.09km.files, -14, -5)))

vod.wkg.09km = extract.timeseries(vod.09km.files, wkg.09km.indices)
sm.wkg.09km = extract.timeseries(sm.09km.files, wkg.09km.indices)

vod.sm.wkg.09km.df = rbind(data.frame('Dates' = vod.09km.dates,
                                     'Year' = substr(vod.09km.dates,1,4),
                                     'Value' = vod.wkg.09km,
                                     'Dataset' = 'VOD',
                                     'Site' = 'WKG'),
                           data.frame('Dates' = sm.09km.dates,
                                      'Year' = substr(sm.09km.dates,1,4),
                                      'Value' = sm.wkg.09km,
                                      'Dataset' = 'SM',
                                      'Site' = 'WKG'))

write.csv(vod.sm.wkg.09km.df,
          file = paste0(git.fp, '/Data/Timeseries_CSVs/MTDCA/', 'KG_VOD_SM_Daily_09km.csv'),
          row.names = FALSE)

# ----------------------------------------
# LPDR VOD (ascending and descending)
# ----------------------------------------

lpdr.25km.fp = paste(data.fp, 'LPDR_v3', 'VOD', sep = '/')

lpdr.25km.asc.files = list.files(lpdr.25km.fp, pattern = 'A.tif', recursive = TRUE, full.names = TRUE)

lpdr.25km.asc.dates = as.Date(substr(list.files(lpdr.25km.fp, pattern = 'A.tif', recursive = TRUE, full.names = FALSE),
                                     start = 6,
                                     stop = 13), 
                              format = '%Y%m%d')

lpdr.25km.desc.files = list.files(lpdr.25km.fp, pattern = 'D.tif', recursive = TRUE, full.names = TRUE)

lpdr.25km.desc.dates = as.Date(substr(list.files(lpdr.25km.fp, pattern = 'D.tif', recursive = TRUE, full.names = FALSE),
                                      start = 6,
                                      stop = 13), 
                               format = '%Y%m%d')

lpdr.wkg.25km.asc = extract.timeseries(lpdr.25km.asc.files, wkg.25km.indices)
lpdr.wkg.25km.desc = extract.timeseries(lpdr.25km.desc.files, wkg.25km.indices)

lpdr.wkg.asc.df = data.frame('Dates' = lpdr.25km.asc.dates,
                             'Year' = substr(lpdr.25km.asc.dates,1,4),
                             'Value' = lpdr.wkg.25km.asc,
                             'Dataset' = 'LPDRv3_VOD_ASCENDING_DAY',
                             'Site' = 'WKG')

lpdr.wkg.desc.df = data.frame('Dates' = lpdr.25km.desc.dates,
                              'Year' = substr(lpdr.25km.desc.dates,1,4),
                              'Value' = lpdr.wkg.25km.desc,
                              'Dataset' = 'LPDRv3_VOD_DESCENDING_NIGHT',
                              'Site' = 'WKG')

lpdr.vod.wkg = rbind(lpdr.wkg.asc.df, lpdr.wkg.desc.df)

write.csv(lpdr.vod.wkg,
          file = paste0(git.fp, '/Data/Timeseries_CSVs/LPDRv3/', 'WKG_LPDRv3_VOD_Daily.csv'),
          row.names = FALSE)

# ----------------------------------------
# LPRM VOD
# ----------------------------------------

lprm.fp = paste(data.fp, 'LPRM', 'GeoTIFF', 'WGS84', sep = '/')

# ------- Ascending files
lprm.c1.asc.files = list.files(lprm.fp, pattern = 'C1_VOD_A', recursive = TRUE, full.names = TRUE)
lprm.c2.asc.files = list.files(lprm.fp, pattern = 'C2_VOD_A', recursive = TRUE, full.names = TRUE)
lprm.x.asc.files = list.files(lprm.fp, pattern = 'X_VOD_A', recursive = TRUE, full.names = TRUE)

lprm.c1.asc.dates = as.Date(substr(list.files(lprm.fp, pattern = 'C1_VOD_A', recursive = TRUE, full.names = FALSE),
                                   start = 12, stop = 21),
                            format = '%Y_%m_%d')

lprm.c2.asc.dates = as.Date(substr(list.files(lprm.fp, pattern = 'C2_VOD_A', recursive = TRUE, full.names = FALSE),
                               start = 12, stop = 21),
                            format = '%Y_%m_%d')

lprm.x.asc.dates = as.Date(substr(list.files(lprm.fp, pattern = 'X_VOD_A', recursive = TRUE, full.names = FALSE),
                              start = 12, stop = 21),
                           format = '%Y_%m_%d')

# ------- Descending files
lprm.c1.desc.files = list.files(lprm.fp, pattern = 'C1_VOD_D', recursive = TRUE, full.names = TRUE)
lprm.c2.desc.files = list.files(lprm.fp, pattern = 'C2_VOD_D', recursive = TRUE, full.names = TRUE)
lprm.x.desc.files = list.files(lprm.fp, pattern = 'X_VOD_D', recursive = TRUE, full.names = TRUE)

lprm.c1.desc.dates = as.Date(substr(list.files(lprm.fp, pattern = 'C1_VOD_D', recursive = TRUE, full.names = FALSE),
                                   start = 12, stop = 21),
                             format = '%Y_%m_%d')

lprm.c2.desc.dates = as.Date(substr(list.files(lprm.fp, pattern = 'C2_VOD_D', recursive = TRUE, full.names = FALSE),
                                   start = 12, stop = 21),
                             format = '%Y_%m_%d')

lprm.x.desc.dates = as.Date(substr(list.files(lprm.fp, pattern = 'X_VOD_D', recursive = TRUE, full.names = FALSE),
                                  start = 12, stop = 21),
                            format = '%Y_%m_%d')

# --- WKG
lprm.c1.asc.vod.wkg = extract.timeseries(lprm.c1.asc.files, wkg.10km.indices)
lprm.c2.asc.vod.wkg = extract.timeseries(lprm.c2.asc.files, wkg.10km.indices)
lprm.x.asc.vod.wkg = extract.timeseries(lprm.x.asc.files, wkg.10km.indices)
lprm.c1.desc.vod.wkg = extract.timeseries(lprm.c1.desc.files, wkg.10km.indices)
lprm.c2.desc.vod.wkg = extract.timeseries(lprm.c2.desc.files, wkg.10km.indices)
lprm.x.desc.vod.wkg = extract.timeseries(lprm.x.desc.files, wkg.10km.indices)

lprm.wkg.df = rbind(data.frame('Dates' = lprm.c1.asc.dates,
                               'Year' = substr(lprm.c1.asc.dates,1,4),
                               'Value' = lprm.c1.asc.vod.kg,
                               'Dataset' = 'LPRM_C1_VOD_ASCENDING_DAY',
                               'Site' = 'WKG'),
                    data.frame('Dates' = lprm.c2.asc.dates,
                               'Year' = substr(lprm.c2.asc.dates,1,4),
                               'Value' = lprm.c2.asc.vod.kg,
                               'Dataset' = 'LPRM_C2_VOD_ASCENDING_DAY',
                              'Site' = 'WKG'),
                    data.frame('Dates' = lprm.x.asc.dates,
                               'Year' = substr(lprm.x.asc.dates,1,4),
                               'Value' = lprm.x.asc.vod.kg,
                               'Dataset' = 'LPRM_X_VOD_ASCENDING_DAY',
                               'Site' = 'WKG'),
                    data.frame('Dates' = lprm.c1.desc.dates,
                               'Year' = substr(lprm.c1.desc.dates,1,4),
                               'Value' = lprm.c1.desc.vod.kg,
                               'Dataset' = 'LPRM_C1_VOD_DESCENDING_NIGHT',
                               'Site' = 'WKG'),
                    data.frame('Dates' = lprm.c2.desc.dates,
                               'Year' = substr(lprm.c2.desc.dates,1,4),
                               'Value' = lprm.c2.desc.vod.kg,
                               'Dataset' = 'LPRM_C2_VOD_DESCENDING_NIGHT',
                               'Site' = 'WKG'),
                    data.frame('Dates' = lprm.x.desc.dates,
                               'Year' = substr(lprm.x.desc.dates,1,4),
                               'Value' = lprm.x.desc.vod.kg,
                               'Dataset' = 'LPRM_X_VOD_DESCENDING_NIGHT',
                               'Site' = 'WKG'))

write.csv(lprm.wkg.df,
          file = paste0(getwd(), '/Data/Timeseries_CSVs/LPRM/', 'KG_LPRM_VOD_Daily.csv'),
          row.names = FALSE)

# ----------------------------------------------------------------
# SMAP 9km L-band data products (VOD, DCA, SCA-H, SCA-V, and VWC)
# ----------------------------------------------------------------

smap.fp = paste(data.fp, 'SMAP_9km', 'GeoTIFF', sep = '/')

# Create function to extract date information from file names
get.smap.dates = function(file.list) {
  dates.index = unname(stringr::str_locate(file.list[1], pattern = '2021'))
  dates.index = c(dates.index[1], dates.index[2]+6)
  dates = as.Date(substr(file.list, dates.index[1], dates.index[2]))
  return(dates)
}

# Create function to extract site-level 09km pixel values from global SMAP files
extract.vars.glob = function(file.list, smap.vars, site.id) {
  smap.09km.df.colnames = c('Dates','Year','Value','Dataset','Var','Site','Period')
  smap.09km.df = as.data.frame(matrix(nrow = 0, ncol = length(smap.09km.df.colnames)))
  names(smap.09km.df) = smap.09km.df.colnames
  date.seq = seq(as.Date('2021-06-30'), as.Date('2021-10-26'), by = 'day')
  for (i in 1 : length(file.list)) {
    print(paste('Extracting',smap.vars[i]))
    if (substr(smap.vars[i],5,6) == 'AM') {var.period = 'Daytime'}
    if (substr(smap.vars[i],5,6) == 'PM') {var.period = 'Nighttime'}
    print(var.period)
    var.files = unlist(file.list[smap.vars[i]])
    var.dates = get.smap.dates(var.files)
    var = extract.timeseries(var.files, wkg.09km.indices)
    var.df = data.frame('Dates' = var.dates,
                        'Year' = '2021',
                        'Value' = var,
                        'Dataset' = 'SMAP_SPL3SMPv005',
                        'Var' = substr(smap.vars[i],1,3),
                        'Site' = site.id, 
                        'Period' = var.period)
    var.df = var.df[var.df$Dates %in% date.seq,]
    smap.09km.df = rbind(smap.09km.df, var.df, make.row.names = FALSE)
  }
  return(smap.09km.df)
}

# Create function to extract spatially averaged vars from data masked to extents of LPDR and LPRM pixel areas
extract.vars.pixel.areas = function(file.list, smap.vars, site.id, pixel.area) {
  smap.pix.df.colnames = c('Dates','Year','Mean','SD','Dataset','Var','Site','Period','Area')
  smap.pix.df = as.data.frame(matrix(nrow = 0, ncol = length(smap.pix.df.colnames)))
  names(smap.pix.df) = smap.pix.df.colnames
  date.seq = seq(as.Date('2021-06-30'), as.Date('2021-10-26'), by = 'day')
  for (i in 1 : length(file.list)) {
    print(paste('Extracting',pixel.area,'aggregated values for',smap.vars[i]))
    if (substr(smap.vars[i],5,6) == 'AM') {var.period = 'Daytime'}
    if (substr(smap.vars[i],5,6) == 'PM') {var.period = 'Nighttime'}
    print(var.period)
    var.files = unname(unlist(file.list[smap.vars[i]]))
    var.dates = get.smap.dates(var.files)
    print('Stacking files')
    var.stack = stack(var.files)
    print('Computing mean')
    var.mean = unname(cellStats(var.stack, stat = mean))
    print('Computing std. dev.')
    var.sd = unname(cellStats(var.stack, stat = sd))
    var.df = data.frame('Dates' = var.dates,
                        'Year' = '2021',
                        'Mean' = var.mean,
                        'SD' = var.sd,
                        'Dataset' = 'SMAP_SPL3SMPv005',
                        'Var' = substr(smap.vars[i],1,3),
                        'Site' = site.id,
                        'Period' = var.period,
                        'Area' = pixel.area)
    var.df = var.df[var.df$Dates %in% date.seq,]
    smap.pix.df = rbind(smap.pix.df, var.df, make.row.names = FALSE)
  }
  smap.pix.df = tidyr::pivot_longer(smap.pix.df, cols = Mean:SD, values_to = 'Value', names_to = 'AggStat') %>%
    as.data.frame()
  smap.pix.df[is.nan(smap.pix.df$Value),9] = NA
  return(smap.pix.df)
}

# Get global file lists
smap.global.fp = paste(smap.fp, 'Global', sep = '/')
smap.vod.am.glob = list.files(smap.global.fp, pattern = '.*_VOD_.*_AM_|.*_AM_.*_VOD_', full.names = TRUE)
smap.vod.pm.glob = list.files(smap.global.fp, pattern = '.*_VOD_.*_PM_|.*_PM_.*_VOD_', full.names = TRUE)
smap.dca.am.glob = list.files(smap.global.fp, pattern = '.*_DCA_.*_AM_|.*_AM_.*_DCA_', full.names = TRUE)
smap.dca.pm.glob = list.files(smap.global.fp, pattern = '.*_DCA_.*_PM_|.*_PM_.*_DCA_', full.names = TRUE)
smap.sch.am.glob = list.files(smap.global.fp, pattern = '.*_SCH_.*_AM_|.*_AM_.*_SCH_', full.names = TRUE)
smap.sch.pm.glob = list.files(smap.global.fp, pattern = '.*_SCH_.*_PM_|.*_PM_.*_SCH_', full.names = TRUE)
smap.scv.am.glob = list.files(smap.global.fp, pattern = '.*_SCV_.*_AM_|.*_AM_.*_SCV_', full.names = TRUE)
smap.scv.pm.glob = list.files(smap.global.fp, pattern = '.*_SCV_.*_PM_|.*_PM_.*_SCV_', full.names = TRUE)
smap.vwc.am.glob = list.files(smap.global.fp, pattern = '.*_VWC_.*_AM_|.*_AM_.*_VWC_', full.names = TRUE)
smap.vwc.pm.glob = list.files(smap.global.fp, pattern = '.*_VWC_.*_PM_|.*_PM_.*_VWC_', full.names = TRUE)

smap.vars = c('VOD_AM','VOD_PM','DCA_AM','DCA_PM','SCH_AM','SCH_PM','SCV_AM','SCV_PM','VWC_AM','VWC_PM')

glob.files.list = list(smap.vod.am.glob, smap.vod.pm.glob, smap.dca.am.glob, smap.dca.pm.glob,
                       smap.sch.am.glob, smap.sch.pm.glob, smap.scv.am.glob, smap.scv.pm.glob,
                       smap.vwc.am.glob, smap.vwc.pm.glob)
names(glob.files.list) = smap.vars

# Extract site-level 09km pixel values
glob.vars = extract.vars.glob(glob.files.list, smap.vars, 'WKG')

# Write to csv file
write.csv(glob.vars,
          file = paste(gitdata.fp,'Timeseries_CSVs','SMAP_09km','WKG_SMAP_09km_VOD_Daily.csv', sep = '/'),
          row.names = FALSE)

# Get LPDR file lists for WKG
smap.wkg.fp = paste(smap.fp,'WKG',sep = '/')
smap.vod.am.lpdr = list.files(paste(smap.wkg.fp,'LPDR',sep = '/'), pattern = '.*_VOD_.*_AM_|.*_AM_.*_VOD_', full.names = TRUE)
smap.vod.pm.lpdr = list.files(paste(smap.wkg.fp,'LPDR',sep = '/'), pattern = '.*_VOD_.*_PM_|.*_PM_.*_VOD_', full.names = TRUE)
smap.dca.am.lpdr = list.files(paste(smap.wkg.fp,'LPDR',sep = '/'), pattern = '.*_DCA_.*_AM_|.*_AM_.*_DCA_', full.names = TRUE)
smap.dca.pm.lpdr = list.files(paste(smap.wkg.fp,'LPDR',sep = '/'), pattern = '.*_DCA_.*_PM_|.*_PM_.*_DCA_', full.names = TRUE)
smap.sch.am.lpdr = list.files(paste(smap.wkg.fp,'LPDR',sep = '/'), pattern = '.*_SCH_.*_AM_|.*_AM_.*_SCH_', full.names = TRUE)
smap.sch.pm.lpdr = list.files(paste(smap.wkg.fp,'LPDR',sep = '/'), pattern = '.*_SCH_.*_PM_|.*_PM_.*_SCH_', full.names = TRUE)
smap.scv.am.lpdr = list.files(paste(smap.wkg.fp,'LPDR',sep = '/'), pattern = '.*_SCV_.*_AM_|.*_AM_.*_SCV_', full.names = TRUE)
smap.scv.pm.lpdr = list.files(paste(smap.wkg.fp,'LPDR',sep = '/'), pattern = '.*_SCV_.*_PM_|.*_PM_.*_SCV_', full.names = TRUE)
smap.vwc.am.lpdr = list.files(paste(smap.wkg.fp,'LPDR',sep = '/'), pattern = '.*_VWC_.*_AM_|.*_AM_.*_VWC_', full.names = TRUE)
smap.vwc.pm.lpdr = list.files(paste(smap.wkg.fp,'LPDR',sep = '/'), pattern = '.*_VWC_.*_PM_|.*_PM_.*_VWC_', full.names = TRUE)

lpdr.files.list = list(smap.vod.am.lpdr, smap.vod.pm.lpdr, smap.dca.am.lpdr, smap.dca.pm.lpdr,
                       smap.sch.am.lpdr, smap.sch.pm.lpdr, smap.scv.am.lpdr, smap.scv.pm.lpdr,
                       smap.vwc.am.lpdr, smap.vwc.pm.lpdr)
names(lpdr.files.list) = smap.vars

# Extract aggregate pixel area values
lpdr.vars = extract.vars.pixel.areas(lpdr.files.list, smap.vars, 'WKG', 'LPDR')

# Write to csv file
write.csv(lpdr.vars,
          file = paste(gitdata.fp,'Timeseries_CSVs','SMAP_09km','WKG_SMAP_09km_VOD_Daily_LPDR_area.csv', sep = '/'),
          row.names = FALSE)

# Get LPRM file lists for WKG
smap.vod.am.lprm = list.files(paste(smap.wkg.fp,'LPRM',sep = '/'), pattern = '.*_VOD_.*_AM_|.*_AM_.*_VOD_', full.names = TRUE)
smap.vod.pm.lprm = list.files(paste(smap.wkg.fp,'LPRM',sep = '/'), pattern = '.*_VOD_.*_PM_|.*_PM_.*_VOD_', full.names = TRUE)
smap.dca.am.lprm = list.files(paste(smap.wkg.fp,'LPRM',sep = '/'), pattern = '.*_DCA_.*_AM_|.*_AM_.*_DCA_', full.names = TRUE)
smap.dca.pm.lprm = list.files(paste(smap.wkg.fp,'LPRM',sep = '/'), pattern = '.*_DCA_.*_PM_|.*_PM_.*_DCA_', full.names = TRUE)
smap.sch.am.lprm = list.files(paste(smap.wkg.fp,'LPRM',sep = '/'), pattern = '.*_SCH_.*_AM_|.*_AM_.*_SCH_', full.names = TRUE)
smap.sch.pm.lprm = list.files(paste(smap.wkg.fp,'LPRM',sep = '/'), pattern = '.*_SCH_.*_PM_|.*_PM_.*_SCH_', full.names = TRUE)
smap.scv.am.lprm = list.files(paste(smap.wkg.fp,'LPRM',sep = '/'), pattern = '.*_SCV_.*_AM_|.*_AM_.*_SCV_', full.names = TRUE)
smap.scv.pm.lprm = list.files(paste(smap.wkg.fp,'LPRM',sep = '/'), pattern = '.*_SCV_.*_PM_|.*_PM_.*_SCV_', full.names = TRUE)
smap.vwc.am.lprm = list.files(paste(smap.wkg.fp,'LPRM',sep = '/'), pattern = '.*_VWC_.*_AM_|.*_AM_.*_VWC_', full.names = TRUE)
smap.vwc.pm.lprm = list.files(paste(smap.wkg.fp,'LPRM',sep = '/'), pattern = '.*_VWC_.*_PM_|.*_PM_.*_VWC_', full.names = TRUE)

lprm.files.list = list(smap.vod.am.lprm, smap.vod.pm.lprm, smap.dca.am.lprm, smap.dca.pm.lprm,
                       smap.sch.am.lprm, smap.sch.pm.lprm, smap.scv.am.lprm, smap.scv.pm.lprm,
                       smap.vwc.am.lprm, smap.vwc.pm.lprm)
names(lprm.files.list) = smap.vars

lprm.vars = extract.vars.pixel.areas(lprm.files.list, smap.vars, 'WKG', 'LPRM')

# Write to csv file
write.csv(lprm.vars,
          file = paste(gitdata.fp,'Timeseries_CSVs','SMAP_09km','WKG_SMAP_09km_VOD_Daily_LPRM_area.csv', sep = '/'),
          row.names = FALSE)
