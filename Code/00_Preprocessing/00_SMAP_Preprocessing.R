library(ncdf4)
library(raster)
library(sf)
library(sp)

setwd('../../')
root.fp = getwd()
gitdata.fp = paste(root.fp, 'Data', sep = '/')
figs.fp = paste(root.fp, 'Figures', sep = '/')

setwd('../../')
data.fp = paste(getwd(), 'Data', sep = '/')
smap.fp = paste(data.fp, 'SMAP_9km', sep = '/')
orig.fp = paste(smap.fp,'Original', sep = '/')
h5.fp = paste(smap.fp,'HDF5', sep = '/')
geotiff.fp = paste(smap.fp,'GeoTIFF', sep = '/')

# Geographic varialbes (EAS Grid 2.0 EPSG:6933)
ease2.ext = c(-17367530.44,17367530.44,-7314540.83,7314540.83) # per NSIDC EASE Grid 2.0 resolution specs
ease2.crs = sp::CRS('+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0, 0 +units=m +no_defs')

# Get shapefiles
shp.files = list.files(paste(gitdata.fp, 'Shapefiles', 'Site_Geometries', sep = '/'), pattern = '.shp', full.names = TRUE)
site.areas = shapefile(shp.files[4])
site.points = shapefile(shp.files[5])
lpdr.pix.areas = st_read(shp.files[1])
lprm.pix.areas = st_read(shp.files[2])
mtdca.pix.areas = st_read(shp.files[3])

wkg.pix.areas = rbind(lpdr.pix.areas[lpdr.pix.areas$SITE_ID == 'WKG',],
                      lprm.pix.areas[lprm.pix.areas$SITE_ID == 'WKG',],
                      mtdca.pix.areas[mtdca.pix.areas$SITE_ID == 'WKG',])

wkg.lpdr.bb = st_transform(x = wkg.pix.areas[wkg.pix.areas$Product == 'LPDR_25km',], 
                           src = st_crs(lpdr.pix.areas), crs = ease2.crs, dst = eas2.crs)
wkg.lprm.bb = st_transform(x = wkg.pix.areas[wkg.pix.areas$Product == 'LPRM_10km',], 
                           src = st_crs(lpdr.pix.areas), crs = ease2.crs, dst = eas2.crs)
wkg.mtdca.bb = st_transform(x = wkg.pix.areas[wkg.pix.areas$Product == 'MTDCA_09km',], 
                            src = st_crs(lpdr.pix.areas), crs = ease2.crs, dst = eas2.crs)


# Get SMAP hdf5 filepaths
h5.files = list.files(h5.fp, pattern = '.h5$', full.names = TRUE)

# Create function to convert vod matrices to rasters
ARRtoRAS = function(vod.arr, date, var.name, overpass) {
  vod.ras = t(raster(vod.arr))
  extent(vod.ras) = ease2.ext
  crs(vod.ras) = ease2.crs
  ras.name = paste(overpass, var.name, date, sep = '_')
  names(vod.ras) = ras.name
  ras.filename = paste0(paste('SMAP_L3v005_09km',ras.name, sep = '_'),'.tif')
  return(list(vod.ras, ras.name))
}

# Create function to write global raster as geoTIFF file
write.raster.global = function(ras) {
  ras.filename = paste0(paste('SMAP_L3v005_09km',ras[[2]],'GLOBAL',sep = '_'),'.tif')
  writeRaster(ras[[1]],
              filename = paste(geotiff.fp, 'Global', ras.filename, sep = '/'),
              format = 'GTiff',
              overwrite = TRUE)
}

# Create function to crop raster to area of SMAP, LPDR, and LPRM pixel areas
crop.write.raster = function(ras, site, lpdr.area, lprm.area, mtdca.area) {
  ras.lpdr = crop(ras[[1]], lpdr.area)
  ras.lprm = crop(ras[[1]], lprm.area)
  ras.mtdca = crop(ras[[1]], mtdca.area)
  ras.lpdr.filename = paste0(paste('SMAP_L3v005_09km',ras[[2]],site,'LPDRmean',sep = '_'),'.tif')
  ras.lprm.filename = paste0(paste('SMAP_L3v005_09km',ras[[2]],site,'LPRMmean',sep = '_'),'.tif')
  ras.mtdca.filename = paste0(paste('SMAP_L3v005_09km',ras[[2]],site,'MTDCAmean',sep = '_'),'.tif')
  writeRaster(ras.lpdr,
              filename = paste(geotiff.fp,site,'LPDR',ras.lpdr.filename,sep = '/'),
              format = 'GTiff',
              overwrite = TRUE)
  writeRaster(ras.lprm,
              filename = paste(geotiff.fp,site,'LPRM',ras.lprm.filename,sep = '/'),
              format = 'GTiff',
              overwrite = TRUE)
  writeRaster(ras.mtdca,
              filename = paste(geotiff.fp,site,'MTDCA',ras.mtdca.filename,sep = '/'),
              format = 'GTiff',
              overwrite = TRUE)
  
}

# Loop through files to extract VOD data, convert to geoTIFF, and export/crop to drive
for (i in 1 : length(h5.files)) {
  
  h5.file.date = unlist(strsplit(tail(unlist(strsplit(h5.files[i], split = '/')), n = 1), split = '_'))[6]
  h5.file.date = paste(substr(h5.file.date,1,4),
                       substr(h5.file.date,5,6),
                       substr(h5.file.date,7,8),
                       sep = '-')
  
  h5.file = nc_open(h5.files[i])
  
  vars = names(h5.file$var)
  
  lat = ncvar_get(h5.file, varid = vars[12])
  lon = ncvar_get(h5.file, varid = vars[14])
  
  # AM variables
  vod.am = ncvar_get(h5.file, varid = vars[47])
  vod.dca.am = ncvar_get(h5.file, varid = vars[48])
  vod.sca.h.am = ncvar_get(h5.file, varid = vars[49])
  vod.sca.v.am = ncvar_get(h5.file, varid = vars[50])
  vwc.am = ncvar_get(h5.file, varid = vars[51])
  
  # PM variables
  vod.pm = ncvar_get(h5.file, varid = vars[99])
  vod.dca.pm = ncvar_get(h5.file, varid = vars[98])
  vod.sca.h.pm = ncvar_get(h5.file, varid = vars[100])
  vod.sca.v.pm = ncvar_get(h5.file, varid = vars[101])
  vwc.pm = ncvar_get(h5.file, varid = vars[102])
  
  nc_close(h5.file)
  
  # Convert to raster and export geoTIFF files to '/Gloal' subdirectory
  # AM
  vod.am.ras = ARRtoRAS(vod.am, h5.file.date, 'VOD', 'AM_0100')
  vod.dca.am.ras = ARRtoRAS(vod.dca.am, h5.file.date, 'DCA', 'AM_0100')
  vod.sca.h.am.ras = ARRtoRAS(vod.sca.h.am, h5.file.date, 'SCH', 'AM_0100')
  vod.sca.v.am.ras = ARRtoRAS(vod.sca.v.am, h5.file.date, 'SCV', 'AM_0100')
  vwc.am.ras = ARRtoRAS(vwc.am, h5.file.date, 'VWC', 'AM_0100')
  write.raster.global(vod.am.ras)
  write.raster.global(vod.dca.am.ras)
  write.raster.global(vod.sca.h.am.ras)
  write.raster.global(vod.sca.v.am.ras)
  write.raster.global(vwc.am.ras)
  
  # PM
  vod.pm.ras = ARRtoRAS(vod.pm, h5.file.date, 'VOD', 'PM_1300')
  vod.dca.pm.ras = ARRtoRAS(vod.dca.pm, h5.file.date, 'DCA', 'PM_1300')
  vod.sca.h.pm.ras = ARRtoRAS(vod.sca.h.pm, h5.file.date, 'SCH', 'PM_1300')
  vod.sca.v.pm.ras = ARRtoRAS(vod.sca.v.pm, h5.file.date, 'SCV', 'PM_1300')
  vwc.pm.ras = ARRtoRAS(vwc.pm, h5.file.date, 'VWC', 'PM_1300')
  write.raster.global(vod.pm.ras)
  write.raster.global(vod.dca.pm.ras)
  write.raster.global(vod.sca.h.pm.ras)
  write.raster.global(vod.sca.v.pm.ras)
  write.raster.global(vwc.pm.ras)
  
  #LPDR, LPRM, and MTDCA pixel footprints at WKG
  # AM
  crop.write.raster(vod.am.ras, 'WKG', wkg.lpdr.bb, wkg.lprm.bb, wkg.mtdca.bb)
  crop.write.raster(vod.dca.am.ras, 'WKG', wkg.lpdr.bb, wkg.lprm.bb, wkg.mtdca.bb)
  crop.write.raster(vod.sca.h.am.ras, 'WKG', wkg.lpdr.bb, wkg.lprm.bb, wkg.mtdca.bb)
  crop.write.raster(vod.sca.v.am.ras, 'WKG', wkg.lpdr.bb, wkg.lprm.bb, wkg.mtdca.bb)
  crop.write.raster(vwc.am.ras, 'WKG', wkg.lpdr.bb, wkg.lprm.bb, wkg.mtdca.bb)
  
  # PM
  crop.write.raster(vod.pm.ras, 'WKG', wkg.lpdr.bb, wkg.lprm.bb, wkg.mtdca.bb)
  crop.write.raster(vod.dca.pm.ras, 'WKG', wkg.lpdr.bb, wkg.lprm.bb, wkg.mtdca.bb)
  crop.write.raster(vod.sca.h.pm.ras, 'WKG', wkg.lpdr.bb, wkg.lprm.bb, wkg.mtdca.bb)
  crop.write.raster(vod.sca.v.pm.ras, 'WKG', wkg.lpdr.bb, wkg.lprm.bb, wkg.mtdca.bb)
  crop.write.raster(vwc.pm.ras, 'WKG', wkg.lpdr.bb, wkg.lprm.bb, wkg.mtdca.bb)
  
}
