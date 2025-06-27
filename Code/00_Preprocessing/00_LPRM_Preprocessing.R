library(ncdf4)
library(raster)
library(stringr)

setwd('../../../')
root.fp = getwd()
data.fp = paste(root.fp, 'Data', sep = '/') # Raw data directory
lprm.fp = paste(data.fp, 'LPRM', sep = '/') # LPRM data directory

years = seq(2023,2023,1) # Specify date range for processing

ext.ras = c(-180,180,-90,90) # Spatial extent (global)
crs.ras = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') # Coord. reference info

out.fp = paste(lprm.fp, 'GeoTIFF', 'WGS84', sep = '/') # Output directory for VOD geotiffs

# Disable .aux file write output and set output VOD filepath
rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")

# Loop through yearly subdirectories
# Open C1, C2, and X-band VOD layers from each daily netCDF file (ascending and descending)
# Convert layers from array to raster and assign spatial extent/coord. ref
# Export each individually in geotiff format 
for (y in 1 : length(years)) {
  
  year = years[y]
  
  asc.files = list.files(paste(lprm.fp, 'Original', 'Ascending', year, sep = '/'), pattern = '.nc4$', full.names = TRUE, recursive = TRUE)
  asc.dates = as.Date(str_sub(asc.files, start = 94, end = 101), format = '%Y%m%d')
  
  desc.files = list.files(paste(lprm.fp, 'Original', 'Descending', year, sep = '/'), pattern = '.nc4$', full.names = TRUE, recursive = TRUE)
  desc.dates = as.Date(str_sub(desc.files, start = 95, end = 102), format = '%Y%m%d')
  
  # ---- Ascending files
  print(paste('Preprocessing LPRM VOD for year', year, '-- Ascending'))
  for (asc.f in 1 : length(asc.files)) {
    
    asc.lprm.file = nc_open(asc.files[asc.f])
    asc.file.date = gsub('-','_',asc.dates[asc.f])
    
    asc.vod.c1 = ncvar_get(asc.lprm.file, 'opt_depth_c1')
    asc.vod.c2 = ncvar_get(asc.lprm.file, 'opt_depth_c2')
    asc.vod.x = ncvar_get(asc.lprm.file, 'opt_depth_x')
    
    nc_close(asc.lprm.file)
    
    # C-band VOD (1)
    asc.vod.c1.ras = raster(asc.vod.c1)
    crs(asc.vod.c1.ras) = wkt(crs.ras)
    extent(asc.vod.c1.ras) = ext.ras
    
    writeRaster(asc.vod.c1.ras,
                paste(out.fp, year, paste0(asc.file.date, '_LPRM_C1_VOD_A.tif'), sep = '/'),
                format = 'GTiff',
                overwrite = TRUE)
    
    # C-band VOD (2)
    asc.vod.c2.ras = raster(asc.vod.c2)
    crs(asc.vod.c2.ras) = wkt(crs.ras)
    extent(asc.vod.c2.ras) = ext.ras
    
    writeRaster(asc.vod.c2.ras,
                paste(out.fp, year, paste0(asc.file.date, '_LPRM_C2_VOD_A.tif'), sep = '/'),
                format = 'GTiff',
                overwrite = TRUE)
    
    # X-band VOD
    asc.vod.x.ras = raster(asc.vod.x)
    crs(asc.vod.x.ras) = wkt(crs.ras)
    extent(asc.vod.x.ras) = ext.ras
    
    writeRaster(asc.vod.x.ras,
                paste(out.fp, year, paste0(asc.file.date, '_LPRM_X_VOD_A.tif'), sep = '/'),
                format = 'GTiff',
                overwrite = TRUE)
  }
  
  # ---- Descending files
  print(paste('Preprocessing LPRM VOD for year', year, '-- Descending'))
  for (desc.f in 1 : length(desc.files)) {
    
    desc.lprm.file = nc_open(desc.files[desc.f])
    desc.file.date = gsub('-','_',desc.dates[desc.f])
    
    desc.vod.c1 = ncvar_get(desc.lprm.file, 'opt_depth_c1')
    desc.vod.c2 = ncvar_get(desc.lprm.file, 'opt_depth_c2')
    desc.vod.x = ncvar_get(desc.lprm.file, 'opt_depth_x')
    
    nc_close(desc.lprm.file)
    
    # C-band VOD (1)
    desc.vod.c1.ras = raster(desc.vod.c1)
    crs(desc.vod.c1.ras) = wkt(crs.ras)
    extent(desc.vod.c1.ras) = ext.ras
    
    writeRaster(desc.vod.c1.ras,
                paste(out.fp, year, paste0(desc.file.date, '_LPRM_C1_VOD_D.tif'), sep = '/'),
                format = 'GTiff',
                overwrite = TRUE)
    
    # C-band VOD (2)
    desc.vod.c2.ras = raster(desc.vod.c2)
    crs(desc.vod.c2.ras) = wkt(crs.ras)
    extent(desc.vod.c2.ras) = ext.ras
    
    writeRaster(desc.vod.c2.ras,
                paste(out.fp, year, paste0(desc.file.date, '_LPRM_C2_VOD_D.tif'), sep = '/'),
                format = 'GTiff',
                overwrite = TRUE)
    
    # X-band VOD
    desc.vod.x.ras = raster(desc.vod.x)
    crs(desc.vod.x.ras) = wkt(crs.ras)
    extent(desc.vod.x.ras) = ext.ras
    
    writeRaster(desc.vod.x.ras,
                paste(out.fp, year, paste0(desc.file.date, '_LPRM_X_VOD_D.tif'), sep = '/'),
                format = 'GTiff',
                overwrite = TRUE)
  }
  print('Done!')
}