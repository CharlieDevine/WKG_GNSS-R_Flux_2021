library(raster)
library(rgdal)
library(stringr)

setwd('../../../')
root.fp = getwd()
data.fp = paste(root.fp, 'Data', sep = '/')
lpdr.fp = paste(data.fp, 'LPDR_v3', sep = '/')

years = seq(2015,2022,1)

for (y in 1 : length(years)) {
  
  # Set year
  year = years[y]
  print(paste('Extrating LPDR VOD for year', year))
  
  # Get all non-QA files for that year
  lpdr.files = grep(pattern = 'QA.tif', invert = TRUE, value = TRUE, 
                    x = list.files(paste(lpdr.fp, 'Original', year, sep = '/'), full.names = TRUE, recursive = FALSE))
  
  # Separate ascending and descending files
  lpdr.asc = grep(pattern = 'D.tif', invert = TRUE, value = TRUE, x = lpdr.files)
  lpdr.desc = grep(pattern = 'A.tif', invert = TRUE, value = TRUE, x = lpdr.files)
  
  # Get julian day number for ascending and descending files
  asc.jdays =  str_sub(lpdr.asc, -8, -6)
  desc.jdays = str_sub(lpdr.desc, -8, -6)
  
  # Convert julian day numbers to dates (YYYY-MM-DD)
  asc.dates = as.Date(as.numeric(asc.jdays), origin = as.Date(paste0(year,'-01-01'))-1)
  desc.dates = as.Date(as.numeric(desc.jdays), origin = as.Date(paste0(year,'-01-01'))-1)
  
  # Disable .aux file write output and set output VOD filepath
  rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")
  lpdr.fp.out = paste(lpdr.fp, 'VOD', year, sep = '/')
  
  # Extract ascending and descending layers and export to .tif
  for (i in 1 : length(asc.dates)){

    asc.file = stack(lpdr.asc[i])
    asc.vod = asc.file[[5]]
    asc.vod[asc.vod == -999] = NA

    writeRaster(asc.vod,
                filename = paste0(lpdr.fp.out, '/', gsub('-','',asc.dates[i]), '_LPDR_VOD_A.tif'),
                format = 'GTiff')
  }
  
  for (j in 1 : length(desc.dates)){
    
    desc.file = stack(lpdr.desc[j])
    desc.vod = desc.file[[5]]
    desc.vod[desc.vod == -999] = NA
    
    writeRaster(desc.vod,
                filename = paste0(lpdr.fp.out, '/', gsub('-','',desc.dates[j]), '_LPDR_VOD_D.tif'),
                format = 'GTiff',
                overwrite = TRUE)
  }
}
