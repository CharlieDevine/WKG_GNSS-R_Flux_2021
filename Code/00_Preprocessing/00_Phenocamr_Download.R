library(phenocamr)

setwd('../../')
data.fp = paste(getwd(), 'Data', sep = '/')
phenocamr.fp = paste(data.fp, 'Phenocamr', sep = '/')

# Get PhenoCam Network ROIs
rois = list_rois()

# Get WKG info
wkg.info = rois[grep('kendall', rois$site),]
wkg.rois = paste(wkg.info$veg_type, wkg.info$roi_id_number, sep = '_')

for (i in 1 : length(wkg.rois)) {
  download_phenocam(site = wkg.info$site[i],
                    veg_type = wkg.info$veg_type[i],
                    roi_id = substr(wkg.rois[i], start = 4, stop = 7),
                    frequency = 1,
                    smooth = FALSE,
                    out_dir = phenocamr.fp)
}