library(amerifluxr)

setwd('../../')
amflux.fp = paste(getwd(), 'Data', 'Ameriflux_BASE', sep = '/')

# Get BASE data
wkg = amf_download_base(user_id = 'cjdevine',
                        user_email = 'cjdevine@arizona.edu',
                        site_id = 'US-Wkg',
                        data_product = "BASE-BADM",
                        data_policy = "CCBY4.0",
                        agree_policy = TRUE,
                        intended_use = "remote_sensing",
                        intended_use_text = "Explore the dataset",
                        verbose = TRUE,
                        out_dir = paste(amflux.fp, 'BASE', sep = '/'))