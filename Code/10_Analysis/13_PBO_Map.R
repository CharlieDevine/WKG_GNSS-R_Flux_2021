library(dplyr)
library(ggplot2)
library(ggpattern)
library(ggpubr)
library(usmap)
library(sp)
library(sf)
library(amerifluxr)
library(flextable)
library(cowplot)

setwd('../../')
git.fp = getwd()
figs.fp = paste(git.fp, 'Figures', sep = '/')
data.fp = paste(git.fp, 'Data', sep = '/')
amflux.fp = paste(getwd(), 'Data', 'Ameriflux', sep = '/')
setwd('../../')
pbo.fp = paste(getwd(), 'Data/PBO_H2O_data', sep = '/')

# Get PBO subdirectories
pbo.dirs = list.dirs(path = pbo.fp, recursive = FALSE)
pbo.dirs = pbo.dirs[1:(length(pbo.dirs)-1)]
pbo.files = list.files(pbo.dirs, pattern = '.csv', full.names = TRUE, recursive = FALSE)

# Create function to loop through each PBO file and extract date range info for NMRI 
pbo.check.nmri.fun = function() {
  
  # Create output data frame
  df.out = as.data.frame(matrix(ncol = 3, nrow = 0))
  colnames(df.out) = c('Site_ID','Date_start','Date_end')
  
  for (i in 1 : length(pbo.files)) {
    
    # Get site name
    site.id.i = unlist(strsplit(tail(unlist(strsplit(pbo.files[i], split = '/')),1), split = '_'))[1]
    print(paste('Checking NMRI data for', site.id.i))
    
    # Open file
    file.i = read.table(pbo.files[i], sep = ',')
    file.i = file.i[,c(1,4,5)]
    colnames(file.i) = c('Dates','Veg_QualFlg','NMRI')
    file.i$Dates = as.Date(substr(file.i$Dates, start = 1, stop = 10), format = '%Y-%m-%d')
    
    # If NMRI data exists for site, get first and last dates
    if (!isTRUE(isTRUE(all(is.na(file.i$NMRI))))) {
      
      # Get row index values of first and last NMRI values
      nmri.first = min(which(!is.na(file.i$NMRI)))
      nmri.last = max(which(!is.na(file.i$NMRI)))
      
      # Get dates corresponding to first and last NMRI
      date.first = file.i$Dates[nmri.first]
      date.last = file.i$Dates[nmri.last]
      
      # Assign info to data frame and append with df.out
      df.i = data.frame('Site_ID' = site.id.i, 'Date_start' = date.first, 'Date_end' = date.last)
      df.out = rbind(df.out, df.i)
    }
    
    # If NMRI data does not exist for site, set outputs to NA
    if (isTRUE(all(is.na(file.i$NMRI)))) {
      df.i = data.frame('Site_ID' = site.id.i, 'Date_start' = NA, 'Date_end' = NA)
      df.out = rbind(df.out, df.i)
    }
  }
  return(df.out)
}

pbo.nmri.site.check = pbo.check.nmri.fun()

# Exclude sites with no NMRI 
pbo.with.nmri = na.omit(pbo.nmri.site.check)

# Get PBO vegetation metadata
pbo.metadata = read.csv(paste(pbo.fp, 'veg_metadata.csv', sep = '/'), header = TRUE, skip = 4)
pbo.metadata$Lon = pbo.metadata$Lon - 360

# Reassign PBO land cover classes
pbo.lc.reclass.fun = function() {
  pbo.metadata$LC_Class = vector(mode = 'character', length = nrow(pbo.metadata))
  for (i in 1 : nrow(pbo.metadata)) {
    if (pbo.metadata$Landcover[i] == ' Tundra ')          { pbo.metadata$LC_Class[i] = 'Tundra' }
    if (pbo.metadata$Landcover[i] == ' Evergreen_Needl ') { pbo.metadata$LC_Class[i] = 'Evergreen Needle Leaf' }
    if (pbo.metadata$Landcover[i] == ' Unknown ')         { pbo.metadata$LC_Class[i] = 'Unknown' }
    if (pbo.metadata$Landcover[i] == ' GrassLands ')      { pbo.metadata$LC_Class[i] = 'Grassland' }
    if (pbo.metadata$Landcover[i] == ' Open_ShrubLands ') { pbo.metadata$LC_Class[i] = 'Open Shrubland' }
    if (pbo.metadata$Landcover[i] == ' Closed_Shrublan ') { pbo.metadata$LC_Class[i] = 'Closed Shrubland' }
    if (pbo.metadata$Landcover[i] == ' Mixed_FOREST ')    { pbo.metadata$LC_Class[i] = 'Mixed Forest' }
    if (pbo.metadata$Landcover[i] == ' Woody_Savannas ')  { pbo.metadata$LC_Class[i] = 'Woody Savanna' }
    if (pbo.metadata$Landcover[i] == ' Savannas ')        { pbo.metadata$LC_Class[i] = 'Savanna' }
    if (pbo.metadata$Landcover[i] == ' CropLand/Natura ') { pbo.metadata$LC_Class[i] = 'Crop' }
    if (pbo.metadata$Landcover[i] == ' CropLands ')       { pbo.metadata$LC_Class[i] = 'Crop' }
    if (pbo.metadata$Landcover[i] == ' Urban_and_Built ') { pbo.metadata$LC_Class[i] = 'Urban' }
  }
  return(pbo.metadata)
}

pbo.metadata = pbo.lc.reclass.fun()

# Exclude points in AK and those classified as "unknown"
pbo.metadata = pbo.metadata[pbo.metadata$Lon > -130 & pbo.metadata$LC_Class != 'Unknown',]

# Filter PBO points to include only those with NMRI (pbo.with.nmri)
pbo.metadata = pbo.metadata[pbo.metadata$Station_ID %in% pbo.with.nmri$Site_ID,]

# Convert to spatial
pbo = st_as_sf(SpatialPointsDataFrame(coords = pbo.metadata[,c(3,2)],
                                      data = pbo.metadata[,c(1,2,3,7)],
                                      proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +type=crs')))

pbo$Network = 'PBO Sites w/ NMRI'

# Get state polygons
western.states.shp = usmap::us_map(regions = 'states', include = c('AZ','NM','CO','UT','WY','ID','CA','WA','OR','NV','MT'))
western.states.df = western.states.shp
western.states.df$geom = NULL
western.states.df = as.data.frame(western.states.df)
western.states.shp = sf::as_Spatial(st_geometry(western.states.shp), IDs = as.character(1:nrow(western.states.df)))
western.states.shp = st_transform(st_as_sf(SpatialPolygonsDataFrame(western.states.shp, data = western.states.df)),
                                  '+proj=longlat +datum=WGS84 +no_defs +type=crs')

# Get Ameriflux site location data
amf.sites = amf_sites()
amf.sites = amf.sites[amf.sites$STATE %in% c('AZ','NM','CO','UT','WY','ID','CA','WA','OR','NV','MT'),]
amf.sites$ID = ''
amf.sites$Network = 'AmeriFlux Sites'
amf.sites = amf.sites[,c(1,4,5,6,7,9,10,16,17)]
colnames(amf.sites)[6:7] = c('LAT','LON')
amf.sites = st_as_sf(SpatialPointsDataFrame(coords = matrix(c(as.numeric(amf.sites$LON), as.numeric(amf.sites$LAT)), ncol = 2),
                                            data = amf.sites,
                                            proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +type=crs')))

# For each PBO point, identify the nearest Ameriflux point
amf.near.pbo = st_nearest_feature(pbo, amf.sites, longlat = isTRUE(st_is_longlat(pbo)))

# Calculate euclidean distance between PBO sites and nearest Ameriflux sites filtered by common years
amf.proximal.distance.to.pbo.fun = function() {
  
  # Set data output
  df.out = data.frame(matrix(ncol = 8, nrow = 0))
  colnames(df.out) = c('PBO_SiteID','AMF_SiteID','DistanceKM','PBO_LC','State','PBO_LAT','PBO_LON','Years')
  
  for (i in 1 : length(amf.near.pbo)) {
    
    # Subset PBO site
    pbo.i = pbo[i,]
    pbo.years.i = unique(substr(read.table(list.files(pbo.dirs, pattern = paste0(pbo.i$Station_ID,'_v1'), full.names = TRUE, recursive = TRUE), sep = ',')[,1], start = 1, stop = 4))
    
    # Subset AMF site
    amf.i = amf.sites[amf.near.pbo[i],] # Subset AMF site data using index value provided in "amf.near.pbo" variable
    amf.years.i = c(amf.i$TOWER_BEGAN, amf.i$TOWER_END)
    if (isTRUE(is.na(amf.years.i[2]))) { amf.years.i[2] = substr(Sys.Date(), start = 1, stop = 4) }
    if (isTRUE(is.na(amf.years.i[1]))) { amf.years.i[1] = substr(Sys.Date(), start = 1, stop = 4) }
    amf.years.i = seq(amf.years.i[1], amf.years.i[2], by = 1)
    
    # Get years where PBO and AMF data overlap
    common.years.i = intersect(pbo.years.i, amf.years.i)
    
    if (isTRUE(length(common.years.i) > 0)) {
      
      # Get first and last years
      year.range.i = c(first(common.years.i), last(common.years.i))
      if (isTRUE(is.na(year.range.i[2])))   {  year.range.i = year.range.i[1]  }
      if (!isTRUE(is.na(year.range.i[2])))  {  year.range.i = year.range.i     }
      if (isTRUE(length(unique(year.range.i)) > 1)) {  year.range.i = paste0(year.range.i[1],'-',year.range.i[2])}
      
      # Calculate distance (km) between PBO and AMF sites
      print(paste('Calculating distance between', pbo.i$Station_ID, 'PBO site and', amf.i$SITE_ID, 'AMF site'))
      dist.i = as.numeric(st_length(st_nearest_points(pbo.i, amf.i))) / 1000 
      
      # Assign info to data frame and append with df.out
      df.i = data.frame('PBO_SiteID' = pbo.i$Station_ID,
                        'AMF_SiteID' = amf.i$SITE_ID,
                        'DistanceKM' = dist.i,
                        'PBO_LC' = pbo.i$LC_Class,
                        'State' = amf.i$STATE,
                        'PBO_LAT' = pbo.i$Lat,
                        'PBO_LON' = pbo.i$Lon,
                        'Years' = year.range.i)
      
      df.out = rbind(df.out, df.i)
    }
  }
  
  # Round distance to 1st decimal value and sort by increasing order of distance
  df.out$DistanceKM = round(df.out$DistanceKM,1)
  df.out = df.out[order(df.out$DistanceKM, decreasing = FALSE),]
  
  # Remove duplicated rows
  df.out = unique(df.out)
  
  # Create label field, subset data frame, and return output
  df.out$Label = seq(1,nrow(df.out),1)
  df.out = df.out[,c(9,1,2,3,4,5,6,7,8)]
  return(df.out)
}

amf.proximal.distance.to.pbo = amf.proximal.distance.to.pbo.fun()

# Factor column names
colnames(amf.proximal.distance.to.pbo) = factor(colnames(amf.proximal.distance.to.pbo),
                                                levels = colnames(amf.proximal.distance.to.pbo),
                                                labels = c('Label','PBO Site','Nearest AMF Site','Distance (km)',
                                                           'PBO Veg. Cover','State','PBO_LAT','PBO_LON','Years Overlap'))

# Create object for plotting PBO labels
pbo.top10.labs = amf.proximal.distance.to.pbo[1:10,]
pbo.top10.labs = st_as_sf(SpatialPointsDataFrame(coords = matrix(c(as.numeric(pbo.top10.labs$PBO_LON), 
                                                                   as.numeric(pbo.top10.labs$PBO_LAT)), 
                                                                 ncol = 2),
                                                 data = pbo.top10.labs,
                                                 proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +type=crs')))

# -------------------------- Plot map
pbo.map = ggplot() +
  # --- Plot invisible PBO location points to set plot spatial reference
  geom_sf(data = pbo, color = 'transparent', show.legend = FALSE) +
  # --- Plot state boundaries
  geom_sf(data = western.states.shp,
          linewidth = 0.75,
          fill = NA, 
          show.legend = FALSE, 
          inherit.aes = FALSE) +
  # --- Plot PBO locations and symbolize by LC class
  geom_sf(data = pbo,
          aes(color = LC_Class),
          size = 1.5) +
  scale_color_manual(values = c('Closed Shrubland' = 'lemonchiffon4',
                                'Crop' = 'steelblue',
                                'Grassland' = 'green3',
                                'Mixed Forest' = 'navy',
                                'Open Shrubland' = 'plum3',
                                'Savanna' = 'orange',
                                'Urban' = 'red',
                                'Woody Savanna' = 'cyan2'),
                     name = 'PBO GNSS Sites\nw/ NMRI') +
  # --- Plot ameriflux site locations
  geom_sf(data = amf.sites, aes(shape = ID), size = 2, color = 'red') +
  scale_shape_manual(values = 3, name = 'AmeriFlux Sites') +
  # --- Label PBO sites listed in table
  ggrepel::geom_text_repel(data = pbo.top10.labs,
                           aes(x = PBO_LON, y = PBO_LAT, label = Label),
                           max.overlaps = Inf,
                           #force = 10,
                           box.padding = 1,
                           size = 6, fontface = 'bold',
                           inherit.aes = FALSE) +
  # --- Set guide/legend and plot themes/graphical parameters
  guides(shape = guide_legend(override.aes = list(size = 5, stroke = 2)),
         color = guide_legend(override.aes = list(size = 5))) +
  xlab('') +
  ylab('') +
  theme_bw() + 
  theme(legend.title = element_text(size = 17, face = 'bold'),
        legend.text = element_text(size = 15),
        legend.position = 'left',
        axis.text = element_text(size = 10),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, linewidth = 2))

# -------------------------- Create table for top-10 PBO stations with the shortest distance to AMF sites
amf.proximal.distance.to.pbo.table = flextable(amf.proximal.distance.to.pbo[1:10,c(1:6,9)]) %>%
  theme_zebra() %>%
  align_text_col(align = 'center') %>%
  align_nottext_col(align = 'center') %>%
  set_table_properties(layout = 'autofit') %>%
  bg(i = 1:10,
     j = 1:7,
     bg = 'white') %>%
  border_remove() %>%
  border_outer(part = 'body', border = officer::fp_border(width = 1, color = 'white')) %>%
  border_outer(part = 'header', border = officer::fp_border(width = 1)) %>%
  vline(j = 1:7, border = officer::fp_border(width = 1)) %>%
  hline(i = 1, border = officer::fp_border(width = 1), part = 'header') %>%
  fix_border_issues() %>%
  add_header_row(values = 'PBO and AmeriFlux (AMF) network proximity (top 10 nearest sites)',
                 colwidths = 7, top = TRUE) %>%
  hline_top(part = 'header', border = officer::fp_border(width = 1, color = 'white')) %>%
  vline_left(part = 'all', border = officer::fp_border(width = 1, color = 'white')) %>%
  vline_right(part = 'header', border = officer::fp_border(width = 1, color = 'white')) %>%
  fontsize(i = 1, part = 'header', size = 16) %>%
  fontsize(i = 2, part = 'header', size = 12) %>%
  fontsize(i = 1:10, part = 'body', size = 12) %>%
  bold(j = 1, bold = TRUE, part = 'body') %>%
  bg(part = 'header', bg = 'white') %>%
  bg(part = 'body', bg = 'white') %>%
  width(j = 1, width = 0.37, unit = 'in') %>%
  width(j = 2, width = 0.5, unit = 'in') %>%
  width(j = 6, width = 0.4, unit = 'in') %>%
  gen_grob(scaling = 'fixed', autowidths = FALSE)

# Combine map and table
map.table.combo = plot_grid(plotlist = list(pbo.map, amf.proximal.distance.to.pbo.table),
                            ncol = 1,
                            rel_heights = c(1,0.5),
                            labels = c('a)','b)'),
                            label_size = 17,
                            hjust = -0.25,
                            vjust = c(1.25,1))

# Save combo figure
ggsave(filename = paste(figs.fp, 'Maps', 'FigS4_PBO_AMF_Sites_Map_and_Table_with_temporal_overlap_filter.png', sep = '/'),
       map.table.combo,
       width = 8.5, height = 9,
       bg = 'white')
