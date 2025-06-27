library(raster)
library(terra)
library(sf)
library(ggplot2)
library(ggpubr)
library(grid)
library(gridExtra)
library(cowplot)

setwd('../../')
root.fp = getwd()
data.fp = paste(root.fp, 'Data', sep = '/')
figs.fp = paste(root.fp, 'Figures', sep = '/')

shp.files = list.files(paste(data.fp, 'Shapefiles', 'Site_Geometries', sep = '/'), pattern = '.shp', full.names = TRUE)

site.areas = terra::vect(shp.files[4])
site.points = terra::vect(shp.files[5])

lpdr.pix.areas = terra::vect(shp.files[1])
lprm.pix.areas = terra::vect(shp.files[2])
mtdca.pix.areas = terra::vect(shp.files[3])

wkg.pix.areas = rbind(lpdr.pix.areas[lpdr.pix.areas$SITE_ID == 'WKG',],
                      lprm.pix.areas[lprm.pix.areas$SITE_ID == 'WKG',],
                      mtdca.pix.areas[mtdca.pix.areas$SITE_ID == 'WKG',])

wkg.areas = site.areas[site.areas$SITE_ID == 'WKG',]
wkg.points = site.points[site.points$SITE_ID == 'WKG',]

# ------- RAP VegCover files
rap.fp = paste(data.fp, 'RAP_Site_VegCover', sep = '/')
rap.vc.21 = stack(list.files(rap.fp, pattern = '2021', full.names = TRUE, recursive = FALSE))

# ------- RAP VegCover classes:
# AFG = Annual forb and grass cover
# BGR = Bare ground
# LTR = Litter
# PFG = Perennial forb and grass cover
# SHR = Shrub cover
# TRE = Tree cover
names(rap.vc.21) = c('AFG','BGR','LTR','PFG','SHR','TRE')

# ------- WKG
wkg.lpdr.rap.21 = crop(rap.vc.21, st_as_sf(wkg.pix.areas[wkg.pix.areas$Product == 'LPDR_25km',]))
wkg.lprm.rap.21 = crop(rap.vc.21, st_as_sf(wkg.pix.areas[wkg.pix.areas$Product == 'LPRM_10km',]))
wkg.mtdca.rap.21 = crop(rap.vc.21, st_as_sf(wkg.pix.areas[wkg.pix.areas$Product == 'MTDCA_09km',]))

rap.vod.plots.fun = function(lpdr.rap, lprm.rap, mtdca.rap, site.name, site.pix.areas, site.points, year) {
  
  # --- LPDR (25 km)
  # AFG
  lpdr.afg = as.data.frame(as(lpdr.rap[[1]], 'SpatialPixelsDataFrame'))
  colnames(lpdr.afg) = c('PctPixel','x','y')
  lpdr.afg$VegCover = 'AFG'
  # BGR
  lpdr.bgr = as.data.frame(as(lpdr.rap[[2]], 'SpatialPixelsDataFrame'))
  colnames(lpdr.bgr) = c('PctPixel','x','y')
  lpdr.bgr$VegCover = 'BGR'
  # LTR
  lpdr.ltr = as.data.frame(as(lpdr.rap[[3]], 'SpatialPixelsDataFrame'))
  colnames(lpdr.ltr) = c('PctPixel','x','y')
  lpdr.ltr$VegCover = 'LTR'
  # PFG
  lpdr.pfg = as.data.frame(as(lpdr.rap[[4]], 'SpatialPixelsDataFrame'))
  colnames(lpdr.pfg) = c('PctPixel','x','y')
  lpdr.pfg$VegCover = 'PFG'
  # SHR
  lpdr.shr = as.data.frame(as(lpdr.rap[[5]], 'SpatialPixelsDataFrame'))
  colnames(lpdr.shr) = c('PctPixel','x','y')
  lpdr.shr$VegCover = 'SHR'
  # TRE
  lpdr.tre = as.data.frame(as(lpdr.rap[[6]], 'SpatialPixelsDataFrame'))
  colnames(lpdr.tre) = c('PctPixel','x','y')
  lpdr.tre$VegCover = 'TRE'
  
  lpdr.rap.df = rbind(lpdr.afg, lpdr.bgr, lpdr.ltr,
                             lpdr.pfg, lpdr.shr, lpdr.tre)
  lpdr.rap.df$Product = 'LPDR_25km'
  
  # --- LPRM (10 km)
  # AFG
  lprm.afg = as.data.frame(as(lprm.rap[[1]], 'SpatialPixelsDataFrame'))
  colnames(lprm.afg) = c('PctPixel','x','y')
  lprm.afg$VegCover = 'AFG'
  # BGR
  lprm.bgr = as.data.frame(as(lprm.rap[[2]], 'SpatialPixelsDataFrame'))
  colnames(lprm.bgr) = c('PctPixel','x','y')
  lprm.bgr$VegCover = 'BGR'
  # LTR
  lprm.ltr = as.data.frame(as(lprm.rap[[3]], 'SpatialPixelsDataFrame'))
  colnames(lprm.ltr) = c('PctPixel','x','y')
  lprm.ltr$VegCover = 'LTR'
  # PFG
  lprm.pfg = as.data.frame(as(lprm.rap[[4]], 'SpatialPixelsDataFrame'))
  colnames(lprm.pfg) = c('PctPixel','x','y')
  lprm.pfg$VegCover = 'PFG'
  # SHR
  lprm.shr = as.data.frame(as(lprm.rap[[5]], 'SpatialPixelsDataFrame'))
  colnames(lprm.shr) = c('PctPixel','x','y')
  lprm.shr$VegCover = 'SHR'
  # TRE
  lprm.tre = as.data.frame(as(lprm.rap[[6]], 'SpatialPixelsDataFrame'))
  colnames(lprm.tre) = c('PctPixel','x','y')
  lprm.tre$VegCover = 'TRE'
  
  lprm.rap.df = rbind(lprm.afg, lprm.bgr, lprm.ltr,
                             lprm.pfg, lprm.shr, lprm.tre)
  lprm.rap.df$Product = 'LPRM_10km'
  
  # --- MTDCA (09 km)
  # AFG
  mtdca.afg = as.data.frame(as(mtdca.rap[[1]], 'SpatialPixelsDataFrame'))
  colnames(mtdca.afg) = c('PctPixel','x','y')
  mtdca.afg$VegCover = 'AFG'
  # BGR
  mtdca.bgr = as.data.frame(as(mtdca.rap[[2]], 'SpatialPixelsDataFrame'))
  colnames(mtdca.bgr) = c('PctPixel','x','y')
  mtdca.bgr$VegCover = 'BGR'
  # LTR
  mtdca.ltr = as.data.frame(as(mtdca.rap[[3]], 'SpatialPixelsDataFrame'))
  colnames(mtdca.ltr) = c('PctPixel','x','y')
  mtdca.ltr$VegCover = 'LTR'
  # PFG
  mtdca.pfg = as.data.frame(as(mtdca.rap[[4]], 'SpatialPixelsDataFrame'))
  colnames(mtdca.pfg) = c('PctPixel','x','y')
  mtdca.pfg$VegCover = 'PFG'
  # SHR
  mtdca.shr = as.data.frame(as(mtdca.rap[[5]], 'SpatialPixelsDataFrame'))
  colnames(mtdca.shr) = c('PctPixel','x','y')
  mtdca.shr$VegCover = 'SHR'
  # TRE
  mtdca.tre = as.data.frame(as(mtdca.rap[[6]], 'SpatialPixelsDataFrame'))
  colnames(mtdca.tre) = c('PctPixel','x','y')
  mtdca.tre$VegCover = 'TRE'
  
  mtdca.rap.df = rbind(mtdca.afg, mtdca.bgr, mtdca.ltr,
                              mtdca.pfg, mtdca.shr, mtdca.tre)
  mtdca.rap.df$Product = 'MTDCA_09km'
  
  # ------- Prepare data for plotting
  # Mosaic LPDR, LPRM, and MTDCA RAP maps
  rap.mosaic = googletraffic::gt_mosaic(list(lpdr.rap, lprm.rap, mtdca.rap))
  rap.mosaic.df = as.data.frame(as(rap.mosaic, 'SpatialPixelsDataFrame'))
  colnames(rap.mosaic.df) = c(c('AFG','BGR','LTR','PFG','SHR','TRE'),'x','y')
  rap.mosaic.df = reshape2::melt(rap.mosaic.df, id.vars = c('x','y'), variable.name = 'VegCover', value.name = 'PctPixel')
  
  # Combine LPDR, LPRM, and MTDCA RAP data frames
  rap.df = rbind(lpdr.rap.df, lprm.rap.df, mtdca.rap.df)
  
  
  # ------- Plots
  rap.barplot = ggplot(rap.df, aes(x = VegCover, y = PctPixel, color = Product)) +
    geom_boxplot() +
    ylim(c(0,100)) +
    xlab('RAP Vegetation Cover Class') +
    ylab('Percentage of Pixel') +
    labs(title = site.name) +
    theme_bw() +
    theme(legend.position = 'bottom',
          legend.title = element_text(face = 'bold'))
  
  rap.map = ggplot(data = rap.mosaic.df) +
    geom_raster(aes(x = x, y = y, fill = PctPixel)) +
    scale_fill_stepsn(colors = c('ivory','palegreen4','darkgreen','orange','tomato'),
                      breaks = seq(0,100,10),
                      limits = c(0,100),
                      guide = guide_colorbar(frame.colour = 'black', ticks = FALSE)) +
    geom_sf(data = st_as_sf(site.pix.areas), aes(color = Product), fill = NA, linewidth = 0.75) +
    geom_sf(data = st_as_sf(site.points), shape = 4, stroke = 2, color = 'red', size = 5, inherit.aes = FALSE) +
    guides(color = 'none') + # Removes pixel areas from plot legend
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    xlab('') +
    ylab('') +
    facet_grid(.~VegCover) +
    theme(legend.key.height = unit(1, 'cm'),
          legend.key = element_rect(color = 'black'),
          legend.title = element_text(face = 'bold'),
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          strip.text.x = element_text(size = 13, face = 'bold'))
  
  plots = grid.arrange(rap.barplot,
                              rap.map,
                              ncol = 1,
                              heights = c(1.2,1))
  
  return(plots)
}

wkg.rap.plots.21 = rap.vod.plots.fun(wkg.lpdr.rap.21, wkg.lprm.rap.21, wkg.mtdca.rap.21,
                                     'Walnut Gulch Kendall Grasslands (WKG)',
                                     wkg.pix.areas, wkg.points)

# ggsave(filename = 'WKG_RAP_VegCover_PixelAreas_2021.png',
#        path = paste(figs.fp, 'Maps', sep = '/'),
#        plot = wkg.rap.plots.21,
#        width = 11, height = 6, units = 'in')

# -----------------------------------------------------------------------------------------------------
# ----------- Convert 3D RAP stacks to 2D maps represending majority pixel coverage of individual PFts
# -----------------------------------------------------------------------------------------------------

rap.major.pft.map = function(rap) {
  
  # Get spatial info
  rap.crs = crs(rap)
  rap.ext = extent(rap)

  nrows = dim(rap)[1]
  ncols = dim(rap)[2]

  # Get RAP PFT layer names
  rap.pfts = names(rap)

  # Set up 2D map output
  output.map = rap[[1]]
  output.map[] = NA
  output.map = as.matrix(output.map)

  # Convert RAP raster stack to 3D array
  rap.array = as.array(rap)

  # Loop through rows/columns to get percent pixel coverage of each PFT vegetation cover class
  for (row in 1 : nrows){
    for (col in 1 : ncols){

      # Get vector of %pixel of each PFT (see index below) at specifc row+col
      # Index = 1-6; 1=AFG, 2=BGR, 3=LTR, 4=PFG, 5=SHR, 6=TRE
      pfts = rap.array[row,col,]

      # Get the index value of the major PFT (i.e. maximum %pixel coverage)
      major.pft = which(pfts == max(pfts, na.rm = TRUE), arr.ind = TRUE)

      # If there are two or more PFTs with the same %pixel coverage, randomly select one and assign the index value to output pixel
      if (length(major.pft) > 1){
        output.map[row,col] = major.pft[sample(length(major.pft),1)]
      } else {output.map[row,col] = major.pft}
    }
  }

  # Rasterize 2D output map and assign spatial info
  output.map = raster(output.map)
  crs(output.map) = rap.crs
  extent(output.map) = rap.ext

  return(output.map)
}

wkg.lpdr.major.pfts = rap.major.pft.map(wkg.lpdr.rap.21)
wkg.lprm.major.pfts = rap.major.pft.map(wkg.lprm.rap.21)
wkg.mtdca.major.pfts = rap.major.pft.map(wkg.mtdca.rap.21)

# Set RAP class names and colors
rap.class.ids = names(wkg.lpdr.rap.21)

rap.class.names = c('Annual forbs\n and grasses',
                    'Bare ground',
                    'Litter',
                    'Perennial forbs\n and grasses',
                    'Shrub',
                    'Tree')

rap.class.cols = c('thistle',
                   'khaki1',
                   '#ffbb22',
                   'forestgreen',
                   'sienna1',
                   'dodgerblue')

rap.class.info = data.frame(Index = seq(1,6,1),
                            IDs = rap.class.ids,
                            Names = rap.class.names,
                            Colors = rap.class.cols)

names(rap.class.cols) = rap.class.names

# Get spatial data for LC map inset
states.shp = map_data('state')
az.shp = states.shp[states.shp$region == 'arizona',][,1:2]
colnames(az.shp) = c('x','y')
az.poly = az.shp %>%
  st_as_sf(coords = c('x','y'), crs = '+proj=longlat +datum=NAD83 +no_defs') %>%
  dplyr::summarise(geometry = st_combine(geometry)) %>%
  st_cast('POLYGON')

az.cities = rbind(data.frame(x = -110.955733,
                             y = 32.243548, 
                             name = 'Tucson'),
                  data.frame(x = -112.070669,
                             y = 33.461279,  
                             name = 'Phoenix'),
                  data.frame(x =  -111.651093,
                             y = 35.195540,
                             name = 'Flagstaff'))

az.cities.sf = st_as_sf(az.cities, coords = c('x','y'), crs = '+proj=longlat +datum=NAD83 +no_defs')


# ----------- Create function to generate dominant PFT map for site
rap.major.pft.map.fun = function(rap, pix.areas, site.point, site.name, inset.xy.position) {
  
  rap.df = as.data.frame(as(rap, 'SpatialPixelsDataFrame'))
  names(rap.df) = c('IDs','x','y')
  rap.df = rap.df[order(rap.df$IDs, decreasing = FALSE),]
  rap.df$Names = NA
  for (i in 1 : length(rap.class.ids)) {rap.df[rap.df$IDs %in% i,4] = rap.class.names[i]}
  
  # RAP map
  rap.map = ggplot(data = rap.df) +
    geom_raster(aes(x = x, y = y, fill = Names), show.legend = FALSE) +
    scale_fill_manual(values = rap.class.cols, name = 'Cover Class') +
    geom_sf(data = st_as_sf(pix.areas), aes(color = Product), fill = NA, linewidth = 1.25, inherit.aes = FALSE) +
    scale_color_manual(values = c('red2','turquoise','steelblue'),
                       labels = c('LPDR 25km', 'LPRM 10km', 'SMAP-DCA 09km'),
                       guide = 'legend') +
    geom_sf(data = st_as_sf(site.point), shape = 4, stroke = 2, color = 'black', size = 5, inherit.aes = FALSE) +
    guides(color = guide_legend(override.aes = list(fill = NA), 
                                title = 'VOD product pixel footprints',
                                title.position = 'top',
                                title.hjust = 0.5)) +
    xlab('') +
    ylab('') +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(title = site.name, subtitle = '2021 Rangeland Analysis Platform (RAP) dominant plant functional type (PFT) per pixel') +
    theme_minimal() +
    ggspatial::annotation_scale(location = 'bl', width_hint = 0.5, text_cex = 1.2) +
    ggspatial::annotation_north_arrow(style = ggspatial::north_arrow_minimal(text_face = 'bold'), location = 'tr') +
    theme(plot.title = element_text(size = 20),
          plot.subtitle = element_text(size = 15),
          axis.ticks = element_line(color = 'black'),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          legend.position = 'bottom',
          #legend.background = element_rect(fill = NA),
          legend.box.spacing = unit(-0.25, 'cm'),
          legend.text = element_text(size = 13),
          legend.title = element_text(size = 14, face = 'bold'),
          aspect.ratio = 0.9)
  
  rap.map.legend = get_plot_component(rap.map + theme(plot.margin = unit(c(0,0,0,0), unit = 'mm')), 'guide-box-bottom')
  
  # AZ inset map
  inset.map = ggplot(data = az.poly) +
    geom_sf(linewidth = 1, show.legend = FALSE) +
    geom_sf(data = st_as_sf(site.point), shape = 4, stroke = 1, color = 'black', size = 2, inherit.aes = FALSE) +
    geom_sf(data = az.cities.sf, shape = 21, color = 'black', fill = 'black', size = 1, inherit.aes = FALSE) +
    geom_text(data = az.cities, aes(x = x, y = y+0.45, label = name), size = 3, fontface = 'bold', inherit.aes = FALSE) +
    xlab('') +
    ylab('') +
    ggtitle('Site Location') +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 12, face = 'bold', margin = margin(0,0,0,0)))
  
  # Combine maps
  final.map = ggdraw(rap.map + theme(legend.position = 'none')) +
    draw_plot(
      { inset.map },
      x = inset.xy.position[1], y = inset.xy.position[2],
      width = 0.3, height = 0.3
    )
  
  return(list(final.map, rap.map.legend))
}

wkg.rap.major.pft.map = rap.major.pft.map.fun(wkg.lpdr.major.pfts,
                                              wkg.pix.areas,
                                              wkg.points,
                                              'Walnut Gulch Experimental Watershed, Kendall Grasslands (WKG)',
                                              c(0.67,0.1))


# ----------- Create function to compute fraction of dominant PFTs within 09km, 10km, and 25km VOD pixel footprints
pft.data.table.fun = function(lpdr, lprm, mtdca) {
  
  pft.info = rap.class.info
  
  # Convert major PFT maps to data frames
  ras.to.spdf = function(ras, VOD_Prod_Name) {
    ras.spdf = as.data.frame(as(ras, 'SpatialPixelsDataFrame'))
    colnames(ras.spdf) = c('PFT_ID','x','y')
    ras.spdf$VOD_Prod = VOD_Prod_Name
    ras.spdf = ras.spdf[order(ras.spdf$PFT_ID, decreasing = FALSE),]
    return(ras.spdf)
  }
  
  df.25km = ras.to.spdf(lpdr, 'LPDR_25km')
  df.10km = ras.to.spdf(lprm, 'LPRM_25km')
  df.09km = ras.to.spdf(mtdca, 'SMAP_DCA_09km')
  
  # Get total number of pixels per VOD footprint area
  tot.npix.25km = nrow(df.25km)
  tot.npix.10km = nrow(df.10km)
  tot.npix.09km = nrow(df.09km)
  
  # Get total number of pixels per LC class per VOD product
  pft.count.25km = as.data.frame(table(df.25km$PFT_ID))
  pft.count.10km = as.data.frame(table(df.10km$PFT_ID))
  pft.count.09km = as.data.frame(table(df.09km$PFT_ID))
  
  # Compute fraction of LC class per total footprint area per VOD product 
  pft.fraction.fun = function(df, tot.npix) {
    colnames(df) = c('PFT_ID','Freq')
    df$PFT_Fraction = NA
    for (i in 1 : nrow(df)) {
      df$PFT_Fraction[i] = round(((df$Freq[i] / tot.npix) * 100), 2)
    }
    return(df)
  }
  
  pft.frac.25km = pft.fraction.fun(pft.count.25km, tot.npix.25km)
  pft.frac.10km = pft.fraction.fun(pft.count.10km, tot.npix.10km)
  pft.frac.09km = pft.fraction.fun(pft.count.09km, tot.npix.09km)
  
  # Extract vectors of unique LC IDs per VOD product
  pft.in.25km = as.numeric(as.character(pft.frac.25km$PFT_ID))
  pft.in.10km = as.numeric(as.character(pft.frac.10km$PFT_ID))
  pft.in.09km = as.numeric(as.character(pft.frac.09km$PFT_ID))
  
  # Complete data frames for higher-resolution VOD products which contain fewer LC classes than larger 25km footprint area
  complete.pft.df.fun = function(pft.frac.in, pft.in) {
    pft.diff = setdiff(pft.in.25km, pft.in)
    pft.diff.df = data.frame(PFT_ID = pft.diff,
                             Freq = rep(0,length(pft.diff)),
                             PFT_Fraction = rep(0,length(pft.diff)))
    pft.diff.df$PFT_ID = factor(pft.diff.df$PFT_ID, levels = pft.diff)
    pft.frac.out = rbind(pft.frac.in, pft.diff.df)
    return(pft.frac.out)
  }
  
  pft.frac.10km = complete.pft.df.fun(pft.frac.10km, pft.in.10km)
  pft.frac.09km = complete.pft.df.fun(pft.frac.09km, pft.in.09km)
  
  # Get LC class names and colors that correspond with LC IDs
  pft.class.names = pft.info[pft.info$Index %in% pft.in.25km,3]
  pft.class.cols = pft.info[pft.info$Index %in% pft.in.25km,4]
  
  # Combine data frames
  pft.frac.df = data.frame(PFT_ID = as.numeric(as.character(pft.frac.25km$PFT_ID)),
                           PFT_Name = pft.class.names,
                           LPDR_25km = pft.frac.25km$PFT_Fraction,
                           LPRM_10km = pft.frac.10km$PFT_Fraction,
                           SMAP_DCA_09km = pft.frac.09km$PFT_Fraction)
  
  pft.frac.df = pft.frac.df[,2:5]
  
  # Set PFT data frame column names to factors
  colnames(pft.frac.df) = factor(colnames(pft.frac.df), 
                                 levels = colnames(pft.frac.df), 
                                 labels = c('PFT', 'LPDR 25km', 'LPRM 10km', 'SMAP-DCA 09km'))
  
  pft.table = ggtexttable(pft.frac.df, 
                         rows = NULL, 
                         theme = ttheme('minimal',
                                        tbody.style = tbody_style(fill = pft.class.cols),
                                        colnames.style = colnames_style(color = 'white', fill = 'black'))) %>%
    table_cell_font(row = 2:7, column = 1, face = 'bold') %>%
    tab_add_title(text = 'Fraction of total pixel area per VOD product', face = 'bold', size = 16) %>%
    tbody_add_border(from.row = 2, to.row = 8,
                     from.column = 1, to.column = 4,
                     linewidth = 2.5)
    
  
  return(pft.table)
}

wkg.rap.pft.table = pft.data.table.fun(wkg.lpdr.major.pfts, wkg.lprm.major.pfts, wkg.mtdca.major.pfts)

# ------------ Combine RAP PFT maps and tables into single figure
wkg.rap.pft.map.table.rightcol = plot_grid(NULL,
                                           as_grob(wkg.rap.pft.table),
                                           wkg.rap.major.pft.map[[2]],
                                           ncol = 1,
                                           rel_heights = c(1,1,1))

wkg.rap.pft.map.table = plot_grid(wkg.rap.major.pft.map[[1]] + theme(legend.position = 'none'),
                                  wkg.rap.pft.map.table.rightcol,
                                  ncol = 2,
                                  rel_widths = c(1,1),
                                  align = 'h',
                                  axis = 'b')

ggsave(filename = paste(figs.fp, 'Maps', 'Fig01a_WKG_RAP_Major_PFT_Map_Table.png', sep = '/'),
       wkg.rap.pft.map.table,
       bg = 'white',
       width = 10.5, height = 5.5, units = 'in')
