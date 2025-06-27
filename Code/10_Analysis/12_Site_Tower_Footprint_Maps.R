library(raster)
library(sf)
library(ggplot2)

code.fp = getwd()
setwd('../../')
gitdata.fp = paste(getwd(), 'Data', sep = '/')
figs.fp = paste(getwd(), 'Figures', sep = '/')
shp.fp = paste(gitdata.fp, 'Shapefiles', 'Site_Geometries', sep = '/')

# Get site geometry shapefiles
shp.files = list.files(shp.fp, pattern = '.shp', full.names = TRUE)
site.centerpoints = raster::shapefile(shp.files[5])
wkg.point = site.centerpoints[site.centerpoints$SITE_ID == 'WKG',]

# Get flux footprint geoTIFFs
ffp.fp = paste(gitdata.fp, 'Flux_Footprints', sep = '/')
ffp.files = list.files(ffp.fp, full.names = TRUE)

wkg.2018.07.day = raster(ffp.files[1])
wkg.2018.08.day = raster(ffp.files[3])
wkg.2018.09.day = raster(ffp.files[5])
wkg.2018.10.day = raster(ffp.files[7])

ext.xmin = min(extent(wkg.2018.07.day)[1],extent(wkg.2018.08.day)[1],extent(wkg.2018.09.day)[1],extent(wkg.2018.10.day)[1])
ext.xmax = max(extent(wkg.2018.07.day)[2],extent(wkg.2018.08.day)[2],extent(wkg.2018.09.day)[2],extent(wkg.2018.10.day)[2])
ext.ymin = min(extent(wkg.2018.07.day)[3],extent(wkg.2018.08.day)[3],extent(wkg.2018.09.day)[3],extent(wkg.2018.10.day)[3])
ext.ymax = max(extent(wkg.2018.07.day)[4],extent(wkg.2018.08.day)[4],extent(wkg.2018.09.day)[4],extent(wkg.2018.10.day)[4])

ext.global = extent(ext.xmin,ext.xmax,ext.ymin,ext.ymax)

wkg.2018.07.day = extend(wkg.2018.07.day, ext.global)
wkg.2018.08.day = extend(wkg.2018.08.day, ext.global)
wkg.2018.09.day = extend(wkg.2018.09.day, ext.global)
wkg.2018.10.day = extend(wkg.2018.10.day, ext.global)

wkg.ffp.2018.summer = brick(wkg.2018.07.day,wkg.2018.08.day,wkg.2018.09.day,wkg.2018.10.day)
wkg.ffp.2018.summer[wkg.ffp.2018.summer == 0] = NA

wkg.ffp.2018.summer.mean = mean(wkg.ffp.2018.summer, na.rm = TRUE)

wkg.point.df = st_as_sf(data.frame(lat = wkg.point@coords[2],
                                   lon = wkg.point@coords[1]),
                        coords = c('lon','lat'),
                        crs = crs(wkg.ffp.2018.summer.mean))

buffer.50m = st_buffer(wkg.point.df, dist = 50)
buffer.100m = st_buffer(wkg.point.df, dist = 100)
buffer.150m = st_buffer(wkg.point.df, dist = 150)
buffer.200m = st_buffer(wkg.point.df, dist = 200)
buffer.250m = st_buffer(wkg.point.df, dist = 250)
buffer.300m = st_buffer(wkg.point.df, dist = 300)

wkg.ffp.2018.summer.mean.df = as.data.frame(as(wkg.ffp.2018.summer.mean, 'SpatialPixelsDataFrame'))
colnames(wkg.ffp.2018.summer.mean.df) = c('MeanSensingArea','x','y')

buffer.labs.df = rbind(data.frame('x' = median(st_coordinates(buffer.50m)[,1]),
                                  'y' = max(st_coordinates(buffer.50m)[,2]-0.0001),
                                  'label' = '50m'),
                       data.frame('x' = median(st_coordinates(buffer.50m)[,1]),
                                  'y' = max(st_coordinates(buffer.100m)[,2]-0.0001),
                                  'label' = '100m'),
                       data.frame('x' = median(st_coordinates(buffer.50m)[,1]),
                                  'y' = max(st_coordinates(buffer.150m)[,2]-0.0001),
                                  'label' = '150m'),
                       data.frame('x' = median(st_coordinates(buffer.50m)[,1]),
                                  'y' = max(st_coordinates(buffer.200m)[,2]-0.0001),
                                  'label' = '200m'),
                       data.frame('x' = median(st_coordinates(buffer.50m)[,1]),
                                  'y' = max(st_coordinates(buffer.250m)[,2]-0.0001),
                                  'label' = '250m'))

# Plot mean flux sensing footprint and tower radii
wkg.ffp.map = ggplot(data = wkg.ffp.2018.summer.mean.df) +
  geom_raster(aes(x = x, y = y, fill = MeanSensingArea)) +
  scale_fill_gradientn(#colors = RColorBrewer::brewer.pal(7, 'Oranges'),
                       colors = RColorBrewer::brewer.pal(7, 'YlOrRd'),
                       limits = c(min(wkg.ffp.2018.summer.mean[],na.rm = TRUE),max(wkg.ffp.2018.summer.mean[],na.rm = TRUE)),
                       breaks = c(min(wkg.ffp.2018.summer.mean[],na.rm = TRUE),max(wkg.ffp.2018.summer.mean[],na.rm = TRUE)),
                       labels = c('Low','High'),
                       name = 'Mean sensing density') +
  geom_sf(data = st_as_sf(wkg.point), shape = 4, stroke = 2, size = 5, inherit.aes = FALSE) +
  geom_sf(data = st_as_sf(buffer.50m), fill = NA, linewidth = 1) +
  geom_sf(data = st_as_sf(buffer.100m), fill = NA, linewidth = 1) +
  geom_sf(data = st_as_sf(buffer.150m), fill = NA, linewidth = 1) +
  geom_sf(data = st_as_sf(buffer.200m), fill = NA, linewidth = 1) +
  geom_sf(data = st_as_sf(buffer.250m), fill = NA, linewidth = 1) +
  geom_text(data = buffer.labs.df,
            aes(x = x, y = y, label = label),
            size = 7, fontface = 'bold', color = 'white', inherit.aes = FALSE) +
  geom_text(data = buffer.labs.df,
            aes(x = x, y = y, label = label),
            size = 7, inherit.aes = FALSE) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  xlab('') +
  ylab('') +
  theme_bw() +
  ggtitle(label = 'Mean flux footprint sensing density (JUN - OCT 2018)',
          subtitle = 'Walnut Gulch Experimental Watershed - Kendall Grassland (WKG)') +
  theme(legend.title = element_blank(),
        #legend.title = element_text(size = 17, face = 'bold', hjust = 0.5),
        #legend.title.position = 'top',
        legend.text = element_text(size = 15),
        legend.key.width = unit(3,'cm'),
        legend.position = 'bottom',
        plot.title = element_text(size = 20, face = 'bold'),
        plot.subtitle = element_text(size = 17),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(angle = 90, hjust = 0.5))

ggsave(filename = paste(figs.fp, 'Maps', 'Fig01b_WKG_FFP_Summer2018.png', sep = '/'),
       wkg.ffp.map,
       width = 8, height = 7.5, bg = 'white')
