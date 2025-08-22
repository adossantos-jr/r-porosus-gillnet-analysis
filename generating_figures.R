setwd('C:/Users/alexa/OneDrive/Documentos/shark_gillnet_paper')

# loading packages

pacman::p_load(readxl, ggplot2, rnaturalearth, terra,
               tidyterra, ggspatial, ggalt, ggtext, raster, marmap, 
               patchwork, gghalves)

# preparating data for study area mapping

## uploading data 

map_data = read_excel('map_data.xlsx')

# getting brazil shapefile

br = ne_states(returnclass = 'sf', country = 'Brazil')

# getting bathymetry data for study area

bathy = getNOAA.bathy(lon1 = -36, 
lon2 = -34,
lat1 = -9.5,
lat2 = -7,
res = 1, keep = F) %>%
  as.xyz() %>%
  subset(V3 > -70 & V3 < 1) %>%
  rasterFromXYZ() %>%
  rast()

## study area map
### map of brazil

br_map = 
  ggplot()+
  geom_sf(data = br, fill = 'floralwhite', color = 'grey50')+
  geom_text(aes(label = "Brazil", x=-50, y=-13), size = 8, color='black')+
  annotate('rect', xmin = -35.3, xmax = -34.5, ymin = -9.1, ymax = -7.5,
           fill = 'transparent', color = 'black', linewidth = .5) +
  theme_void()

### map of pernambuco

pe_map =
  ggplot(map_data)+
  geom_spatraster_contour_filled(data = bathy)+
  geom_sf(data = br, fill = 'floralwhite')+
  geom_point(aes(x = lon, y = lat), pch = 4, size = 2, alpha = .6)+
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(8, 'Greys')))+
  coord_sf(xlim = c(-35.3, -34.5), ylim = c(-9.1, -7.5))+
  theme_test()+
  labs(x = '', y = '', fill = 'Bathymetry (m)')+
  geom_text(aes(y = -8.1, x = -35.15, label = 'Pernambuco'),
            angle = 60, color = 'grey20')+
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', width = unit(0.6, 'cm'),
                         height = unit(1, 'cm'), 
                         style = north_arrow_fancy_orienteering())+
  scale_x_continuous(breaks = c(-35.2, -34.9, -34.6))+
  facet_wrap(~period)+
  theme(legend.position = 'right',
        axis.text = element_text(color = 'black'))+
  guides(fill = guide_colorsteps(barwidth = .5, barheight = 5))

(br_map + pe_map + plot_layout(widths = c(.7, 1))) %>%
 ggsave(filename = 'Figure_1.png',
       bg = 'white',
       dpi = 1000,
       h = 20/4,
       w = 30/4)


# preparating shark size data for analysis
## uploading 
 
shark_df = read_excel('size_data.xlsx')

# defining l50

l50 = (0.8565*60) - 2.3636 # check size_data.xlsx for fl by tl regression

# categorizing & preparating 

shark_df$cat = ifelse(shark_df$fl <= l50, 'Juvenile', 'Adult')

shark_wide = 
shark_df %>% group_by(date, lat, lon, period, depth,
                     bottom, effort) %>% count(cat) %>%
 pivot_wider(values_from = n, names_from = cat)

shark_wide[is.na(shark_wide)] = 0

shark_wide$totaln = shark_wide$Juvenile + shark_wide$Adult

shark_wide$per_juv = shark_wide$Juvenile/shark_wide$totaln

shark_wide$juv_cpue = (shark_wide$Juvenile/shark_wide$effort)*1000
shark_wide$tot_cpue = (shark_wide$totaln/shark_wide$effort)*1000


# mapping 

 ( ggplot(shark_wide)+
  geom_spatraster_contour_filled(data = bathy)+
  geom_sf(data = br, fill = 'floralwhite')+
  geom_point(aes(x = lon, y = lat, color = per_juv,
                 size = tot_cpue), alpha = .7)+
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(8, 'Greys')))+
  scale_color_gradientn(colors = c('blue', 'yellow', 'red3'))+
  coord_sf(xlim = c(-35.3, -34.5), ylim = c(-9.1, -7.5))+
  theme_test()+
  labs(x = '', y = '', color = 'Proportion of *R. porosus* juveniles',
       size = '*R. porosus* CPUE (nº/km²/h)', 
       fill = 'Bathymetry (m)')+
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', width = unit(0.6, 'cm'),
                         height = unit(1, 'cm'), 
                         style = north_arrow_fancy_orienteering())+
  scale_x_continuous(breaks = c(-35.2, -34.9, -34.6))+
  facet_wrap(~period)+
  theme(legend.position = 'right',
        legend.title = element_markdown(),
        axis.text = element_text(color = 'black'))+
  guides(fill = guide_colorsteps(barwidth = .5, barheight = 5),
         color = guide_colorbar(barwidth = .5,
                                barheight = 5)) ) %>%
 ggsave(filename = 'Figure_2.png',
       bg = 'white',
       dpi = 1000,
       h = 20/3.3,
       w = 20/3.3)


# plots of size by spatial variables 

fl_by_depth = 
 ggplot(shark_df, aes(x = depth, y = fl))+
  geom_point(alpha = .3)+
   geom_hline(yintercept = l50, color = 'red3', linetype = 'dashed')+
   geom_smooth(method = 'lm', se = F, color = 'black')+
  ggpmisc::stat_poly_eq()+
   theme_bw()+
   theme(axis.text = element_text(color = 'black'))+
   labs(x = 'Depth (m)', y = 'Fork length (cm)')+
   facet_wrap(~period)
 

fl_by_bottom =  
ggplot(shark_df, aes(y = fl, x =  bottom))+
  geom_half_boxplot(outliers = F, width = .3,
               fill = 'floralwhite')+
  geom_half_violin(fill = 'transparent', side = 'r')+
  geom_half_point_panel(alpha = .3, side = 'l')+
  geom_hline(yintercept = 54, color = 'red3', linetype = 'dashed')+
  theme_bw()+
  labs(x = 'Bottom type',y = 'Fork length (cm)')+
  theme(axis.text = element_text(color = 'black'))+
  facet_wrap(~period)

(fl_by_depth / fl_by_bottom + plot_annotation(tag_levels = 'a')) %>%
 ggsave(filename = 'Figure_3.png',
       bg = 'white',
       dpi = 1000,
       h = 18/3, w = 15/3)


