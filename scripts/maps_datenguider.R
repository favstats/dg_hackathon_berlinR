# Use Cases with MAPS
#*******************
## install the following packages:
# devtools::install_github("CorrelAid/datenguideR")
if(!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, datenguideR, sf, magrittr, tmap)

# 1) Download spatial Data für Nuts1 & Nuts2
# 2) Create static map with sf and tmap
# 3) Create animated map for time series

#*******************************************************
# DOWNLOAD SPATIAL DATA ----
#*******************************************************

# Bundesländer - Germany NUTS1 level
#***********************
# Spatial data is downloaded from https://gadm.org/download_country_v3.html
# Please check their licensing for further (commercial) use of the data.

# Germany data for NUTS1 Level, data comes as sf object 
# More information of spatial data handling with sf: https://r-spatial.github.io/sf/articles/sf1.html
d_nuts1 <- readRDS(url('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_DEU_1_sf.rds'))
class(d_nuts1) # sf object is a data.frame with a geometry column 
d_nuts1 # Longitude-Latitude WGS 84 Coordinate System, not projected; with st_transform() you can transform/reproject the CRS
plot(st_geometry(d_nuts1)) # plot only spatial extents

# optionally drop unrelevant columns
d_nuts1 %<>% 
  dplyr::select(NAME_1, VARNAME_1) # geometry column is "sticky" and will never be dropped unless you force it

# Germany level NUTS2
#***********************
# for the finer NUTS2 level I recommend the download of the shapefiles (about 12MB) provided 
# by the Bundesamt für Kartographie und Geodäsie (Federal Agency for Cartography and Geodesy)
# https://gdz.bkg.bund.de/index.php/default/open-data.html?p=2

temp <- tempfile(fileext = ".zip")
download.file('https://daten.gdz.bkg.bund.de/produkte/vg/nuts250_1231/aktuell/nuts250_12-31.utm32s.shape.zip',
              temp)
unzip(temp)
d_nuts2 <- sf::st_read('nuts250_2018-12-31.utm32s.shape/nuts250/250_NUTS2.shp')
plot(st_geometry(d_nuts2))


# Germany level NUTS3 - not recommended...
#*************
# folder also has shapefile for nuts3 level, however join with statistical data will be very difficult because of inconsistent names
#d_nuts3 <- st_read('nuts250_2018-12-31.utm32s.shape/nuts250/250_NUTS3.shp')
#plot(st_geometry(d_nuts3))


#*******************************************************
# CREATE STATIC MAP----
#*******************************************************

#***************
# BIG COMPANIES
#***************
# data about company size
big_companies <- dg_call(year = c(2011),
                         stat_name = 'BETR01', 
                         substat_name = 'BESGK6', 
                         parameter = 'BESGKL10', # extracts companies with >= 1000 employees
                         nuts_nr = 2) %>% 
  dplyr::distinct() # remove duplicate rows (don't know where they come from...)

# optionally drop unrelevant columns
big_companies %<>%
  dplyr::select(id, year, name, comps_bigger1000 = value, substat_description, param_description)

# delete part of the names for merge with sptial data
big_companies$name %<>%
  stringr::str_remove_all(", Regierungsbezirk") %>% 
  stringr::str_remove_all(", Stat. Region")

# merge spatial and thematic data
companies_sp <- merge(d_nuts2, big_companies, by.x = "NUTS_NAME", by.y = "name")


# with sf
#************
# you can adjust graphical parameters with plotting options
plot(companies_sp['comps_bigger1000'], main = "Number of companies >=1000 employees")

# with t_map
#************
p_load(tmap)
#vignette("tmap-getstarted")
# useful resource beside the vignette: https://geocompr.robinlovelace.net/adv-map.html#fig:urban-animated

# define bounding box of map (make more)
bb <- st_bbox(companies_sp) 
bb[1] <- bb[1] - (0.15 * (bb$xmax - bb$xmin)) # add some space on the left ergghm in the west ;-)

companies <- 
  tm_shape(companies_sp, bbox = bb) +
  tm_fill("comps_bigger1000", title = "Number of companies with 
>= 1000 employees", breaks = c(0, 7, 14, 21, 28, 35, 48)) +
  tm_borders() +
  tm_style("cobalt") + # there are a few pre-defined style types available
  tm_layout(legend.position = c("left","top")) 
 
# export map as png
tmap_save(companies, "bigcomps_map.png", dpi = 150)


#***************
# HEATING
#***************
heating <- dg_call(year = c(2017),
                   stat_name = 'BAU017', # Baugenehmigungen für Wohngebäude
                   substat_name = 'BAUPHE', # Primär verwendete Heizenergie
                   nuts_nr = 1) 

heating$name %<>%
  stringr::str_replace('Baden-Württemberg, Land', 'Baden-Württemberg')

heating$param_description %<>%
  stringr::str_replace_all(fixed(" "), "") %>% 
  stringr::str_replace_all("[/()]", "_") 

heating  %<>% dplyr::select(id, year, name, nmbr = value, param_description) %>% 
  tidyr::pivot_wider(names_from = param_description, values_from = nmbr) %>% 
  dplyr::mutate(
    sustain_rel = (Geothermie + Biogas_Biomethan + KeineEnergie_einschl.Passivhaus_ 
                   + Holz + SonstigeBiomasse + Umweltthermie_Luft_Wasser_ + Solarthermie)/Insgesamt)
# aggregate sustainable heating energy sources (my fast and not very elaborated choice of what is sustainable...)

# merge with spatial data
heating_sp <- merge(d_nuts1, heating, by.x = "NAME_1", by.y = "name")

bb <- st_bbox(heating_sp) 
bb[3] <- bb[3] + (0.15 * (bb$xmax - bb$xmin))

sust_heat_map <- tm_shape(heating_sp) +
  tm_fill("sustain_rel", title = "") +
  tm_borders() +
  tm_style("natural") +
  tm_layout(panel.show = T,
            panel.labels = "Anteil der Nutzung von nachhaltigen Heizenergiequellen 
bei Baugenehmigungen für Wohngebäude 2017",
            panel.label.bg.color = "gray90",
            panel.label.height = 2.5)

# export map as png
tmap_save(sust_heat_map, "sustainableheat_map.png")

#*******************************************************
# ANIMATED MAP FOR TIME SERIES----
#*******************************************************

# DE-DOMAINS
#***************
de_doms <- dg_call(year = c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
                            2012, 2013, 2014, 2015, 2016, 2017, 2018),
                      stat_name = 'DEDOM1',
                      nuts_nr = 2)

de_doms$name %<>%
  stringr::str_remove_all(", Regierungsbezirk") %>% 
  stringr::str_remove_all(", Stat. Region")

de_doms %<>%
  dplyr::select(id, year, de_doms = value, name)

de_doms_sp <- merge(d_nuts2, de_doms, by.x = "NUTS_NAME", by.y = "name")
#plot(st_geometry(de_doms_sp))
bb <- st_bbox(de_doms_sp) 
bb[3] <- bb[3] + (0.15 * (bb$xmax - bb$xmin))

doms_anim <- tm_shape(de_doms_sp, bbox = bb) +
  tm_fill("de_doms", title = "Number of .de Domains") +
  tm_borders() +
  tm_facets(along = "year", free.coords = FALSE) 

# for export of animations you need to install ImageMagick
tmap_animation(doms_anim, filename = "dedomains.gif", width = 600, dpi = 80, delay = 150)


#************
# !!!Check the tmap package for further options, e.g. tm_symbols to add symbols and 
# further information to your map!!!

