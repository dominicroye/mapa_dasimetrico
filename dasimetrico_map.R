# packages
library(tidyverse)
library(sf)
library(readxl)
library(biscale)
library(patchwork)
library(raster)

# raster de CORINE LAND COVER 2018
urb <- raster("u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif")

# limites censales del INE
limits <- read_sf("SECC_CE_20200101.shp") 
# limites municipales CNIG
mun_limit <- st_read("au_AdministrativeUnit_4thOrder0.gml")

# filter a CCAA Madrid
limits <- filter(limits, NCA == "Comunidad de Madrid")
limits_ccaa <- group_by(limits, CUMUN) %>% summarise() # limite CCAA

# crop + mask Land Cover
urb_mad <- crop(urb, st_transform(limits, projection(urb))) %>% mask(st_transform(limits, projection(urb)))

# elimino pixeles no urbanos (1 es urbano continuo y 2 es discontinuo)
urb_mad[!urb_mad %in% 1:2] <- NA 

# proyectar 
urb_mad <- projectRaster(urb_mad, crs = CRS("+proj=longlat +datum=WGS84 +no_defs") )

# tansform a xyz y sf object
urb_mad <- as.data.frame(urb_mad, xy = TRUE, na.rm = TRUE)
urb_mad <- st_as_sf(urb_mad, coords = c("x", "y"), crs = 4326)

urb_mad <- urb_mad %>% rename(urb = 1) %>% cbind(st_coordinates(urb_mad))


## datos renta y gini INE
renta <- read_excel("30824.xlsx")
renta <- mutate(renta, NATCODE = str_extract(CUSEC, "[0-9]{5,10}"), 
                nc_len = str_length(NATCODE))
renta_muni <- filter(renta, nc_len > 5) %>% 
         mutate(mun_name = str_remove(CUSEC, NATCODE)%>%str_trim())

gini <- read_excel("37677.xlsx")
gini <- mutate(gini, NATCODE = str_extract(CUSEC, "[0-9]{5,10}"), 
               nc_len = str_length(NATCODE))
gini_muni <- filter(gini, nc_len > 5) %>% 
        mutate(mun_name = str_remove(CUSEC, NATCODE)%>%str_trim())


# join renta y gini
mad <- left_join(limits, renta, by = c("CUSEC"="NATCODE")) %>% 
          left_join(gini_muni, by = c("CUSEC"="NATCODE"))

# data columns to numeric
mad <- mutate_at(mad, c(23:27, 30:31), as.numeric)


## crear clasificación bivariante
databi2 <- bi_class(mad, GINI_2017, RNMP_2017, style = "quantile", dim = 3) %>% 
             mutate(bi_class = ifelse(str_detect(bi_class, "NA"), NA, bi_class))


## redistribuir los pixeles urbanos de CORINE a la desigualdad
temp <- st_join(urb_mad, st_transform(databi2, 4326))

# leyenda bivariante
legend2 <- bi_legend(pal = "DkViolet",
                     dim = 3,
                     xlab = "Más desigual",
                     ylab = "Más renta",
                     size = 9)



library(ragg) #rstudio device cambia a ragg en Global Options/Graphics RStudio version mas actual
#descarga de fuente
sysfonts::font_add_google("Montserrat", "Montserrat")

p1 <- ggplot(temp) + 
  geom_tile(aes(X, Y, 
                fill = bi_class), 
            show.legend = FALSE) +
  geom_sf(data = limits_ccaa,  
          color = "grey80", 
          fill = NA, 
          size = 0.2) +
  annotation_custom(ggplotGrob(legend2), 
                    xmin = -3.25, xmax = -2.65,
                    ymin = 40.55, ymax = 40.95) +
  bi_scale_fill(pal = "DkViolet", 
                dim = 3, 
                na.value = "grey90") +
  bi_theme() +
  labs(x = "", 
       y = "", 
       title = "dasimétrico") +
  coord_sf(crs = 4326, 
           xlim = c(-4.5, -3.1), 
           ylim = c(39.8, 41.2))

p2 <- ggplot(databi2) + 
  geom_sf(aes(fill = bi_class), 
          colour = NA, 
          size = .1, 
          show.legend = FALSE) +
  geom_sf(data = filter(mun_limit, text != "Madrid"),  
          color = "white", 
          fill = NA, 
          size = 0.2) +
  bi_scale_fill(pal = "DkViolet", 
                dim = 3, 
                na.value = "grey90") +
  labs(title = "coroplético") +
  bi_theme() +
  coord_sf(crs = 4326, 
           xlim = c(-4.5, -3.1),
           ylim = c(39.8, 41.2)) +
  theme(plot.title = element_text(family = "Montserrat", size = 30, face = "bold"),
        plot.subtitle = element_text(family = "Montserrat", size = 20, face = "plain"),
        plot.caption = element_text(family = "Montserrat", size = 12))


# combinar ambos mapas

p <- p2 | p1


# exportar
ggsave("dasimetrico.tiff", p, dpi = 400)


