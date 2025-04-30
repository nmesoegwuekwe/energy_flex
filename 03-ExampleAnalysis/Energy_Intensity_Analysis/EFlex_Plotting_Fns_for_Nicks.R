# Plotting functions for Nick's Spatial Analysis

# Load packages 
library(geojsonio)
library(sp)
library(broom)
library(ggplot2)
library(dplyr)
library(mapproj)
library(rgeos)
library(rgdal)
library(ggmap)
library(jsonlite)
library(wesanderson)
library(cowplot)

# Set directory to GitHub Repo Folder
setwd("/Users/apn30/Documents/GitHub/energy_flex/02-EnergyModel/Energy_Intensity_Analysis")

# Load csv's from Nick's Analysis
LAD_df_nick_clean <- sf::st_set_crs(LAD_df_nick_clean, 4277)
LAD_df_nick_clean <- sf::st_transform(LAD_df_nick_clean, 4326)

GIOrd <- read.csv(file="EnergyFlex_GIOrd.csv")
MGWR <- read.csv(file="EnergyFlex_MGWR.csv")
MGWR_non_sig <- read.csv(file="EnergyFlex_MGWR_nonsig.csv")

GIOrd_LAD <- left_join(LAD_df_nick_clean, GIOrd, by="local_authority")
GIOrd_LAD_sig <- filter(GIOrd_LAD, Gs_sig == "True")
GIOrd_LAD_nsig <- filter(GIOrd_LAD, Gs_sig != "True")
# Create region bounding boxes
map <- get_stamenmap(bbox = c(left = -6.5, bottom = 49.8, right = 1.8, top = 55.9), zoom = 7, maptype = "toner")
ggmap(map) + theme_void()


# Create Plotting fn

EFlex_Nationwide_Plot <- function(efdf, efdfns){
  # London
  efdf_london <- filter(efdf, grepl("E09",efdf$local_authority))
  London_bb = sf::st_as_sfc(sf::st_bbox(efdf_london))
  London_bb = sf::st_buffer(London_bb, dist = 0.05)
  
  # Manchester
  efdf_manx <- filter(efdf, efdf$local_authority %in% c("E06000009","E08000033","E08000015","E07000037"))
  Manx_bb = sf::st_as_sfc(sf::st_bbox(efdf_manx))
  Manx_bb = sf::st_buffer(Manx_bb, dist = 0.05)
  
  eigg1 <- ggplot()+ #ggmap(map) +
    geom_sf(data=efdfns, aes(group = local_authority), fill="grey90", colour="white",  size=0.05, alpha = 0.25, inherit.aes = FALSE) +
    geom_sf(data=efdf, aes(group = local_authority, fill=Zs), colour="white", size=0.05, alpha = 0.25, inherit.aes = FALSE) +
    geom_sf(data = London_bb, fill=NA, color="red", size=5, inherit.aes = FALSE)+
    geom_sf(data = Manx_bb, fill=NA, color="red", size=5,inherit.aes = FALSE)+
    #geom_point(data = cdf_df, aes(x=cdf_df$lon_real, y=cdf_df$lat_real, colour=hdd), size=5) +
    #scale_fill_gradientn(colours = rev(MetBrewer::met.brewer("Hiroshige", type="continuous"))[3:10],na.value="white",  name = "GI* Energy Intensity Values")+
    scale_fill_gradient2(low=rev(MetBrewer::met.brewer("Hiroshige", type="continuous"))[8], mid = "white", high = rev(MetBrewer::met.brewer("Hiroshige", type="continuous"))[3], na.value="white",  name = "GI* Values for \nEnergy Intensity")+
    # scale_fill_gradient2(low = rev(MetBrewer::met.brewer("Hiroshige", 19))[5],
    #                      mid = rev(MetBrewer::met.brewer("Hiroshige", 19))[11], 
    #                      high = rev(MetBrewer::met.brewer("Hiroshige", 19))[18],
    #                      midpoint=0,
    #                      na.value="white", name = "Energy \nIntensity")+
    # #scale_alpha(limits=c(20,100), breaks=2, name = "Energy Intensity")+
    #facet_wrap(~age, nrow=2) +
    theme_void() + 
    theme(legend.position=c(1.15, 0.7), legend.title = element_text(size=10, hjust=0),
          plot.margin = unit(c(0,0,0,-2), "cm"))+ guides(fill= guide_colourbar(barwidth = 1, barheight = 10, title.position = "top"))+
    coord_sf()
  
  eigg2 <-   ggplot()+ #ggmap(map) +
    geom_sf(data=efdfns, aes(group = local_authority), fill="grey90", colour="white",  size=0.05, alpha = 0.25, inherit.aes = FALSE) +
    geom_sf(data=efdf, aes(group = local_authority, fill=Zs), colour="white", alpha=0.25, size=0.05, inherit.aes = FALSE) +
    #geom_point(data = cdf_df, aes(x=cdf_df$lon_real, y=cdf_df$lat_real, colour=hdd), size=5) +
    #scale_fill_gradientn(colours = rev(MetBrewer::met.brewer("Hiroshige", type="continuous"))[3:10],na.value="white",  name = "GI* Energy Intensity Values")+
    scale_fill_gradient2(low=rev(MetBrewer::met.brewer("Hiroshige", type="continuous"))[8], mid = "white", high = rev(MetBrewer::met.brewer("Hiroshige", type="continuous"))[3], na.value="white",  name = "GI* Values for \nEnergy Intensity")+
    #scale_alpha(limits=c(20,100), breaks=2, name = "Energy Intensity")+
    #facet_wrap(~age, nrow=2) +
    labs(
      title = "Greater London"
    ) +
    theme_void() +  theme(legend.position="none",plot.title = element_text(size=11, hjust=0))+
    
    coord_sf(xlim = sf::st_bbox(London_bb)[c(1, 3)],
             ylim = sf::st_bbox(London_bb)[c(2, 4)])
  
  eigg3 <-   ggplot()+ #ggmap(map) + 
    geom_sf(data=efdfns, aes(group = local_authority), fill="grey90", colour="white", size=0.05, alpha = 0.25, inherit.aes = FALSE) +
    geom_sf(data=efdf, aes(group = local_authority, fill=Zs), colour="white", size=0.05,  alpha=0.25, inherit.aes = FALSE) +
    #geom_point(data = cdf_df, aes(x=cdf_df$lon_real, y=cdf_df$lat_real, colour=hdd), size=5) +
    #scale_fill_gradientn(colours = rev(MetBrewer::met.brewer("Hiroshige", type="continuous"))[3:10],na.value="white",  name = "GI* Energy Intensity Values")+
    scale_fill_gradient2(low=rev(MetBrewer::met.brewer("Hiroshige", type="continuous"))[8], mid = "white", high = rev(MetBrewer::met.brewer("Hiroshige", type="continuous"))[3], na.value="white",  name = "GI* Values for \nEnergy Intensity")+
    #scale_alpha(limits=c(20,100), breaks=2, name = "Energy Intensity")+
    #facet_wrap(~age, nrow=2) +
    labs(
      title = "Liverpool & Manchester"
    ) +
    theme_void() +  theme(legend.position="none",plot.title = element_text(size=11, hjust=1))+
    
    coord_sf(xlim = sf::st_bbox(Manx_bb)[c(1, 3)],
             ylim = sf::st_bbox(Manx_bb)[c(2, 4)])
  
  gg_inset_map1 = ggdraw() +
    draw_plot(eigg1) +
    draw_plot(eigg2, x = 0.7, y = 0.05, width = 0.25, height = 0.25) +
    draw_plot(eigg3, x = 0.05, y = 0.45, width = 0.3, height = 0.3)
 
  return(gg_inset_map1) 
}





# Now to plot the MGWR

MGWR_LAD <- left_join(LAD_df_nick_clean, MGWR, by="local_authority")
MGWR_LAD_NS <- left_join(MGWR_non_sig, LAD_df_nick_clean, by="local_authority")

EFlex_Nationwide_MGWR_Plot <- function(efdf, efdfns, yvar, title){

  eigg1 <- ggplot()+ #ggmap(map) +
    geom_sf(data=filter(efdf, !(local_authority %in% unique(efdfns$local_authority))), aes_string(group = "local_authority", fill=paste(yvar)), colour="white", size=0.05, alpha = 0.25, inherit.aes = FALSE) +
    geom_sf(data=efdfns, aes(geometry = geometry.y, group = local_authority), fill="grey90", colour="white",  size=0.05, alpha = 0.25, inherit.aes = FALSE) +
    #geom_point(data = cdf_df, aes(x=cdf_df$lon_real, y=cdf_df$lat_real, colour=hdd), size=5) +
    #scale_fill_gradientn(colours = rev(MetBrewer::met.brewer("Hiroshige", type="continuous"))[3:10],na.value="white",  name = "GI* Energy Intensity Values")+
    scale_fill_gradient2(low=rev(MetBrewer::met.brewer("Hiroshige", type="continuous"))[8], mid = "white", high = rev(MetBrewer::met.brewer("Hiroshige", type="continuous"))[3], na.value="grey50", name="")+
    # scale_fill_gradient2(low = rev(MetBrewer::met.brewer("Hiroshige", 19))[5],
    #                      mid = rev(MetBrewer::met.brewer("Hiroshige", 19))[11], 
    #                      high = rev(MetBrewer::met.brewer("Hiroshige", 19))[18],
    #                      midpoint=0,
    #                      na.value="white", name = "Energy \nIntensity")+
    # #scale_alpha(limits=c(20,100), breaks=2, name = "Energy Intensity")+
    #facet_wrap(~age, nrow=2) +
    theme_void() + 
    ggtitle(paste(title))+
    theme(legend.position=c(1.1, 0.5), legend.title = element_blank(), plot.title = element_text(hjust=0.5, size = 14),
          plot.margin = unit(c(0,0,0,-2), "cm"))+ guides(fill= guide_colourbar(barwidth = 1, barheight = 30))+
    coord_sf()
  
  
  return(eigg1) 
}

EFlex_Nationwide_MGWR_Plot(MGWR_LAD, MGWR_LAD_NS, "mgwr_intercept", "MGWR Intercept (Bandwidth = 35.0)")
EFlex_Nationwide_MGWR_Plot(MGWR_LAD, MGWR_LAD_NS, "mgwr_heating_degree_days", "Heating Degree Days (Bandwidth = 294.0)")
EFlex_Nationwide_MGWR_Plot(MGWR_LAD, MGWR_LAD_NS, "mgwr_income_before_housing", "Income before Housing (Bandwidth = 292.0)")
EFlex_Nationwide_MGWR_Plot(MGWR_LAD, MGWR_LAD_NS, "mgwr_imd_mean_score", "IMD Mean Score (Bandwidth = 180.0)")


