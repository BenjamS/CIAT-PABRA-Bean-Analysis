#setwd("D:/OneDrive - CGIAR/Documents")
library(tidyverse)
library(circlize)
#source('./FAOdat_createRegionGroups.R', echo = F)
#=======================================================================
shift_legend2 <- function(p) {
  # ...
  # to grob
  gp <- ggplotGrob(p)
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  
  # establish name of empty panels
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  names <- empty.facet.panels$name
  # example of names:
  #[1] "panel-3-2" "panel-3-3"
  
  # now we just need a simple call to reposition the legend
  #lemon::reposition_legend(p, 'center', panel=names)
  p_out <- lemon::reposition_legend(p, 'center', panel=names)
  #class(lemon::reposition_legend(p, 'center', panel=names))
  return(p_out)
}
#=======================================================================
FAOdat_createRegionGroups <- function(df_raw, exclude_these, keep_FAOregions = F,
                                      df_is_trade_matrix = F,
                                      consolidate_LAC = T,
                                      consolidate_WEur = T,
                                      consolidate_AusNZPacIslands = T,
                                      consolidate_SSA = T,
                                      output_countryVecs = F)
{
  #=================================
  #--Africa
  countries_NAfrica <- as.character(unique(read.csv("Country list - Northern Africa.csv")[,"Area"]))
  countries_MAfrica <- as.character(unique(read.csv("Country list - Middle Africa.csv")[,"Area"]))
  countries_WAfrica <- as.character(unique(read.csv("Country list - Western Africa.csv")[,"Area"]))
  countries_EAfrica <- as.character(unique(read.csv("Country list - Eastern Africa.csv")[,"Area"]))
  countries_SAfrica <- as.character(unique(read.csv("Country list - Southern Africa.csv")[,"Area"]))
  #--Americas
  countries_SAmer <- as.character(unique(read.csv("Country list - South America.csv")[,"Area"]))
  countries_CAmer <- as.character(unique(read.csv("Country list - Central America.csv")[,"Area"]))
  countries_Carib <- as.character(unique(read.csv("Country list - Caribbean.csv")[,"Area"]))
  countries_NAmer <- as.character(unique(read.csv("Country list - Northern America.csv")[,"Country"]))
  #--Asia
  countries_EAsia <- as.character(unique(read.csv("Country list - Eastern Asia.csv")[,"Area"]))
  #countries_EAsia <- countries_EAsia[!(countries_EAsia %in% c("China, Hong Kong SAR", "China, Macao SAR"))]
  countries_SEAsia <- as.character(unique(read.csv("Country list - South-Eastern Asia.csv")[,"Area"]))
  countries_SAsia <- as.character(unique(read.csv("Country list - Southern Asia.csv")[,"Area"]))
  countries_WAsia <- as.character(unique(read.csv("Country list - Western Asia.csv")[,"Area"]))
  countries_CAsia <- as.character(unique(read.csv("Country list - Central Asia.csv")[,"Area"]))
  #--Europe
  countries_NEurope <- as.character(unique(read.csv("Country list - Northern Europe.csv")[,"Area"]))
  countries_WEurope <- as.character(unique(read.csv("Country list - Western Europe.csv")[,"Area"]))
  countries_EEurope <- as.character(unique(read.csv("Country list - Eastern Europe.csv")[,"Area"]))
  countries_SEurope <- as.character(unique(read.csv("Country list - Southern Europe.csv")[,"Area"]))
  #--Oceania
  countries_Oceania <- as.character(unique(read.csv("Country list - Oceania.csv")[,"Area"]))
  countries_AusNZea <- c("Australia", "New Zealand")
  countries_PacifIs <- setdiff(countries_Oceania, countries_AusNZea)
  #=================================
  #--Create region groupings
  if(df_is_trade_matrix){
    u <- df_raw$Partner.Countries
  }else{
    u <- df_raw$Area
  }
  df_raw$Region <- NA
  df_raw$Region[which(u %in% countries_NAmer)] <- "North America"
  df_raw$Region[which(u %in% countries_SAmer)] <- "South America"
  df_raw$Region[which(u %in% countries_CAmer)] <- "Central America"
  df_raw$Region[which(u %in% countries_Carib)] <- "Caribbean"
  df_raw$Region[which(u %in% countries_NAfrica)] <- "Northern Africa"
  df_raw$Region[which(u %in% countries_SAfrica)] <- "Southern Africa"
  df_raw$Region[which(u %in% countries_WAfrica)] <- "Western Africa"
  df_raw$Region[which(u %in% countries_EAfrica)] <- "Eastern Africa"
  df_raw$Region[which(u %in% countries_MAfrica)] <- "Middle Africa"
  df_raw$Region[which(u %in% countries_CAsia)] <- "Central Asia"
  df_raw$Region[which(u %in% countries_WAsia)] <- "Western Asia"
  df_raw$Region[which(u %in% countries_SAsia)] <- "Southern Asia"
  df_raw$Region[which(u %in% countries_EAsia)] <- "Eastern Asia"
  df_raw$Region[which(u %in% countries_SEAsia)] <- "South-Eastern Asia"
  df_raw$Region[which(u %in% countries_NEurope)] <- "Northern Europe"
  df_raw$Region[which(u %in% countries_SEurope)] <- "Southern Europe"
  df_raw$Region[which(u %in% countries_WEurope)] <- "Western Europe"
  df_raw$Region[which(u %in% countries_EEurope)] <- "Eastern Europe"
  df_raw$Region[which(u %in% countries_PacifIs)] <- "Pacific Islands"
  df_raw$Region[which(u %in% countries_AusNZea)] <- "Australia & New Zealand"
  rm(u)
  #--------------------
  #--See what countries escaped designation
  #unique(df_raw$Area[which(is.na(df_raw$Region))])
  #--Assign these to their proper regions
  #(Leave out "China" as it is already covered under "China, mainlaind", "Hong Kong", etc.)
  if(df_is_trade_matrix){
    u <- df_raw$Partner.Countries
  }else{
    u <- df_raw$Area
  }
  df_raw$Region[which(u %in% c("Åland Islands", "Isle of Man", "Greenland"))] <- "Northern Europe"
  df_raw$Region[which(u %in% c("Anguilla", "Bermuda", "Cayman Islands", "Curaçao"))] <- "Caribbean"
  df_raw$Region[which(u %in% c("Côte d'Ivoire"))] <- "Western Africa"
  df_raw$Region[which(u %in% c("Palau"))] <- "Pacific Islands"
  df_raw$Region[which(u %in% c("Maldives", "Réunion"))] <- "Southern Asia"
  df_raw$Region[which(u %in% c("French Guiana"))] <- "South America"
  rm(u)
  #===========================
  #unique(df_raw$Area[which(is.na(df_raw$Region) == T)])
  if(!(df_is_trade_matrix)){
    ind_NA <- which(is.na(df_raw$Region) == T)
    ind_noNA <- which(is.na(df_raw$Region) == F)
    if(keep_FAOregions == F){
      df_raw <- df_raw[ind_noNA,]
    }else{
      df_raw$Region[ind_NA] <- df_raw$Area[ind_NA] 
    }
  }
  #===========================
  #--
  if(df_is_trade_matrix){
    df_raw <- subset(df_raw, !(Partner.Countries %in% exclude_these))
  }else{
    df_raw <- subset(df_raw, !(Area %in% exclude_these))
  }
  #--
  if(consolidate_LAC){
    LAC <- c("Central America", "Caribbean", "South America")
    u <- df_raw$Region
    df_raw$Region[which(u %in% LAC)] <- "LAC"
    rm(u)
  }
  if(consolidate_WEur){
    Europe_WNS <- c("Southern Europe", "Western Europe", "Northern Europe")
    u <- df_raw$Region
    df_raw$Region[which(u %in% Europe_WNS)] <- "W/N/S Europe"
    rm(u)
  }
  if(consolidate_AusNZPacIslands){
    AusNZPacIslands <- c("Australia & New Zealand", "Pacific Islands")
    u <- df_raw$Region
    df_raw$Region[which(u %in% AusNZPacIslands)] <- "Aus/NZ/Oceania"
    rm(u)
  }
  if(consolidate_SSA){
    SSA <- c("Eastern Africa", "Southern Africa", "Western Africa", "Middle Africa")
    u <- df_raw$Region
    df_raw$Region[which(u %in% SSA)] <- "Sub-Saharan Africa"
    rm(u)
  }
  
  # Europe_E <- "Eastern Europe"
  # ESE_Asia <- c("South-Eastern Asia", "Eastern Asia")
  #--
  # u <- df_raw$Region
  # df_raw$Region[which(u %in% LAC)] <- "LAC"
  # df_raw$Region[which(u %in% Europe_E)] <- "E. Europe"
  # df_raw$Region[which(u %in% ESE_Asia)] <- "E. & S.E. Asia"
  # rm(u)
  #--
  if(output_countryVecs){
    out_list <- list()
    out_list[["Northern Africa"]] <- countries_NAfrica
    out_list[["Middle Africa"]] <- countries_MAfrica
    out_list[["Western Africa"]] <- countries_WAfrica
    out_list[["Eastern Africa"]] <- countries_EAfrica
    out_list[["Southern Africa"]] <- countries_SAfrica
    out_list[["df_raw"]] <- df_raw
    return(out_list)
  }else{
    return(df_raw)
  }
  
}
# regionGroups_vec <- paste0("countries_",
#                            #--Africa
#                            c("NAfrica", "MAfrica",
#                              "WAfrica", "EAfrica",
#                              "SAfrica",
#                              #--Americas
#                              "SAmer", "CAmer",
#                              "Carib", "NAmer",
#                              #--Asia
#                              "EAsia",
#                              "SEAsia", "SAsia",
#                              "WAsia", "CAsia",
#                              #--Europe
#                              "NEurope", "EEurope",
#                              "WEurope", "SEurope",
#                              #--Oceania
#                              "Oceania", "AusNZea",
#                              "PacifIs"))
# #=================================
# countries_NAfrica <- as.character(unique(read.csv("Country list - Northern Africa.csv")[,"Area"]))
# 
# ind_load <- which(exists(regionGroups_vec[4]) == F)
# 
# "Northern Africa"
# "Middle Africa"
# "Western Africa"
# "Eastern Africa"
# "Southern Africa"
# #--Americas
# "South America"
# "Central America"
#==========================================================================
dir <- "C:/Users/bensc/OneDrive/Documents/"
this_folder <- "Data/FAO Data/"
this_file <- "FAO Beans Trade Matrix.csv"
this_filepath <- paste0(dir, this_folder, this_file)
#==========================================================================
df_raw <- read.csv(this_filepath, stringsAsFactors = F)
df_raw$X <- NULL
#colnames(df_raw)
colnames(df_raw)[6:ncol(df_raw)] <- gsub("X", "", colnames(df_raw)[6:ncol(df_raw)])
gathercols <- colnames(df_raw)[6:ncol(df_raw)]
df_raw <- df_raw %>% gather_("Year", "Value", gathercols)
#--------------------------------------------------------------------------
unique(df_raw$Partner.Countries[grep("Tanzania", df_raw$Partner.Countries)])
#--------------------------------------------------------------------------
setwd("C:/Users/bensc/OneDrive/Documents/Data/")
#--------------------------------------------------------------------------
out_list <- FAOdat_createRegionGroups(df_raw, exclude_these = NULL, keep_FAOregions = F,
                                    df_is_trade_matrix = T,
                                    consolidate_LAC = T,
                                    consolidate_WEur = T,
                                    consolidate_AusNZPacIslands = T,
                                    consolidate_SSA = F,
                                    output_countryVecs = T)
#-------------------------------------------------------------------------
setwd("C:/Users/bensc/OneDrive/Documents/Beans AVISA EIB CTEH PABRA/")
#-------------------------------------------------------------------------
df_raw <- out_list[["df_raw"]]
EAfrica_vec <- out_list[["Eastern Africa"]]
EAfrica_vec <- c(EAfrica_vec, "Mayotte")
WAfrica_vec <- out_list[["Western Africa"]]
NAfrica_vec <- out_list[["Northern Africa"]]
MAfrica_vec <- out_list[["Middle Africa"]]
SAfrica_vec <- out_list[["Southern Africa"]]
EAfrica_vec[grep("Tanzania", EAfrica_vec)] <- "Tanzania"
SAfrica_vec[grep("Eswatini", SAfrica_vec)] <- "Swaziland"
WAfrica_vec[grep("CÃ´te d'Ivoire", WAfrica_vec)] <- "Côte d'Ivoire"

Africa_vec <- c(EAfrica_vec, WAfrica_vec, NAfrica_vec, MAfrica_vec, SAfrica_vec)
SSA_vec <- c(EAfrica_vec, WAfrica_vec, MAfrica_vec, SAfrica_vec)
#unique(df_raw$Region)
#--------------------------------------------------------------------------
u <- df_raw$`Reporter.Countries`
df_raw$`Reporter.Countries`[grep("Tanzania", u)] <- "Tanzania"
df_raw$`Reporter.Countries`[which(u == "Eswatini")] <- "Swaziland"
df_raw$`Reporter.Countries`[which(u == "Belgium-Luxembourg")] <- "Belgium"
u <- df_raw$`Partner.Countries`
df_raw$`Partner.Countries`[grep("Tanzania", u)] <- "Tanzania"
df_raw$`Partner.Countries`[which(u == "Eswatini")] <- "Swaziland"
#df_raw$`Partner.Countries`[which(u == "Belgium-Luxembourg")] <- "Belgium"
df_raw <- subset(df_raw, Partner.Countries != "Belgium-Luxembourg")
df_raw$Region[which(df_raw$Partner.Countries == "Swaziland")] <- "Southern Africa"
df_raw$Region[which(df_raw$Partner.Countries %in% c("North Macedonia",
                                                    "Serbia and Montenegro"))] <- "W/N/S Europe"
df_raw$Region[which(df_raw$Partner.Countries == "Pitcairn Islands")] <- "Aus/NZ/Oceania"
df_raw$Region[which(df_raw$Partner.Countries == "Mayotte")] <- "Eastern Africa"
df_raw$Region[which(df_raw$Partner.Countries == "Palestine")] <- "Western Asia"

df_raw$Year <- as.integer(df_raw$Year)

#unique(df$Region)
#----------------------------------------------------------------------------
df_check <- subset(df_raw, Year %in% as.character(c(2013:2017)) &
                     Reporter.Countries %in% SSA_vec &
                     Element == "Export Quantity")
df_check$Value[which(is.na(df_check$Value))] <- 0

df_check <- df_check %>% group_by(Reporter.Countries) %>%
  summarise(Value = sum(Value, na.rm = T))
#unique(df_check$Reporter.Countries)
thresh <- quantile(df_check$Value, probs = 0.77)
these_exporters <- df_check$Reporter.Countries[which(df_check$Value >= thresh)]
df_check$x <- NA
df_check$x[which(df_check$Reporter.Countries %in% these_exporters)] <- "Top Exporters"
df_check$x[which(is.na(df_check$x))] <- "The rest"
df_check <- df_check %>% group_by(x) %>% summarise(Value = sum(Value))
df_check <- df_check %>% spread(x, Value)
pct_of_allTrade <- df_check$`Top Exporters` / (df_check$`The rest` + df_check$`Top Exporters`)
pct_of_allTrade
#----------------------------------------------------------------------------
df <- subset(df_raw, Reporter.Countries %in% these_exporters &
               Element == "Export Quantity" &
               Year >= 2000)
df$Value[which(is.na(df$Value))] <- 0

# df <- as.data.frame(df %>% group_by(Reporter.Countries, Year) %>% mutate(totval_yr = sum(Value, na.rm = T)))
# df$ValueShare <- df$Value / df$totval_yr
# df$Region[which(df$ValueShare < 0.05)] <- "Residual"
# df$Region[which(df$Region == "Unspecified Area")] <- "Residual"
# 
# df <- df %>% group_by(Reporter.Countries, Partner.Countries) %>%
#   mutate(n_times = length(which(Value > 0))) %>%
#   as.data.frame()
# 
# u <- df$n_times
# df$Region[which(u < 3)] <- "Sporadic"
# rm(u)
# 
# df$Region[which(df$Region %in% c("Sporadic", "Residual"))] <- "Residual/Sporadic"

ESEasia_vec <- c("South-Eastern Asia", "Eastern Asia")
df$Region[which(df$Region %in% ESEasia_vec)] <- "E./S.E. Asia"
RoW_vec <- c("North America", "Central Asia", "LAC", "Western Asia", "Aus/NZ/Oceania")
df$Region[which(df$Region %in% RoW_vec)] <- "Rest of World"
df$Region[grep("Europe", df$Region, ignore.case = T)] <- "Europe"
df$Region[grep("Africa", df$Region, ignore.case = T)] <- "Africa"
unique(df$Partner.Countries[which(is.na(df$Region))])
df$Region[which(is.na(df$Region))] <- "Rest of World"

colnames(df)[2] <- "Destination"

df_circle <- df %>% group_by(Reporter.Countries, Region, Year) %>%
  summarise(Value = sum(Value, na.rm = T)) %>%
  as.data.frame()

#----------------------------------------------------------------------------
#unique(df$Region)
df_plot <- subset(df_circle, Year %in% as.character(2013:2017))
df_plot <- df_plot %>% group_by(Reporter.Countries, Region) %>%
  summarise(Value = sum(Value, na.rm = T)) %>% as.data.frame()
#df_plot <- df_plot[which(df_plot$Value > 1000), ]
#hist(log(df_plot$Value))
#length(which(df_plot$Value < 50))
df_plot$Value <- df_plot$Value / 1000
colnames(df_plot)[ncol(df_plot)] <- "Bean exports ('000 metric tons)"
#df_plot$Value <- log(df_plot$Value)
# n <- length(unique(df_plot$Reporter.Countries))
# bag_of_colors <- randomcoloR::distinctColorPalette(k = n)
# these_colors <- sample(bag_of_colors, n)
these_colors <- c("#006EB6", "#5C9AD2", "#8BC53F", "#00954C", "#CAD32B",
                  "#F5D226", "#FBB040", "#F68B33", "#BB3A26", "#92278F",
                  "#92278F", "#163E70", "#414042", "#A4A3A3")
names(these_colors) <- unique(df_plot$Reporter.Countries)
png("FAO SSA bean trade chord diagram.png",
    width = 8, height = 8, units = "in", res = 400)
chordDiagram(df_plot, grid.col = these_colors, directional = 1, 
             direction.type = c("diffHeight", "arrows"),
             link.arr.type = "big.arrow",
             link.lwd = 1, link.lty = 1, link.border = "black",
             annotationTrack = "grid",
             preAllocateTracks = list(track.height = 0.3))
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)
mytitle1 = "Sub-Saharan Africa Bean Exports ('000 Metric Tons), 2013-2017"
mytitle2 = "(Accounts for 95% of SSA bean exports during period)"
mytitle3 = "FAO data"
mtext(side=3, line = -1, adj=0, cex=1.2, mytitle1)
mtext(side=3, line = -2, adj=0, cex=1, mytitle2)
mtext(side=3, line = -3, adj=0, cex=1, mytitle3)
dev.off()
circos.clear()
#===========================================================================
#===========================================================================
#===========================================================================
#===========================================================================
#===========================================================================
#===========================================================================
df <- subset(df_raw, Reporter.Countries %in% these_exporters &
               Element == "Export Quantity" &
               Year >= 2005)
df$Value[which(is.na(df$Value))] <- 0

#setdiff(WAfrica_vec, unique(df$Partner.Countries[which(df$Region == "Western Africa")]))
#unique(df$Partner.Countries[which(is.na(df$Region))])
#unique(df$Region)
ESEasia_vec <- c("South-Eastern Asia", "Eastern Asia")
df$Region[which(df$Region %in% ESEasia_vec)] <- "E./S.E. Asia"
RoW_vec <- c("North America", "Central Asia", "LAC", "Aus/NZ/Oceania")
df$Region[which(df$Region %in% RoW_vec)] <- "Rest of World"
df$Region[grep("Europe", df$Region, ignore.case = T)] <- "Europe"
df$Region[which(is.na(df$Region))] <- "Rest of World"
colnames(df)[2] <- "Destination"


df_plot1 <- df[, c("Reporter.Countries", "Destination", "Region", "Year", "Value")]
u <- df_plot1$Region
df_plot1$Region[which(u == "Northern Africa")] <- "Mid. East/N. Africa"
df_plot1$Region[which(u == "Western Asia")] <- "Mid. East/N. Africa"
df_plot1$Region[which(u == "Western Africa")] <- "West Africa"
df_plot1$Region[which(u == "Eastern Africa")] <- "East Africa"
df_plot1$Destination <- df_plot1$Region
df_plot1 <- df_plot1 %>% group_by(Reporter.Countries, Destination, Year) %>%
  summarise(Value = sum(Value, na.rm = T)) %>% as.data.frame()
df_plot2 <- df_plot1 %>% group_by(Destination, Year) %>%
  summarise(Value = sum(Value, na.rm = T)) %>% as.data.frame()
df_plot2$Reporter.Countries <- "All SSA top bean exporters"
df_plot2 <- df_plot2[, c("Reporter.Countries", "Destination", "Year", "Value")]
df_plot <- as.data.frame(rbind(df_plot1, df_plot2))
#df_plot <- subset(df_plot, !(Destination %in% c("Unspecified", "Aus/NZ/Oceania")))
#unique(df_plot$Destination[which(df_plot$Value > 500)])
df_plot$Value <- df_plot$Value / 1000
colnames(df_plot)[ncol(df_plot)] <- "Bean exports ('000 metric tons)"

# n <- length(unique(df_plot$Destination))
# bag_of_colors <- randomcoloR::distinctColorPalette(k = 2 * n)
# these_colors <- sample(bag_of_colors, n)
these_colors <- c("#8BC53F", "#F68B33", "#006EB6", "#CAD32B", "#A4A3A3",
                  "#00954C", "#FBB040", "#5C9AD2", "#F5D226")

gg <- ggplot(df_plot, aes(x = Year, y = `Bean exports ('000 metric tons)`,
                          fill = Destination))
gg <- gg + geom_area(position = "stack")
gg <- gg + scale_fill_manual(values = these_colors)
gg <- gg + scale_x_continuous(breaks = seq(2005, 2017, 2))
gg <- gg + facet_wrap(~Reporter.Countries, ncol = 3, scales = "free_y")
gg <- gg + theme(axis.title.x = element_blank(),
                 legend.position = "bottom",
                 legend.title = element_blank())
#gg <- gg + guides(fill = guide_legend(nrow = 5))
gg <- gg + labs(title = "Where are the beans going?",
                subtitle = "Top bean exporters in Sub-Saharan Africa (accounts for 95% of total SSA bean exports)",
                caption = "Source: FAO")
# gg <- shift_legend2(gg)
# gg <- ggplotify::as.ggplot(gg)
gg
ggsave("FAO Where are the beans going.png", width = 9, height = 5.5, dpi = 400)
#===========================================================================
#===========================================================================
#===========================================================================
#===========================================================================
#===========================================================================
FEWS_exporters <- c("Zambia", "Tanzania",
                    "South Africa", "Mozambique", "Malawi")
FEWS_importers <- c("Zambia", "Tanzania", "Zimbabwe",
                    "Democratic Republic of the Congo",
                    "South Africa", "Mozambique", "Malawi")
df_compareFEWS <- subset(df_raw, Reporter.Countries %in% FEWS_exporters &
                           Partner.Countries %in% FEWS_importers &
                           Element == "Export Quantity" &
                           Year == 2015)
df_compareFEWS <- df_compareFEWS[which(!is.na(df_compareFEWS$Value)), ]
df_plot <- df_compareFEWS[, c("Reporter.Countries", "Partner.Countries", "Value")]
df_plot$Value <- as.numeric(df_plot$Value)
df_plot$Partner.Countries[grep("Congo", df_plot$Partner.Countries)] <- "DRC"
#df_plot$Value <- df_plot$Value / 1000
#df_plot$Value <- log(df_plot$Value)
n <- length(unique(df_plot$Reporter.Countries))
bag_of_colors <- randomcoloR::distinctColorPalette(k = 2 * n)
these_colors <- sample(bag_of_colors, n)
names(these_colors) <- unique(df_plot$Reporter.Countries)
png("Compare FAO and FEWS bean trade data.png", width = 8, height = 8, units = "in", res = 300)
chordDiagram(df_plot,grid.col = these_colors, directional = 1,
             direction.type = c("diffHeight", "arrows"),
             link.arr.type = "big.arrow",
             link.lwd = 1, link.lty = 1, link.border = "black",
             annotationTrack = "grid",
             preAllocateTracks = list(track.height = 0.4))
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)
#title(main = list("2017 E. African Bean Trade Flows (FAO data)", cex = 0.8))
dev.off()
circos.clear()