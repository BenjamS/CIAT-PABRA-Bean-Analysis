dir <- "C:/Users/bensc/OneDrive/Documents/"
this_folder <- "Beans AVISA EIB CTEH PABRA/"
this_wd <- paste0(dir, this_folder)
setwd(this_wd)

shape_vec <- c(21:24, 4)
point_size <- 1.5
smallPoint_size <- 1
label_size <- 2.5
smallLabel_size <- 2
title_size <- 8
subtitle_size <- 7
legendText_size <- 7
axisText_size <- 6
axisTitle_size <- 7
facetTitle_size <- 7


this_file <- "Bennetts Law FAO FBS and WDI data.csv"
df_bennett <- read.csv(this_file, stringsAsFactors = F)
df_bennett$Year <- as.integer(df_bennett$Year)
u <- colnames(df_bennett)
colnames(df_bennett) <- gsub("\\.\\.\\.", "), ", u)
u <- colnames(df_bennett)
colnames(df_bennett) <- gsub("P\\.c", "P/c", u)
u <- colnames(df_bennett)
colnames(df_bennett) <- gsub("\\.\\.", " (", u)
u <- colnames(df_bennett)
colnames(df_bennett) <- gsub("\\.", ")", u)
u <- colnames(df_bennett)
colnames(df_bennett) <- gsub("n\\)", "n ", u)
u <- colnames(df_bennett)
# colnames(df_bennett) <- gsub("\\.", " ", u)
# u <- colnames(df_bennett)
# colnames(df_bennett) <- gsub("s ", "s)", u)

# Slovakia an outlier with >50% fat share of diet. Rest of countries <44%
df_bennett <- subset(df_bennett, Area != "Slovakia")
#============================================================================
# Diets are diverse plot
# df_diets <- df_bennett[, c("Area", "Year", "Element", "Item", "Value")]
# df_diets <- subset(df_diets, Element == "Food supply (kcal/capita/day)" &
#                      !(Item %in% c("Animal fats", "Meat")))
# #unique(df_diets$Element)
# #unique(df_diets$Item)
# df_tot <- subset(df_diets[, c("Area", "Year", "Item", "Value")], Item == "Grand Total")
# df_diets <- subset(df_diets, Item != "Grand Total")
# colnames(df_tot)[which(colnames(df_tot) == "Value")] <- "Total (kcal/capita/day)"
# df_tot$Item <- NULL
# df_diets$Item[grep("Cereals", df_diets$Item)] <- "Cereals"
# df_diets$Item[grep("Sugar", df_diets$Item)] <- "Sugar\n& sweeteners"
# df_diets$Item[grep("Animal", df_diets$Item)] <- "Animal\nproducts"
# df_diets$Item[grep("Roots", df_diets$Item)] <- "Starchy\nroots"
# df_diets$Item[grep("Fruit", df_diets$Item)] <- "Fruits\n& Vegetables"
# df_diets$Item[grep("Veget", df_diets$Item)] <- "Fruits\n& Vegetables"
# df_diets <- df_diets %>% 
#   group_by(Area, Year, Item) %>%
#   summarise(Value = sum(Value, na.rm = T)) %>% as.data.frame()
# df_diets <- merge(df_diets, df_tot, by = c("Area", "Year"))
# colnames(df_diets)[which(colnames(df_diets) == "Value")] <- "Food supply (kcal/capita/day)"
# df_diets$`Share of daily diet` <- df_diets$`Food supply (kcal/capita/day)` / df_diets$`Total (kcal/capita/day)`
# df_other <- df_diets %>% group_by(Area, Year, `Total (kcal/capita/day)`) %>%
#   summarise(`Share of daily diet` = sum(`Share of daily diet`, na.rm = T)) %>% as.data.frame()
# df_other$`Share of daily diet` <- 1 - df_other$`Share of daily diet`
# df_other$`Food supply (kcal/capita/day)` <- df_other$`Share of daily diet` * df_other$`Total (kcal/capita/day)`
# df_other$Item <- "Other"
# df_other <- df_other[, colnames(df_diets)]
# df_diets <- rbind(df_diets, df_other)
# #-----------------------------------------------------------------------------
# area_vec <- c("Kenya", "Uganda", "Ethiopia", "Tanzania", "Rwanda")
# df_plot <- subset(df_diets, Area %in% area_vec &
#                     Year > 1992)
# yr_min <- min(df_plot$Year)
# yr_max <- max(df_plot$Year)
# n <- length(unique(df_plot$Item))
# bag_of_colors <- randomcoloR::distinctColorPalette(k = 2 * n)
# color_vec <- sample(bag_of_colors, n)
# #good_colors <- c("#B346E2", "#74B4D0", "#E16953", "#D6E1D8", "#C7E84D", "#6BE47C", "#E4B64D")
# # c("#D6A5D7" "#7EB0DD" "#E0AF9B" "#85E1DC" "#727AD6" "#DA74DA" "#729381" "#DB6A4A")
# #color_vec <- good_colors
# df_plot <- df_plot %>% group_by(Area, Item) %>%
#   mutate(mu_food_area_item = mean(`Share of daily diet`, na.rm = T)) %>%
#   as.data.frame()
# df_plot$Item <- factor(df_plot$Item,
#                        levels = unique(df_plot$Item[order(df_plot$mu_food_area_item, df_plot$Item, decreasing = T)]),
#                        ordered = T)
# gg <- ggplot(df_plot, aes(x = Year, y = `Share of daily diet`, fill = Item))
# gg <- gg + geom_area(position = "stack")
# gg <- gg + scale_x_continuous(breaks = seq(yr_min, yr_max, length.out = 4))
# gg <- gg + scale_fill_manual(values = color_vec)
# gg <- gg + facet_wrap(~Area, ncol = 3)
# gg <- gg + theme(legend.title = element_blank(),
#                  legend.text = element_text(size = 7),
#                  legend.key.size = unit(0.3, "cm"),
#                  strip.background = element_blank(),
#                  strip.text = element_text(size = facetTitle_size),
#                  axis.title.x = element_blank(),
#                  axis.title.y = element_text(size = axisTitle_size),
#                  axis.text = element_text(size = axisText_size))
# gg <- gg + guides(fill = guide_legend(nrow = 4, byrow = T))
# gg <- shift_legend2(gg)
# gg <- ggplotify::as.ggplot(gg)
#============================================================================
gdpPop_vars <- c("GDP/capita (USD), logged", "GDP (million USD)", "Population (millions)")
these_cols <- c("Area", "Year", "Region", "Element",
                "Item", "Value", gdpPop_vars)
# these_items <- c("Cereals - Excluding Beer", "Starchy Roots",
#                  "Grand Total")
exclude_items <- c("Animal fats", "Meat")
#unique(df_bennett$Item)
#setdiff(these_cols, colnames(df_bennett))
df_foodShare <- subset(df_bennett[, these_cols], !(Item %in% exclude_items) &
                         Element == "Food supply (kcal/capita/day)")

df_tot <- subset(df_foodShare[, c("Area", "Year", "Item", gdpPop_vars, "Value")], Item == "Grand Total")

df_tot$Item <- NULL
colnames(df_tot)[ncol(df_tot)] <- "Total (kcal/capita/day)"
df_foodShare <- subset(df_foodShare, Item != "Grand Total")
df_foodShare$Item[grep("Cereals|Starchy", df_foodShare$Item)] <- "Cereals & starchy roots"
df_foodShare$Item[grep("Fruits|Vegetables", df_foodShare$Item)] <- "Fruits & vegetables"
df_foodShare$Item[grep("Animal Products", df_foodShare$Item)] <- "Animal products"
df_foodShare$Item[grep("Sugar", df_foodShare$Item)] <- "Sugar & sweeteners"
df_foodShare <- df_foodShare %>% 
  group_by(Region, Area, Year, Item) %>%
  summarise(Value = sum(Value, na.rm = T)) %>% as.data.frame()
#---
df_foodShare <- merge(df_foodShare, df_tot, by = c("Area", "Year"))
colnames(df_foodShare)[which(colnames(df_foodShare) == "Value")] <- "Diet share (kcal/capita/day)"
df_foodShare$`Diet share (%)` <- 100 * df_foodShare$`Diet share (kcal/capita/day)` /
  df_foodShare$`Total (kcal/capita/day)`
df_foodShare$`Diet share (%), logged` <- 
  log(df_foodShare$`Diet share (%)`)
df_foodShare$`Diet share (kcal/capita/day), logged` <- 
  log(df_foodShare$`Diet share (kcal/capita/day)`)
#-----------------------------------------------------------------------------
# Highlight some countries in plot
# label_Africa <- c("Tanzania", "Uganda", "Ethiopia", "South Africa",
#                   "Ghana")
# label_NAfricaWAsia <- c("Turkey", "Saudi Arabia")
# label_Asia <- c("India", "Iran", "China, mainland", "Vietnam", "Japan",
#                 "South Korea")
# label_NAmEurAusNZ <- c("Italy", "France", "Australia")
# label_LAC <- c("Nicaragua", "Colombia", "Brazil")
# labelThese_vec <- c(label_Africa, label_NAfricaWAsia, label_Asia,
#                     label_NAmEurAusNZ, label_LAC)

# label_Africa <- c("Uganda", "South Africa")
# label_NAfricaWAsia <- c("Turkey", "Saudi Arabia")
# label_Asia <- c("India", "China, mainland", "Vietnam")
# label_NAmEurAusNZ <- c("France", "Australia")
# label_LAC <- c("Nicaragua", "Colombia", "Brazil")
# labelThese_vec <- c(label_Africa, label_NAfricaWAsia, label_Asia,
#                     label_NAmEurAusNZ, label_LAC)
# df_foodShare$label_these <- NA
# u <- df_foodShare$Area
# df_foodShare$label_these[which(u %in% labelThese_vec)] <- df_foodShare$Area[which(u %in% labelThese_vec)]
#-----------------------------------------------------------------------------
# Subset data for just cereals and starchy roots
df_plot <- subset(df_foodShare, Item == "Cereals & starchy roots" &
                    Year == 2017 & Region != "Aggregated")
colnames(df_plot) <- gsub("Diet share", "Cereals & starchy roots", colnames(df_plot))
colnames(df_plot) <- gsub("%", "% of diet", colnames(df_plot))
#-----------------------------------------------------------------------------
# Fit parameters
mod <- lm(`Cereals & starchy roots (% of diet), logged` ~ `GDP/capita (USD), logged`, data = df_plot)
#summary(mod)
#plot(mod$fitted.values, mod$residuals)
#ind_rm <- which(mod$residuals == min(mod$residuals))
#df_plot$Area[ind_rm]
m <- round(mod$coefficients[2], 4)
b <- round(mod$coefficients[1], 4)
df_out <- as.data.frame(broom::glance(mod))
adjR2 <- round(df_out$adj.r.squared, 2)
sampleSize2017 <- df.residual(mod)
#-----------------------------------------------------------------------------
# Get colors for region groupings and title
# n <- length(unique(df_plot$Region))
# bag_of_colors <- randomcoloR::distinctColorPalette(k = 2 * n)
# color_vec <- sample(bag_of_colors, n)
color_vec <- c("#8BC53F", "#5C9AD2", "#F68B33", "#FBB040", "#414042")
this_subtitle <- paste0("2017, N = ", sampleSize2017, ", Adj. R-squared = ", adjR2, "\nSlope = ", m, ", Y intercept = ", b)
#-----------------------------------------------------------------------------
gg <- ggplot(df_plot, aes(x = `GDP/capita (USD), logged`,
                          y = `Cereals & starchy roots (% of diet), logged`,
                          group = Region, fill = Region,
                          shape = Region))#,
#label = label_these))
gg <- gg + geom_smooth(aes(group = NULL, fill = NULL, shape = NULL), method = lm, se = F)
gg <- gg + geom_point(alpha = 0.6, size = point_size, color = "black", stroke = 0.5)
gg <- gg + scale_fill_manual(values = color_vec)
gg <- gg + scale_shape_manual(values = shape_vec)
#gg <- gg + geom_text_repel(color = "black", size = label_size)
gg <- gg + labs(title = "Bennett's law", subtitle = this_subtitle)#,
gg <- gg + guides(fill = guide_legend(nrow = 2, byrow = T, override.aes = list(linetype = 0)),
                  color = guide_legend(override.aes = list(linetype = 0)))
gg <- gg + theme(legend.position = "bottom",
                 legend.spacing.x = unit(0.25, 'cm'),
                 legend.title = element_blank(),
                 legend.text = element_text(size = legendText_size),
                 plot.title = element_text(size = title_size),
                 plot.subtitle = element_text(size = subtitle_size),
                 axis.title = element_text(size = axisTitle_size),
                 axis.text = element_text(size = axisText_size))
gg_bennett <- gg

this_filename <- "Bennetts Law 2017 FAO.png"
this_filepath <- paste0(dir, this_folder, this_filename)
ggsave(this_filepath, width = 4, height = 4, units = "in", dpi = 400)

#---------------------------------------------------------------------------
# these_cols <- c("Area", "Year", "Region", "Item", "Diet share (%), logged",
#                 gdpPop_vars)
# df_cerRootShare <- subset(df_foodShare, Item == "Cereals & starchy roots")
# u <- colnames(df_cerRootShare)
# colnames(df_cerRootShare) <- gsub("Diet share", "Cereals & starchy roots", u)
# u <- colnames(df_cerRootShare)
# colnames(df_cerRootShare) <- gsub("%", "% of diet", u)
# df_cerRootShare$Item <- NULL
# df_params <- df_cerRootShare
# df_params <- subset(df_params, Region != "Aggregated")
# yr_vec <- unique(df_params$Year)
# n_yrs <- length(yr_vec)
# mBennett_vec <- c()
# bBennett_vec <- c()
# adjR2Bennett_vec <- c()
# NBennett_vec <- c()
# for(i in 1:n_yrs){
#   this_year <- yr_vec[i]
#   this_df <- subset(df_params, Year == this_year)
#   this_df$Year <- NULL
#   #---------------------------------------------------
#   mod <- lm(`Cereals & starchy roots (% of diet), logged` ~ `GDP/capita (USD), logged`, this_df)
#   mBennett_vec[i] <- round(mod$coefficients[2], 6)
#   bBennett_vec[i] <- round(mod$coefficients[1], 6)
#   df_out <- as.data.frame(broom::glance(mod))
#   adjR2Bennett_vec[i] <- df_out$adj.r.squared
#   NBennett_vec[i] <- length(mod$fitted.values)
#   
# }
# #---------------------------------------------------------------------------
# # Get slope range for text
# ind <- which(yr_vec >= 1980)
# mBen_min <- round(min(mBennett_vec[ind]), 3)
# mBen_max <- round(max(mBennett_vec[ind]), 3)
# #---------------------------------------------------------------------------
# df_plotBennett <- data.frame(Year = yr_vec, Slope = mBennett_vec, bBennett_vec, adjR2Bennett_vec, NBennett_vec)
# colnames(df_plotBennett)[3:5] <- c("Y intercept", "Adj. R-squared", "Sample size")
# df_plotBennett$Year <- as.integer(as.character(df_plotBennett$Year))
# 
# 
# df_plot <- df_plotBennett %>% gather(Type, Value, Slope:`Sample size`)
# gg <- ggplot(df_plot, aes(x = Year, y = Value))
# gg <- gg + geom_line()
# # gg <- gg + scale_x_continuous(breaks = seq(yr_vec[1], yr_vec[n_yrs - 1], length.out = 24))
# gg <- gg + facet_wrap(~Type, ncol = 1, scales = "free_y")
# gg <- gg + labs(subtitle = "Bennet's law parameter trajectory, 1970-2017")
# gg <- gg + theme(axis.title = element_blank(),
#                  #axis.text.x = element_text(angle = 60, hjust = 1),
#                  strip.background = element_blank(),
#                  strip.text = element_text(size = facetTitle_size),
#                  plot.title = element_text(size = title_size),
#                  plot.subtitle = element_text(size = subtitle_size),
#                  axis.text = element_text(size = axisText_size))
# gg_bennettStab <- gg
# #----------------------------------------------------------------------------
# gg_bennett + gg_bennettStab + plot_layout(ncol = 2, widths = c(1, 4 / 9), guides = "collect") & theme(legend.position = 'bottom')
#=============================================================================
#=============================================================================
#=============================================================================
# df_plot <- subset(df_foodShare, Year == 2017 &
#                     Region != "Aggregated")
# gg <- ggplot(df_plot, aes(x = `GDP/capita (USD), logged`,
#                           y = `Diet share (%), logged`,
#                           group = Region, fill = Region,
#                           shape = Region))#,
# #label = label_these))
# #gg <- gg + geom_smooth(aes(group = NULL, fill = NULL, shape = NULL), method = lm, se = F)
# gg <- gg + geom_point(alpha = 0.6, size = point_size, color = "black", stroke = 0.5)
# gg <- gg + scale_fill_manual(values = color_vec)
# gg <- gg + scale_shape_manual(values = shape_vec)
# gg <- gg + facet_wrap(~Item, ncol = 3, scales = "free_y")
# #gg <- gg + geom_text_repel(color = "black", size = label_size)
# gg <- gg + guides(fill = guide_legend(nrow = 2, byrow = T, override.aes = list(linetype = 0)),
#                   color = guide_legend(override.aes = list(linetype = 0)))
# gg <- gg + theme(legend.position = "bottom",
#                  legend.spacing.x = unit(0.25, 'cm'),
#                  legend.title = element_blank(),
#                  legend.text = element_text(size = legendText_size),
#                  strip.background = element_blank(),
#                  strip.text = element_text(size = facetTitle_size),
#                  plot.title = element_text(size = title_size),
#                  plot.subtitle = element_text(size = subtitle_size),
#                  axis.title = element_text(size = axisTitle_size),
#                  axis.text = element_text(size = axisText_size))
# gg_foodGroupLaws <- gg
# # gg <- shift_legend2(gg)
# # gg <- ggplotify::as.ggplot(gg)
# gg_foodGroupLaws <- gg # (Moved this to appendix)
#===========================================================================
#===========================================================================
# Create macronutrient analogue to Bennett's law graphic
these_cols <- c("Area", "Region", "Year", "Element", "Item", "Value",
                "GDP/capita (USD)",
                "GDP/capita (USD), logged")
df_macnut <- subset(df_bennett[, these_cols], Item == "Grand Total")# &
#Region != "Aggregated")
df_macnut$Item <- NULL

# Convert g to kcal
df_macnut$Value[grep("Protein", df_macnut$Element)] <- 4 *
  df_macnut$Value[grep("Protein", df_macnut$Element)]
df_macnut$Value[grep("Fat", df_macnut$Element)] <- 9 *
  df_macnut$Value[grep("Fat", df_macnut$Element)]
df_macnut$Element[grep("Protein", df_macnut$Element)] <- "Protein supply (kcal/capita/day)"
df_macnut$Element[grep("Fat", df_macnut$Element)] <- "Fat supply (kcal/capita/day)"
# Calculate carb kcal
df_macnut <- df_macnut %>% spread(Element, Value)
df_macnut$`Carb supply (kcal/capita/day)` <- df_macnut$`Food supply (kcal/capita/day)` -
  df_macnut$`Protein supply (kcal/capita/day)` - df_macnut$`Fat supply (kcal/capita/day)`
# Create % of diet variable
df_tot <- df_macnut[, c("Area", "Year", "Food supply (kcal/capita/day)")]
df_macnut$`Food supply (kcal/capita/day)` <- NULL
gathercols <- colnames(df_macnut)[6:ncol(df_macnut)]
df_macnut <- df_macnut %>% gather_("Item", "Value", gathercols)
df_macnut$Item[grep("Protein", df_macnut$Item)] <- "Protein"
df_macnut$Item[grep("Fat", df_macnut$Item)] <- "Fat"
df_macnut$Item[grep("Carb", df_macnut$Item)] <- "Carbohydrate"
df_macnut <- merge(df_macnut, df_tot, by = c("Area", "Year"))
colnames(df_macnut)[7:8] <- c("Supply (kcal/capita/day)", "Total (kcal/capita/day)")
df_macnut$`Diet share (%)` <- 100 * df_macnut$`Supply (kcal/capita/day)` / df_macnut$`Total (kcal/capita/day)`
df_macnut$`Diet share (%), logged` <- log(df_macnut$`Diet share (%)`)
df_macnut$`Supply (kcal/capita/day), logged` <- log(df_macnut$`Supply (kcal/capita/day)`)
#---------------------------------------------------------------------------
# Plot macronutrient vs GDP/capita
df_plot <- subset(df_macnut, Year == 2017 & Region != "Aggregated")

these_items <- unique(df_plot$Item)
facet_labels <- c()
adjR2_vec <- c()
for(i in 1:length(these_items)){
  this_item <- these_items[i]
  this_df <- subset(df_plot, Item == this_item)
  mod <- lm(`Diet share (%), logged` ~ `GDP/capita (USD), logged`, this_df)
  #mod <- lm(`Supply (kcal/capita/day), logged` ~ `GDP/capita (USD), logged`, this_df)
  #summary(mod)
  #plot(mod$fitted.values, mod$residuals)
  m <- round(mod$coefficients[2], 4)
  b <- round(mod$coefficients[1], 4)
  df_out <- as.data.frame(broom::glance(mod))
  adjR2 <- round(df_out$adj.r.squared, 2)
  N <- df.residual(mod)
  this_facet_label <- paste0(this_item, " kcals", "\nAdj. R-squared = ", adjR2, "\nSlope = ", m, ", Y intercept = ", b)
  facet_labels[i] <- this_facet_label
  adjR2_vec[i] <- adjR2
  
}
sampleSize2017 <- N
names(facet_labels) <- these_items
this_subtitle <- paste("2017, N =", sampleSize2017)

gg <- ggplot(df_plot, aes(x = `GDP/capita (USD), logged`,
                          y = `Diet share (%), logged`,
                          #y = `Supply (kcal/capita/day), logged`,
                          group = Region, fill = Region,
                          shape = Region))
gg <- gg + geom_smooth(aes(group = NULL, fill = NULL, shape = NULL), method = lm, se = F)
gg <- gg + geom_point(alpha = 0.6, color = "black", stroke = 0.5)
gg <- gg + scale_fill_manual(values = color_vec)
gg <- gg + scale_shape_manual(values = shape_vec)
gg <- gg + labs(title = "Macronutrient laws", subtitle = this_subtitle)
gg <- gg + facet_wrap(~Item, ncol = 1, scales = "free_y",
                      labeller = labeller(Item = facet_labels))
gg <- gg + theme(legend.position = "bottom",
                 legend.spacing.x = unit(0.25, 'cm'),
                 legend.title = element_blank(),
                 legend.text = element_text(size = legendText_size),
                 strip.background = element_blank(),
                 strip.text = element_text(size = facetTitle_size),
                 plot.title = element_text(size = title_size),
                 plot.subtitle = element_text(size = subtitle_size),
                 axis.title = element_text(size = axisTitle_size),
                 axis.text = element_text(size = axisText_size))
gg <- gg + guides(fill = guide_legend(nrow = 2, byrow = T, override.aes = list(linetype = 0)),
                  color = guide_legend(override.aes = list(linetype = 0)))
# gg <- shift_legend2(gg)
# gg <- ggplotify::as.ggplot(gg)
gg_macnut <- gg
#---------------------------------------------------------------------------
df_carb <- subset(df_macnut, Item == "Carbohydrate")
u <- colnames(df_carb)
colnames(df_carb) <- gsub("Supply", "Carbohydrates", u)

df_params <- subset(df_carb, Region != "Aggregated")
yr_vec <- unique(df_params$Year)
n_yrs <- length(yr_vec)
mCarb_vec <- c()
bCarb_vec <- c()
adjR2Carb_vec <- c()
NCarb_vec <- c()
for(i in 1:n_yrs){
  this_year <- yr_vec[i]
  this_df <- subset(df_params, Year == this_year)
  this_df$Year <- NULL
  #---------------------------------------------------
  mod <- lm(`Diet share (%), logged` ~ `GDP/capita (USD), logged`, this_df)
  mCarb_vec[i] <- round(mod$coefficients[2], 6)
  bCarb_vec[i] <- round(mod$coefficients[1], 6)
  df_out <- as.data.frame(broom::glance(mod))
  adjR2Carb_vec[i] <- df_out$adj.r.squared
  NCarb_vec[i] <- df.residual(mod)
  #NCarb_vec[i] <- length(mod$fitted.values)
  
}
#----------------------------------------------------------------------------
# Get max and min Carb Law slope for text
ind <- which(yr_vec >= 1980)
mCarb_min <- round(min(mCarb_vec[ind]), 3)
mCarb_max <- round(max(mCarb_vec[ind]), 3)
#----------------------------------------------------------------------------
df_plotCarb <- data.frame(Year = yr_vec, Slope = mCarb_vec, bCarb_vec, adjR2Carb_vec, NCarb_vec)
colnames(df_plotCarb)[3:5] <- c("Y intercept", "Adj. R-squared", "Sample size")
df_plotCarb$Year <- as.integer(as.character(df_plotCarb$Year))

df_plot <- df_plotCarb %>% gather(Type, Value, Slope:`Sample size`)

gg <- ggplot(df_plot, aes(x = Year, y = Value))
gg <- gg + geom_line()
# gg <- gg + scale_x_continuous(breaks = seq(yr_vec[1], yr_vec[n_yrs - 1], length.out = 24))
gg <- gg + facet_wrap(~Type, ncol = 1, scales = "free_y")
gg <- gg + labs(subtitle = "Carbohydrate law\nparameter trajectory, 1970-2017")
gg <- gg + theme(axis.title = element_blank(),
                 #axis.text.x = element_text(angle = 60, hjust = 1),
                 strip.background = element_blank(),
                 strip.text = element_text(size = facetTitle_size),
                 plot.title = element_text(size = title_size),
                 plot.subtitle = element_text(size = subtitle_size),
                 axis.text = element_text(size = axisText_size))
gg_carbStab <- gg
#---------------------------------------------------------------------------
#===========================================================================
#===========================================================================
# Plot macronutrient analogue to Bennett's law graphic created in previous chunk
gg_macnut + gg_carbStab + plot_layout(ncol = 2, widths = c(1, 4 / 9), guides = "collect") & theme(legend.position = 'bottom')
#===========================================================================
#===========================================================================
# Create fat-carb frontier graphic
df_macnutShare <- df_macnut[, c("Area", "Region", "Item", "Year", "Diet share (%)", "GDP/capita (USD)")]
df_macnutShare <- df_macnutShare %>% spread(Item, `Diet share (%)`)
# Get parameters
df_mod <- subset(df_macnutShare, Year == 2017 & Region != "Aggregated")
mod <- lm(`Fat`~`Carbohydrate`, df_mod)
#summary(mod)
m <- round(mod$coefficients[2], 4)
b <- round(mod$coefficients[1], 4)
df_out <- as.data.frame(broom::glance(mod))
adjR2 <- round(df_out$adj.r.squared, 2)
sampleSize2017 <- df.residual(mod)
this_subtitle <- paste0("2017, N = ", sampleSize2017, ", Adj. R-squared = ", adjR2, "\nSlope = ", m, ", Y intercept = ", b)
#Plot
colnames(df_macnutShare)[5:7] <- paste(colnames(df_macnutShare)[5:7], "(% of diet)")

df_plot <- subset(df_macnutShare, Year == 2017 & Region != "Aggregated")
#---
# df_plot$label_these <- NA
# u <- df_plot$Area
# df_plot$label_these[which(u %in% labelThese_vec)] <- df_plot$Area[which(u %in% labelThese_vec)]
#---
color_vec <- c("#8BC53F", "#5C9AD2", "#F68B33", "#FBB040", "#414042")

gg <- ggplot(df_plot, aes(x = `Carbohydrate (% of diet)`,
                          y = `Fat (% of diet)`,
                          group = Region, fill = Region,
                          shape = Region, size = `GDP/capita (USD)`))#,
#label = label_these))
gg <- gg + geom_smooth(aes(group = NULL, fill = NULL, shape = NULL), method = lm, se = F)
gg <- gg + geom_point(alpha = 0.6, color = "black", stroke = 0.5)
gg <- gg + scale_fill_manual(values = color_vec)
gg <- gg + scale_shape_manual(values = shape_vec)
gg <- gg + labs(title = "The fat-carb frontier", subtitle = this_subtitle)
#gg <- gg + geom_text_repel(color = "black", size = label_size)
gg <- gg + theme(legend.position = "bottom",
                 legend.spacing.x = unit(0.25, 'cm'),
                 legend.title = element_blank(),
                 legend.text = element_text(size = legendText_size),
                 plot.title = element_text(size = title_size),
                 plot.subtitle = element_text(size = subtitle_size),
                 axis.title = element_text(size = axisTitle_size),
                 axis.text = element_text(size = axisText_size))
gg <- gg + guides(fill = guide_legend(nrow = 2, byrow = T, override.aes = list(linetype = 0)),
                  color = guide_legend(override.aes = list(linetype = 0)),
                  size = F)
gg_fatcarb <- gg

ggsave("FatCarbFrontier2017.png", width = 4, height = 4, units = "in", dpi = 400)
