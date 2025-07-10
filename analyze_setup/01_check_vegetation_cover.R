rm(list = ls())

library(raster)
library(tidyverse)
library(sp)
library(sf)
library(progress)
library(ggplot2)
library(patchwork)
library(grid)


# Import data -------------------------------------------------------------

mapbox_token = readLines("analyze_setup/mapbox_token.txt", warn = FALSE)

file_vegetation = "analyze_setup/Copernicus_TreeCover_on_roi-ESRI31493.tif"
file_vegetation_EPSG3035 = "analyze_setup/Copernicus_TreeCover_on_roi.tif" # EPSG:3035 - ETRS89-extended / LAEA Europe
file_plots = "analyze_setup/Plot selection/20230413-Plot_selection-ESRI31493.shp"
file_plots_WGS84 = "analyze_setup/Plot selection/20230413-Plot_selection.shp"
file_roads = "analyze_setup/Roads_inside_buffer_200m_20230413_plots-ESRI31493.shp"
file_roads_WGS84 = "analyze_setup/Roads_inside_buffer_200m_20230413_plots.shp"

buffer_radius = 50 # meters
plot_cell_dimension = 150 # meters

# folder_out = NULL
folder_out = "analyze_setup/out/"
types_plot_out = c("png", "pdf")

if (!is.null(folder_out)) {
  for (typ in types_plot_out) {
    if (!dir.exists(paste0(folder_out, typ, "/"))) {
      dir.create(paste0(folder_out, typ, "/"), recursive = TRUE)
      dir.create(paste0(folder_out, typ, "/all_canopy_cover/"), recursive = TRUE)
      dir.create(paste0(folder_out, typ, "/all_satellites/"), recursive = TRUE)
    }
  }
}

r_vegetation = raster(file_vegetation)
r_vegetation_EPSG3035 = raster(file_vegetation_EPSG3035)

plots = shapefile(file_plots)
plots_EPSG3857 = as(st_transform(st_as_sf(plots), crs = 3857), "Spatial")
plots_EPSG3035 = as(st_transform(st_as_sf(plots), crs = 3035), "Spatial")
plots_WGS84 = shapefile(file_plots_WGS84)

roads = shapefile(file_roads)
roads_EPSG3857 = as(st_transform(st_as_sf(roads), crs = 3857), "Spatial")
roads_EPSG3035 = as(st_transform(st_as_sf(roads), crs = 3035), "Spatial")
roads_WGS84 = shapefile(file_roads_WGS84)

palette_types_subplots = c("forest" = "#009E73", "path" = "#56B4E9", 
                           "road" = "#E69F00", "edge" = "#CC79A7")
plots@data$color = palette_types_subplots[plots@data$plot_type]
plots_EPSG3857@data$color = palette_types_subplots[plots_EPSG3857@data$plot_type]
plots_EPSG3035@data$color = palette_types_subplots[plots_EPSG3035@data$plot_type]
plots_WGS84@data$color = palette_types_subplots[plots_WGS84@data$plot_type]


# Prepare dataframes ------------------------------------------------------

r_vegetation_df <- as.data.frame(r_vegetation, xy = TRUE)
r_vegetation_df_EPSG3035 <- as.data.frame(r_vegetation_EPSG3035, xy = TRUE)
plots_df <- data.frame(x = plots@coords[, 1], y = plots@coords[, 2],
                       relat_cfb = plots@data$relat_cfb,
                       color = plots@data$color, plot_type = plots@data$plot_type)
plots_df_EPSG3035 <- data.frame(x = plots_EPSG3035@coords[, 1], y = plots_EPSG3035@coords[, 2],
                                relat_cfb = plots_EPSG3035@data$relat_cfb,
                                color = plots_EPSG3035@data$color, plot_type = plots_EPSG3035@data$plot_type)

df_all_raster_subset <- data.frame()
df_circles = data.frame()
df_all_raster_subset_EPSG3035 <- data.frame()
df_circles_EPSG3035 = data.frame()
tt <- seq(0, 2*pi, length.out = 100)

pbar = progress_bar$new(total = length(unique(plots@data$relat_cfb)) * length(unique(plots@data$plot_type)),
                        clear = FALSE, width = 90)

for (this_relat_cfb in unique(plots@data$relat_cfb)) {
  for (this_plot_type in unique(plots@data$plot_type)) {
    
    this_center = plots@coords[plots@data$relat_cfb == this_relat_cfb & plots@data$plot_type == this_plot_type, ]
    # Raster vegetation
    raster_subset <- subset(
      r_vegetation_df, 
      (x >= this_center[1] - plot_cell_dimension / 2) & (x <= this_center[1] + plot_cell_dimension / 2) &
      (y >= this_center[2] - plot_cell_dimension / 2) & (y <= this_center[2] + plot_cell_dimension / 2)
    )
    raster_subset$relat_cfb <- this_relat_cfb
    raster_subset$plot_type <- this_plot_type
    df_all_raster_subset <- rbind(df_all_raster_subset, raster_subset)
    # Circles
    df_circles <- rbind(df_circles, 
                        data.frame(x = this_center[1] + buffer_radius * cos(tt),
                                   y = this_center[2] + buffer_radius * sin(tt),
                                   relat_cfb = this_relat_cfb, plot_type = this_plot_type))
    
    
    this_center_EPSG3035 = plots_EPSG3035@coords[plots_EPSG3035@data$relat_cfb == this_relat_cfb & plots_EPSG3035@data$plot_type == this_plot_type, ]
    # Raster vegetation
    raster_subset_EPSG3035 <- subset(
      r_vegetation_df_EPSG3035, 
      (x >= this_center_EPSG3035[1] - plot_cell_dimension / 2) & (x <= this_center_EPSG3035[1] + plot_cell_dimension / 2) &
      (y >= this_center_EPSG3035[2] - plot_cell_dimension / 2) & (y <= this_center_EPSG3035[2] + plot_cell_dimension / 2)
    )
    raster_subset_EPSG3035$relat_cfb <- this_relat_cfb
    raster_subset_EPSG3035$plot_type <- this_plot_type
    df_all_raster_subset_EPSG3035 <- rbind(df_all_raster_subset_EPSG3035, raster_subset_EPSG3035)
    # Circles
    df_circles_EPSG3035 <- rbind(df_circles_EPSG3035, 
                                 data.frame(x = this_center_EPSG3035[1] + buffer_radius * cos(tt),
                                            y = this_center_EPSG3035[2] + buffer_radius * sin(tt),
                                            relat_cfb = this_relat_cfb, plot_type = this_plot_type))
    
    
    pbar$tick()
  }
}

df_all_raster_subset <- df_all_raster_subset %>%
  rename(vegetation = Copernicus_TreeCover_on_roi.ESRI31493)
df_all_raster_subset_EPSG3035 <- df_all_raster_subset_EPSG3035 %>%
  rename(vegetation = Copernicus_TreeCover_on_roi)


# Single plots ------------------------------------------------------------

pbar = progress_bar$new(total = length(unique(plots@data$relat_cfb)) * length(unique(plots@data$plot_type)),
                        clear = FALSE, width = 90)

for (this_relat_cfb in unique(plots_df$relat_cfb)) {
  for (this_plot_type in unique(plots_df$plot_type)) {
    
    p <- ggplot() +
      # Vegetation
      geom_raster(data = df_all_raster_subset %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type),
                  aes(x = x, y = y, fill = vegetation), alpha = 0.85) +
      scale_fill_gradient(low = "white", high = "tomato", limits = c(0, 100)) +
      # Plots and circle
      geom_point(data = plots_df %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type),
                 aes(x = x, y = y, color = plot_type), size = 4) +
      scale_color_manual(values = palette_types_subplots) +
      geom_path(data = df_circles %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type),
                aes(x = x, y = y), color = "#555555", linewidth = 1) +
      # Scale legend
      geom_errorbarh(data = plots_df %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type),
                     aes(xmin = x - plot_cell_dimension / 2 + 10, xmax = x - plot_cell_dimension / 2 + 10 + 20,
                         y = y - plot_cell_dimension / 2 + 10), height = 5, color = "#555555", linewidth = 0.5) +
      geom_text(data = plots_df %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type),
                aes(x = x - plot_cell_dimension / 2 + 10 + 10, y = y - plot_cell_dimension / 2 + 10 + 4,
                    label = "20 m"), color = "#555555", size = 3) +
      # Appearance of the plot
      coord_fixed() +
      ggtitle(paste("Canopy Cover on CFB", this_relat_cfb, "-", this_plot_type)) +
      labs(fill = "Canopy\nCover (%)", color = "Plot Type",
           x = "Longitude (ESRI:31493)", y = "Latitude (ESRI:31493)") +
      # Theme with x-y axes but no grid
      theme_minimal() +
      theme(axis.ticks = element_line(color = "black", size = 0.5),
            axis.ticks.length = unit(0.3, "cm"),
            axis.text = element_text(color = "black", size = 8))
    
    if (!is.null(folder_out)) {
      for (typ in types_plot_out) {
        file_out = paste0(folder_out, typ, "/all_canopy_cover/canopy_cover-", this_plot_type, "_", this_relat_cfb, ".", typ)
        ggsave(p, filename = file_out, dpi = 300, device = typ,
               width = 6, height = 6)
      }
    }
    pbar$tick()
  }
}


# Patchwork ---------------------------------------------------------------

# Function to create a single plot
create_plot <- function(this_relat_cfb, this_plot_type) {
  p <- ggplot() +
    # Vegetation
    geom_raster(data = df_all_raster_subset %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type),
                aes(x = x, y = y, fill = vegetation), alpha = 0.85) +
    scale_fill_gradient(low = "white", high = "tomato", limits = c(0, 100)) +
    # Plots and circle
    geom_point(data = plots_df %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type),
               aes(x = x, y = y, color = plot_type), size = 4) +
    scale_color_manual(values = palette_types_subplots) +
    geom_point(data = plots_df %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type),
               aes(x = x, y = y), color = "#555555", size = 1) +
    geom_path(data = df_circles %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type),
              aes(x = x, y = y), color = "#555555", linewidth = 0.8, linetype = "dashed") +
    # Scale legend
    geom_errorbarh(data = plots_df %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type),
                   aes(xmin = x - plot_cell_dimension / 2 + 10, xmax = x - plot_cell_dimension / 2 + 10 + 20,
                       y = y - plot_cell_dimension / 2 + 10), height = 5, color = "#555555", linewidth = 0.5) +
    geom_text(data = plots_df %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type),
              aes(x = x - plot_cell_dimension / 2 + 10 + 10, y = y - plot_cell_dimension / 2 + 10 + 6,
                  label = "20 m"), color = "#555555", size = 2) +
    # Appearance of the plot
    coord_fixed() +
    ggtitle(paste("CFB plot", this_relat_cfb, "-", this_plot_type)) +
    labs(fill = "Canopy\nCover (%)", color = "Plot Type",
         x = "Longitude (ESRI:31493)", y = "Latitude (ESRI:31493)") +
    # Theme with x-y axes but no grid
    theme_minimal() +
    theme(axis.ticks = element_line(color = "black", size = 0.5),
          axis.ticks.length = unit(0.3, "cm"),
          axis.text = element_text(color = "black", size = 8))
  # Remove legend color
  p <- p + guides(color = "none")
  
  if (this_plot_type != "edge") {
    p <- p + theme(legend.position = "none")  # Hide legend for edge plots
  }
  if (this_plot_type != "forest") {
    p <- p + theme(axis.title.y = element_blank())  # Hide y-axis for non-forest plots
  }
  if (this_relat_cfb != unique(plots_df$relat_cfb)[length(unique(plots_df$relat_cfb))]) {
    p <- p + theme(axis.title.x = element_blank())  # Hide x-axis for non-last CFB plots
  }
  
  return(p)
}

# Build Patchwork
plot_list <- list()
for (this_relat_cfb in unique(plots_df$relat_cfb)) {
  for (this_plot_type in unique(plots_df$plot_type)) {
    plot_list[[paste(this_relat_cfb, this_plot_type, sep = "_")]] <- create_plot(this_relat_cfb, this_plot_type)
  }
}
combined_plot <- wrap_plots(plot_list, ncol = length(unique(plots_df$plot_type)))

# print(combined_plot)

if (!is.null(folder_out)) {
  for (typ in types_plot_out) {
    file_out = paste0(folder_out, typ, "/patchwork-canopy_cover.", typ)
    ggsave(combined_plot, filename = file_out, dpi = 300, device = typ,
           width = length(unique(plots_df$plot_type)) * 3, height = length(unique(plots_df$relat_cfb)) * 3)
  }
}


# Analysis ----------------------------------------------------------------


p <- ggplot(df_all_raster_subset) + 
  geom_violin(aes(x = plot_type, y = vegetation, fill = plot_type), 
              scale = "width", trim = TRUE) +
  labs(title = "Canopy Cover over all plots", x = "Plot Type", y = "Canopy Cover (%)") +
  theme_minimal() +
  scale_fill_manual(values = palette_types_subplots) +
  scale_x_discrete(limits = c("forest", "path", "road", "edge")) +
  theme(legend.position = "none")

if (!is.null(folder_out)) {
  for (typ in types_plot_out) {
    file_out = paste0(folder_out, typ, "/global-canopy_cover_all_plots.", typ)
    ggsave(p, filename = file_out, dpi = 300, device = typ, width = 5, height = 4)
  }
}

p <- ggplot(df_all_raster_subset) + 
  facet_wrap(~ relat_cfb, scales = "free", labeller = labeller(relat_cfb = function(x) paste0("CFB plot ", x))) +
  geom_violin(aes(x = plot_type, y = vegetation, fill = plot_type), 
              scale = "width", trim = TRUE) +
  labs(title = "Canopy Cover over by CFB plot", x = "Plot Type", y = "Canopy Cover (%)") +
  theme_minimal() +
  scale_fill_manual(values = palette_types_subplots) +
  scale_x_discrete(limits = c("forest", "path", "road", "edge")) +
  theme(legend.position = "none")

if (!is.null(folder_out)) {
  for (typ in types_plot_out) {
    file_out = paste0(folder_out, typ, "/global-canopy_cover_by_cfb.", typ)
    ggsave(p, filename = file_out, dpi = 300, device = typ, 
           width = 3 * sqrt(length(unique(plots_df$relat_cfb))), height = 2 * sqrt(length(unique(plots_df$relat_cfb))))
  }
}


# Summary statistics ------------------------------------------------------

df_stats <- df_all_raster_subset %>%
  group_by(relat_cfb, plot_type) %>%
  summarise(
    vegetation_mean = mean(vegetation, na.rm = TRUE),
    vegetation_sd = sd(vegetation, na.rm = TRUE),
    vegetation_min = min(vegetation, na.rm = TRUE),
    vegetation_max = max(vegetation, na.rm = TRUE),
    vegetation_q5 = quantile(vegetation, 0.05, na.rm = TRUE),
    vegetation_q25 = quantile(vegetation, 0.25, na.rm = TRUE),
    vegetation_median = median(vegetation, na.rm = TRUE),
    vegetation_q75 = quantile(vegetation, 0.75, na.rm = TRUE),
    vegetation_q95 = quantile(vegetation, 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  pivot_longer(cols = starts_with("vegetation_"), names_to = "statistic", values_to = "value") %>%
  mutate(statistic = factor(statistic, 
                           levels = c("vegetation_mean", "vegetation_sd", "vegetation_min", 
                                      "vegetation_max", "vegetation_q5", "vegetation_q25", 
                                      "vegetation_median", "vegetation_q75", "vegetation_q95"),
                           labels = c("Mean", "Standard Deviation", "Minimum", 
                                      "Maximum", "5th Percentile", "25th Percentile", 
                                      "Median", "75th Percentile", "95th Percentile")))

p <- ggplot(df_stats) +
  facet_wrap(~ statistic, scales = "free_y") +
  geom_line(aes(x = plot_type, y = value, group = relat_cfb), alpha = 0.7) +
  scale_x_discrete(limits = c("forest", "path", "road", "edge")) + 
  labs(title = "Canopy Cover Statistics by CFB Plot", x = "Plot Type", y = "Value") +
  theme_minimal()
if (!is.null(folder_out)) {
  for (typ in types_plot_out) {
    file_out = paste0(folder_out, typ, "/statistics-canopy_cover.", typ)
    ggsave(p, filename = file_out, dpi = 300, device = typ, 
           width = 3 * sqrt(length(unique(plots_df$relat_cfb))), height = 2 * sqrt(length(unique(plots_df$relat_cfb))))
  }
}

p <- ggplot(df_stats %>% mutate(relat_cfb = as.factor(relat_cfb))) +
  facet_wrap(~ statistic, scales = "free_y") +
  geom_line(aes(x = plot_type, y = value, color = relat_cfb, group = relat_cfb), alpha = 0.7) +
  scale_x_discrete(limits = c("forest", "path", "road", "edge")) + 
  labs(title = "Canopy Cover Statistics by CFB Plot", x = "Plot Type", y = "Value") +
  theme_minimal()
if (!is.null(folder_out)) {
  for (typ in types_plot_out) {
    file_out = paste0(folder_out, typ, "/statistics-canopy_cover-colored.", typ)
    ggsave(p, filename = file_out, dpi = 300, device = typ, 
           width = 3 * sqrt(length(unique(plots_df$relat_cfb))), height = 2 * sqrt(length(unique(plots_df$relat_cfb))))
  }
}



# Models ------------------------------------------------------------------

df_models = df_all_raster_subset
df_models$vegetation = df_models$vegetation / 100
df_models$relat_cfb <- as.factor(df_models$relat_cfb)

library(betareg)
library(lme4)
library(mgcv)
library(emmeans)
library(rstatix)

# Beta regression
model1 <- betareg(vegetation ~ plot_type, data = df_models)
summary(model1)
emmeans_model1 <- emmeans(model1, pairwise ~ plot_type, adjust = "bonferroni")
summary(emmeans_model1)
# Beta regression removing edge plots
model1a <- betareg(vegetation ~ plot_type, data = df_models %>% filter(plot_type != "edge"))
summary(model1a)
emmeans_model1a <- emmeans(model1a, pairwise ~ plot_type, adjust = "bonferroni")
summary(emmeans_model1a)

# GAM with random effects
model2 <- gam(vegetation ~ plot_type + s(relat_cfb, bs = "re"), data = df_models, 
              family = betar(), method = "REML")
summary(model2)

# Linear mixed model
model3 <- lmer(vegetation ~ plot_type + (1 | relat_cfb), data = df_models)
summary(model3)
emmeans_model3 <- emmeans(model3, pairwise ~ plot_type, adjust = "bonferroni")
summary(emmeans_model3)
# Linear mixed model except edge
model3a <- lmer(vegetation ~ plot_type + (1 | relat_cfb), data = df_models %>% filter(plot_type != "edge"))
summary(model3a)
emmeans_model3a <- emmeans(model3a, pairwise ~ plot_type, adjust = "bonferroni")
summary(emmeans_model3a)

# Linear model
model4 <- lm(vegetation ~ plot_type, data = df_models)
summary(model4)
emmeans_model4 <- emmeans(model4, pairwise ~ plot_type, adjust = "bonferroni")
summary(emmeans_model4)

anova4 <- aov(vegetation ~ plot_type, data = df_models)
summary(anova4)
TukeyHSD(anova4)

# Linear model except edge
model5 <- lm(vegetation ~ plot_type, data = df_models %>% filter(plot_type != "edge"))
summary(model5)
emmeans_model5 <- emmeans(model5, pairwise ~ plot_type, adjust = "bonferroni")
summary(emmeans_model5)

anova5 <- aov(vegetation ~ plot_type, data = df_models %>% filter(plot_type != "edge"))
summary(anova5)
TukeyHSD(anova5)


# Non parametric tests

# Define the file path
output_file <- paste0(folder_out, "comparisons_canopy_cover_plot_type.txt")

# Open a connection to the text file
sink(output_file, type = "output")

cat("# Tests on the effect of plot_type on vegetation (vegetation ~ plot_type)\n\n")
# Run the Kruskal-Wallis tests
cat("------------------------------------------------------------------------\n")
cat("Kruskal-Wallis Tests:\n\n")
cat(" >  On all types of plots:\n")
kruskal.test(vegetation ~ plot_type, data = df_models)
cat("\n >  On forest, path:\n")
kruskal.test(vegetation ~ plot_type, data = df_models %>% filter(plot_type %in% c("forest", "path")))
cat("\n >  On forest, road:\n")
kruskal.test(vegetation ~ plot_type, data = df_models %>% filter(plot_type %in% c("forest", "road")))
cat("\n >  On path, road:\n")
kruskal.test(vegetation ~ plot_type, data = df_models %>% filter(plot_type %in% c("path", "road")))
cat("\n >  On forest, edge:\n")
kruskal.test(vegetation ~ plot_type, data = df_models %>% filter(plot_type %in% c("forest", "edge")))
cat("\n >  On path, edge:\n")
kruskal.test(vegetation ~ plot_type, data = df_models %>% filter(plot_type %in% c("path", "edge")))
cat("\n >  On road, edge:\n")
kruskal.test(vegetation ~ plot_type, data = df_models %>% filter(plot_type %in% c("road", "edge")))

# Run the Dunn tests
cat("\n\n------------------------------------------------------------------------\n")
cat("Dunn Tests:\n\n")
cat(" >  On all types of plots (correction methods = Bonferroni):\n\n")
dunn_test(vegetation ~ plot_type, p.adjust.method = "bonferroni", data = df_models)
cat("\n >  On all types of plots (correction methods = Holm):\n\n")
dunn_test(vegetation ~ plot_type, p.adjust.method = "holm", data = df_models)
cat("\n >  On all types of plots except edge (correction methods = Bonferroni):\n\n")
dunn_test(vegetation ~ plot_type, p.adjust.method = "bonferroni", data = df_models %>% filter(plot_type != "edge"))
cat("\n >  On all types of plots except edge (correction methods = Holm):\n\n")
dunn_test(vegetation ~ plot_type, p.adjust.method = "holm", data = df_models %>% filter(plot_type != "edge"))

# Close the connection
sink()




# Roads -------------------------------------------------------------------

# sort(unique(roads@data$relat_cfb))
# sort(unique(roads@data$plot_type))
# roads_df <- data.frame()

this_relat_cfb = 38
this_plot_type = "path"

roads_df = fortify(roads)
roads_infos = data.frame(id = 1:length(roads@data$relat_cfb), relat_cfb = roads@data$relat_cfb, 
                         plot_type = roads@data$plot_type, fclass = roads@data$fclass) %>% 
  mutate(id = as.character(id))

roads_df = left_join(roads_df, roads_infos, by = "id")

ggplot() +
  geom_path(data = roads_df %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type),
            aes(x = long, y = lat, group = id, color = fclass), size = 0.5) +
  coord_fixed() +
  ggtitle("Roads with relat_cfb == 38")
  


# Vegetation and roads ----------------------------------------------------

# Function to create a single plot
create_plot_veg_roads <- function(this_relat_cfb, this_plot_type, in_patchwork = TRUE) {
  
  # Roads
  roads_df_EPSG3035 = fortify(roads_EPSG3035)
  roads_infos_EPSG3035 = data.frame(id = 1:length(roads_EPSG3035@data$relat_cfb), relat_cfb = roads_EPSG3035@data$relat_cfb, 
                           plot_type = roads_EPSG3035@data$plot_type, fclass = roads_EPSG3035@data$fclass) %>% 
    mutate(id = as.character(id))
  roads_df_EPSG3035 = left_join(roads_df_EPSG3035, roads_infos, by = "id")
  
  # palette_paths <- c(
  #   "service" =      "#f3ce13",
  #   "track" =        "#1f6406",
  #   "track_grade1" = "#0f3a00",
  #   "track_grade2" = "#165200",
  #   "track_grade3" = "#419523",
  #   "track_grade4" = "#61a04b",
  #   "track_grade5" = "#85b167",
  #   "path" =         "#15bfa8",
  #   "unclassified" = "#898989"
  # )
  palette_paths <- scales::hue_pal()(9)
  names(palette_paths) = c("service", "track", "track_grade1", "track_grade2", "track_grade3",
                           "track_grade4", "track_grade5", "path", "unclassified")
  this_color_plot = plots_df_EPSG3035 %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type) %>% pull(color)
  
  this_filter = plots_EPSG3035@data$relat_cfb == this_relat_cfb & plots_EPSG3035@data$plot_type == this_plot_type
  xlims = c(plots_EPSG3035@coords[this_filter, 1] - plot_cell_dimension / 2 - 5, 
            plots_EPSG3035@coords[this_filter, 1] + plot_cell_dimension / 2 + 5)
  ylims = c(plots_EPSG3035@coords[this_filter, 2] - plot_cell_dimension / 2 - 5, 
            plots_EPSG3035@coords[this_filter, 2] + plot_cell_dimension / 2 + 5)
  
  
  p <- ggplot() +
    # Vegetation
    geom_raster(data = df_all_raster_subset_EPSG3035 %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type),
                aes(x = x, y = y, fill = vegetation), alpha = 0.85) +
    scale_fill_gradient(low = "white", high = "tomato", limits = c(0, 100)) +
    # Plots and circle
    geom_point(data = plots_df_EPSG3035 %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type),
               aes(x = x, y = y), color = this_color_plot, size = 4) +
    geom_path(data = roads_df_EPSG3035 %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type),
              aes(x = long, y = lat, group = id, color = fclass), size = 1) + 
    scale_color_manual(values = palette_paths) + #, labels = names(palette_paths), 
                       # breaks = names(palette_paths), drop = FALSE) + 
    geom_point(data = plots_df_EPSG3035 %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type),
               aes(x = x, y = y), color = "#555555", size = 1) +
    geom_path(data = df_circles_EPSG3035 %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type),
              aes(x = x, y = y), color = "#555555", linewidth = 0.8, linetype = "dashed") +
    # Scale legend
    geom_errorbarh(data = plots_df_EPSG3035 %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type),
                   aes(xmin = x - plot_cell_dimension / 2 + 10, xmax = x - plot_cell_dimension / 2 + 10 + 20,
                       y = y - plot_cell_dimension / 2 + 10), height = 5, color = "#555555", linewidth = 0.5) +
    geom_text(data = plots_df_EPSG3035 %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type),
              aes(x = x - plot_cell_dimension / 2 + 10 + 10, y = y - plot_cell_dimension / 2 + 10 + 6,
                  label = "20 m"), color = "#555555", size = 2) +
    # Appearance of the plot
    # coord_fixed() + xlim(xlims) + ylim(ylims) +
    # coord_cartesian(xlim = xlims, ylim = ylims) +
    coord_sf(xlim = xlims, ylim = ylims, expand = FALSE, crs = st_crs(3035)) +
    ggtitle(paste("CFB plot", this_relat_cfb, "-", this_plot_type)) +
    labs(fill = "Canopy\nCover (%)", color = "Plot Type",
         x = "Longitude (EPSG:3035)", y = "Latitude (EPSG:3035)") +
    # Theme with x-y axes but no grid
    theme_minimal() +
    theme(axis.ticks = element_line(color = "black", size = 0.5),
          axis.ticks.length = unit(0.3, "cm"),
          axis.text = element_text(color = "black", size = 7)) + 
    guides(shape = guide_legend(override.aes = list(size = 0.5)))
  
  if (in_patchwork) {
    # if (this_plot_type != "edge") {
    #   p <- p + theme(legend.position = "none")  # Hide legend for edge plots
    # }
    if (this_plot_type != "forest") {
      p <- p + theme(axis.title.y = element_blank())  # Hide y-axis for non-forest plots
    }
    if (this_relat_cfb != unique(plots_df$relat_cfb)[length(unique(plots_df$relat_cfb))]) {
      p <- p + theme(axis.title.x = element_blank())  # Hide x-axis for non-last CFB plots
    }
  }
  
  return(p)
}

# Build Patchwork
pbar = progress_bar$new(total = length(unique(plots@data$relat_cfb)) * length(unique(plots@data$plot_type)),
                        clear = FALSE, width = 90)

plot_list_veg_roads <- list()
for (this_relat_cfb in unique(plots_df$relat_cfb)) {
  for (this_plot_type in unique(plots_df$plot_type)) {
    plot_list_veg_roads[[paste(this_relat_cfb, this_plot_type, sep = "_")]] <- 
      create_plot_veg_roads(this_relat_cfb, this_plot_type)
    
    ggsave(create_plot_veg_roads(this_relat_cfb, this_plot_type, in_patchwork = FALSE), 
           filename = paste0(folder_out, typ, "/all_canopy_cover/canopy_cover_with_roads-", this_plot_type, "_", this_relat_cfb, ".", typ), 
           dpi = 300, device = typ, width = 6, height = 6)
    
    pbar$tick()
    
  }
}
combined_plot_veg_roads <- wrap_plots(plot_list_veg_roads, ncol = length(unique(plots_df$plot_type)))

# print(combined_plot)

if (!is.null(folder_out)) {
  for (typ in types_plot_out) {
    file_out_veg_roads = paste0(folder_out, typ, "/patchwork-canopy_cover_with_roads.", typ)
    ggsave(combined_plot_veg_roads, filename = file_out_veg_roads, dpi = 300, device = typ,
           width = length(unique(plots_df$plot_type)) * 4.5, height = length(unique(plots_df$relat_cfb)) * 3.5)
  }
}




# Satellite ---------------------------------------------------------------

library(basemaps)
library(sf)

set_defaults(map_token = mapbox_token)




# this_relat_cfb = 38
# this_plot_type = "road"


create_plot_sat <- function(this_relat_cfb, this_plot_type, with_roads=TRUE) {
  
  this_filter = plots_WGS84@data$relat_cfb == this_relat_cfb & plots_WGS84@data$plot_type == this_plot_type
  center_x <- plots_WGS84@coords[this_filter, 1]
  center_y <- plots_WGS84@coords[this_filter, 2]
  
  # Create the extent by buffering the center point
  center_point <- st_point(c(center_x, center_y))
  center_sf <- st_sf(geometry = st_sfc(center_point, crs = 4326)) # WGS 84 CRS
  extent_sf <- st_buffer(center_sf, dist = plot_cell_dimension / 2)
  bbox <- st_bbox(extent_sf)
  polygon <- st_polygon(list(rbind(c(bbox["xmin"], bbox["ymin"]), c(bbox["xmin"], bbox["ymax"]), c(bbox["xmax"], bbox["ymax"]),
                                   c(bbox["xmax"], bbox["ymin"]), c(bbox["xmin"], bbox["ymin"]) )))
  ext <- st_sf(geometry = st_sfc(polygon, crs = st_crs(center_sf)),
                X_leaflet_id = 1, feature_type = "rectangle")
  
  # basemap_ggplot(ext, map_service = "mapbox", map_type = "satellite")
  
  
  # Roads
  roads_df_EPSG3857 = fortify(roads_EPSG3857)
  roads_infos_EPSG3857 = data.frame(id = 1:length(roads_EPSG3857@data$relat_cfb), relat_cfb = roads_EPSG3857@data$relat_cfb, 
                                    plot_type = roads_EPSG3857@data$plot_type, fclass = roads_EPSG3857@data$fclass) %>% 
    mutate(id = as.character(id))
  roads_df_EPSG3857 = left_join(roads_df_EPSG3857, roads_infos_EPSG3857, by = "id")
  
  # Plots
  plots_df_EPSG3857 <- data.frame(x = plots_EPSG3857@coords[, 1], y = plots_EPSG3857@coords[, 2],
                                  relat_cfb = plots_EPSG3857@data$relat_cfb,
                                  color = plots_EPSG3857@data$color, plot_type = plots_EPSG3857@data$plot_type)
  
  # Buffer
  plots_sf <- st_as_sf(plots_df_EPSG3857, coords = c("x", "y"), crs = 3857)
  plots_buffer <- st_buffer(plots_sf, dist = buffer_radius)
  plots_buffer_df <- plots_buffer %>%
    st_cast("POLYGON") %>%
    st_coordinates() %>%
    as.data.frame() %>%
    mutate(id = rep(1:nrow(plots_sf), each = nrow(.) / nrow(plots_sf))) %>% 
    left_join(plots_df_EPSG3857 %>% mutate(id = row_number()), by = "id")
  
  
  # Prepare plot
  st_bbox(st_transform(ext, crs = 3857))["xmin"]
  xlims = c(st_bbox(st_transform(ext, crs = 3857))["xmin"], st_bbox(st_transform(ext, crs = 3857))["xmax"])
  ylims = c(st_bbox(st_transform(ext, crs = 3857))["ymin"], st_bbox(st_transform(ext, crs = 3857))["ymax"])
  # palette_paths <- c(
  #   "service" =      "#f3ce13",
  #   "track" =        "#1f6406",
  #   "track_grade1" = "#0f3a00",
  #   "track_grade2" = "#165200",
  #   "track_grade3" = "#419523",
  #   "track_grade4" = "#61a04b",
  #   "track_grade5" = "#85b167",
  #   "path" =         "#15bfa8",
  #   "unclassified" = "#898989"
  # )
  palette_paths <- scales::hue_pal()(9)
  names(palette_paths) = c("service", "track", "track_grade1", "track_grade2", "track_grade3",
                           "track_grade4", "track_grade5", "path", "unclassified")
  this_color_plot = plots_df_EPSG3857 %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type) %>% pull(color)
  
  # Show
  p <- basemap_ggplot(ext, map_service = "mapbox", map_type = "satellite")
  title = paste("Satellite view on CFB", this_relat_cfb, "-", this_plot_type)
  if (with_roads) {
    p <- p + geom_path(data = roads_df_EPSG3857 %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type),
                       aes(x = long, y = lat, group = id, color = fclass), size = 1) + 
      scale_color_manual(values = palette_paths)
    title = paste("Satellite view and roads on CFB", this_relat_cfb, "-", this_plot_type)
  }
  p <- p +
    geom_point(data = plots_df_EPSG3857 %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type),
               aes(x = x, y = y), color = this_color_plot, size = 3) +
    geom_polygon(data = plots_buffer_df %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type),
                 aes(x = X, y = Y, group = id), alpha = 0.2, fill = this_color_plot, color = this_color_plot) +
    # Scale legend
    # White rectangle
    geom_rect(data = plots_df_EPSG3857 %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type),
              aes(xmin = x - plot_cell_dimension / 2 - 2, xmax = x - plot_cell_dimension / 2 + 24,
                  ymin = y - plot_cell_dimension / 2 - 3, ymax = y - plot_cell_dimension / 2 + 11),
              fill = "white", color = "black", linewidth = 0.2, alpha = 0.5) +
    geom_errorbarh(data = plots_df_EPSG3857 %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type),
                   aes(xmin = x - plot_cell_dimension / 2 + 1, xmax = x - plot_cell_dimension / 2 + 1 + 20,
                       y = y - plot_cell_dimension / 2 + 1), height = 5, color = "black", linewidth = 0.5) +
    geom_text(data = plots_df_EPSG3857 %>% filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type),
              aes(x = x - plot_cell_dimension / 2 + 1 + 10, y = y - plot_cell_dimension / 2 + 1 + 6,
                  label = "20 m"), color = "black", size = 2) +
    # Appearance of the plot
    # coord_cartesian(xlim = xlims, ylim = ylims) +
    # xlim(xlims) + ylim(ylims) +
    # coord_fixed() +
    coord_sf(xlim = xlims, ylim = ylims, expand = FALSE, crs = st_crs(3857)) +
    ggtitle(title) +
    labs(x = "Longitude (EPSG:3857)", y = "Latitude (EPSG:3857)", color = "Road Type") +
    # Theme with x-y axes but no grid
    theme_minimal() +
    theme(axis.ticks = element_line(color = "black", size = 0.5),
          axis.ticks.length = unit(0.3, "cm"),
          axis.text = element_text(color = "black", size = 8))
  
  return(p)
}

create_plot_sat(this_relat_cfb = 38, this_plot_type = "path")


# Build Patchwork
plot_list_sat_only <- list()
plot_list_sat_road <- list()
i = 1
for (this_relat_cfb in unique(plots_df$relat_cfb)) {
  cat("\n\n -> Processing CFB plot", this_relat_cfb, "...(", i, "/" , length(unique(plots_df$relat_cfb)), ")\n")
  i = i + 1
  for (this_plot_type in unique(plots_df$plot_type)) {
    cat("   - Plot type: ", this_plot_type, "...\n")
    
    plot_list_sat_road[[paste(this_relat_cfb, this_plot_type, sep = "_")]] <- 
      create_plot_sat(this_relat_cfb, this_plot_type)
    plot_list_sat_only[[paste(this_relat_cfb, this_plot_type, sep = "_")]] <-
      create_plot_sat(this_relat_cfb, this_plot_type, with_roads = FALSE)
    
    if (!is.null(folder_out)) {
      for (typ in types_plot_out) {
        
        file_out_road = paste0(folder_out, typ, "/all_satellites/satellite_road-", this_plot_type, "_", this_relat_cfb, ".", typ)
        ggsave(plot_list_sat_road[[paste(this_relat_cfb, this_plot_type, sep = "_")]], 
               filename = file_out_road, dpi = 300, device = typ, width = 6, height = 6)
        file_out_only = paste0(folder_out, typ, "/all_satellites/satellite_only-", this_plot_type, "_", this_relat_cfb, ".", typ)
        ggsave(plot_list_sat_only[[paste(this_relat_cfb, this_plot_type, sep = "_")]] +
                 theme(plot.margin = margin(1, 1, 1, 1, "cm")), 
               filename = file_out_only, dpi = 300, device = typ, width = 6, height = 6)
      }
    }
  }
}

combined_plot_sat_road <- wrap_plots(plot_list_sat_road, ncol = length(unique(plots_df$plot_type)))
combined_plot_sat_only <- wrap_plots(plot_list_sat_only, ncol = length(unique(plots_df$plot_type)))

if (!is.null(folder_out)) {
  for (typ in types_plot_out) {
    file_out_road = paste0(folder_out, typ, "/patchwork-satellites_road.", typ)
    ggsave(combined_plot_sat_road, filename = file_out_road, dpi = 300, device = typ,
           width = length(unique(plots_df$plot_type)) * 4.5, height = length(unique(plots_df$relat_cfb)) * 3.5)
    file_out_only = paste0(folder_out, typ, "/patchwork-satellites_only.", typ)
    ggsave(combined_plot_sat_only, filename = file_out_only, dpi = 300, device = typ,
           width = length(unique(plots_df$plot_type)) * 4.5, height = length(unique(plots_df$relat_cfb)) * 4.5)
  }
}











# # Fourrier transform ------------------------------------------------------
# 
# this_relat_cfb = 188
# for (this_plot_type in unique(df_all_raster_subset$plot_type)) {
#   
#   df_tmp = df_all_raster_subset %>% 
#     filter(relat_cfb == this_relat_cfb & plot_type == this_plot_type)
#   df_tmp %>% head()
#   p1 = ggplot(data = df_tmp, aes(x = x, y = y, fill = vegetation)) +
#     geom_raster() + 
#     ggtitle(paste0("Canopy Cover\nCFB ", this_relat_cfb, " - ", this_plot_type))
#   
#   print(p1)
#   
#   # Custom fftshift function
#   fftshift <- function(fft_matrix) {
#     n <- nrow(fft_matrix)
#     shifted_matrix <- matrix(0, nrow = n, ncol = n)
#     for (i in 1:n) {
#       for (j in 1:n) {
#         shifted_matrix[i, j] <- fft_matrix[(i + n/2 - 1) %% n + 1, (j + n/2 - 1) %% n + 1]
#       }
#     }
#     return(shifted_matrix)
#   }
#   
#   # Convert the raster data to a matrix
#   raster_matrix <- matrix(df_tmp$vegetation, nrow = length(unique(df_tmp$x)), ncol = length(unique(df_tmp$y)), byrow = TRUE)
#   # Apply the Fourier Transform
#   fft_matrix <- fft(raster_matrix)
#   # Shift the zero-frequency component to the center
#   shifted_fft_matrix <- fftshift(fft_matrix)
#   # Calculate the magnitude spectrum
#   magnitude_spectrum <- abs(shifted_fft_matrix)
#   # Convert the magnitude spectrum to a data frame for plotting
#   magnitude_spectrum_df <- as.data.frame(magnitude_spectrum)
#   magnitude_spectrum_df$x <- as.numeric(rownames(magnitude_spectrum_df))
#   magnitude_spectrum_df <- tidyr::gather(magnitude_spectrum_df, key = "y", value = "value", -x)
#   magnitude_spectrum_df$y <- as.numeric(gsub("V", "", magnitude_spectrum_df$y))
#   
#   # Visualize the magnitude spectrum
#   p2 = ggplot(magnitude_spectrum_df, aes(x = x, y = y, fill = log(value))) +
#     geom_tile() +
#     scale_fill_gradient(low = "white", high = "black") +
#     ggtitle(paste0("Magnitude Spectrum of the Fourier Transform\nCFB ", this_relat_cfb, " - ", this_plot_type)) +
#     theme_minimal() +
#     theme(axis.title.x = element_blank(),
#           axis.title.y = element_blank(),
#           axis.text = element_blank(),
#           axis.ticks = element_blank())
#   print(p2)
# }
# 
# 
# 
# # Previous --------------------------------------------------------------------
# 
# # Empty dataframe with columns id, relat_cfb, plot_type, and vegetation
# df_full = NULL
# 
# 
# pbar = progress_bar$new(
#   format = "  Computing vegetation cover [:bar] :percent eta: :eta",
#   total = length(unique(plots@data$relat_cfb)) * length(unique(plots@data$plot_type)),
#   clear = FALSE, width = 90
# )
# 
# # this_relat_cfb = unique(plots@data$relat_cfb)[1]
# # this_plot_type = unique(plots@data$plot_type)[1]
# 
# for (this_relat_cfb in unique(plots@data$relat_cfb)) {
#   for (this_plot_type in unique(plots@data$plot_type)) {
#     
#     this_point = c(this_relat_cfb, this_plot_type)
#     this_filter = plots@data$relat_cfb == this_point[1] & plots@data$plot_type == this_point[2]
#     this_center = plots@coords[this_filter, ]
#     this_color = plots@data$color[this_filter]
#     
#     # Plot the landscape
#     if (!is.null(folder_out)) {
#       file_out = paste0(folder_out, "vegetation_cover-", this_point[2], "_", this_point[1], ".png")
#       png(file_out, width = 800, height = 600)
#     }
#     plot(r_vegetation, main = paste("Vegetation Cover (Canopy Cover Density)\nCFB", this_point[1], "-", this_point[2]),
#          col = colorRampPalette(c("white", "tomato"))(100), zlim = c(0, 100), 
#          xlim = c(this_center[1] - plot_cell_dimension/2, this_center[1] + plot_cell_dimension/2),
#          ylim = c(this_center[2] - plot_cell_dimension/2, this_center[2] + plot_cell_dimension/2))
#     plot(plots, add = TRUE, col = plots@data$color, pch = 20, cex = 2)
#     symbols(this_center[1], this_center[2], circles = buffer_radius, inches = FALSE, 
#             add = TRUE, lwd = 2, fg = "#555555")
#     # add = TRUE, lwd = 2, fg = this_color)
#     if (!is.null(folder_out)) {
#       dev.off()
#     }
#     
#     # Compute statistics within the buffer
#     buffer_polygon = sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(cbind(
#       this_center[1] + buffer_radius * cos(seq(0, 2 * pi, length.out = 100)),
#       this_center[2] + buffer_radius * sin(seq(0, 2 * pi, length.out = 100))
#     ))), ID = "buffer")))
#     r_vegetation_masked = mask(r_vegetation, buffer_polygon)
#     these_values = r_vegetation_masked@data@values[!is.na(r_vegetation_masked@data@values)]
#     
#     # df_stats[df_stats$relat_cfb == this_point[1] & df_stats$plot_type == this_point[2], 
#     #           c("vegetation_mean", "vegetation_sd", "vegetation_min", 
#     #             "vegetation_max", "vegetation_q5", "vegetation_q25", 
#     #             "vegetation_median", "vegetation_q75", "vegetation_q95")] = 
#     #   c(mean(these_values), sd(these_values), min(these_values), 
#     #     max(these_values), quantile(these_values, 0.05), quantile(these_values, 0.25), 
#     #     median(these_values), quantile(these_values, 0.75), quantile(these_values, 0.95))
#     
#     if (is.null(df_full)) {
#       df_full = data.frame(relat_cfb = this_point[1], plot_type = this_point[2], 
#                            vegetation = these_values)
#     } else {
#       df_full = rbind(df_full, data.frame(relat_cfb = this_point[1], plot_type = this_point[2], 
#                                           vegetation = these_values))
#     }
#     
#     pbar$tick()
#     
#   }
# }
# 
# head(df_full)

  
  
  



