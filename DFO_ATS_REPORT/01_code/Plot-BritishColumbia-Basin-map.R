# ============================================================
# 0. LIBRARIES & FUNCTIONS
# ============================================================

library(sf)
library(dplyr)
library(ggplot2)

sf::sf_use_s2(FALSE)   # avoid spherical geometry warnings

# Scaling ggplot data for ggsave 3.5 x 3.5" and 600 dpi
rescale_plot_for_panel <- function(p,
                                   target_width = 3.5,
                                   target_height = 3.5,
                                   reference_width = 7,
                                   reference_height = 7) {
  
  # Compute scale factor (use width; assumes square-ish layouts)
  scale_factor <- min(target_width / reference_width,
                      target_height / reference_height)
  
  # --- Scale theme text ---------------------------------------
  p <- p + theme(
    text = element_text(size = rel(1) * scale_factor),
    plot.title = element_text(size = rel(1.2) * scale_factor),
    axis.title = element_text(size = rel(1) * scale_factor),
    axis.text = element_text(size = rel(0.8) * scale_factor),
    legend.title = element_text(size = rel(0.9) * scale_factor),
    legend.text = element_text(size = rel(0.8) * scale_factor)
  )
  
  # --- Scale geom linewidths ----------------------------------
  p$layers <- lapply(p$layers, function(layer) {
    
    # Handle modern ggplot (linewidth)
    if (!is.null(layer$aes_params$linewidth)) {
      layer$aes_params$linewidth <- layer$aes_params$linewidth * scale_factor
    }
    
    # Handle older ggplot (size)
    if (!is.null(layer$aes_params$size)) {
      layer$aes_params$size <- layer$aes_params$size * scale_factor
    }
    
    return(layer)
  })
  
  return(p)
}

# ============================================================
# 1. LOAD DATA (ENSURE CRS = 4326)
# ============================================================

zoom_lat_low <- 45
zoom_lat_hi  <- 60

zoom_long_low <- -113
zoom_long_hi  <- -136

bbox_map <- st_as_sfc(st_bbox(
  c(xmin = -136, xmax = -103,
    ymin = 41, ymax = 63),
  crs = 4326))

#rivers_full <- st_read(
#  "/DFO-Report-2026/maps/HYDRO/HydroRIVERS_v10_na_shp/HydroRIVERS_v10_na.shp") %>%
#  st_transform(4326) %>%
#  st_crop(st_bbox(c(xmin = -140, xmax = -103,
#                    ymin = 41, ymax = 63), crs = 4326))
#saveRDS(rivers_full, "/DFO-Report-2026/maps/HYDRO/HydroRIVERS_v10_na_shp/rivers_full.rds")
rivers_full <- readRDS("/Users/aliceassmar/Documents/00-LDP-DFO-Project/DFO-Report-2026/maps/HYDRO/HydroRIVERS_v10_na_shp/rivers_full.rds")

#lakes <- st_read(
#  "/DFO-Report-2026/maps/HYDRO/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10.shp") %>% st_transform(4326)
#saveRDS(lakes, "lakes.rds")
lakes_complete <- readRDS("/Users/aliceassmar/Documents/00-LDP-DFO-Project/DFO-Report-2026/maps/HYDRO/HydroLAKES_polys_v10_shp/lakes.rds")

lakes <- lakes_complete %>%
  filter(Country %in% c("Canada", "United States of America")) %>%
  arrange(Lake_name)

# New shapefile
#lakes_new <- st_read(
#    "/DFO-Report-2026/maps/HYDRO/LIO-2019-10-03/WATERSHED_PRIMARY.shp") %>% 
#  st_transform(4326)
#  saveRDS(lakes_new, "/DFO-Report-2026/maps/HYDRO/LIO-2019-10-03/WATERSHED_PRIMARY.rds")
#lakes_new <- readRDS("/DFO-Report-2026/maps/HYDRO/LIO-2019-10-03/WATERSHED_PRIMARY.rds")

# ==================
# 2. FWA dataset using gdb files
# ==================

# Read the entire dataset
gdb_path <- "/Users/aliceassmar/Documents/00-LDP-DFO-Project/DFO-Report-2026/maps/HYDRO/FWA_BC.gdb"
st_layers(gdb_path)

# read the lake layer
bc_lakes <- st_read(gdb_path, layer = "FWA_LAKES_POLY")

### 2.1 Prepare the outside tables
############## Read csv tables
Acoustic_data <- read.csv("./00_data/Target_OUTPUT_data_FINAL_(1977_2007).csv")
Trawl_data <- read.csv("./00_data/Trawl_data_FINAL_1977-1999.csv")
lake_name <- read.csv("./00_data/lake_codes.csv")

# re-assign lake codes
Acoustic_data <- Acoustic_data %>%
  mutate(lake_code = case_when(lake_code == 248 ~ 180,
                               lake_code == 250	~ 50,
                               lake_code == 247	~ 121,
                               lake_code == 251	~ 237,
                               lake_code == 249	~ 59,
                               lake_code == 252	~ 122,
                               lake_code == 255	~ 241,
                               TRUE ~ lake_code))

# Join the tables
Acoustic_data <- Acoustic_data %>%
  mutate(lake_code = case_when(lake_code == 124 ~ 180,
                               TRUE ~ lake_code)) %>%
  left_join(lake_name, by = "lake_code")

# extract the lakes in the dataset
lakes_in_acoustic <- unique(Acoustic_data$lake_name) # extract the lake names acoustic dataset
lakes_in_trawl <- unique(Trawl_data$lake_name) # Extract the lake names trawl dataset

lakes_in_ATS_database <- c(lakes_in_acoustic, lakes_in_trawl) # combine the lists
lakes_in_ATS_database <- lakes_in_ATS_database[!is.na(lakes_in_ATS_database)] # remove NAs

# Search the lakes in the FWA database
bc_loi <- bc_lakes %>%
  filter(GNIS_NAME_1 %in% lakes_in_ATS_database)

bc_loi <- bc_loi %>%
  arrange(GNIS_NAME_1)
# ============================================================
# 1. LOAD DATA (ENSURE CRS = 4326) - Continuation
# ============================================================

# Set the lakes to the coordinate reference system that the other are
st_crs(bc_loi)
bc_loi_4326 <- st_transform(bc_loi, crs = 4326)

target_lakes <- bc_loi_4326 %>%
  st_intersection(bbox_map)

target_lakes_US <- lakes %>%
  filter(Lake_name %in% lakes_in_ATS_database) %>%
  st_intersection(bbox_map)

#basins <- st_read(
# "/DFO-Report-2026/maps/HYDRO/hybas_lake_na_lev01-12_v1c/hybas_lake_na_lev05_v1c.shp") %>%
# st_transform(4326)
#saveRDS(basins, "/DFO-Report-2026/maps/HYDRO/hybas_lake_na_lev01-12_v1c/basins.rds")
basins <- readRDS("/Users/aliceassmar/Documents/00-LDP-DFO-Project/DFO-Report-2026/maps/HYDRO/hybas_lake_na_lev01-12_v1c/basins.rds")

#countries <- st_read("/DFO-Report-2026/maps/HYDRO/10m_cultural/ne_10m_admin_0_countries.shp")%>%
#  st_transform(4326)
#saveRDS(countries, "/DFO-Report-2026/maps/HYDRO/10m_cultural/countries.rds")
countries <- readRDS("/Users/aliceassmar/Documents/00-LDP-DFO-Project/DFO-Report-2026/maps/HYDRO/10m_cultural/countries.rds")

# Dissolve all countries into one landmass first
land_union <- st_union(countries)
# Extract ONLY the outer coastline
coast <- st_boundary(land_union)

# --- Extract Canada–USA border ---
canada <- countries %>% filter(ADMIN == "Canada")
usa    <- countries %>% filter(ADMIN == "United States of America")

# Shared boundary = international border
intl_border <- st_intersection(
  st_boundary(canada),
  st_boundary(usa))

# Extract BC and Alberta border
provinces <- st_read("/Users/aliceassmar/Documents/00-LDP-DFO-Project/DFO-Report-2026/maps/HYDRO/lpr_000b16a_e/lpr_000b16a_e.shp") %>%
  st_transform(4326)

# Extract British Columbia, Northern Territories, Yukon and Alberta:
bc <- provinces %>% filter(PRENAME == "British Columbia")

ab <- provinces %>% filter(PRENAME == "Alberta")

nt <- provinces %>% filter(PRENAME == "Northwest Territories")

yt <- provinces %>% filter(PRENAME == "Yukon")

# Then calculate their shared boundary:
bc_ab_border <- st_intersection(st_boundary(bc), st_boundary(ab))

bc_yt_border <- st_intersection(st_boundary(bc), st_boundary(yt))

bc_nt_border <- st_intersection(st_boundary(bc),st_boundary(nt))

# ============================================================
# 3. CLIP RIVERS TO BASIN
# ============================================================

# --- REMOVE minor rivers EARLY ---
rivers_filtered <- rivers_full %>%
  filter(DIS_AV_CMS >= 20)   # keep only medium + major

# --- then crop (cheap) ---
rivers_columbia <- rivers_filtered %>%
  st_crop(st_bbox(bbox_map)) # %>%
  # st_intersection(columbia_basin)

# ============================================================
# 4. BUILD RIVER HIERARCHY (CLEAN + ROBUST)
# ============================================================

rivers_major  <- rivers_columbia %>% filter(DIS_AV_CMS >= 500) %>% st_simplify(dTolerance = 0.001)
rivers_medium <- rivers_columbia %>% filter(DIS_AV_CMS >= 30 & DIS_AV_CMS < 500) %>% st_simplify(dTolerance = 0.001)
rivers_minor  <- rivers_columbia %>% filter(DIS_AV_CMS < 30) %>% st_simplify(dTolerance = 0.001)

# ============================================================
# 5B. CLIP COASTLINE TO MAP EXTENT
# ============================================================

coast_clipped <- st_intersection(coast, bbox_map)


#============================================================
# Include labels (manual, stable)
#============================================================

### River labels
lake_labels <- read.csv(file.choose())
lake_labels <- st_as_sf(lake_labels, coords = c("lon", "lat"), crs = 4326)

# ============================================================
# 5. FINAL BASIN MAP (PUBLICATION-READY)
# ============================================================

locator_map <- ggplot() +
  theme_bw() +
  
  # --- Ocean background 
  geom_sf(data = bbox_map, fill = "lightblue", 
          #"#f7fbff",
          color = NA) +
  
  # --- Land mask (countries) ---
  geom_sf(data = countries, fill = "white", color = NA) +
  
  # ----- BC / AB ---
  geom_sf(data = bc_ab_border,
    color = "grey40", linewidth = 0.6, linetype = "solid") +
  
  # --- coastline ---
  geom_sf(data = coast_clipped, color = "black", linewidth = 0.3) +
  
  # --- International border ---
  geom_sf(data = intl_border, color = "grey50", linewidth = 0.5, linetype = "dashed") +
  
  # --- rivers ---
   geom_sf(data = rivers_minor,
           color = "grey85",
           linewidth = 0.15) +
  
  geom_sf(data = rivers_medium, color = "lightblue", linewidth = 0.3) +
  
  geom_sf(data = rivers_major, color = "steelblue", linewidth = 0.8) +
  
  # Lakes (add after rivers so they sit on top)
  geom_sf(data = target_lakes, fill = "lightblue", color = "green", linewidth = 0.8) +
  
  # Lakes (add after rivers so they sit on top)
  geom_sf(data = target_lakes_US, fill = "lightblue", color = "green", linewidth = 0.8) +

  coord_sf(                   # full Columbia Basin
    xlim = c(-136, -103), 
    ylim = c(41, 63), expand = FALSE) +
  
  coord_sf(                     # Columbia Basin 
    xlim = c(zoom_long_hi, zoom_long_low),
    ylim = c(zoom_lat_low, zoom_lat_hi),
    expand = FALSE) +
  
  scale_x_continuous(
    breaks = seq(zoom_long_hi, zoom_long_low, by = 1),
    labels = function(x) paste0(abs(x), "°W")) +
  
  scale_y_continuous(
    breaks = seq(zoom_lat_low, zoom_lat_hi, by = 1),
    labels = function(y) paste0(y, "°N")) +
  
  labs(title = "", # "Locator Map",
    x = NULL, y = NULL) +
  
  # --- Region labels ---
  annotate("label", fill = "white", linewidth=0,
           x = -122, y = 59.75,
           label = "British",
           size = 4,
           fontface = "bold",
           color = "black") +
  
  annotate("label", fill = "white", linewidth=0,
           x = -118, y = 59.75,
           label = "Alberta",
           size = 4,
           fontface = "bold",
           color = "black") +
#
  annotate("label", fill = "white", linewidth=0, 
           x = -122, y = 59.1,
           label = "Columbia",
           size = 4,
           fontface = "bold",
           color = "black") +
  
  annotate("label", fill = "white", linewidth=0, 
           x = -115, y = 49.3,
           label = "(CAN)",
           size = 3,
           fontface = "bold",
           color = "black") +
#
    #annotate("label", fill = "white", linewidth=0,  
    #       x = -115, y = 48,
    #       label = "Washington",
    #       size = 4,
    #       fontface = "bold",
    #       color = "black") +
#
    annotate("label", fill = "white", linewidth=0, 
           x = -115, y = 48.6,
           label = "(USA)",
           size = 3,
           fontface = "bold",
           color = "black") +
  
  annotate("label", fill = "white", linewidth=0,
           x = -121.5, y = 45.95,
           label = "Columbia",
           size = 3,
           fontface = "italic",
           color = "steelblue") +
  
  annotate("label", fill = "white", linewidth=0,
           x = -121.5, y = 45.4,
           label = "River",
           size = 3,
           fontface = "italic",
           color = "steelblue") +
  
  geom_sf_text(
    data = lake_labels,
    aes(label = name,
      hjust = hjust,
      angle = angle), color = "steelblue", size = 2,
    fontface = "italic"
    #nudge_x = river_labels$nudge_x,
    #nudge_y = river_labels$nudge_y
    ) +
  
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.ticks = element_line(),
    axis.ticks.length = unit(-2, "mm"),
    axis.text.x = element_text(angle = 60, vjust = 0.5, size = 8, margin = margin(t = 5)),
    axis.text.y = element_text(size = 8, margin = margin(r = 5)))

print(locator_map)


# Save the map
ggsave("./03_figs/Columbia_Locator_Map.png",
       locator_map,
       width = 7, height = 10, # cm
       dpi = 600,
       bg = "white")
