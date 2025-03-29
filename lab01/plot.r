library(ggplot2)
library(maps)
library(dplyr)
library(ggrepel)
library(ggpmisc)
library(khroma)
library(tablesgg)
library(RColorBrewer)

# Create dataframes for inflow and outflow data
migration_data <- data.frame(
  country = c("UAE", "USA", "Singapore", "Canada", "Australia",
              "Italy", "Switzerland", "Greece", "Portugal", "Japan",
              "China", "UK", "India", "South Korea", "Russia", 
              "Brazil", "South Africa", "Taiwan", "Nigeria", "Vietnam"
            ),
  net_flow = c(6700, 3800, 3500, 3200, 2500,
               2200, 1500, 1200, 800, 400,
               -15200, -9500, -4300, -1200, -1000,
               -800, -600, -400, -300, -300
              ),
  # Data from Wikipedia (https://en.wikipedia.org/wiki/List_of_countries_by_number_of_millionaires)
  total_millionaries = c(202201, 21951319, 333204, 1991416, 1936114,
                         1338142, 1054293, NA, 171797, 2827956,
                         6013282, 3061553, 868660, 1295674, 381726,
                         380585, 90595, 788799, NA, NA
                  ),
  stringsAsFactors = FALSE
)

migration_data$net_flow_fac = migration_data$net_flow / migration_data$total_millionaries * 1000

migration_top5low = head(migration_data[order(migration_data$net_flow_fac), ], 5)
migration_top5high = head(migration_data[order(migration_data$net_flow_fac, decreasing=TRUE), ], 5)
net_flow_fac_LABEL = 'Net migr. rate (promiles)'
migration_top5low[net_flow_fac_LABEL] = round(migration_top5low$net_flow_fac, 2)
migration_top5high[net_flow_fac_LABEL] = round(migration_top5high$net_flow_fac, 2)

migration_top5high

# Add column for map matching
migration_data$map_region <- c("United Arab Emirates", "USA", "Singapore", "Canada", "Australia",
                               "Italy", "Switzerland", "Greece", "Portugal", "Japan",
                               "China", "UK", "India", "South Korea", "Russia", 
                               "Brazil", "South Africa", "Taiwan", "Nigeria", "Vietnam"
                              )

migration_data = migration_data[complete.cases(migration_data), ]

# Get world map data and remove Antarctica
world_map <- map_data("world") %>% 
  filter(region != "Antarctica")

# Calculate area for each region to filter out small islands
region_areas <- world_map %>%
  group_by(region) %>%
  summarize(area = abs(sum(long * lag(lat) - lag(long) * lat, na.rm = TRUE)) / 2)

# Keep only major regions (adjust threshold as needed)
large_regions <- region_areas %>% 
  filter(area > 1) %>% 
  pull(region)

# Filter the world map to remove small islands
world_map_filtered <- world_map %>%
  filter(region %in% large_regions)

# Create centroids for label placement
country_centers <- world_map_filtered %>%
  group_by(region) %>%
  summarize(
    long = mean(range(long)),
    lat = mean(range(lat))
  )

# Manual adjustment for USA to fix offset
usa_idx <- which(country_centers$region == "USA")
if(length(usa_idx) > 0) {
  country_centers$long[usa_idx] <- -98  # Center of continental USA
  country_centers$lat[usa_idx] <- 39    # Center of continental USA
}

# Join with our migration data
label_data <- country_centers %>%
  inner_join(migration_data, by = c("region" = "map_region"))

# Format labels
label_data$label <- ifelse(
  label_data$net_flow > 0,
  paste0(label_data$country, ": ", format(round(label_data$net_flow_fac, 2), big.mark=",", scientific=F)),
  paste0(label_data$country, ": ", format(round(label_data$net_flow_fac, 2), big.mark=",", scientific=F))
)

# Join map data with migration data
mapped_data <- left_join(world_map_filtered, migration_data, by = c("region" = "map_region"))

# Set background color
bg_color <- "#19192D"

# Use sans font for reliability
font_family <- "sans"

nightfall <- color("nightfall")

# Create map
p <- ggplot() +
  # Set background
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = bg_color, color = NA),
    panel.background = element_rect(fill = bg_color, color = NA),
    plot.title = element_text(size = 18, face = "bold", color = "white", hjust = 0.5, family = font_family),
    legend.position = "bottom",
    legend.background = element_rect(fill = bg_color, color = NA),
    legend.text = element_text(color = "white", family = font_family),
    legend.title = element_text(color = "white", face = "bold", family = font_family),
    legend.key.width = unit(1.5, "cm"),
    legend.justification = "center",
    legend.box.just = "center",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    text = element_text(family = font_family)
  ) +
  # Base map layer - all countries in gray
  geom_polygon(data = world_map_filtered, aes(x = long, y = lat, group = group),
               fill = "#696969", color = "#3F3F5F", size = 0.2) +
  # Countries with migration data
  geom_polygon(data = mapped_data, aes(x = long, y = lat, group = group, fill = net_flow_fac),
               color = "#3F3F5F", size = 0.2) +
  # Color scale with stronger green for positive values
  scale_fill_gradientn(
    colors = brewer.pal(7,'RdYlGn'),,
    values = scales::rescale(c(-7, -5, -3, 0, 3, 5, 40)),
    breaks = c(-7, 0, 10, 20, 30, 40),
    na.value = "#696969",
    name = "Net Migration",
    limits = c(-7, 40),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  # Labels with sufficient spacing
  geom_text_repel(
    data = label_data,
    aes(x = long, y = lat, label = label),
    size = 3,
    fontface = "bold",
    color = "white",
    bg.color = "black",
    bg.r = 0.15,
    box.padding = 0.8,
    point.padding = 0.5,
    segment.color = "white",
    segment.size = 0.3,
    min.segment.length = 0.1,
    max.overlaps = 15,
    force = 10,
    family = font_family
  ) +
  # Table
  #annotate(geom = 'table',
  #         x=-200,
  #         y=-20,
  #         label=list(migration_top5high[c("country", net_flow_fac_LABEL)])) +
  #annotate(geom = 'table',
  #         x=-200,
  #         y=-60,
  #         label=list(migration_top5low[c("country", net_flow_fac_LABEL)])) +
  # Title
  labs(title = "Millionaire Migration in 2024") +
  # Fixed coordinate ratio with appropriate limits
  coord_fixed(1.3, ylim = c(-60, 85))

# Save the plot
ggsave("/tmp/plot.png", p, width = 12, height = 8, dpi = 300, bg = bg_color)


