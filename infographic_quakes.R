
suppressPackageStartupMessages(library(tidyverse))

# ---- 1. Read and validate command-line arguments --------------------------
args <- commandArgs(trailingOnly = TRUE)

usage <- "Usage: Rscript infographic_quakes.R <min_mag>\n  min_mag  number between 4.0 and 6.4\nExample: Rscript infographic_quakes.R 4.5"

if (length(args) != 1) {
  stop("Expected 1 argument, got ", length(args), ".\n", usage, call. = FALSE)
}

min_mag <- suppressWarnings(as.numeric(args[1]))
if (is.na(min_mag) || min_mag < 4.0 || min_mag > 6.4) {
  stop("'min_mag' must be a number between 4.0 and 6.4, got '", args[1],
       "'.\n", usage, call. = FALSE)
}

# Fixed settings (not arguments)
palette <- "plasma"
outfile <- "infographic.png"

# ---- 2. Data ---------------------------------------------------------------
# 'quakes' is part of base R's datasets package: same data on every machine.
dat <- quakes |>
  filter(mag >= min_mag) |>
  mutate(
    depth_band = cut(
      depth,
      breaks = c(0, 100, 300, Inf),
      labels = c("Shallow (0-100 km)",
                 "Medium (100-300 km)",
                 "Deep (300+ km)")
    )
  )

if (nrow(dat) == 0) {
  stop("No earthquakes with magnitude >= ", min_mag, ".\n", call. = FALSE)
}

# Numbers behind the picture
band_summary <- dat |>
  group_by(depth_band) |>
  summarise(n = n(),
            mean_depth_km = round(mean(depth)),
            median_long = round(median(long), 1),
            .groups = "drop")

# ---- 3. Plot ---------------------------------------------------------------
p <- ggplot(dat, aes(x = long, y = lat, colour = depth_band)) +
  geom_point(size = 2.5, alpha = 0.7) +
  scale_colour_viridis_d(option = palette, end = 0.85, name = NULL) +
  coord_fixed() +
  labs(
    title    = "Earthquake depth near Fiji",
    subtitle = paste0(nrow(dat), " earthquakes with magnitude ", min_mag,
                      " or higher"),
    x = "Longitude",
    y = "Latitude",
    caption  = "Data: R dataset 'quakes'"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 18),
    plot.subtitle   = element_text(colour = "grey30"),
    legend.position = "bottom"
  )

# ---- 4. Save ---------------------------------------------------------------
ggsave(outfile, plot = p, width = 8, height = 6.5, dpi = 150, bg = "white")
message("Wrote ", outfile)

print(band_summary)