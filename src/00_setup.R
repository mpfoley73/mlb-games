# Define theme colors for plots.
cle_colors <- c("navy_blue" = "#0C2340", "red" = "#E31937")
oak_colors <- c("gold" = "#efb21e", "kellygreen" = "#003831")
vintage_colors <- c("#CBBCB1", "#AF6B58", "#556052", "#F2EFEA")
all_colors <- c(cle_colors, oak_colors, vintage_colors)
names(all_colors) <- NULL

# Instead of typing this out for every ggplot.
theme_set(
  theme_light() +
    theme(
      text = element_text(size = 16, family = "Rockwell Condensed", color = cle_colors["navy_blue"]),
      plot.caption = element_text(family = "Calibri Light", hjust = 0, size = 10),
      panel.grid.minor = element_blank()
    )
)

# Format decimal hours into h:m.
pretty_hm <- function(hrs) { 
  str_sub(chron::times(hrs %/% 60 + hrs %% 60 / 60) / 24, 5, 8) 
}
