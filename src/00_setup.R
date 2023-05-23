# Define theme colors for plots.
# https://colorkit.co/palettes/mlb/
# https://teamcolorcodes.com/mlb-color-codes/
pal_cle <- c("cle_blue" = "#0C2340", "cle_red" = "#E31937")
pal_oak <- c("oak_gold" = "#efb21e", "oak_green" = "#003831")
pal_sfg <- c("sfg_orange" = "#fd5a1e", "sfg_black" = "#27251f", "sfg_cream" = "#efd19f", "sfg_brown" = "#ae8f6f")
pal_old <- c("old_dust" = "#CBBCB1", "old_dirt" = "#AF6B58", "old_grass" = "#556052", "old_plate" = "#F2EFEA")
pal_mlb <- c(pal_cle, pal_oak, pal_sfg, pal_old)
# names(all_colors) <- NULL

# Instead of typing this out for every ggplot.
theme_set(
  theme_light() +
    theme(
      text = element_text(family = "Rockwell Condensed", size = 16, color = "#003831"),
      plot.subtitle = element_text(family = "Rockwell", size = 12),
      plot.caption = element_text(size = 10, hjust = 0, color = "#666666"),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      axis.title.y.right = element_text(margin = margin(0, 0, 0, 10))
    )
)

caption_title <- function(title, desc) { glue("**Fig. {(fig_no <- fig_no + 1)} {title}**. {desc}") }

# Format decimal hours into h:m.
pretty_hm <- function(hrs, verbose = FALSE) { 
  my_datetime <- as_datetime(today() + dhours(hrs)) %>% round_date("minute")
  if(verbose) {
    my_str <- paste(hour(my_datetime), "hours", minute(my_datetime), "minutes")
  } else {
    my_str <- format(my_datetime, "%H:%M")
  }
  my_str
}

# Format decimal minutes into m:s.
pretty_ms <- function(minutes, verbose = FALSE) { 
  my_datetime <- as_datetime(today() + dminutes(minutes)) %>% round_date("second")
  if(verbose) {
    my_str <- paste(minute(my_datetime), "minutes", second(my_datetime), "seconds")
  } else {
    my_str <- format(my_datetime, "%M:%S")
  }
  my_str
}
