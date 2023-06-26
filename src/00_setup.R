# Define theme colors for plots.
# https://colorkit.co/palettes/mlb/
# https://teamcolorcodes.com/mlb-color-codes/
pal <- list()
pal$cle <- c("B" = "#0C2340", "R" = "#E31937")
pal$oak <- c("Y" = "#efb21e", "G" = "#003831")
pal$sfg <- c("O" = "#fd5a1e", "B" = "#27251f", "C" = "#efd19f", "T" = "#ae8f6f")
pal$old <- c("dust" = "#CBBCB1", "dirt" = "#AF6B58", "grass" = "#556052", "plate" = "#F2EFEA")
pal$mlb <- as.character(c(pal$cle, pal$oak["Y"], pal$sfg["O"], pal$old[2:3]))
# names(all_colors) <- NULL

# Instead of typing this out for every ggplot.
theme_set(
  theme_light() +
    theme(
      strip.background = element_rect(fill = pal$old["grass"]),
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

proj_dir <- "C:/Users/mpfol/OneDrive/Documents/GitHub/mlb-games"
