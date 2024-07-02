#' Create faceted plot of DOY to reach threshold over time
#' 
#' Creates and saves to disk a ggplot heatmap of the DOY a GDD threshold is met
#' each year.  When there are pixels that never meet the threshold (DOY =
#' `Inf`), they share the same color as the maximum finite DOY and the color bar
#' is labeled "≥{max}".  If all values are finite, the maximum value is always
#' labeled on the color bar.
#' 
#' This creates the plot and saves it in one step because the ggplot2 object
#' can't be saved and re-loaded in another R session easily since the
#' geom_spatraster() layer saves a C++ pointer to data in memory
#'
#' @param gdd_stack SpatRaster; with layers corresponding to years and values of DOY
#' @param threshold numeric; the corresponding GDD threshold
#' @param out_dir character; the path to save the file (filenames created programmatically)
#' @param ext character; file extention to use for saving (passed to `ggsave()`)
#' @param ... other arguments passed to `ggsave()`, e.g. `width` and `height`.
#'
plot_doy <- function(gdd_stack, threshold, out_dir = "output/figs", ext = "png", ...) {
  fs::dir_create(out_dir)
  p <- ggplot() +
    geom_spatraster(data = gdd_stack) +
    scale_fill_viridis_c(
      na.value = "transparent", 
      option = "B",
      direction = -1, #earlier DOY = hotter color
      end = 0.9, #don't use the lightest yellow—hard to see on white background
      oob = scales::oob_squish_infinite,
      breaks = breaks_squish_doy(values(gdd_stack))
    ) +    
    facet_wrap(~lyr, ncol = 10) +
    labs(
      title = glue::glue("Days to reach > {threshold} GDD"),
      fill = "DOY"
    ) +
    scale_y_continuous(n.breaks = 2) +
    scale_x_continuous(n.breaks = 2) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
  filename <- glue::glue("doy_{threshold}.{ext}")
  ggsave(filename = filename, path = out_dir, plot = p, bg = "white", ...)
}




