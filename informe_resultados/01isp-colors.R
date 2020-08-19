# 0. Set up
# ISP corporate colors
isp_colors <- c(`red`= "#d40c04",`pink` = "#e05e5a",`lightpink` = "#fee4e2",`white` = "#e3dac9",`lightgrey`= "#d3d3d3")
isp_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (isp_colors)
  
  isp_colors[cols]
}
isp_palettes <- list(`main`  = isp_cols("red", "pink", "lightpink",  "white", "black",  "lightgrey"))
isp_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- isp_palettes[[palette]]
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}
scale_color_isp <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- isp_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("isp_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}
scale_fill_isp <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- isp_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("isp_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

