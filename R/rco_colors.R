rco_color_list <- c(
  #blue to orange
  `blue1`  = "#2C3B97",
  `blue2`  = "#3C4CA0",
  `green`  = "#8DC63F",
  `brown1` = "#B34826",
  `orange` = "#F15622",

  #red to brown
  `red1`       = "#D81F26",
  `red2`       = "#E23326",
  `peach1`     = "#E27425",
  `greenbrown` = "#996C29",
  `brown2`     = "#CC5127",

  #teal to brown
  `teal1`  = "#40BEB1",
  `teal2`  = "#008072",
  `peach2` = "#F4753B",
  `brown3` = "#802F19",
  `brown2` = "#CC5127",

  `white` = "#FFFFFF"

)

### function takes a group of strings and returns the string and hex code
### can be used to call one of the 97 colors contained in the full color list
### within a plotting function

#'Retrieve a Color
#'
#'This function takes a string or group of strings and returns hex codes
#'for the corresponding colors
#'
#'A full list of the colors in this package can be returned by running the
#'function with no input
#'
#' @export
rco_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (rco_color_list)

  rco_color_list[cols]
}

rco_color_palettes <- list(
  `blue to orange` = rco_cols("blue1", "blue2", "green", "brown1", "orange"),

  `red to brown` = rco_cols("red1", "red2", "peach1", "greenbrown", "brown2"),

  `teal to brown` = rco_cols("teal1", "teal2", "peach2", "brown3", "brown2"),

  `blue_ramp` = rco_cols("blue1", "white"),

  `red_ramp` = rco_cols("red1", "white"),

  `teal_ramp` = rco_cols("teal2","white")
)

#'Retrieve a Palette
#'
#'This function excepts a character string for a palette and returns a function
#'that can be turned into a palette
#'
#'adding a number after the function call will create a palette with that number
#'of steps
#'
#'@param palette A string representing a color palette, see https://bfroebrco.github.io/rcocolorsr_pages/
#'for a full list of palettes
#'
#'@param reverse When true the default order of the color palette is reversed
#'
#'@return a function to create a color palette
#'
#' @export
rco_color_pal <- function(palette = "logo", reverse = FALSE, ...) {
  pal <- rco_color_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

#'Create a Color Scale for ggplot
#'
#'This function is inserted into a ggplot call to replace a scale_color
#'command.
#'
#'By default the palette will be discrete.
#'
#'@param palette A string representing a color palette, see https://bfroebrco.github.io/rcocolorsr_pages/
#'for a full list of palettes
#'
#'@param discrete Changes between discrete or continuous scales
#'
#'@param reverse When true the default order of the color palette is reversed
#'
#'@return a function to create a color palette
#' @export
scale_color_rco <- function(palette = "logo", discrete = TRUE, reverse = FALSE, ...) {
  pal <- rco_color_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("rco_color_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#'Create a Fill Scale for ggplot
#'
#'This function is inserted into a ggplot call to replace a scale_fill
#'command.
#'
#'By default the palette will be discrete.
#'
#'@param palette A string representing a color palette, see https://bfroebrco.github.io/rcocolorsr_pages/
#'for a full list of palettes
#'
#'@param discrete Changes between discrete or continuous scales
#'
#'@param reverse When true the default order of the color palette is reversed
#'
#'@return a function to create a color palette
#' @export
scale_fill_rco <- function(palette = "logo", discrete = TRUE, reverse = FALSE, ...) {
  pal <- rco_color_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("rco_color_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
