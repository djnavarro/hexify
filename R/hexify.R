
#' Create hex sticker from image
#'
#' @param from Path to the source image
#' @param to Path to the output image
#' @param text_label Text to display as the label
#' @param text_colour Colour of the text label
#' @param border_colour Colour of the border
#' @param border_opacity Opacity of the border
#'
#' @return Same as magick::image_write
#' @export
#'
#' @examples
hexify <- function (from, to, text_label = NULL, text_colour = "white",
                    border_colour = "grey20", border_opacity = 60) {
  
  image <- magick::image_read(path = from)
  image <- magick::image_background(image, color = "#ffffff00")
  image <- sticker_crop(image, crop = 1)
  image <- sticker_chop(image)
  image <- sticker_rescale(image, hex_width = 2000)
  image <- sticker_border(image, opacity = border_opacity,
                          colour = border_colour)
  image <- sticker_annotate(image, text_label = text_label,
                            text_colour = text_colour, text_size = 40)
  
  magick::image_write(image, path = to)
}


sticker_crop <- function (image, crop) {
  height <- magick::image_info(image)$height
  width <- magick::image_info(image)$width
  if (width > height) {
    geom <- magick::geometry_area(height, height, (width - height)/2, 0)
    image <- magick::image_crop(image, geom)
  }
  if (height > width) {
    geom <- magick::geometry_area(width, width, 0, (height - width)/2)
    image <- magick::image_crop(image, geom)
  }
  if (crop != 1) {
    height <- magick::image_info(image)$height
    width <- magick::image_info(image)$width
    geom <- magick::geometry_area(
      width = width * crop,
      height = height * crop,
      x_off = width * (1 - crop)/2,
      y_off = height * (1 - crop)/2
    )
    image <- magick::image_crop(image = image, geometry = geom)
  }
  return(image)
}

sticker_chop <- function(image) {
  height <- magick::image_info(image)$height
  width <- magick::image_info(image)$width
  trim_corner <- paste0("0x", round(height * 0.25))
  trim_edge <- paste0(round(height * 0.067), "x0")
  
  # to do: make this not-awful :)
  image <- magick::image_rotate(image, 30)
  image <- magick::image_chop(image, trim_corner)
  image <- magick::image_rotate(image, -60)
  image <- magick::image_trim(image)
  image <- magick::image_chop(image, trim_corner)
  image <- magick::image_rotate(image, 240)
  image <- magick::image_trim(image)
  image <- magick::image_chop(image, trim_corner)
  image <- magick::image_rotate(image, -60)
  image <- magick::image_trim(image)
  image <- magick::image_chop(image, trim_corner)
  image <- magick::image_rotate(image, 210)
  image <- magick::image_trim(image)
  image <- magick::image_chop(image, trim_edge)
  image <- magick::image_rotate(image, 180)
  image <- magick::image_trim(image)
  image <- magick::image_chop(image, trim_edge)
  image <- magick::image_rotate(image, -180)
  image <- magick::image_trim(image)
  
  return(image)
}

sticker_rescale <- function (image, hex_width) {
  image_width <- magick::image_info(image)$width
  scf <- magick::geometry_size_percent(hex_width/image_width * 100)
  image <- magick::image_resize(image, scf)
  return(image)
}

sticker_border <- function (image, thickness = 5, opacity, colour) {
  
  width <- magick::image_info(image)$width
  height <- magick::image_info(image)$height
  
  base_hex_img <- system.file("extdata", "base_hex.png", package = "hexify")
  base <- magick::image_read(base_hex_img)
  
  base_width <- magick::image_info(base)$width
  base_height <- magick::image_info(base)$height
  
  scaled <- magick::image_scale(
    image = base,
    geometry = magick::geometry_size_percent(100 * width/base_width)
  )
  smaller <- magick::image_scale(
    image = scaled,
    geometry = magick::geometry_size_percent(100 - thickness)
  )
  
  small_width <- magick::image_info(smaller)$width
  small_height <- magick::image_info(smaller)$height
  diff_x <- (width - small_width)/2
  diff_y <- (height - small_height)/2
  
  outside <- magick::image_composite(
    image = image,
    composite_image = smaller,
    operator = "DstOut",
    offset = magick::geometry_point(diff_x, diff_y)
  )
  inside <- magick::image_composite(
    image = image,
    composite_image = smaller,
    operator = "DstIn",
    offset = magick::geometry_point(diff_x, diff_y)
  )
  
  border <- magick::image_colorize(
    image = outside,
    opacity = opacity,
    color = colour
  )
  
  output <- magick::image_mosaic(
    image = c(inside, border),
    operator = "Blend"
  )
  return(output)
}

sticker_annotate <- function (image, text_label, text_colour, text_size) {
  width <- magick::image_info(image)$width
  height <- magick::image_info(image)$height
  if (!is.null(text_label)) {
    image <- magick::image_annotate(
      image = image, color = text_colour,
      text = text_label,
      gravity = "SouthWest",
      degrees = -30,
      size = text_size,
      location = magick::geometry_point(width/2, height/30)
    )
  }
  return(image)
}

