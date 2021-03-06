% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hexify.R
\name{hexify}
\alias{hexify}
\title{Create hex sticker from image}
\usage{
hexify(
  from,
  to,
  text_label = NULL,
  text_colour = "white",
  border_colour = "grey20",
  border_opacity = 60
)
}
\arguments{
\item{from}{Path to the source image}

\item{to}{Path to the output image}

\item{text_label}{Text to display as the label}

\item{text_colour}{Colour of the text label}

\item{border_colour}{Colour of the border}

\item{border_opacity}{Opacity of the border}
}
\value{
Same as magick::image_write
}
\description{
Create hex sticker from image
}
