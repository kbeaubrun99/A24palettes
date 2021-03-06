% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/color_palettes.R
\name{A24_palette}
\alias{A24_palette}
\title{An A24 color palette function}
\usage{
A24_palette(name, n, type = c("discrete", "continuous"))
}
\arguments{
\item{name}{is the name of the movie. the choices are \code{moonlight},
\code{ladyBird}, \code{exMachina}, \code{midsommar}, \code{floridaProject}, and
\code{uncutGems}}

\item{n}{is the number of colors needed.}

\item{type}{is either continuous or discrete}
}
\value{
a vector of colors
}
\description{
This function lets you create custom color palettes based on popular A24 movies.
}
\examples{
A24_palette()
# if more colors are needed than the palette contains then a continuous type
# can be used
pal <- A24_palette(21, name = "moonlight", type = "continuous")
}
\keyword{color}
