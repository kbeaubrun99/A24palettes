#' A list of color palettes from popular A24 movies
#' @export
A24_palettes <- list(
  moonlight = c("#0ebbc1", "#222e51", "#e187d5", "#54235c", "#8c9ec9"),
  ladyBird = c("#51141a", "#742b2b", "#31282a", "#3c3e56", "#5b6278"),
  exMachina = c("#e44404", "#e99c7c", "#822504", "#946454", "#bc9c90"),
  midsommar = c("#003f2e", "#377856", "#be9a5c", "#daa190", "#d99800",
                "#d54d01", "#b70002", "#aa2366"),
  floridaProject = c("#480f44", "#612d54", "#846380", "#a17f9a", "#b2a6aa",
                     "#625b62", "#657670", "#804f48"),
  uncutGems = c("#d4dee8", "#93a3c7", "#5d4e61", "#706d64", "#7b6a3f",
                "#bebc8b", "#6e9371")
  )

#' An A24 color palette function
#'
#' This function lets you create custom color palettes based on popular A24 movies.
#' @param n is the number of colors needed.
#' @param type is either continuous or discrete
#' @param name is the name of the movie. the choices are \code{moonlight},
#'  \code{ladyBird}, \code{exMachina}, \code{midsommar}, \code{floridaProject}, and
#'  \code{uncutGems}
#' @keywords color
#' @return a vector of colors
#' @export
#' @examples
#' A24_palette()
#' # if more colors are needed than the palette contains then a continuous type
#' # can be used
#' pal <- A24_palette(21, name = "moonlight", type = "continuous")
A24_palette <- function(name, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)

  pal <- A24_palettes[[name]]
  if (is.null(pal))
    stop("Invalid palette.")

  if (missing(n)) {
    n <- length(pal)
  }

  if (type == "discrete" && n > length(pal)) {
    stop("Palette cannot handle this many colors.")
  }

  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = name)
}

#' @return shows the colors in the selected palette
#' @example A24_palette("ladyBird")
print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "mono")
}

