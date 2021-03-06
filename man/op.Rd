% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/specialGesturesOP.R
\name{op}
\alias{op}
\title{Return the control points of the bezier curve, path and starting information that would be used to draw the gesture OP}
\usage{
op(xStart, yStart, headingStart = 45, method = "bezier")
}
\arguments{
\item{xStart}{The start position of the x-coordinate. Can be a decimal number with .5 or whole number. But either x- and y- coordinate must have one whole number and a decimal number.}

\item{yStart}{The start position of the y-coordinate. Can be a decimal number with .5 or whole number.But either x- and y- coordinate must have one whole number and a decimal number.}

\item{headingStart}{The start orientation of the gesture. Valid headings or orientations are 45°, 135°, 225° and 315°.}
}
\value{
A list with the starting information (x- and y-coordinate, initial orientation), the path and the control points.
}
\description{
\code{op} This function returns the points of the path and the exact control points that would be used to draw the series of line segments for the bezier curve. The control points capture the shape of the gesture. The function draws a special gesture in the lexicon.
}
\examples{
op(xStart = 0, yStart = 5.5, headingStart = 225)

}
