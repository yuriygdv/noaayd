% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geoms.R
\name{geom_timeline}
\alias{geom_timeline}
\title{Plotting a time line of earthquakes}
\usage{
geom_timeline(mapping = NULL, data = NULL, stat = "identity",
  position = "identity", na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE, ...)
}
\arguments{
\item{mapping}{ggplot parameter}

\item{data}{ggplot parameter}

\item{stat}{ggplot parameter}

\item{position}{ggplot parameter}

\item{na.rm}{ggplot parameter}

\item{show.legend}{ggplot parameter}

\item{inherit.aes}{ggplot parameter}

\item{...}{other args}
}
\value{
a plot with the earthquakes on the timeline
}
\description{
This is a function thatplots a time line of earthquakes ranging from xmin to xmaxdates
 with a point for each earthquake. Optional aesthetics include color, size, and alpha (for transparency).
 The xaesthetic is a date and an optional y aesthetic is a factor indicating some stratification
 in which case multiple time lines will be plotted for each level of the factor (e.g. country).
}
\examples{
\dontrun{
ggplot(clean_data, aes(x=DATE, size = EQ_PRIMARY, colour = DEATHS)) +
      geom_timeline(alpha = 0.5) +
      theme(panel.background = element_blank(), legend.position = "bottom",
      axis.line.x = element_line(size = 1)) +
      scale_x_date() +scale_size_continuous(name = "Richter scale value")

ggplot(aes(x=DATE, size = EQ_PRIMARY, colour = DEATHS)) +
      geom_timeline(aes(y = COUNTRY), alpha = 0.5) +
      theme(panel.background = element_blank(), legend.position = "bottom",
      axis.line.x = element_line(size = 1)) +
      scale_x_date() +
      scale_size_continuous(name = "Richter scale value")
}

}
