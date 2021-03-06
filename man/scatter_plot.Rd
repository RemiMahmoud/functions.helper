% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scatter_plot.R
\name{scatter_plot}
\alias{scatter_plot}
\title{scatter plot with parameters}
\usage{
scatter_plot(
  data_plot,
  x = final_density,
  title = "title",
  point_color = NULL,
  point_shape = crop_type,
  labs_y = "Biomass seed (t/ha)",
  labs_x = "Final density (plants/m2)",
  t = theme_bw(),
  group = NULL
)
}
\arguments{
\item{data_plot}{a dataset filtered to have only one trait}

\item{x}{x value of aesthetics}

\item{title}{title of the plot}

\item{point_color}{aesthetic for the color of the point}

\item{point_shape}{aesthetic for the shape of the point}

\item{labs_y}{text of the y lab}

\item{labs_x}{text of the x lab}

\item{t}{a theme for the plot}

\item{group}{aesthetic group}
}
\value{
a scatterplot
}
\description{
Compressed version of data %>% ggplot() + geom_point
}
