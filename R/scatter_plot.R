#' scatter plot with parameters
#'
#' @param data_plot a dataset filtered to have only one trait
#' @param title title of the plot
#' @param point_color aesthetic for the color of the point
#' @param point_shape aesthetic for the shape of the point
#' @param labs_y text of the y lab
#' @param labs_x text of the x lab
#' @param x x value of aesthetics
#' @param t a theme for the plot
#' @param group aesthetic group
#'
#' @return a scatterplot
#' @export
#' @importFrom ggplot2 ggplot geom_point theme_bw ggtitle labs
#' @importFrom magrittr %>%
#' @description Compressed version of data %>% ggplot() + geom_point
scatter_plot <- function(data_plot,
                     x = final_density,
                     title = "title" ,
                     point_color = NULL,
                     point_shape = crop_type,
                     labs_y = "Biomass seed (t/ha)",
                     labs_x = "Final density (plants/m2)",
                     t = theme_bw(),
                     group = NULL){

  data_plot %>%
    ggplot(aes(x = {{x}}, y = value, group = {{group}})) +
    geom_point(aes(color = {{point_color}}, shape = {{point_shape}})) +
    t +
    ggtitle(title) +
    labs(y = labs_y, x = labs_x)

}
