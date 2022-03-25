#' Plot to show longitudinal data
#'
#' Function takes in data, and plots points and lines longitudinally, using
#' loess (you can change span, but it is set to default) You can also choose
#' to show the local maximum for analytical purposes
#'
#' @param data - data set from which we plot
#' @param x - what is on the x-axis
#' @param y - what is on the y-axis
#' @param by - data groupings we choose (different coloured, different
#' calculations) - example - plotting bodyweight over mouse age, grouping by
#' strain to see how different strains tend to gain or lose weight over time
#' @param title - this is the title of your plot. This parameter is optional
#' @param xlab - this is the x-axis label. This parameter is optional
#' @param ylab - this is the y-axis label. This parameter is optional
#' @param xlow - lower bound on the x-axis. This parameter is optional, and if
#' you skip it, it will recalculate based on data
#' @param xhigh - upper bound on the x-axis. This parameter is optional, and if
#' you skip it, it will recalculate based on data
#' @param ylow - lower bound on the y-axis. This parameter is optional, and if
#' you skip it, it will recalculate based on data
#' @param yhigh - upper bound on the y-axis. This parameter is optional, and if
#' you skip it, it will recalculate based on data
#' @param color1 - palette you want to use. This is optional - if you don't set
#' it, r will use default pallette
#' @param color2 - if you want a different color for the lines than the points,
#' indicate it here, otherwise it is the same as color1 by default
#' @param percentile - this refers to which points you don't want to plot. For
#' example, if it is set to 0.05, meaning that 5% of  highest and 5% of lowest
#' points don't get plotted. Default is 0, so no points get removed
#' @param show_max - this asks if you want to put a line through local maximum
#' of y. Default is FALSE
#' @param span - this refers to span for LOESS lines. Default is set to r
#' defalut, which is 0.75
#'
#' @return Plot
#'
#' @author ....
#'
#' @export

plot_long <- function(
  data,
  x,
  y,
  by,
  title,
  xlab,
  ylab,
  xlow,
  xhigh,
  ylow,
  yhigh,
  color1,
  color2 = color1,
  percentile = 0.00,
  show_max=FALSE,
  span = 0.75) {

  #subsetting
  y2 <- paste0("!is.na(", y, ")")
  y3 <- paste0("!is.na(", by, ")")

  df <- base::subset(base::subset(data,
                                  eval(parse(text = y2))),
                     eval(parse(text = y3)))

  sub <- paste0(y, " < quantile(", y, ", ", as.character(1 - percentile), ") & ",
                y, " > quantile(", y, ", ", as.character(percentile), ")")

  # in cae tehy didn't input palette
  if (missing(color1)) {
    color1 <- scales::hue_pal()(length(unique(df[[by]])))
  }

  z <- Inf

  # in case author skipped xlow, xhigh, ylow, and yhigh
  if (missing(xlow)) {
    xlow <- min(subset(df, eval(parse(text = sub)))[[x]], na.rm = TRUE)
  }

  if (missing(xhigh)) {
    xhigh <- max(subset(df, eval(parse(text = sub)))[[x]], na.rm = TRUE)
  }

  if (missing(ylow)) {
    ylow <- min(subset(df, eval(parse(text = sub)))[[y]], na.rm = TRUE)
  }

  if (missing(yhigh)) {
    yhigh <- max(subset(df, eval(parse(text = sub)))[[y]], na.rm = TRUE)
  }

  if (missing(xlab)) {
    xlab <- ""
  }

  if (missing(ylab)) {
    ylab <- ""
  }

  if (missing(title)) {
    title <- ""
  }

  p1 <- ggplot2::ggplot(subset(df, eval(parse(text = sub))),
                        ggplot2::aes_string(x = x, y = y)) +

    #dots
    ggplot2::geom_point(ggplot2::aes_string(fill = by),
                        alpha=0.5,
                        position = "jitter",
                        size = 1,
                        shape=21,
                        stroke = 0.01) +
    ggplot2::scale_fill_manual(values = color1) +

    #trendlines
    ggplot2::geom_smooth(ggplot2::aes_string(group = by),
                         linetype = "solid",
                         color = "black",
                         size = 1.8,
                         span = span) +
    ggplot2::geom_smooth(ggplot2::aes_string(color = by),
                         linetype = "solid",
                         span = span) +

    #labels
    ggplot2::labs(x = xlab, y = ylab, title = title) +
    ggplot2::xlim(xlow, xhigh) +
    ggplot2::ylim(ylow, yhigh) +

    ggplot2::scale_linetype_manual(labels = c("All"),
                                   name = "",
                                   values = c("solid", "dashed", "solid"),
                                   guide = FALSE) +
    ggplot2::scale_color_manual(values = color2)

  gb <- ggplot2::ggplot_build(p1)

  exact_x_value_of_the_curve_maximum <- gb$data[[2]]$x[which.max(gb$data[[2]]$y)]

  if (show_max == TRUE) {

    p1 <- p1 +
      ggplot2::geom_vline(xintercept = exact_x_value_of_the_curve_maximum) +
      ggplot2::annotate(x = exact_x_value_of_the_curve_maximum,
                        y = 20, 
                        label = round(exact_x_value_of_the_curve_maximum),
                        geom = 'label')

  } else {p1}

  return(p1)

}
