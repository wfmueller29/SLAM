#' Function which returns plot of averages, standard devations, and points
#'
#' Function takes in a database, and plots points based on 3
#' variables - y-axis, x-axis, and grouping. Function calculates standard
#' deviations and means by group and point on x-axis. Only y-axis needs to be
#' numeric, but if you want to ensure the x-axis are ordered correctly, I
#' suggest ordering it before plotting
#'
#' @param data - database to pull from
#' @param x - this is the GROUPING (e.g. sex)
#' @param y - this is the outcome variable
#' @param by - this is the facet_wrap that appears as the x-axis
#' (e.g. age in months)
#' @param title - OPTIONALnplot title
#' @param bar_color - OPTIONAL color of bars indicating averages (you could
#' switch it, it is just the parameter you pass to 'geom'). Needs to be the
#' same length as unique(data$x)
#' @param point_color - OPTIONAL color of points. List needs to be the same
#' length as bar_color. Default is black
#' @param errorbar_color - OPTIONAL color of errorbars (standard deviation).
#' Default is grey4 (almost black)
#' @param geom - OPTIONAL to pass to 'geom'. Default is bar
#' @param points - OPTIONAL. Default is TRUE. Whether or not it shows points
#' @param percentile - this refers to which points you don't want to plot.
#' For example, if it is set to 0.05, meaning that 5% of  highest and 5% of
#' lowest points don't get plotted. Default is 0, so no points get removed
#' @param color_by - OPTIONAL, do you want to color the by x or by the by
#' variable? Default is x
#'
#' @return Plot
#'
#' @author .....
#'
#' @export


plot_bar <- function(data,
                     x,
                     y,
                     by,
                     title = "",
                     bar_color,
                     point_color = "black",
                     errorbar_color = "grey4",
                     geom = "bar",
                     points = TRUE,
                     percentile = 0,
                     color_by = x) {


  # shortening amount of writing to do
  dd <- data[[x]]
  df <- data[[y]]
  dby <- data[[by]]

  # setting y variable as numeric
  data[[y]] <- as.numeric(data[[y]])

  # getting rid of NA values
  data <- data[!is.na(data[[x]]), ]
  data <- data[!is.na(data[[y]]), ]
  data <- data[!is.na(data[[by]]), ]

  # x as factor
  if (is.numeric(data[[x]])) {
    data <- dplyr::arrange(data, data[[x]])
  }

  data[[x]] <- factor(data[[x]], levels = unique(data[[x]]))

  data <- data[quantile(data[[y]], percentile) < data[[y]] & data[[y]] < quantile(data[[y]], 1 - percentile), ]

  # if bar_color is missing
  if (missing(bar_color)) {
    bar_color <- scales::hue_pal()(length(unique(data[[color_by]])))
  }

  # creating means and standard deviations
  stats_formula <- paste(y, "~", x, "+", by)
  stats_table.mean <- stats::aggregate(
    data = data, eval(parse(text = stats_formula)),
    FUN = function(x) mean(x)
  )
  stats_table.sd <- stats::aggregate(
    data = data, eval(parse(text = stats_formula)),
    FUN = function(x) stats::sd(x)
  )
  stats_table <- dplyr::left_join(stats_table.mean, stats_table.sd, by = c(x, by))
  colnames(stats_table) <- c(x, by, "means", "st_dev")

  # merging mean and sd stats with the dataframe
  data <- dplyr::left_join(data, stats_table, by = c(x, by))

  if (points == TRUE) {
    if (point_color != "black") {
      plotty <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y)) +
        ggplot2::stat_summary(
          fun = "mean",
          geom = geom,
          col = "grey4",
          ggplot2::aes_string(fill = color_by)
        ) +
        ggplot2::facet_grid(~ eval(parse(text = by)),
          space = "free_x",
          scales = "free_x"
        ) +
        ggplot2::scale_fill_manual(values = bar_color) +
        ggplot2::geom_jitter(ggplot2::aes_string(x = x, y = y, col = color_by),
          shape = 5,
          size = 0.5,
          position = ggplot2::position_jitter(0.2),
          alpha = 0.4
        ) +
        ggplot2::scale_colour_manual(values = point_color) +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = means - st_dev,
            ymax = means + st_dev
          ),
          col = errorbar_color,
          size = 0.3,
          width = 0.17
        ) +
        ggplot2::ylab(paste(sub("(.)", "\\U\\1", y, perl = TRUE), "(g)")) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 45,
            hjust = 1,
            size = 10
          )
        ) +
        ggplot2::ggtitle(paste(sub("(.)", "\\U\\1", y, perl = TRUE), title))
    } else {
      plotty <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y)) +
        ggplot2::stat_summary(
          fun = "mean",
          geom = geom,
          col = "grey4",
          ggplot2::aes_string(fill = color_by)
        ) +
        ggplot2::facet_grid(~ eval(parse(text = by)),
          space = "free_x",
          scales = "free_x"
        ) +
        ggplot2::scale_fill_manual(values = bar_color) +
        ggplot2::geom_jitter(
          ggplot2::aes_string(x = x, y = y),
          col = "black",
          shape = 5,
          size = 0.5,
          position = ggplot2::position_jitter(0.2),
          alpha = 0.4
        ) +
        ggplot2::scale_colour_manual(values = point_color) +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = means - st_dev,
            ymax = means + st_dev
          ),
          col = errorbar_color,
          size = 0.3,
          width = 0.17
        ) +
        ggplot2::ylab(paste(sub("(.)", "\\U\\1", y, perl = TRUE), "(g)")) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 45,
            hjust = 1,
            size = 10
          )
        ) +
        ggplot2::ggtitle(paste(sub("(.)", "\\U\\1", y, perl = TRUE), title))
    }
  } else {
    plotty <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y)) +
      ggplot2::stat_summary(
        fun = "mean",
        geom = geom,
        col = "grey4",
        ggplot2::aes_string(fill = color_by)
      ) +
      ggplot2::facet_grid(
        ~ eval(parse(text = by)),
        space = "free_x",
        scales = "free_x"
      ) +
      ggplot2::scale_fill_manual(values = bar_color) +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          ymin = means - st_dev,
          ymax = means + st_dev
        ),
        col = errorbar_color,
        size = 0.3,
        width = 0.17
      ) +
      ggplot2::ylab(paste(sub("(.)", "\\U\\1", y, perl = TRUE), "(g)")) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          angle = 45,
          hjust = 1,
          size = 10
        )
      ) +
      ggplot2::ggtitle(paste(sub("(.)", "\\U\\1", y, perl = TRUE), title))
  }

  return(plotty)
}
