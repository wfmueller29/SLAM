#' Visualize Measurement Frequency
#'
#' Creates a barplot of measurement frequencies by some id column
#'
#' @param data a dataframe in long format
#' @param id a character string specifying subject id
#' @param ... optional parameters to pass to barplot function
#'
#' @return
#' prints a barplot
#'
#' @seealso \link[graphics]{barplot}
#'
#' @author William Mueller
#'
#' @export
#'


plot_mfreq <- function(data, id, ...){
  data <- data.table::as.data.table(data)
  j <- quote(.N)
  by <- id
  cl <- quote(data[, j = eval(j), by = eval(by)][,N])
  count <- eval(cl)
  barplot(table(count), xlab = "Observations per idno", ...)
}
