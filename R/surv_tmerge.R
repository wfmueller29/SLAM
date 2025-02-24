#' Tmerge Long Data
#'
#' This will take data in long form and create a tmerge dataframe
#' that can be used in time dependent survival analysis
#'
#' @param data dataframe in long form for outcome measurements
#' @param id character string specifying subject in data
#' @param age character string specifying age in data
#' @param age_death character string specifiying age of death in data
#' @param dead_censor character string specifying censor in data.
#' A death is represented by 1 and censorship is represented by 0
#' @param outcomes vector or list of strings specifying the outcome variables
#' at each timepoint
#'
#' @return outputs dataframe with tstart, tstop, death that can be used for
#' time dependent survival analysis
#'
#' @author William Mueller
#'
#' @example R\examples\surv.R
#'
#' @seealso \link[survival]{tmerge}
#'
#' @export


surv_tmerge <- function(data, id, age, age_death, dead_censor, outcomes) {
  # Create baseline data -------------------------------------------------------

  # Reoder Dataset by Id and Age
  data_baseline <- data[order(data[[id]], data[[age]]), , drop = FALSE]

  ## take first observation for each id
  data_baseline <- data_baseline[!duplicated(data_baseline[[id]]),
                                 ,
                                 drop = FALSE]

  # First tmerge ---------------------------------------------------------------
  # Create call for first tmerge
  cl_tmerge1 <- rlang::call2("tmerge",
    data1 = as.symbol("data_baseline"),
    data2 = as.symbol("data_baseline"),
    id = as.symbol(id),
    tstart = as.symbol(age),
    tstop = as.symbol(age_death),
    .ns = "survival"
  )

  # Call first tmerge, this will create a tstart, tstop
  # tstart is the time of first observation
  # tstop is the time of last observation
  data1 <- eval(cl_tmerge1)

  # Second tmerge --------------------------------------------------------------
  # Create call for second tmerge,
  # Create tmerge ... arguments and store them as args. age is included so it
  # will be constant
  args <- lapply(c(outcomes, age), function(outcome) {
    call("tdc", as.symbol(age), as.symbol(outcome))
  })
  event <- call("event", as.symbol(age_death), as.symbol(dead_censor))
  args <- c(args, event)
  # name args
  names(args) <- c(outcomes, age, dead_censor)
  # Create call
  cl_tmerge2 <- rlang::call2("tmerge",
    data1 = as.symbol("data1"),
    data2 = as.symbol("data"),
    id = as.symbol(id),
    !!!args,
    .ns = "survival"
  )

  # Call second tmerge
  data2 <- eval(cl_tmerge2)

  return(data2)
}
