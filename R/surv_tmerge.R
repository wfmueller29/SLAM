#' Tmerge Long Data
#'
#' This will take data in long form and create a tmerge dataframe
#' that can be used in time dependent survival analysis
#' @param df dataframe in long form for outcome measurements
#' @param id character string specifying subject in `df`
#' @param age character string specifying age in `df`
#' @param age_death character string specifiying age of death in `df`
#' @param death_censor character string specifying censor in `df`.
#' A death is represented by 1 and censorship is represented by 0
#' @param outcomes vector or list of strings specifying the outcome variables
#' at each timepoint
#' @return outputs dataframe with tstart, tstop, death that can be used for time
#' dependent survival analysis
#' @importFrom survival tmerge
#' @importFrom rlang call2
#' @seealso \link[survival]{tmerge}
#' @export


surv_tmerge <- function(df, id, age, age_death, death_censor, outcomes){

  ## Reoder Dataset by Id and Age
  df_baseline <- df[order(df[[id]], df[[age]]), , drop = FALSE]

  ## take first observation for each id
  df_baseline <- df[!duplicated(df[[id]]), , drop = FALSE]

  ## Create call for first tmerge
  event <- call("event", as.symbol(age_death), as.symbol(death_censor))
  cl_tmerge1 <- rlang::call2("tmerge",
                 data1 = as.symbol("df_baseline"),
                 data2 = as.symbol("df_baseline"),
                 id = as.symbol(id),
                 tstart = as.symbol(age),
                 tstop = as.symbol(age_death),
                 death = event,
                 .ns = "survival")

  ## Call first tmerge, this will create a tstart, tstop, and death column
    # tstart is the time of first observation
    # tstop is the time of last observation
    # death is weather or not the individual died following the time of last observation
  df1 <- eval(cl_tmerge1)

  ## Create call for second tmerge,
  ## This will fill in all the times between the first and last observation for each subject and make death column 0
    # Create tmerge ... arguments and store them as args
  args <- lapply(outcomes, function(outcome){
    call("tdc", as.symbol(age), as.symbol(outcome))
  })
    # name args
  names(args) <- outcomes
  ## Create call
  cl_tmerge2 <- rlang::call2("tmerge",
                             data1 = as.symbol("df1"),
                             data2 = as.symbol("df"),
                             id = as.symbol(id),
                             !!!args,
                             .ns = "survival")

  ## Call second tmerge
  df2 <- eval(cl_tmerge2)

  return(df2)
}
