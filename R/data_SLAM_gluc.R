#' SLAM Glucose and Lactate Data
#'
#' A Dataset that has the glucose and lactate levels acquired from routine
#' bleeds in SLAM
#'
#' @references 
#' 
#' Palliyaguru DL, Shiroma EJ, Nam JK, Duregon E, Vieira Ligo Teixeira C, 
#' Price NL, Bernier M, Camandola S, Vaughan KL, Colman RJ, Deighan A, 
#' Korstanje R, Peters LL, Dickinson SL, Ejima K, Simonsick EM, Launer LJ, 
#' Chia CW, Egan J, Allison DB, Churchill GA, Anderson RM, Ferrucci L, 
#' Mattison JA, de Cabo R. Fasting blood glucose as a predictor of 
#' mortality: Lost in translation. Cell Metab. 2021 Nov 2;33(11):2189-2200.e3. 
#' doi: 10.1016/j.cmet.2021.0#` 8.013. Epub 2021 Sep 10. PMID: 34508697.
#' 
#' Palliyaguru DL, Vieira Ligo Teixeira C, Duregon E, di Germanio C, Alfaras 
#' I, Mitchell SJ, Navas-Enamorado I, Shiroma EJ, Studenski S, Bernier M, 
#' Camandola S, Price NL, Ferrucci L, de Cabo R. Study of Longitudinal Aging 
#' in Mice: Presentation of Experimental Techniques. J Gerontol A Biol Sci Med 
#' Sci. 2021 Mar 31;76(4):552-560. doi: 10.1093/gerona/glaa285. 
#' PMID: 33211821; PMCID: PMC8011709.
#' 
#' @docType data
#' @usage data(data_SLAM_gluc)
#' @format a dataframe 17467 obs and 6 variables
#' \describe{
#'   \item{idno}{unique identifier}
#'   \item{date}{date of bleed}
#'   \item{gluc}{Glucose levels in mg/dL}
#'   \item{lact}{Lactate levels in mg/dL}
#' }
"data_SLAM_gluc"
