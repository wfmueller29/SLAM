#' SLAM Glucose Data
#'
#' Dataset that contains fasting blood glucose values for the mice in SLAM
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
#' @usage data(gluc)
#' @format a dataframe 3,738 obs and 18 variables
#'
#' \describe{
#'   \item{idno}{unique identifier}
#'   \item{date}{date of bleed}
#'   \item{gluc}{Glucose levels in mg/dL}
#'   \item{lact}{Lactate levels in mg/dL}
#'   \item{cohort}{cohort of the mouse}
#'   \item{animal_id}{unique identification number for vivarium}
#'   \item{tag}{unique identifier for each mouse}
#'   \item{taghistory}{history of unique identifiers for each mouse, they
#'   sometimes have changed
#'   over the course of the study}
#'   \item{sex}{M or F denoting Male or Female}
#'   \item{dob}{date variable denoting date of birth}
#'   mouse enrolled and n
#'   is the most recent mouse enrolled}
#'   \item{cage}{the number denoting the cage. The mice were housed mostly in
#'   groups of 4}
#'   \item{eartag}{marking pattern on mouses ears so animal technicians could
#'   identify the mouse}
#'   \item{name}{random name given to each mouse}
#'   \item{cod}{Cause of death for each mouse}
#'   \item{died}{date of death for each mouse}
#'   \item{dead_censor}{indicates whether the mouse died a natural death (1) or
#'   if the mouse should be censored (0)}
#' }
"gluc"
