#' rm_banova_mf
#'
#' @description Basic function for running repeated measures Bayesian ANOVA
#' @param cs1 cs 1
#' @param cs2 cs 2
#' @param data a data frame containing the dv and iv
#' @param time should time be included? Default to \code{TRUE}
#' @param subj column nmae with the participant number.
#' It should be a unique number.
#' @param data a data frame containing the dv and iv
#' @param group name of the group variable, if it is present, default to \code{NULL}
#' @param phase Different tests will be run for different phases. That is why
#' the phase needs to be specified here. Possible values are \code{acquisition},
#' or \code{acq}, \code{extinction}, or \code{extinction}. See Details for more
#' information.
#' @param dv name of the dependent variable, default to "SCR"
#' @param exclusion If any exclusion was done, default to \code{full data}
#' @return A basic function for running repeated measures ANOVAs
#' @details In case the \code{time} argument is set to true, the function will
#' include this as a within subjects factor, assuming that the columns in
#' \code{cs1} and \code{cs2} corrrespond to ascending time points (e.g., cs1
#' trial 1, cs1 trial 2 ... cs1 trial \code{n}). If this is not the case, the
#' results are not to be trusted.
#'
#' The function uses the \code{BayesFactor::anovaBF} function.
#'
#' @export
rm_banova_mf <- function(cs1,
                         cs2,
                         data,
                         subj,
                         time = TRUE,
                         group = NULL,
                         phase = "acquisition",
                         dv = "scr",
                         exclusion = "full data") {
  collection_warning(
    cs1 = cs1,
    cs2 = cs2,
    data = data,
    subj = subj
  )

  data <-
    data_preparation(
      cs1 = cs1,
      cs2 = cs2,
      data = data,
      subj = subj,
      time = TRUE,
      group = NULL
    )

  # Decide which terms you will have in order to feed in the ANOVA later on
  if (time && (!is.null(group))) {
    anova_terms <- "resp ~ group*cs*time + subj"
    selected_term <- "BUG" # Extend to groups
  } else if (!time && (is.null(group)))  {
    anova_terms <- "resp ~ cs + subj"
    group = NULL
    selected_term <- "cs:subj"
  } else if (time && is.null(group)) {
    anova_terms <- "resp ~ cs*time + subj"
    group = NULL
    selected_term <- "cs:subj:time"
  } else if (!time && !is.null(group)) {
    anova_terms <- "resp ~ group*cs + subj"
    group = NULL
    selected_term <- "BUG" # Extend to groups
  }

  # Run the main ANOVA
  tmpBANOVA <-
    BayesFactor::anovaBF(
      formula = eval(parse(text = anova_terms)),
      data = data,
      whichRandom = subj,
      progress = FALSE
    )

  # Bayes factors for matched models
  bfmm <-
    bayestestR::bayesfactor_inclusion(tmpBANOVA, match_models = TRUE) %>%
    dplyr::mutate(model = rownames(.)) %>%
    dplyr::filter(model == selected_term) %>%
    dplyr::select(BF) %>%
    as.numeric()

  # Bayes factors for non-matched models
  bfum <-
    bayestestR::bayesfactor_inclusion(tmpBANOVA, match_models = FALSE) %>%
    dplyr::mutate(model = rownames(.)) %>%
    dplyr::filter(model == selected_term) %>%
    dplyr::select(BF) %>%
    as.numeric()

  bfmm_res <- data.frame(
    x = selected_term,
    y = dv,
    exclusion = exclusion,
    model = "rep BANOVA Matched",
    controls = NA,
    method = paste("rep BANOVA Matched", selected_term),
    p.value = NA,
    effect.size = NA,
    estimate = bfmm,
    statistic = NA,
    conf.low = NA,
    conf.high = NA
  )

  bfum_res <- data.frame(
    x = selected_term,
    y = dv,
    exclusion = exclusion,
    model = "rep BANOVA Unmatched",
    controls = NA,
    method = paste("rep BANOVA Unmatched", selected_term),
    p.value = NA,
    effect.size = NA,
    estimate = bfum,
    statistic = NA,
    conf.low = NA,
    conf.high = NA
  )

  res <- rbind(bfmm_res, bfum_res) %>%
    dplyr::mutate(data_used = list(data))

  return(res)

}
