#' rm_banova_mf
#'
#' @description Basic function for running the Bayesian repeated measures ANOVA
#'
#' \lifecycle{experimental}
#'
#' @inheritParams rm_anova_mf
#'
#' @param multicore The option to run the analysis in multiple cores, not available under Windows. Default to TRUE
#' @return A tibble with the following column names:
#' x: the name of the independent variable (e.g., cs)
#' y: the name of the dependent variable as this defined in the \code{dv} argument
#' exclusion: see \code{exclusion} argument
#' model: the model that was run (e.g., t-test)
#' controls: ignore this column for this test
#' method: the model that was ru
#' p.value: irrelevant here
#' effect.size: irrelevant here
#' estimate: the estimate of the test run
#' statistic: the t-value
#' conf.low: the lower confidence interval for the estimate
#' conf.high: the higher confidence interval for the estimate
#' data_used: a list with the data used for the specific test
#'
#' @details In case the \code{time} argument is set to true, the function will
#' include this as a within subjects factor, assuming that the columns in
#' \code{cs1} and \code{cs2} corrrespond to ascending time points (e.g., cs1
#' trial 1, cs1 trial 2 ... cs1 trial \code{n}). If this is not the case, the
#' results are not to be trusted.
#'
#' # Briefly define argument values that will be plugged in later on in the functions
#' cs1 <- paste0("CSP", 1:10)
#' cs2 <- paste0("CSM", 1:10)
#' subj <- "id"
#' group <- "group"
#'
#' # Repeated measures ANOVA
#' rm_banova_mf(cs1 = cs1, cs2 = cs2, subj = subj, data = example_data, time = TRUE)
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
                         exclusion = "full data",
                         multicore = TRUE) {
  collection_warning(
    cs1 = cs1,
    cs2 = cs2,
    data = data,
    subj = subj
  )

  data <-
    data_preparation_anova(
      cs1 = cs1,
      cs2 = cs2,
      data = data,
      subj = subj,
      time = TRUE,
      group = group
    )

  # Decide which terms you will have in order to feed in the ANOVA later on
  if (time && (!is.null(group))) {
    anova_terms <- "resp ~ group*cs*time + subj"
    selected_term <- "cs:group:subj:time"
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
    selected_term <- "cs:group:subj"
  }

  # Run the main ANOVA
  tmpBANOVA <-
    BayesFactor::anovaBF(
      formula = eval(parse(text = anova_terms)),
      data = data,
      whichRandom = subj,
      progress = FALSE,
      multicore = multicore
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
    estimate = as.character(bfmm),
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
    estimate = as.character(bfum),
    statistic = NA,
    conf.low = NA,
    conf.high = NA
  )

  res <- rbind(bfmm_res, bfum_res) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(data_used = list(data))

  return(res)

}