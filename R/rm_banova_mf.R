#' rm_banova_mf
#'
#' @description Basic function for running the Bayesian repeated measures analysis of Variance
#'
#' @inheritParams rm_anova_mf
#'
#' @return A tibble with the following column names:
#'
#' x: the name of the independent variable (e.g., cs)
#'
#' y: the name of the dependent variable as this defined in the \code{dv} argument
#'
#' exclusion: see \code{exclusion} argument
#'
#' model: the model that was run (e.g., rep ANOVA)
#'
#' controls: ignore this column for this test
#'
#' method: the model that was run
#'
#' p.value: irrelevant here
#'
#' effect.size: irrelevant here
#'
#' effect.size.ma: irrelevant here
#'
#' effect.size.lci: irrelevant here
#'
#' effect.size.hci: irrelevant here
#'
#' estimate: the estimate of the test run
#'
#' statistic: the Bayes factor
#' conf.low: the lower confidence interval for the estimate
#'
#' conf.high: the higher confidence interval for the estimate
#'
#' framework: were the data analysed within a NHST or Bayesian framework?
#'
#' data_used: a list with the data used for the specific test
#'
#' @details In case the \code{time} argument is set to true, the function will
#' include this as a within subjects factor, assuming that the columns in
#' \code{cs1} and \code{cs2} correspond to ascending time points (e.g., cs1
#' trial 1, cs1 trial 2 ... cs1 trial \code{n}). If this is not the case, the
#' results are not to be trusted.
#'
#' The ANOVA will run *all* possible models and combinations. Please note that
#' in case of many factors, this will mean that the analysis will take a long
#' time to be completed.
#'
#' @examples
#' # Briefly define argument values that will be plugged in later on in the functions.
#' # We only use two trials as the function takes a long time to run.
#'
#' cs1 <- paste0("CSP", 1:2)
#' cs2 <- paste0("CSM", 1:2)
#' subj <- "id"
#'
#' # Bayesian Repeated measures ANOVA without groups
#' rm_banova_mf(cs1 = cs1, cs2 = cs2, subj = subj,
#' data = example_data, time = TRUE)
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
                         cut_off = "full data") {
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
      time = time,
      group = group
    )

  # Decide which terms you will have in order to feed in the ANOVA later on
  if (time && (!is.null(group))) {
    anova_terms <- "resp ~ group*cs*time + subj"
    #selected_term <- "cs:group:subj:time"
    selected_term <- "cs:group:time"
  } else if (!time && (is.null(group)))  {
    anova_terms <- "resp ~ cs + subj"
    group = NULL
    #selected_term <- "cs:subj"
    selected_term <- "cs"
  } else if (time && is.null(group)) {
    anova_terms <- "resp ~ cs*time + subj"
    group = NULL
    #selected_term <- "cs:subj:time"
    selected_term <- "cs:time"
  } else if (!time && !is.null(group)) {
    anova_terms <- "resp ~ group*cs + subj"
    group = NULL
    #selected_term <- "cs:group:subj"
    selected_term <- "cs:group"
  }

  # Run the main ANOVA
  tmpBANOVA <-
    suppressMessages(BayesFactor::anovaBF(
      formula = eval(parse(text = anova_terms)),
      data = data,
      whichRandom = "subj",
      progress = FALSE))

  # Bayes factors for matched models
  bfmm <-
    bayestestR::bayesfactor_inclusion(tmpBANOVA, match_models = TRUE) %>%
    dplyr::mutate(model = rownames(.)) %>%
    dplyr::filter(model == selected_term) %>%
    data.frame() %>%
    dplyr::select(log_BF) %>%
    as.numeric() %>%
    exp()

  # Bayes factors for non-matched models
  bfum <-
    bayestestR::bayesfactor_inclusion(tmpBANOVA, match_models = FALSE) %>%
    dplyr::mutate(model = rownames(.)) %>%
    dplyr::filter(model == selected_term) %>%
    data.frame() %>%
    dplyr::select(log_BF) %>%
    as.numeric() %>%
    exp()

  bfmm_res <- data.frame(
    x = selected_term,
    y = dv,
    exclusion = exclusion,
    cut_off = cut_off,
    model = "rep BANOVA",
    controls = NA,
    method = paste("rep BANOVA Matched", selected_term),
    p.value = NA,
    effect.size = NA,
    effect.size.ma = NA,
    effect.size.ma.lci = NA,
    effect.size.ma.hci = NA,
    estimate = bfmm,
    statistic = NA,
    conf.low = NA,
    conf.high = NA,
    framework = "Bayesian"
  )

  bfum_res <- data.frame(
    x = selected_term,
    y = dv,
    exclusion = exclusion,
    cut_off = cut_off,
    model = "rep BANOVA",
    controls = NA,
    method = paste("rep BANOVA Unmatched", selected_term),
    p.value = NA,
    effect.size = NA,
    effect.size.ma = NA,
    effect.size.ma.lci = NA,
    effect.size.ma.hci = NA,
    estimate = bfum,
    statistic = NA,
    conf.low = NA,
    conf.high = NA,
    framework = "Bayesian"
  )

  res <- rbind(bfmm_res) %>%#, bfum_res) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(data_used = list(data))

  return(res)

}
