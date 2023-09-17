#' rm_anova_mf
#'
#' @description Basic function for running the frequentist's repeated measures analysis of variance (ANOVA)
#'
#' @inheritParams t_test_mf
#' @param time should time be included? Default to \code{TRUE}
#' @param correction whether the Greenhouse-Geisser correction should be applied or not. Default to \code{FALSE}
#' @return A basic function for running repeated measures ANOVAs.
#' @details In case the \code{time} argument is set to \cite{TRUE} (default value), the function will include this as a within subjects factor, assuming that the columns in
#' \code{cs1} and \code{cs2} correspond to ascending time points (e.g., cs1
#' trial 1, cs1 trial 2 ... cs1 trial \code{n}). If this is not the case, the
#' results are not to be trusted.
#'
#' The function uses the \code{ez::ezANOVA} function. The function gives by default a warning regarding the collapsing of factors. This function here suppresses this warning but the user should be aware of it. Please note that at the moment no sphericity correction is performed. The reported effect size is omega squared as this is computed by \code{sjstats::omega_sq}. The meta-analytic effect size is eta squared.
#'
#' @return A tibble with the following column names:
#'
#' x: the name of the independent variable (e.g., cs)
#'
#' y: the name of the dependent variable as this defined in the \code{dv} argument
#'
#' exclusion: see \code{exclusion} argument
#'
#' model: the model that was run (e.g., t-test)
#'
#' controls: ignore this column for this test
#'
#' method: the model that was run
#'
#' p.value: the p-value of the test
#'
#' effect.size: the estimated effect size
#'
#' effect.size.ma: the estimated effect size for the meta-analytic plots
#'
#' effect.size.ma.lci: low confidence intervals for the meta-analytic effect size
#'
#' effect.size.ma.hci: high confidence intervals for the meta-analytic effect size
#'
#' estimate: the estimate of the test run
#'
#' statistic: the F-value
#'
#' conf.low: the lower confidence interval for the estimate
#'
#' conf.high: the higher confidence interval for the estimate
#'
#' framework: were the data analysed within a NHST or Bayesian framework?
#'
#' data_used: a list with the data used for the specific test
#'
#' @examples
#' # Load example data
#' data(example_data)
#'
#' # Briefly define argument values that will be plugged in later on in the functions
#' cs1 <- paste0("CSP", 1:10)
#' cs2 <- paste0("CSM", 1:10)
#' subj <- "id"
#' group <- "group"
#'
#' # Repeated measures ANOVA without groups
#' rm_anova_mf(cs1 = cs1, cs2 = cs2, subj = subj, data = example_data, time = TRUE)
#'
#' # Repeated measures ANOVA with groups
#' rm_anova_mf(cs1 = cs1, cs2 = cs2, subj = subj, group = "group",
#' data = example_data, time = TRUE)
#' @importFrom stats time
#' @importFrom stats na.omit
#' @importFrom ez ezANOVA
#' @export
rm_anova_mf <- function(cs1,
                        cs2,
                        data,
                        subj,
                        time = TRUE,
                        group = NULL,
                        phase = "acquisition",
                        dv = "scr",
                        exclusion = "full data",
                        cut_off = "full data",
                        correction = FALSE) {
  collection_warning(cs1 = cs1, cs2 = cs2, data = data, subj = subj)

  data <-
    data_preparation_anova(cs1 = cs1, cs2 = cs2, data = data, subj = subj,
                     time = time, group = group)

  # Decide which terms you will have in order to feed in the ANOVA later on
  if (time && (!is.null(group))) {
    anova_terms <- c("cs", "time")
    selected_term <- "group:cs:time"
  } else if (!time && (is.null(group)))  {
    anova_terms <- c("cs")
    group = NULL
    selected_term <- "cs"
  } else if (time && is.null(group)) {
    anova_terms <- c("cs", "time")
    group = NULL
    selected_term <- "cs:time"
  } else if (!time && !is.null(group)) {
    anova_terms <- c("cs")
    selected_term <- "group:cs"
  }

  # Run the main ANOVA
  if(is.null(group)){
  tmpANOVA <-
    suppressWarnings(eval(parse(
      text =
        paste0(
          'ez::ezANOVA(
          data = data,
          dv = resp,
          wid = subj,
          within = c(',
          paste(anova_terms, collapse = ","),
          '),
          between = NULL,
          type = 3,
          detailed = TRUE,
          return_aov = TRUE
        )'
        ))))
  } else{
    tmpANOVA <-
      suppressWarnings(eval(parse(
        text =
          paste0(
            'ez::ezANOVA(
            data = data,
            dv = resp,
            wid = subj,
            within = c(',
            paste(anova_terms, collapse = ","),
            '),
            between = as.factor(group),
            type = 3,
            detailed = TRUE,
            return_aov = TRUE
            )'
        ))))
  }

  # This is what is returned from the data_preparation_anova function
  # Effect size. Here we have the omega squared
  eff_size <- effectsize::omega_squared(tmpANOVA$aov, partial = FALSE) %>%
    data.frame() %>%
    dplyr::filter(Parameter %in% c(paste0("subj:", selected_term), selected_term))

  # meta-analytic effect size. We use explained variance so we have eta squared
  es.ma <- effectsize::eta_squared(tmpANOVA$aov, partial = FALSE, ci = .90) %>%
    data.frame() %>%
    dplyr::filter(Parameter %in% c(paste0("subj:", selected_term), selected_term))

  # Shape the object for the results. The suppressWarnings is there due to
  # the warning returned by broom::tidy.
  res_preparation <-
    suppressWarnings(purrr::map_df(tmpANOVA$aov, .f = broom::tidy)) %>%
    dplyr::filter(term %in% c(paste0("subj:", selected_term), selected_term)) %>%
    dplyr::rename_all(list(~stringr::str_replace(., "term", "method"))) %>%
    dplyr::mutate(
      method = paste("rep ANOVA", selected_term),
      x = selected_term,
      y = dv,
      exclusion = exclusion,
      cut_off = cut_off,
      model = "rep ANOVA",
      controls = NA,
      estimate = NA,
      conf.low = NA,
      conf.high = NA,
      effect.size = eff_size$Omega2,
      effect.size.lci = eff_size$CI_low,
      effect.size.hci = eff_size$CI_high,
      effect.size.ma = es.ma$Eta2,#Cohens_f, #Eta2, #es.ma$es,
      effect.size.ma.lci = es.ma$CI_low, #es.ma$ci.lo,
      effect.size.ma.hci = es.ma$CI_high, #es.ma$ci.hi,
      framework = "NHST"
    )

  # Check for violation correction and correct it
  if (!is.null(tmpANOVA$`Sphericity Corrections`) && correction) {
    res_preparation$p.value <-
      tmpANOVA$`Sphericity Corrections` %>%
      dplyr::filter(Effect == selected_term) %>%
      dplyr::select("p[GG]") %>% as.numeric()
  }

  res <- res_preparation %>%
    dplyr::select(x,
                  y,
                  exclusion,
                  cut_off,
                  model,
                  controls,
                  method,
                  p.value,
                  effect.size,
                  effect.size.lci,
                  effect.size.hci,
                  effect.size.ma,
                  effect.size.ma.lci,
                  effect.size.ma.hci,
                  estimate,
                  statistic,
                  conf.low,
                  conf.high,
                  framework) %>%
    dplyr::mutate(data_used = list(data))

  return(res)

}
