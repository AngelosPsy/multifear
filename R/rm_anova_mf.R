#' rm_anova_mf
#'
#' @description Basic function for running repeated measures ANOVA
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
#' The function uses the \code{ez::ezANOVA} function. The function gives by default a warning regarding the collapsing of factors. This function here suppresses this warning but the user should be aware of it.
#'
#' The effect size is omega squared.
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom stats time
#' @export
rm_anova_mf <- function(cs1,
                        cs2,
                        data,
                        subj,
                        time = TRUE,
                        group = NULL,
                        phase = "acquisition",
                        dv = "scr",
                        exclusion = "full data") {
  collection_warning(cs1 = cs1, cs2 = cs2, data = data, subj = subj)

  data <-
    data_preparation_anova(cs1 = cs1, cs2 = cs2, data = data, subj = subj,
                     time = TRUE, group = NULL)

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
    group = NULL
    selected_term <- "group:cs"
  }

  # Run the main ANOVA
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
          between = ',
          group,
          ',
          type = 3,
          return_aov = TRUE
        )$aov'
)
    )))

  # Effect size
  eff_size <- sjstats::omega_sq(tmpANOVA) %>%
    dplyr::filter(stratum == paste0("subj:", selected_term)) %>%
    dplyr::select(omegasq) %>%
    as.numeric()

  # Shape the object for the results. The suppressWarnings is there due to
  # warning from broom::tidy.
  res <-
    suppressWarnings(purrr::map_df(tmpANOVA, .f = broom::tidy)) %>%
    dplyr::filter(term %in% selected_term) %>%
    dplyr::rename_all(list(~stringr::str_replace(., "term", "method"))) %>%
    dplyr::mutate(
      method = paste("rep ANOVA", selected_term),
      x = selected_term,
      y = dv,
      exclusion = exclusion,
      model = "rep ANOVA",
      controls = NA,
      estimate = NA,
      conf.low = NA,
      conf.high = NA,
      effect.size = eff_size
    ) %>%
    dplyr::select(x,
                  y,
                  exclusion,
                  model,
                  controls,
                  method,
                  p.value,
                  effect.size,
                  estimate,
                  statistic,
                  conf.low,
                  conf.high) %>%
    dplyr::mutate(data_used = list(data))

  return(res)

}
