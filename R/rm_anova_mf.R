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
  # Check data
  collection_warning(cs1 = cs, cs2 = cs2, data = data, subj = subj)

  cs1 <-
    data %>% dplyr::select(all_of(!!dplyr::enquo(cs1))) %>% tibble::as_tibble()
  cs2  <-
    data %>% dplyr::select(all_of(!!dplyr::enquo(cs2))) %>% tibble::as_tibble()
  subj <-
    data %>% dplyr::select(all_of(!!dplyr::enquo(subj))) %>% tibble::as_tibble()

  # Renaming objects to make life a bit easier
  cs1  <- cs1 %>% dplyr::select(cs1_ = dplyr::everything())
  cs2  <- cs2 %>% dplyr::select(cs2_ = dplyr::everything())
  subj <- subj %>% dplyr::select(subj = dplyr::everything())

  if (is.null(group)) {
    group_new <-
      data %>%
      dplyr::mutate(group = rep("NULL", nrow(data))) %>%
      dplyr::select(group)
    group <- NULL
  } else{
    group_new <- data %>%
      dplyr::select(tidyselect::all_of(!!dplyr::enquo(group)))
  }

  data <- dplyr::bind_cols(subj, cs1, cs2, group_new)

  # In case time is selected, create a time object
  if (time) {
    # Check if length of cs1 and cs2 is the same. Otherwise stop
    if (ncol(cs1) != ncol(cs2)) {
      stop(
        "You have selected that you want to analyse time effects but the
        cs1 and cs2 have different number of time points. Stopping function
        now."
      )
    }
    }
  data %>%
    reshape2::melt(
      id.var = c("subj", "group"),
      variable.name = "var_old",
      value.name = "resp",
      factorsAsStrings = TRUE
    ) %>% # Until pivot_longer gets better
    dplyr::mutate(
      cs = as.factor(stringr::str_sub(var_old, 1, 3)),
      time = as.factor(sub(".*_", "", .$var_old)),
      subj = as.factor(subj),
      group = as.factor(group)
    ) -> data # Better than stringr
  #}

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

  # Shape the object for the results. The suppressWarnings is there due to
  # warning from broom::tidy.
  res <-
    suppressWarnings(purrr::map_df(tmpANOVA, .f = broom::tidy)) %>%
    dplyr::filter(term %in% selected_term) %>%
    dplyr::rename_all(dplyr::funs(stringr::str_replace(., "term", "method"))) %>%
    dplyr::mutate(
      method = paste("rep ANOVA", selected_term),
      x = selected_term,
      y = "cr",
      exclusion = exclusion,
      model = "rep ANOVA",
      controls = NA,
      estimate = NA,
      conf.low = NA,
      conf.high = NA
    ) %>%
    dplyr::select(x,
                  y,
                  exclusion,
                  model,
                  controls,
                  method,
                  estimate,
                  statistic,
                  conf.low,
                  conf.high) %>%
    dplyr::mutate(data_used = list(data))

  return(res)

}
