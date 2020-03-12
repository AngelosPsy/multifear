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
#' @param group name of the group variable, if it is present (default to NULL)
#' @param phase Different tests will be run for different phases. That is why
#' the phase needs to be specified here. Possible values are \code{acquisition},
#' or \code{acq}, \code{extnction}, or \code{extinction}. See Details for more
#' information.
#' @return A basic function for running repeated measures ANOVAs
#' @details In case the \code{time} argument is set to true, the function will
#' include this as a within subjects factor, assuming that the columns in
#' \code{cs1} and \code{cs2} corrrespond to ascending time points (e.g., cs1
#' trial 1, cs1 trial 2 ... cs1 trial \code{n}). If this is not the case, the
#' results are not to be trusted.
#' @export

rm_anova_mf <- function(cs1,
                        cs2,
                        time = TRUE,
                        subj,
                        data,
                        group = NULL,
                        phase = "acquisition") {
  cs1 <-
    data %>% dplyr::select(!!dplyr::enquo(cs1)) %>% tibble::as_tibble()
  cs2 <-
    data %>% dplyr::select(!!dplyr::enquo(cs2)) %>% tibble::as_tibble()
  subj  <-
    data %>% dplyr::select(!!dplyr::enquo(subj)) %>% tibble::as_tibble()

  if (!is.null(group)) {
    group  <-
      data %>% dplyr::select(!!dplyr::enquo(group)) %>% tibble::as_tibble()
  }


  # Renaming objects to make life a bit easier
  cs1  <- cs1 %>% dplyr::select(cs1_ = dplyr::everything())
  cs2  <- cs2 %>% dplyr::select(cs2_ = dplyr::everything())
  subj <- subj %>% dplyr::select(subj = dplyr::everything())
  data <- dplyr::bind_cols(subj, cs1, cs2)

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
    data %>%
      reshape2::melt(
        id.var = "subj",
        variable.name = "var_old",
        value.name = "resp",
        factorsAsStrings = TRUE
      ) %>% # Until pivot_longer gets better
      dplyr::mutate(
        cs = as.factor(stringr::str_sub(var_old, 1, 3)),
        time = as.factor(sub(".*_", "", .$var_old)),
        subj = as.factor(subj)
      ) -> data # Better than stringr
    }

  # You need to have an aov object to feed in glance
  tmpANOVA <-
    ez::ezANOVA(
      data = data,
      dv = resp,
      wid = subj,
      within = .(cs, time),
      #between = group,
      type = 3,
      return_aov = TRUE
    )$aov

  res <-
    purrr::map_df(tmpANOVA, .f = broom::tidy) %>% filter(term %in% c("cs", "time", "cs:time")) %>% select(term, statistic)

  return(res)
  }
