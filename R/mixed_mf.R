#' mixed_mf
#'
#' @description Mixed model function
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
#' @export
mixed_mf <- function(cs1,
                     cs2,
                     data,
                     subj,
                     time = TRUE,
                     group = NULL,
                     phase = "acquisition",
                     dv = "scr",
                     exclusion = "full data") {
  # Check data
  collection_warning(
    cs1 = cs1,
    cs2 = cs2,
    data = data,
    subj = subj
  )

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

  # Time effects contrasts
  cut_point <- mean(1:length(unique(data$time)))
  data <- data %>%
    dplyr::mutate(time2 = as.numeric(as.character(time)) - cut_point)

  # Two standardizations
  data <- data %>%
    dplyr::mutate(resp_stand_dv = scale(resp)) %>%
    dplyr::group_by(subj) %>%
    dplyr::mutate(resp_stand_pp = scale(resp)) %>%
    dplyr::ungroup()

  ##########################################
  ##########################################
  # ID only as random factor
  ##########################################
  ##########################################

  # Run the models 1st standardization
  base_model_stand_dv <-
    nlme::lme(resp_stand_dv ~ 1,
              random = ~ 1 | subj,
              data = data,
              method = "ML",
              control=list(opt = "optim", msMaxIter = 500),
              na.action = stats::na.omit,
              correlation = nlme::corAR1())

  cs_model_stand_dv <- stats::update(base_model_stand_dv, . ~ . + cs)
  cs_time_model_stand_dv <-
    stats::update(base_model_stand_dv, . ~ . + cs + time2 + cs:time2)

  # Run the models 2st standardization
  base_model_stand_pp <-
    nlme::lme(resp_stand_pp ~ 1,
              random = ~ 1 | subj,
              data = data,
              method = "ML",
              control=list(opt = "optim", msMaxIter = 500),
              na.action = stats::na.omit,
              correlation = nlme::corAR1())

  cs_model_stand_pp <- stats::update(base_model_stand_pp, . ~ . + cs)
  cs_time_model_stand_pp <-
    stats::update(base_model_stand_pp, . ~ . + cs + time2 + cs:time2)

  ##########################################
  ##########################################
  # Add time as random factor
  ##########################################
  ##########################################

  # Run the models 1st standardization
  base_model_rtime_stand_dv <-
    nlme::lme(resp_stand_dv ~ 1,
              random = ~ 1 + time2 | subj,
              data = data,
              method = "ML",
              control=list(opt = "optim", msMaxIter = 500),
              na.action = stats::na.omit,
              correlation = nlme::corAR1())

  cs_model_rtime_stand_dv <- stats::update(base_model_rtime_stand_dv, . ~ . + cs)
  cs_time_model_rtime_stand_dv <-
    stats::update(base_model_rtime_stand_dv, . ~ . + cs + time2 + cs:time2)

  # Run the models 2st standardization
  base_model_rtime_stand_pp <-
    nlme::lme(resp_stand_pp ~ 1,
              random = ~ 1 | subj,
              data = data,
              method = "ML",
              control=list(opt = "optim", msMaxIter = 500),
              na.action = stats::na.omit,
              correlation = nlme::corAR1())

  cs_model_rtime_stand_pp <- stats::update(base_model_rtime_stand_pp, . ~ . + cs)
  cs_time_model_rtime_stand_pp <-
    stats::update(base_model_rtime_stand_pp, . ~ . + cs + time2 + cs:time2)

  ##########################################
  ##########################################
  # Model selection
  ##########################################
  ##########################################



  if (all(sapply(
    c(
      "base_model_stand_dv",
      "cs_model_stand_dv",
      "cs_time_model_stand_dv"
    ),
    exists
  ))) {
    b_mc_dv <-
      stats::anova(base_model_stand_dv,
            cs_model_stand_dv,
            cs_time_model_stand_dv) %>%
      dplyr::mutate(models = rownames(.)) %>%
      dplyr::slice(which.min(BIC)) %>%
      dplyr::select(models) %>%
      as.character() %>% parse(text = .) %>%
      eval() %>%
      summary()
  } else{
    b_mc_dv <- NA
  }


  if (all(sapply(
    c(
      "base_model_stand_pp",
      "cs_model_stand_pp",
      "cs_time_model_stand_pp"
    ),
    exists
  ))) {
    b_mc_pp <-
      stats::anova(base_model_stand_pp,
            cs_model_stand_pp,
            cs_time_model_stand_pp) %>%
      dplyr::mutate(models = rownames(.)) %>%
      dplyr::slice(which.min(BIC)) %>%
      dplyr::select(models) %>%
      as.character() %>% parse(text = .) %>%
      eval() %>%
      summary()
  } else{
    b_mc_pp <- NA
  }


  if (all(sapply(
    c(
      "base_model_rtime_stand_dv",
      "cs_model_rtime_stand_dv",
      "cs_time_model_rtime_stand_dv"
    ),
    exists
  ))) {
    t_mc_dv <-
      stats::anova(
        base_model_rtime_stand_dv,
        cs_model_rtime_stand_dv,
        cs_time_model_rtime_stand_dv
      ) %>%
      dplyr::mutate(models = rownames(.)) %>%
      dplyr::slice(which.min(BIC)) %>%
      dplyr::select(models) %>%
      as.character() %>% parse(text = .) %>%
      eval() %>%
      summary()
  } else{
    t_mc_dv <- NA
  }

  if (all(sapply(
    c(
      "base_model_rtime_stand_pp",
      "cs_model_rtime_stand_pp",
      "cs_time_model_rtime_stand_pp"
    ),
    exists
  ))) {
    t_mc_dv <-
      stats::anova(
        base_model_rtime_stand_pp,
        cs_model_rtime_stand_pp,
        cs_time_model_rtime_stand_pp
      ) %>%
      dplyr::mutate(models = rownames(.)) %>%
      dplyr::slice(which.min(BIC)) %>%
      dplyr::select(models) %>%
      as.character() %>% parse(text = .) %>%
      eval() %>%
      summary()
  } else{
    t_mc_dv <- NA
  }

  base_model_time <-
    nlme::lme(resp ~ time2,
              random = ~ 1 |  subj,
              data = data,
              method = "ML",
              control=list(opt="optim"),
              correlation = nlme::corAR1())

  time_cs_model_time <-
    nlme::lme(resp ~ time2 + cs + time2:cs,
              random = ~ 1 |  subj,
              data = data,
              method = "ML",
              control=list(opt="optim"),
              correlation = nlme::corAR1())

  base_model_time_rand <-
    nlme::lme(resp ~ time2,
              random = ~ 1 + time2 |  subj,
              data = data,
              method = "ML",
              control=list(opt="optim"),
              correlation = nlme::corAR1())

  cs_model_time_rand <-
    nlme::lme(resp ~ time2 + cs + time2:cs,
              random = ~ 1 + time2 |  subj,
              data = data,
              method = "ML",
              control=list(opt="optim"),
              correlation = nlme::corAR1())

  t_anova <-
    stats::anova(base_model_time, base_model_time_rand) %>%
    dplyr::mutate(models = rownames(.)) %>%
    dplyr::slice(which.min(BIC)) %>%
    dplyr::select(models) %>%
    as.character() %>%
    parse(text = .) %>%
    eval() %>%
    summary()

  t_anova2 <-
    stats::anova(base_model_time_rand,
          cs_model_time_rand) %>%
    dplyr::mutate(models = rownames(.)) %>%
    dplyr::slice(which.min(BIC)) %>%
    dplyr::select(models) %>%
    as.character() %>% parse(text = .) %>%
    eval() %>%
    summary()

  # Gather the terms now
  base_model_stand_pp_tab <-
    select_term(base_model_stand_pp, "(Intercept)")
  cs_model_stand_pp_tab <- select_term(cs_model_stand_pp, "cscs2")
  cs_time_model_stand_pp_tab <-
    select_term(cs_time_model_stand_pp, "cscs2:time2")

  base_model_rtime_stand_dv_tab <-
    select_term(base_model_rtime_stand_dv, "(Intercept)")
  cs_model_rtime_stand_dv_tab <-
    select_term(cs_model_rtime_stand_dv, "cscs2")
  cs_time_model_rtime_stand_dv_tab <-
    select_term(cs_time_model_rtime_stand_dv, "cscs2:time2")

  base_model_rtime_stand_pp_tab <-
    select_term(base_model_rtime_stand_pp, "(Intercept)")
  cs_model_rtime_stand_pp_tab <-
    select_term(cs_model_rtime_stand_pp, "cscs2")
  cs_time_model_rtime_stand_pp_tab <-
    select_term(cs_time_model_rtime_stand_pp, "cscs2:time2")


  res <- tibble::tibble(
    base_model_tab = base_model_tab,
    cs_model_tab = cs_model_tab,
    base_model_time_tab = base_model_time_tab,
    base_model_time_rand_tab = base_model_time_rand_tab,
    time_cs_model_time_tab = time_cs_model_time_tab,
    cs_model_time_rand_tab = cs_model_time_rand_tab
  )

  # Work on the results
  res <- tibble::tibble(
    dplyr::bind_rows(
      base_model_stand_pp_tab,
      cs_model_stand_pp_tab,
      cs_time_model_stand_pp_tab,
      base_model_rtime_stand_dv_tab,
      cs_model_rtime_stand_dv_tab,
      cs_time_model_rtime_stand_dv_tab,
      base_model_rtime_stand_pp_tab,
      cs_model_rtime_stand_pp_tab,
      cs_time_model_rtime_stand_pp_tab
    )
  )

  return(res)
}
