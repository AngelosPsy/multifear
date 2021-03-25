#' mixed_mf
#'
#'
#' @description Basic function for running mixed models for the multiverse analysis
#' @inheritParams rm_anova_mf
#' @return A data frame with the results.
#' @details The function assumes that you include more than 1 trial per CS. The function returns an error if that is not the function.
#'
#' The function performs by default two dependent variable standardizations, the one per subject
#' and the other one without taking subject into account.
#'
#' In case time is included, the function computes the intercept -- i.e., the 0 point -- on the middle of the time sequence.
#'
#' The following models are run and compared: a) Intercept only model, b) Intercept plus CS model, and c) Intercept plus CS \code{x} Time interaction.
#'
#' Separate models are run with `Subject` as random factor, as well as `Subject and Time` as random factors.
#'
#' The model is fit by maximizing the log-likelihood (i.e., "ML" term in nlme::lme).
#'
#' The model comparison is done using `BIC`.
#'
#' @return The data frame returned is the standard one returned in all function in the package. Specifically we have:
#'
#' A tibble with the following column names:
#'
#' x: the name of the independent variable (e.g., cs). There, you can see the term of the model that is returned. So, not the full model is returned but only this particular term.
#'
#' y: the name of the dependent variable as this defined in the \code{dv} argument
#'
#' exclusion: see \code{exclusion} argument
#'
#' model: the model that was run (e.g., mixed_model)
#'
#' controls: ignore this column for this test
#'
#' method: the model that was run
#'
#' p.value: the p-value for each factor
#'
#' effect.size: irrelevant here
#'
#' effect.size.ma: irrelevant here
#'
#' effect.size.ma.lci: irrelevant here
#'
#' effect.size.ma.hci: irrelevant here
#'
#' statistic: the t-value for each factor
#'
#' conf.low: the lower confidence interval for the estimate
#'
#' conf.high: the higher confidence interval for the estimate
#'
#' data_used: a list with the data used for the specific test
#'
#' @seealso
#' \code{\link[nlme]{lme}}
#'
#' @examples
#' cs1 <- paste0("CSP", 1:2)
#' cs2 <- paste0("CSM", 1:2)
#' subj <- "id"
#'
#' # mixed models without groups
#' mixed_mf(cs1 = cs1, cs2 = cs2, subj = subj, data = example_data)
#'
#' # mixed models with groups
#' mixed_mf(cs1 = cs1, cs2 = cs2, subj = subj, group = "group", data = example_data)
#'
#'
#' @importFrom nlme lme
#' @importFrom nlme lme.formula
#' @export
mixed_mf <- function(cs1,
                     cs2,
                     data,
                     subj,
                     group = NULL,
                     phase = "acquisition",
                     dv = "scr",
                     exclusion = "full data",
                     cut_off = "full data") {

  # Check data
  collection_warning(
    cs1 = cs1,
    cs2 = cs2,
    data = data,
    subj = subj
  )

  if (length(cs1) < 2 ||
      length(cs2) < 2) {
    stop("The CS1 and/or CS2 have smaller than 2 levels.
         The mixed_mf function was not run.")
  }

  data <-
    data_preparation_anova(cs1 = cs1, cs2 = cs2, data = data, subj = subj,
                     time = TRUE, group = NULL)

  # Time effects contrasts
  cut_point <- mean(1:length(unique(data$time)))
  data <- data %>%
    dplyr::mutate(time2 = as.numeric(as.character(time)) - cut_point)

  # Had to change resp column as it was returning this bug in some cases:
  # Error in array(x, c(length(x), 1L), if (!is.null(names(x))) list(names(x),  :
  data$resp2 <- data$resp

  # Two standardizations
  data <- data %>%
    dplyr::mutate(resp_stand_dv = scale(resp2)) %>%
    dplyr::group_by(subj) %>%
    dplyr::mutate(resp_stand_pp = scale(resp2)) %>%
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
              control=list(opt = "optim", msMaxIter = 500),
              correlation = nlme::corAR1())

  time_cs_model_time <-
    nlme::lme(resp ~ time2 + cs + time2:cs,
              random = ~ 1 |  subj,
              data = data,
              method = "ML",
              control=list(opt = "optim", msMaxIter = 500),
              correlation = nlme::corAR1())

  base_model_time_rand <-
    nlme::lme(resp ~ time2,
              random = ~ 1 + time2 |  subj,
              data = data,
              method = "ML",
              control=list(opt = "optim", msMaxIter = 500),
              correlation = nlme::corAR1())

  cs_model_time_rand <-
    nlme::lme(resp ~ time2 + cs + time2:cs,
              random = ~ 1 + time2 |  subj,
              data = data,
              method = "ML",
              control=list(opt = "optim", msMaxIter = 500),
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

  # Gather the terms now. Inside the function we compute the meta_analyric effect size as well
  base_model_stand_pp_tab <-
    select_term(base_model_stand_pp, "(Intercept)", dv = dv, exclusion = exclusion)
  cs_model_stand_pp_tab <-
    select_term(cs_model_stand_pp, "cscs2", dv = dv,exclusion = exclusion)
  cs_time_model_stand_pp_tab <-
    select_term(cs_time_model_stand_pp, "cscs2:time2", dv = dv, exclusion = exclusion)

  base_model_rtime_stand_dv_tab <-
    select_term(base_model_rtime_stand_dv, "(Intercept)", dv = dv, exclusion = exclusion)
  cs_model_rtime_stand_dv_tab <-
    select_term(cs_model_rtime_stand_dv, "cscs2", dv = dv,exclusion = exclusion)
  cs_time_model_rtime_stand_dv_tab <-
    select_term(cs_time_model_rtime_stand_dv, "cscs2:time2", dv = dv,exclusion = exclusion)

  base_model_rtime_stand_pp_tab <-
    select_term(base_model_rtime_stand_pp, "(Intercept)", dv = dv,exclusion = exclusion)
  cs_model_rtime_stand_pp_tab <-
    select_term(cs_model_rtime_stand_pp, "cscs2", dv = dv,exclusion = exclusion)
  cs_time_model_rtime_stand_pp_tab <-
    select_term(cs_time_model_rtime_stand_pp, "cscs2:time2", dv = dv,exclusion = exclusion)

  # Work on the results
  res_tmp <- tibble::tibble(
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

  #res <- res_tmp$`dplyr::bind_rows(...)`
  res <- res_tmp %>% dplyr::filter(x != "(Intercept)")
  # Remove intercepts, keep the rest

  return(res)
}
