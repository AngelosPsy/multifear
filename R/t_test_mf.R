#' t_test_mf
#'
#'
#' @description Basic function for running the frequentist's t-tests included in the main analyses
#' @param cs1 The column name(s) of the conditioned responses for the first conditioned stimulus
#' @param cs2 The column name(s) of the conditioned responses for the second conditioned stimulus
#' @param subj The name of the column including the participant numbers. Unique numbers are expected
#' @param group The name of the column including the group name. Default to \code{NULL} (i.e., no groups)
#' @param data A data frame containing all the relevant columns for the analyses
#' @param paired Whether the t-test refers to dependent (i.e., paired) or to independent sample(s). Default to \code{TRUE}
#' @param quanz Quantiles for the meta-analytic effect sizes. Default to .05 (lower) and.95 (upper)
#' @param phase The conditioned phase that the analyses refer to. Accepted values are  \code{acquisition}, \code{acq}, \code{extinction}, or \code{ext}
#' @param dv name of the measured conditioned response. Default to \code{"SCR"}
#' @param group the name of the group, if included, default to \code{NULL}
#' @param na.rm Whether NAs should be removed, default to \code{FALSE}
#' @param meta.effect How the meta-analytic effect should be computed, Default to \code{"d_to_eta2"} (see details for more information)
#' @param exclusion Name of the data reduction procedure used. Default to \code{full data}
#' @param cut_off cut off Name of the cut_off applied. Default to \code{full data}
#' @details Given the correct names for the \code{cs1}, \code{cs2}, \code{subj}, and \code{data}, the function will run one- and two-sided frequentist's t-tests. In case \code{cs1} or \code{cs2} refer to multiple columns, the mean -- per row -- for each one of these variables will be computed first before running the t-test. Please note that cs1 is implicitly referred to the cs that is reinforced, and cs2 to the cs that is not reinforced.
#' Depending on whether the data refer to an acquisition or extinction phase (as defined in the \code{phase} argument), the function will return a positive one sided, or negative one-sided, respectively t-test in addition to the two-sided t-test. The returned effect size is  Hedge's g in the column effect size. For the meta-analytic effect size (effect.size.ma), the returned effect size is eta-squared.
#'
#' The function by default runs a Welch t-test, meaning it assumes unequal variances. This is due to calls that such a test should be preferred over Student's t-test, at least for paired samples t-test. Please note that if we let R decide which test to run -- this is done by default in \code{stats::t.test}, then for some test there would be a Student t-test whereas in some others not.
#' There are two different ways to compute the meta-analytic effect sizes but the results may differ. The option
#' "t_to_eta2" computes the eta squared via the t values whereas the "d_to_eta2" the eta squared is computed via
#' the Cohen's d value.
#'
#' @return A tibble with the following column names:
#'
#' x: the name of the independent variable (e.g., cs)
#'
#' y: the name of the dependent variable as this defined in the \code{dv} argument
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
#' effect.size.ma: the estimated effect size for the meta-analytic plots. Here we used eta squared
#'
#' effect.size.ma.lci: low confidence intervals for the meta-analytic effect size
#'
#' effect.size.ma.hci: high confidence intervals for the meta-analytic effect size
#'
#' estimate: the estimate of the test run. For the t-test is the mean of the differences
#'
#' statistic: the t-value
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
#' # Paired sample t-tests
#' t_test_mf(cs1 = "CSP1", cs2 = "CSM1", subj = "id", data = example_data)
#'
#' # Independent  sample t-tests
#' t_test_mf(cs1 = "CSP1", cs2 = "CSM1", subj = "id",  group = "group", data = example_data)
#'
#' @importFrom dplyr %>%
#' @export

t_test_mf <-
  function(cs1,
           cs2,
           data,
           subj,
           group = NULL,
           na.rm = FALSE,
           paired = TRUE,
           quanz = c(.05, .95),
           meta.effect = "d_to_eta2",
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

    match.arg(meta.effect, c("d_to_eta2", "t_to_eta2"))

    data <-
      data_preparation_ttest(
        cs1 = cs1,
        cs2 = cs2,
        data = data,
        subj = subj,
        group = group,
        na.rm = na.rm
      )

    # Here we run all t.tests and we select later on which ones we want.

    #####################################
    # Independent samples
    #####################################

    if (!is.null(group)) {
      ttest_prep_tmp <- purrr::map_dfr(.x = seq_len(3), ~ data) %>%
        dplyr::mutate(
          cs = cs.1 - cs.2,
          alternative = rep(c("two.sided", "less", "greater"),
                            each = nrow(data)),
          group2 = alternative
        )

      ttest_prep <- ttest_prep_tmp %>%
        dplyr::group_by(group2) %>%
        dplyr::group_map(
          ~ stats::t.test(
            formula = .$cs ~ .$group,
            data = .,
            #paired = FALSE,
            alternative = .$alternative[1],
            var.equal = FALSE
          ) %>%
            broom::tidy()
        )

      # Compute effect size
      ttest_es <-
        effsize::cohen.d(
          ttest_prep_tmp$cs ~ as.factor(ttest_prep_tmp$group),
          pooled = TRUE,
          paired = FALSE,
          na.rm = na.rm,
          hedges.correction = TRUE
        )

      ttest_prep <- ttest_prep_tmp %>%
        dplyr::group_by(group2) %>%
        dplyr::group_map(
          ~ stats::t.test(
            formula = .$cs ~ .$group,
            data = .,
            #paired = FALSE,
            alternative = .$alternative[1],
            var.equal = FALSE
          ) %>%
            broom::tidy()
        )

      if (meta.effect == "d_to_eta2") {
        ttest_es_ma <- effsize::cohen.d(
          ttest_prep_tmp$cs ~ as.factor(ttest_prep_tmp$group),
          pooled = TRUE,
          paired = FALSE,
          na.rm = na.rm,
          hedges.correction = FALSE
        )
      } else if(meta.effect == "t_to_eta2") {
        ttest_es_ma <-  stats::t.test(
          ttest_prep_tmp$cs ~ as.factor(ttest_prep_tmp$group),
          pooled = TRUE,
          #paired = FALSE,
          alternative = "two.sided",
          hedges.correction = FALSE
        )
      }

    } else {

      #####################################
      # Paired samples t-test
      #####################################
      ttest_prep <- purrr::map_dfr(.x = seq_len(3), ~ data) %>%
        dplyr::mutate(alternative = rep(c("two.sided", "less", "greater"),
                                        each = nrow(data)),
                      group2 = alternative) %>%
        tidyr::gather(CS, value, -subj, -alternative, -group, -group2) %>%
        tidyr::separate(CS, c("CS", "N")) %>%
        dplyr::group_by(group2) %>%
        dplyr::group_map(
          ~ stats::t.test(
            #formula = .$value ~ .$N,
            stats::Pair(unlist(data %>% dplyr::select(all_of("cs.1"))),
            unlist(data %>% dplyr::select(all_of("cs.2")))),
            data = .,
            #paired = paired,
            alternative = .$alternative[1],
            var.equal = FALSE
          ) %>%
            broom::tidy()
        )

      # Compute effect size
      ttest_es <-
        effsize::cohen.d(
          unlist(data %>% dplyr::select(all_of("cs.1"))),
          unlist(data %>% dplyr::select(all_of("cs.2"))),
          pooled = TRUE,
          paired = TRUE,
          na.rm = na.rm,
          hedges.correction = TRUE
        )

      if (meta.effect == "d_to_eta2") {
        ttest_es_ma <-  effsize::cohen.d(
          unlist(data %>% dplyr::select(all_of("cs.1"))),
          unlist(data %>% dplyr::select(all_of("cs.2"))),
          pooled = TRUE,
          paired = TRUE,
          na.rm = na.rm,
          hedges.correction = FALSE,
          conf.level = .90
        )
      } else if(meta.effect == "t_to_eta2") {
        ttest_es_ma <- ttest_es <-
          stats::t.test(
            stats::Pair(unlist(data %>% dplyr::select(all_of("cs.1"))),
            unlist(data %>% dplyr::select(all_of("cs.2")))),
            pooled = TRUE,
            #paired = TRUE,
            hedges.correction = FALSE
          )
      }
    }

    if (meta.effect == "d_to_eta2") {
      # Compute CIs and convert them
      es_ma <- ttest_es_ma$estimate
      ci_ma <- ttest_es_ma$estimate - ttest_es_ma$conf.int[1]
      #ci_boot <- t_boot(data, paired = TRUE, quanz = c(.05, .95))
      ttest_res <-
        purrr::invoke("rbind", ttest_prep) %>%
        dplyr::mutate(effect.size = rep(ttest_es$estimate, 3),
                      effect.size.lci = rep(ttest_es$conf.int[[1]], 3),
                      effect.size.hci = rep(ttest_es$conf.int[[2]], 3),
                      effect.size.ma = rep(esc::eta_squared(d = es_ma), 3),
                     effect.size.ma.lci = rep(esc::eta_squared(d = es_ma - ci_ma), 3),
                     effect.size.ma.hci = rep(esc::eta_squared(d = es_ma + ci_ma), 3))

    } else if(meta.effect == "t_to_eta2") {
      es_ma <- ttest_es_ma$estimate
      #ci_ma <- ttest_es_ma$estimate - ttest_es_ma$conf.int[1]
      ci_boot <- t_boot(data, paired = TRUE, quanz = c(.05, .95))
      ttest_res <-
        purrr::invoke("rbind", ttest_prep) %>%
        dplyr::mutate(effect.size = rep(ttest_es$estimate, 3),
                      effect.size.ma = rep(t_to_eta2(ttest_es_ma), 3),
                      effect.size.ma.lci = rep(ci_boot[1], 3),
                      effect.size.ma.hci = rep(ci_boot[2], 3))
    }

   # ttest_res <-
    #  purrr::invoke("rbind", ttest_prep) %>%
    #  dplyr::mutate(effect.size = rep(ttest_es$estimate, 3),
    #                effect.size.ma = rep(esc::eta_squared(d = es_ma), 3),
    #                effect.size.ma.lci = rep(esc::eta_squared(d = es_ma - ci_ma), 3),
    #                effect.size.ma.hci = rep(esc::eta_squared(d = es_ma + ci_ma), 3))

   # ttest_res <-
  #    purrr::invoke("rbind", ttest_prep) %>%
  #    dplyr::mutate(effect.size = rep(ttest_es$estimate, 3),
  #                  effect.size.ma = rep(ttest_es_ma$estimate/2, 3),
  #                  effect.size.ma.lci = rep(ttest_es_ma$conf.int[1]/2, 3),
  #                  effect.size.ma.hci = rep(ttest_es_ma$conf.int[2]/2, 3))

    # List to be pasted to broom functions
    if (!!phase %in% c("acquisition", "acq")) {
      ttl <-
        ttest_res %>% dplyr::filter(alternative %in% c("two.sided", "greater"))
      #used_model = paste("t-test", c("two.sided", "greater"))
    }

    if (!!phase %in% c("extinction", "ext")) {
      ttl <-
        ttest_res %>% dplyr::filter(alternative %in% c("two.sided", "less"))
      #used_model = paste("t-test", c("two.sided", "less"))
    }

    res <- ttl %>%
      dplyr::mutate(
        method = ttl$alternative,
        x = "cs",
        y = dv,
        exclusion = exclusion,
        cut_off = cut_off,
        model = "t-test",
        controls = NA,
        framework = "NHST"
      ) %>%
      dplyr::select(
        x,
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
        framework
      ) %>%
      dplyr::mutate(data_used = list(data))

    return(res)
  }
