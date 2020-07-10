#' bt_test_mf
#'
#' \lifecycle{experimental}
#'
#' Basic function for running the Bayesian t-tests included in the main analyses.
#' @inheritParams t_test_mf
#' @param rscale r scale to be used in the prior of the alternative hypothesis, default to "medium".
#' @return A tibble with the following column names:
#' x: the name of the independent variable (e.g., cs)
#' y: the name of the dependent variable as this defined in the \code{dv} argument
#' exclusion: see \code{exclusion} argument
#' model: the model that was run (e.g., t-test)
#' controls: ignore this column for this test
#' method: the model that was run
#' p.value: irrelevant here
#' effect.size: irrelevant here
#' estimate: the estimate of the test run
#' statistic: the t-value
#' conf.low: the lower confidence interval for the estimate
#' conf.high: the higher confidence interval for the estimate
#' framework: were the data analysed within a NHST or Bayesian framework?
#' data_used: a list with the data used for the specific test
#'
#' @details This is a wrapper function function around the BayesFactor::ttestBF(),
#' running multiple Bayesian t-tests. Similar to the \code{t_test_mf} function, the function will run different t-tests based on the phase that the t-tests refer to.
#'
#' @examples
#' # Load example data
#' data(example_data)
#'
#' # Paired sample t-tests
#' bt_test_mf(cs1 = "CSP1", cs2 = "CSM1", subj = "id", data = example_data)
#'
#' # Independent  sample t-tests
#' bt_test_mf(cs1 = "CSP1", cs2 = "CSM1", subj = "id",  group = "group", data = example_data)
#'
#'
#' @importFrom dplyr %>%
#' @export

bt_test_mf <-
  function(cs1,
           cs2,
           data,
           subj,
           group = NULL,
           na.rm = FALSE,
           paired = TRUE,
           rscale = "medium",
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

    data <-
      data_preparation_ttest(
        cs1 = cs1,
        cs2 = cs2,
        data = data,
        subj = subj,
        group = group,
        na.rm = na.rm
      )

    # Here we run all t.tests and we select later on which one we wants. It is
    # a bit too much to run all tests but we save all the if else statements


    if (is.null(group)) {
      ttest_prep <- purrr::map_dfr(.x = seq_len(3), ~ data) %>%
        dplyr::mutate(alternative = rep(c("c(-Inf, Inf)", "c(-Inf, 0)", "c(0, Inf)"),
                                        each = nrow(data)), group2 = alternative) %>%
        tidyr::gather(CS, value, -subj, -alternative, -group2, -group) %>%
        tidyr::separate(CS, c("CS", "N"),
                        convert = FALSE, remove = TRUE, fill = "right") %>%
        dplyr::group_by(group2)

        ttest_run <- ttest_prep %>%
        dplyr::group_map(
          ~ BayesFactor::ttestBF(
            x = .[.$N == 1,]$value,
            y = .[.$N == 2,]$value,
            rscale = rscale,
            nullInterval = eval(parse(text = .$alternative[1]))
          ) %>%
            BayesFactor::extractBF(onlybf = TRUE)
        ) %>%
        purrr::invoke(.f = "rbind") %>%
        data.frame() %>%
        dplyr::select(X1) %>%
        unlist()
    } else{
      ttest_prep <- purrr::map_dfr(.x = seq_len(3), ~ data) %>%
        dplyr::mutate(alternative = rep(c("c(-Inf, Inf)", "c(-Inf, 0)", "c(0, Inf)"),
                                        each = nrow(data)), group2 = alternative) %>%
        dplyr::mutate(cs = cs.1 - cs.2) %>%
        tidyr::gather(CS, value, -subj, -alternative, -group2, -group) %>%
        tidyr::separate(CS, c("CS", "N"),
                        convert = FALSE, remove = TRUE, fill = "right") %>%
        dplyr::group_by(group2)

      ttest_run <- ttest_prep %>%
        dplyr::group_map(
          ~ BayesFactor::ttestBF(
            x = .$value,
            y = .$group,
            rscale = rscale,
            nullInterval = eval(parse(text = .$alternative[1]))
          ) %>%
            BayesFactor::extractBF(onlybf = TRUE)
        ) %>%
        purrr::invoke(.f = "rbind") %>%
        data.frame() %>%
        dplyr::select(X1) %>%
        unlist()
    }

    # List to be pasted to broom functions
    if (!!phase %in% c("acquisition", "acq")) {
      ttl <-
        ttest_run[c(3, 1)]
      type_test = c("higher", "equal")
    }

    if (!!phase %in% c("extinction", "ext")) {
      ttl <-
        ttest_run[c(3, 1)]
      type_test = c("lower", "equal")
    }

    res <- data.frame(
      x = "cs",
      y = dv,
      exclusion = exclusion,
      cut_off = cut_off,
      model = paste("Bayesian t-test", type_test),
      controls = NA,
      method = "t-test",
      p.value = NA,
      effect.size = NA,
      estimate = ttl,
      statistic = NA,
      conf.low = NA,
      conf.high = NA,
      framework = "Bayesian"
    ) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(data_used = list(data))

    return(res)
  }
