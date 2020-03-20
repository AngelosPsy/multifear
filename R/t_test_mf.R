#' t_test_mf
#'
#' @description Basic function for running a t-test
#' @param cs1 cs 1
#' @param cs2 cs 2
#' @param data a data frame containing the dv and iv
#' @param paired whether the t-test refers to a paired or independent t-test.
#' @param phase Different tests will be run for different phases. That is why
#' the phase needs to be specified here. Possible values are \code{acquisition},
#' or \code{acq}, \code{extnction}, or \code{extinction}. See Details for more
#' information.
#' @return a basic function for running a t-test within the \code{multifear}
#' package
#' @importFrom dplyr %>%
#' @export

t_test_mf <-
  function(cs1,
           cs2,
           data,
           paired = TRUE,
           phase = "acquisition") {
    cs1 <- data %>% dplyr::select(!!dplyr::enquo(cs1)) %>% unlist()
    cs2 <- data %>% dplyr::select(!!dplyr::enquo(cs2)) %>% unlist()

    # Here we run all t.tests and we select later on which one we wants. It is
    # a bit too much to run all tests but we save all the if else statements
    tte <-
      stats::t.test(cs1, cs2, paired = paired, alternative = "two.sided")
    ttg <-
      stats::t.test(cs1, cs2, paired = paired, alternative = "greater")
    ttg <-
      stats::t.test(cs1, cs2, paired = paired, alternative = "less")

    # List to be pasted to broom functions
    if (!!phase %in% c("acquisition", "acq")) {
      ttl <- list(tte, ttg)
    }

    if (!!phase %in% c("extinction", "ext")) {
      ttl <- list(tte, ttg)
    }

    res <- purrr::map_df(ttl, .f = broom::tidy) %>%
      dplyr::mutate(
        method = paste("t-test"),
        x = "cs",
        y = "cr",
        model = "t-test",
        controls = NA
      ) %>%
      dplyr::select(x,
                    y,
                    model,
                    controls,
                    method,
                    estimate,
                    statistic,
                    conf.low,
                    conf.high)
    return(res)
  }
