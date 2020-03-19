#' multi_cs
#'
#' @description Basic function for conducting multi-verse analyses of conditioning
#' data
#' @inheritParams rm_anova_mf
#' @param na.rm Should NAs be removed? Default set the \code{FALSE}
#' @param print_output Whether to print the output or not. Default set to \code{TRUE}
#' @export

multi_cs <-
  function(cs1,
           cs2,
           data,
           subj,
           group = NULL,
           phase = "acquisition",
           na.rm = FALSE,
           print_output = TRUE) {
    # Prepare data for multiple analyses
    cs1  <-
      data %>% dplyr::select(all_of(!!dplyr::enquo(cs1))) %>% tibble::as_tibble()
    cs2  <-
      data %>% dplyr::select(all_of(!!dplyr::enquo(cs2))) %>% tibble::as_tibble()
    subj <-
      data %>% dplyr::select(all_of(!!dplyr::enquo(subj))) %>% tibble::as_tibble()

    # Renaming objects to make life a bit easier
    cs1  <- cs1 %>% dplyr::select(cs1_ = dplyr::everything())
    cs2  <- cs2 %>% dplyr::select(cs2_ = dplyr::everything())
    subj <- subj %>% dplyr::select(subj = dplyr::everything())

    # What happens in case of groups
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

    # Perform ANOVA
    if (is.null(group)) {
      anovaNOTIME <-
        multifear::rm_anova_mf(
          cs1 = colnames(cs1),
          cs2 = colnames(cs2),
          time = TRUE,
          subj = colnames(subj),
          data = data,
          group = NULL,
          phase = phase
        )
      anovaTIME<-
        multifear::rm_anova_mf(
          cs1 = colnames(cs1),
          cs2 = colnames(cs2),
          time = FALSE,
          subj = colnames(subj),
          data = data,
          group = NULL,
          phase = phase
        )
    }
    else{
      anovaNOTIME <-
        multifear::rm_anova_mf(
          cs1 = colnames(cs1),
          cs2 = colnames(cs2),
          time = FALSE,
          subj = colnames(subj),
          data = data,
          group = group,
          phase = phase
        )
      anovaTIME <-
        multifear::rm_anova_mf(
          cs1 = colnames(cs1),
          cs2 = colnames(cs2),
          time = TRUE,
          subj = colnames(subj),
          data = data,
          group = group,
          phase = phase
        )
    }

    # Perform t-test
    # First combine css
    csc <-
      multifear::combine_cs(cs1 = colnames(cs1),
                            cs2 = colnames(cs2),
                            data = data)
    ttestFULL <-
      multifear::t_test_mf(cs1_mean, cs2_mean, csc, paired = TRUE, phase = phase)

    res <- list(
      `Collapsed data` = csc,
      `t-test full` = ttestFULL,
      `repeated measures ANOVA with time` = anovaTIME,
      `repeated measures ANOVA without time` = anovaNOTIME
    )

    if (print_output) {
      return(res)
    } else{
      invisible(res)
    }

  }
