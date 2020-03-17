#' multi_cs
#'
#' @description Basic function for conducting multi-verse analyses of conditioning
#' data
#' @inheritParams rm_anova_mf
#' @param na.rm Should NAs be removed? Default set the \code{FALSE}.
#' @export

multi_cs <- function(cs1, cs2, data, subj, group = NULL, phase = "acquisition", na.rm = FALSE) {

  # Prepare data for multiple analyses
  cs1  <- data %>% dplyr::select(!!dplyr::enquo(cs1)) %>% tibble::as_tibble()
  cs2  <- data %>% dplyr::select(!!dplyr::enquo(cs2)) %>% tibble::as_tibble()
  subj <- data %>% dplyr::select(!!dplyr::enquo(subj)) %>% tibble::as_tibble()

  # Renaming objects to make life a bit easier
  cs1  <- cs1 %>% dplyr::select(cs1_ = dplyr::everything())
  cs2  <- cs2 %>% dplyr::select(cs2_ = dplyr::everything())
  subj <- subj %>% dplyr::select(subj = dplyr::everything())
  data <- dplyr::bind_cols(subj, cs1, cs2)

  # Perform ANOVA
  anovaFULL <- multifear::rm_anova_mf(cs1 = colnames(cs1), cs2 = colnames(cs2), time = TRUE,
              subj = subj, data, group = group, phase = phase)

  # Perform t-test
  # First combine css
  csc <- multifear::combine_cs(cs1 = colnames(cs1), cs2 = colnames(cs2), data = data)
  ttestFULL <- multifear::t_test_mf(cs1_mean, cs2_mean, csc, paired = TRUE, phase = phase)

  res <- list(csc = csc, ttestFULL = ttestFULL)

  return(res)
}
