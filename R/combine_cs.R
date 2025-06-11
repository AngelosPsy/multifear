#' combine_cs
#'
#' @description Function for computing mean responses across CSs
#' @inheritParams t_test_mf
#' @importFrom dplyr %>%
#' @return A tibble with the initial data frame (given by the \code{data} argument)
#' together with an additional column with the means for the columns defined
#' in the cs1 and cs2 arguments.
#' @export
combine_cs <- function(cs1, cs2, data, na.rm = FALSE) {
  # Check data
  collection_warning(cs1 = cs1, cs2 = cs2, data = data, subj = NULL)

  cs1 <- data %>% dplyr::select(all_of(cs1))
  cs2 <- data %>% dplyr::select(all_of(cs2))
  data <- data %>% tibble::as_tibble()

  data %>%
    dplyr::mutate(cs1_mean = rowMeans(cs1, na.rm = na.rm),
                  cs2_mean = rowMeans(cs2, na.rm = na.rm)) -> res

  return(res)
}
