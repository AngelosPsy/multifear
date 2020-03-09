#' combine_cs
#'
#' @description Function for computing mean responses across CSs
#' @param cs1 index for the CS1 responses
#' @param cs2 index for the CS2 responses
#' @param data Data frame containing the CS1 and CS2 columns
#' @param na.rm Should NAs be removed? Default to FALSE.
#' @importFrom dplyr %>%
#' @export

combine_cs <- function(cs1, cs2, data, na.rm = FALSE) {
  cs1 <- data %>% dplyr::select(!!dplyr::enquo(cs1))
  cs2 <- data %>% dplyr::select(!!dplyr::enquo(cs2))
  data <- data %>% as_tibble()

  data %>%
    mutate(cs1_mean = rowMeans(cs1, na.rm = na.rm),
           cs2_mean = rowMeans(cs2, na.rm = na.rm)) -> res

  return(res)
}
