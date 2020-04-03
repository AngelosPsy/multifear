#' inference_cs
#'
#' @description Function for summarizing the multiverse results
#' @param data a data frame with the results of a multiverse analyses
#' @param alpha_level What should be the alpha level used (default to 0.05)
#' @param na.rm Should NAs be removed? Default to FALSE
#' @return A data frame with results
#' @details For now the function returns only mean p values and proportion of p values below a criterion defined by the \code{alpha_level} parameter (default to 0.05).
#' @export

inference_cs <-
  function(data, alpha_level = 0.05, na.rm = FALSE) {
    # Check data
    inference_warning(data = data)
    mean_p_value <-
      data %>% dplyr::select(p.value) %>% unlist() %>% mean()
    prop_p_value <-
      length(which(data$p.value < alpha_level)) / nrow(data) * 100

    res <-
      data.frame(mean_p_value = mean_p_value, prop_p_value = prop_p_value)

    return(res)
  }
