#' chop_cs
#'
#' @description Function for separating the conditioned responses into multiple pieces
#' @param cs index for the CS responses
#' @param data Data frame containing the CS columns
#' @param na.rm Should NAs be removed? Default to FALSE.
#' @export

chop_cs <- function(cs, data, na.rm = FALSE) {
  cs <- data %>% dplyr::select(!!dplyr::enquo(cs))

  # Determine how many trials you have. Based on that, you will determine
  # how many parts you will have to make.
  nc_cs <- ncol(cs) %>% tidyselect::all_of(nc_cs)

  if((nc_cs %% 2) == 0){
    # Half of the trials
    cs_fhalf <- data %>% dplyr::select(1, nc_cs/2) %>% rowMeans()
    cs_lhalf <- data %>% dplyr::select(nc_cs/2 + 1, nc_cs) %>% rowMeans()
    cs_half  <- dplyr::bind_cols(cs_fhalf, cs_lhalf)
    #    cbind(seq(1, nc_cs, 2), seq(2, nc_cs, 2))
  } else{
    # You do the nc_cs + 1 trick because it is an odd number. Have to think
    # more of that
    cs_fhalf <- data %>% dplyr::select(1, (nc_cs + 1)/2) %>% rowMeans()
    cs_lhalf <- data %>% dplyr::select((nc_cs + 1)/2 + 1, nc_cs) %>% rowMeans()
    cs_half  <- dplyr::bind_cols(cs_fhalf, cs_lhalf)
  }

  res <- cs_half

  return(res)
}
