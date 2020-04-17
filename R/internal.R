#' internal
#'
#' @description Internal functions, not available to the user
#' @noRd
#' @noMd
#' @keywords internal

subj_warning = function(subj_name) {
  if (!is.character(subj_name)) {
    stop("The subject variable is not a character object. Function terminated.")
  }
}

cs_warning = function(cs) {
  if (!is.character(cs)) {
    stop("The cs variable is not a character object. Function terminated.")
  }
}

data_warning = function(data) {
  if (!is.data.frame(data)) {
    stop("The data object is not a data frame. Function terminated.")
  }
}

collection_warning = function(cs1,
                              cs2 = NULL,
                              data,
                              subj = NULL) {
  cs_warning(cs1)
  if (!is.null(cs2)) {
    cs_warning(cs2)
  }
  data_warning(data)
  if (!is.null(subj)) {
    subj_warning(subj)
  }
}

chop_css_warning = function(data) {
  if (!any(class(data) == "chop_css")) {
    stop("The data object is not of class chop_css. Function terminated.")
  }
}

inference_warning = function(data){
  if (!any(class(data) == "data.frame")) {
    stop("The data object is not of class data frame. Function terminated.")
  }
}

select_term = function(obj, term, y = "y", exclusion = "full data"){
  if(exists("obj")){
    summ <- summary(obj)

    valz <- summary(obj)$tTable %>%
      data.frame() %>%
      dplyr::mutate(model = rownames(.)) %>%
      dplyr::filter(model %in% !!dplyr::enquo(term)) %>%
      dplyr::select(Value, t.value, p.value)

    res <- tibble::tibble(
      x = term,
      y = y,
      exclusion = exclusion,
      model = "mixed_model",
      controls = NA,
      method = paste("mixed_model", x),
      p.value = valz$p.value,
      effect.size = valz$Value,
      estimate = valz$Value,
      statistic = valz$t.value,
      conf.low = NA,
      conf.high = NA
    )
  } else{
    tibble::tibble(
      x = term,
      y = y,
      exclusion = exclusion,
      model = "mixed_model",
      controls = NA,
      method = paste("mixed_model", x),
      p.value = NA,
      effect.size = NA,
      estimate = NA,
      statistic = NA,
      conf.low = NA,
      conf.high = NA
    )
  }
  res <- res %>%
    dplyr::mutate(data_used = list(summ$data))

  return(res)
}
