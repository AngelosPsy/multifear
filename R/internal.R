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

data_preparation_anova = function(cs1,
                            cs2,
                            data,
                            subj,
                            time = TRUE,
                            group = NULL){
  # Check data
  collection_warning(cs1 = cs1, cs2 = cs2, data = data, subj = subj)

  cs1 <-
    data %>% dplyr::select(all_of(!!dplyr::enquo(cs1))) %>% tibble::as_tibble()
  cs2  <-
    data %>% dplyr::select(all_of(!!dplyr::enquo(cs2))) %>% tibble::as_tibble()
  subj <-
    data %>% dplyr::select(all_of(!!dplyr::enquo(subj))) %>% tibble::as_tibble()

  # Renaming objects to make life a bit easier
  cs1  <- cs1 %>% dplyr::select(cs1_ = dplyr::everything())
  cs2  <- cs2 %>% dplyr::select(cs2_ = dplyr::everything())
  subj <- subj %>% dplyr::select(subj = dplyr::everything())

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

  # In case time is selected, create a time object
  if (time) {
    # Check if length of cs1 and cs2 is the same. Otherwise stop
    if (ncol(cs1) != ncol(cs2)) {
      stop(
        "You have selected that you want to analyse time effects but the
        cs1 and cs2 have different number of time points. Stopping function
        now."
      )
    }
    }
  data %>%
    reshape2::melt(
      id.var = c("subj", "group"),
      variable.name = "var_old",
      value.name = "resp",
      factorsAsStrings = TRUE
    ) %>% # Until pivot_longer gets better
    dplyr::mutate(
      cs = as.factor(stringr::str_sub(var_old, 1, 3)),
      time = as.factor(sub(".*_", "", .$var_old)),
      subj = as.factor(subj),
      group = as.factor(group)
    ) -> data # Better than stringr

  res <- data

  return(res)

}


data_preparation_ttest = function(cs1,
                                  cs2,
                                  data,
                                  subj,
                                  na.rm = FALSE){
  collection_warning(cs1 = cs1, cs2 = cs2, data = data, subj = subj)

  # Restructure data. rowMeans is used in case multiple trails have been fed
  cs1 <-
    data %>% dplyr::select(all_of(!!dplyr::enquo(cs1))) %>%
    rowMeans(na.rm = na.rm) %>% tibble::enframe(name = NULL)  %>%
    dplyr::rename(cs.1 = value)
  cs2 <-
    data %>% dplyr::select(all_of(!!dplyr::enquo(cs2))) %>%
    rowMeans(na.rm = na.rm) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(cs.2 = value)
  subj <-
    data %>% dplyr::select(all_of(!!dplyr::enquo(subj))) %>%
    tibble::as_tibble() %>%
    dplyr::select(subj = dplyr::everything())

  res <- dplyr::bind_cols(subj, cs1, cs2)

  return(res)
}

