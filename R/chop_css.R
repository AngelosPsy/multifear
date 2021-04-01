#' chop_css
#'
#'
#' @description Function for separating the conditioned responses into multiple pieces for two CSs.
#' @inheritParams t_test_mf
#' @param cs_paired A character vector with the trials that were paired. Default is set to \code{NULL}, suggesting that there was full reinforcement
#'  @return A tibble with the following column names -- separately forr cs1 and cs2:
#' "id" = id column;
#' "top10per" = mean of top 10% of trials;
#' "bottom10per" = mean of bottom 10% of the trials;
#' "minfirst = all trials minus the first one;
#' "all" = mean of all trials;
#' "t33per = mean of top 33% of the trials;
#' "m33per" = mean of medium 33% of the trials;
#' "b33per" = mean of bottom 33% of the trials;
#' "fhalf" = mean of first half of the trials;
#' "lhalf" = mean of last half of the trials;
#' "ftrial" = first trial;
#' "ltrial" = last trial;
#' "t20per" = mean of top 20% of the trials;
#' "b20per" = mean of bottom 20% of the trials;
#' "f2trial" = mean of first two trials;
#' "l2trial" = mean of last two trials;
#' "per2_X" = mean per two trials (i.e., block), with X referring to the number
#' of block.
#' @export

chop_css <-
  function(cs1,
           cs2,
           data,
           subj,
           cs_paired = NULL,
           group = NULL,
           na.rm = FALSE) {
    # Check data
    collection_warning(
      cs1 = cs1,
      cs2 = cs2,
      data = data,
      subj = subj,
      cs_paired = cs_paired
    )

    # Stop in case of no equal lengths of cs1 and cs2
    if (length(cs1) != length(cs2)) {
      stop("No equal length of the two CSs.")
    }

    if (is.null(group)) {
      group_new <-
        data %>%
        dplyr::mutate(group = rep("NULL", nrow(data))) %>%
        dplyr::select(group)
      group <- NULL
    } else{
      group_new <- data %>%
        dplyr::select(all_of(!!dplyr::enquo(group))) %>%
        tibble::as_tibble() %>%
        dplyr::rename(group = eval(group))
    }

    cs1_tmp <-
      multifear::chop_cs(
        cs = cs1,
        data = data,
        subj = subj,
        prefix = "cs1"
      )

    cs2_tmp <-
      multifear::chop_cs(
        cs = cs2,
        data = data,
        subj = subj,
        prefix = "cs2"
      )

    if (!is.null(cs_paired)) {
      cs2_paired <-
        multifear::chop_cs(
          cs = cs2[1:length(cs_paired)],
          data = data,
          subj = subj,
          prefix = "cs2_p"
        )
      cs_paired <-
        multifear::chop_cs(
          cs = cs_paired,
          data = data,
          subj = subj,
          prefix = "cs1_p"
        )
    }

    # Compute differential scores. Maybe delete?
    csbind <-
      dplyr::bind_rows(
        cs2_tmp %>% tibble::rownames_to_column(),
        cs1_tmp %>% tibble::rownames_to_column()
      ) %>%
      dplyr::select(-id) %>% # This is because we have changed the id colname
      dplyr::group_by(rowname) %>%
      dplyr::summarise_all(diff) %>%
      dplyr::select(-rowname)

    csbind <- cbind(csbind, cs1_tmp, cs2_tmp)

    if (!is.null(cs_paired)) {
      # Here we need to have the pairing of cs_paired with the cs2. The point
      # is that we do not have enough cs2, so we are going to select the cs2
      # based on the number of trials we have for the cs_paired

      csbind_2 <-
        dplyr::bind_rows(
          cs2_paired %>% tibble::rownames_to_column(),
          cs_paired %>% tibble::rownames_to_column()
        ) %>%
        dplyr::select(-id) %>%
        dplyr::group_by(rowname) %>%
        dplyr::summarise_all(diff) %>%
        dplyr::select(-rowname)

      colnames(csbind_2) <- paste0(colnames(csbind_2), "_p")

      csbind <- csbind_2
    }

    res <- cbind(cs1_tmp, cs2_tmp[, -1], group_new)

    class(res) <- c("chop_css", class(res))

    return(res)
  }
