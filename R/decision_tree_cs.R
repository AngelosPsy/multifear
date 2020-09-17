#' decision_tree_cs
#'
#' @description Plot decision tree
#'
#' \lifecycle{experimental}
#'
#' @param dat a dat frame with the results of a multiverse analyses.
#'
#' @return A plot
#' @details This plot is a slight modification of the \code{specr::plot_decisiontree}. Please check the references.
#'
#' @references
#' Masur, Philipp K. & Scharkow, M. (2019). specr: Statistical functions for conducting specification curve analyses. Available from https://github.com/masurp/specr.

#' @importFrom dplyr case_when
#' @export

decision_tree_cs  = function (dat) {

  # Create data set for graph transformation
  dat <- dat %>%
    dplyr::select(.data$model, .data$x, .data$y, .data$cut_off, .data$exclusion) %>%
    dplyr::arrange(.data$model, .data$x, .data$y, .data$cut_off, .data$exclusion) %>%
    dplyr::mutate(start = "raw data") %>%
    dplyr::select(start, dplyr::everything()) %>%
    dplyr::mutate(x = paste0(.data$x, " & ", .data$model),
                  y = paste0(.data$y, " & ", .data$x),
                  cut_off = paste0(.data$cut_off, " & ", .data$y),
                  exclusion = paste0(.data$exclusion, " & ", .data$cut_off))

  # Create edges
  edges_level1 <- dat %>%
    dplyr::select(.data$start, .data$model) %>%
    dplyr::rename(from = .data$start, to = .data$model) %>%
    unique %>%
    dplyr::mutate(decisions = "model")
  edges_level2 <- dat %>%
    dplyr::select(.data$model, .data$x) %>%
    dplyr::rename(from = .data$model, to = .data$x) %>%
    unique %>%
    dplyr::mutate(decisions = "independent variable")
  edges_level3 <- dat %>%
    dplyr::select(.data$x, .data$y) %>%
    dplyr::rename(from = .data$x, to = .data$y) %>%
    unique %>%
    dplyr::mutate(decisions = "dependent variable")
  edges_level4 <- dat %>%
    dplyr::select(.data$y, .data$cut_off) %>%
    dplyr::rename(from = .data$y, to = .data$cut_off) %>%
    dplyr::mutate(decisions = "cut_off variables")
  edges_level5 <- dat %>%
    dplyr::select(.data$cut_off, .data$exclusion) %>%
    dplyr::rename(from = .data$cut_off, to = .data$exclusion) %>%
    dplyr::mutate(decisions = "exclusion")

  # Combine edges
  edge_list <- rbind(edges_level1,
                     edges_level2,
                     edges_level3,
                     edges_level4,
                     edges_level5)

  # Plot edges
  plot <- edge_list %>%
    igraph::graph_from_data_frame() %>%
    ggraph::ggraph(layout = "dendrogram",
                   circular = FALSE, repel = TRUE) +
    ggraph::geom_edge_diagonal() +
    ggplot2::theme_void() +
    ggplot2::theme(text = ggplot2::element_text(size=2))

    plot <- plot +
      ggraph::geom_edge_diagonal(ggplot2::aes(color = .data$decisions)) +
      ggraph::scale_edge_color_brewer(type='div', palette=2)

    plot <- plot +
      ggraph::geom_node_text(ggplot2::aes(label = .data$name,
                                 filter = .data$leaf),
                             angle=90,
                             hjust=1,
                             nudge_y = -0.10) +
      ggplot2::ylim(-8, 5)

  return(plot)

}
