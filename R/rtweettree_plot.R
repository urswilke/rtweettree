# https://stackoverflow.com/a/49894698 :

#' Plot rtweettree
#'
#' Plot a tree graph of the data resulting of the `status_id` of a tweet.
#'
#'
#' The following functions are imported and then re-exported
#' from the ggplot2 package to avoid loading them.
#'
#' rtweettree exported operators and S3 methods
#'
#' @param x rtweet status_id (character string), rtweettree_data or rtweettree_tbl_graph object
#' @param add_profile_pics logical whether to add the profile pictures of the
#' users to the graph; defaults to TRUE; (should be set to FALSE for large graphs)
#' @param ... arguments passed to methods
#'
#' @return rtweettree graph
#' @export
#'
#' @examples
#' main_status_id <- "1438481824922181635"
#' \dontrun{
#' l <- rtweettree_data(main_status_id)
#' autoplot(l)
#' }
#' # when you're not interested to store the scraped data in an R object,
#' # you can also directly plot it with:
#' # autoplot(main_status_id)
#'
#' @importFrom ggplot2 autoplot
#'
#' @name autoplot
#' @export
NULL

#' @export
#' @describeIn autoplot The `status_id` charcter string is transformed to a rtweettree_tbl_graph which is then plotted with ggraph.
autoplot.character <- function(x, add_profile_pics = TRUE, ...) {
  g <- rtweettree_tbl_graph(x, add_profile_pics, ...)

  g1 <- g %>% ggraph::ggraph(...)

  if (add_profile_pics) {
    df_profile_pic <- g %>%
      dplyr::as_tibble() %>%
      dplyr::filter(purrr::map_lgl(.data$profile_pic, ~!is.null(.x))) %>%
      dplyr::select(.data$screen_name, .data$profile_pic)

    g1 <- add_profile_pics_to_tree_ggraph(
      g1,
      df_profile_pic
    )
  }

  g1 +
    ggraph::geom_edge_diagonal(ggplot2::aes(color = .data$type)) +
    ggraph::scale_edge_colour_hue(name = "action") +
    ggraph::geom_node_point(ggplot2::aes(shape = .data$type)) +
    ggplot2::scale_color_viridis_c(direction = -1) +
    ggplot2::theme_void()

}
#' @export
#' @describeIn autoplot The rtweettree_data object is transformed to a rtweettree_tbl_graph which is then plotted with ggraph.
autoplot.rtweettree_data <- autoplot.character
#' @export
#' @export
#' @describeIn autoplot The rtweettree_tbl_graph is plotted with ggraph.
autoplot.rtweettree_tbl_graph <- autoplot.character


