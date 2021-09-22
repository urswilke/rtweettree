#' Plot rtweettree
#'
#' @param x rtweet status_id or rtweettree_data object
#' @param add_profile_pics logical whether to add the profile pictures of the
#' users to the graph; defualts to TRUE
#' @param ... for the moment not used
#'
#' @return rtweettree
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
autoplot.character <- function(x, add_profile_pics = TRUE, ...) {
  l <- rtweettree_data(x)


  g <- rtweettree_tbl_graph(l)

  g1 <- g %>% ggraph::ggraph(...)

  if (add_profile_pics) {
    df_profile_pic <- get_profile_pic_df(dplyr::bind_rows(l[c("df_tls", "df_favs", "df_main_status")]))

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
autoplot.rtweettree_data <- autoplot.character
#' @export
autoplot.list <- autoplot.character

