#' Create a tidygraph tbl_graph object
#'
#' Create a `tidygraph::tbl_graph` object representing the tree structure of a
#' tweet and all replies, quotes and likes that could be scraped using rtweet.
#'
#' @param x rtweet status_id or rtweettree_data object
#' @param ... for the moment not used
#'
#' @return A tidygraph tbl_graph object representing the tree structure of all scraped subtweets of the tweet.
#' @export
#'
#' @examples
#'\dontrun{
#' main_status_id <- "1438481824922181635"
#' df_main_status <- rtweet::lookup_statuses(main_status_id)
#' df_tree <- search_tree(main_status_id)
#' tree_ids <- df_tree$user_id %>% unique()
#' df_tls <- scrape_timelines(tree_ids)
#' df0 <- df_main_status %>%
#'   dplyr::filter(status_id == main_status_id) %>%
#'   dplyr::select(to = status_id, user_id) %>%
#'   dplyr::mutate(from = "root", type = "root")
#' tweet_edges <-
#'   find_connections_rec(dplyr::bind_rows(df_tree, df_tls), df0)
#' ids <- tweet_edges$user_id %>% unique()
#' df_favs <- scrape_favs2(ids, main_status_id)
#' tweet_ids <- list(df_tls, df_favs, df_main_status) %>%
#'   dplyr::bind_rows() %>%
#'   pull(status_id) %>%
#'   unique()
#' df_retweets <- tweet_ids %>% purrr::map_dfr(~rtweet::get_retweets(.x))
#'
#' l <- tibble::lst(df_main_status, df_tree, df_tls, df_favs, df_retweets)
#' g <- rtweettree_tbl_graph(l)
#' g %>% ggraph::ggraph() + ggraph::geom_node_point() + ggraph::geom_edge_link()
#' }
rtweettree_tbl_graph <- function(x, ...) {
  UseMethod("rtweettree_tbl_graph")
}

#' @export
rtweettree_tbl_graph.rtweettree_data <- function(x, ...) {
  g <-
    create_tweet_tbl_graph(x)

  class(g) <- c("rtweettree_tbl_graph", "tbl_graph", "igraph")
  g
}

#' @export
rtweettree_tbl_graph.rtweettree_tbl_graph <- function(x, ...) {
  x
}


# TODO: add example in docs how to add dist_to_center to tbl_graph object:
# g <- g %>%
# # calculate the distance to the tree root with tidygraph:
#   dplyr::mutate(dist_to_center = tidygraph::node_distance_from(tidygraph::node_is_source()))
#' @export
rtweettree_tbl_graph.list <- rtweettree_tbl_graph.character <- function(x, ...) {
  l <- rtweettree_data(x)
  g <-
    rtweettree_tbl_graph.rtweettree_data(l)
}
#' @export
rtweettree_tbl_graph.default <- function(x, ...) {
  stop("rtweettree_tbl_graph() not defined for object of class ", class(x))
}






