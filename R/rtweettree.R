scrape_rtweettree_data_from_status_id <- function(main_status_id, new_tls = TRUE) {

  df_main_status <- rtweet::lookup_statuses(main_status_id)
  df_tree <- search_tree(main_status_id)
  tree_ids <- df_tree$user_id %>% unique()
  if (new_tls) {
    df_main_status <- rtweet::lookup_statuses(main_status_id)
    main_user_name <- paste0("@", df_main_status$screen_name)
    # df_tls <- scrape_timelines(tree_ids)
    df_tls <- rtweet::search_tweets(main_user_name, n = 10000)
  } else {
    df_tls <- rtweet::get_timelines(tree_ids)
  }
  df0 <- df_main_status %>%
    dplyr::filter(.data$status_id == main_status_id) %>%
    dplyr::select(to = .data$status_id, .data$user_id) %>%
    dplyr::mutate(from = "root", type = "root")
  tweet_edges <-
    find_connections_rec(dplyr::bind_rows(df_tree, df_tls), df0)
  ids <- tweet_edges$user_id %>% unique()
  # Scrape the likes of the users scraped:
  df_favs <- scrape_favs2(ids, main_status_id)
  tweet_ids <- list(df_tls, df_favs, df_main_status) %>% dplyr::bind_rows() %>% dplyr::pull(.data$status_id) %>% unique()
  df_retweets <- tweet_ids %>% purrr::map_dfr(~rtweet::get_retweets(.x)) %>% tibble::as_tibble()
  l <- tibble::lst(df_main_status, df_tree, df_tls, df_favs, df_retweets)

  class(l) <- c("rtweettree_data", class(l))
  l
}
#' Scrape rtweettree dataframes from rtweet status_id
#'
#' @param x rtweet status_id
#' @param ... for the moment not used
#'
#' @return object of class rtweettree_data: a list containing the objects df_main_status, df_tree, df_tls, df_favs & df_retweets (see `vignette("visualize_tree", package = "rtweettree")`)
#' @export
#'
#' @examples
#' \dontrun{
#' l <- rtweettree_data("1438481824922181635")
#' l
#' }
rtweettree_data <- function(x, ...) {
  UseMethod("rtweettree_data")
}
#' @export
rtweettree_data.character <- function(x, ...) {
  scrape_rtweettree_data_from_status_id(x)
}
#' @export
rtweettree_data.list <- function(x, ...) {
  structure(
    x,
    class = c("rtweettree_data", "list")
  )
}
#' @export
rtweettree_data.rtweettree_data <- function(x, ...) {
  x
}

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
#' main_status_id <- "1289565453707173889"
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
  g <- g %>%
    # calculate the distance to the tree root with tidygraph:
    dplyr::mutate(dist_to_center = tidygraph::node_distance_from(tidygraph::node_is_source()))

  class(g) <- c("rtweettree_tbl_graph", "tbl_graph", "igraph")
  g
}

#' @export
rtweettree_tbl_graph.rtweettree_tbl_graph <- function(x, ...) {
  x
}


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










#' Plot rtweettree
#'
#' @param x rtweet status_id or rtweettree_data object
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
autoplot.list <- autoplot.character <- function(x, add_profile_pics = TRUE, ...) {
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

#' @importFrom graphics plot
plot.character <- function(x, ...) {
  print(ggplot2::autoplot(x, ...))
}
