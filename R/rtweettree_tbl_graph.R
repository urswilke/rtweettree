# TODO: make sure to correctly auto-link (pkgdown) the inline expressions inside "``"!
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

#' @describeIn rtweettree_tbl_graph Construct rtweettree_tbl_graph object from rtweettree_data.
rtweettree_tbl_graph.rtweettree_data <- function(x, ...) {
  suppressMessages(df <- x %>% purrr::reduce(dplyr::full_join) %>% dplyr::distinct(.data$status_id, .keep_all = TRUE))
  df_root <-
    x$df_main_status %>%
    # df_tree %>%
    # filter(status_id == main_status_id) %>%
    dplyr::select(to = .data$status_id, .data$user_id, .data$screen_name) %>%
    dplyr::mutate(from = "root", type = "root")
  tweet_edges <-
    find_connections_rec(dplyr::bind_rows(x$df_tree, x$df_tls, x$df_favs), df_root)
  user_tweet_edges <-
    tweet_edges %>%
    dplyr::bind_rows(df_root %>% dplyr::mutate(from = .data$screen_name)) %>%
    dplyr::transmute(.data$user_id,
                     .data$screen_name,
                     from = .data$to,
                     to = .data$user_id,
                     type = "by")


  fav_edges <-
    x$df_favs %>%
    dplyr::filter(.data$status_id %in% tweet_edges$to) %>%
    dplyr::transmute(from = .data$status_id, to = .data$favorited_by, user_id = .data$favorited_by, .data$screen_name) %>%
    # dplyr::add_count(.data$to, name = "n_likes") %>%
    dplyr::mutate(type = "like")


  retweet_edges <- x$df_retweets %>%
    dplyr::filter(.data$is_retweet) %>%
    dplyr::transmute(from = .data$retweet_status_id, to = .data$user_id, .data$user_id, .data$screen_name) %>%
    dplyr::mutate(type = "retweet")

  edges <-
    dplyr::bind_rows(
      tweet_edges,
      fav_edges,
      user_tweet_edges,
      retweet_edges
    ) %>%
    dplyr::filter(.data$from != "root")

  user_nodes <-
    tibble::tibble(name =
                     c(df_root$user_id,
                       user_tweet_edges$user_id) %>%
                     unique(),
                   type = "user") %>%
    dplyr::left_join(df %>% dplyr::select(name = .data$user_id, .data$screen_name) %>% dplyr::distinct()) %>%
    dplyr::mutate(url = glue::glue("https://twitter.com/{screen_name}/"))

  tweet_nodes <-
    tibble::tibble(name =
                     c(tweet_edges$to,
                       tweet_edges$from) %>%
                     unique(),
                   type = "tweet") %>%
    dplyr::left_join(df %>% dplyr::select(name = .data$status_id, .data$screen_name, .data$text) %>% dplyr::distinct()) %>%
    dplyr::mutate(url = glue::glue("https://twitter.com/fake_screen_name/status/{name}"))
  # the correct url would be:
  # dplyr::mutate(url = glue::glue("https://twitter.com/{screen_name}/status/{name}"))
  # however, this probably wouldn't be completely inline with the twitter terms of use...
  # (twitter will correct for the correct screen_name if the tweet is still available)
  nodes <-
    dplyr::full_join(user_nodes,
                     tweet_nodes) %>%
    dplyr::mutate(label = dplyr::coalesce(.data$text, .data$screen_name))

  # dirty hack to prevent error:
  #"
  # Error in (function (edges, n = max(edges), directed = TRUE)  :
  #             At type_indexededgelist.c:116 : cannot create empty graph with negative number of vertices, Invalid value
  #"
  xxx <- unique(nodes$name)
  yyy <- unique(c(edges$from, edges$to))
  ww <- dplyr::setdiff(yyy, xxx)
  oo <- edges %>% dplyr::filter(.data$from %in% ww | .data$to %in% ww)
  edges <- edges %>%
    dplyr::anti_join(oo)
  g <- tidygraph::tbl_graph(
    nodes %>%
      # TODO: remove row with root
      tidyr::drop_na(.data$screen_name),
    edges
  )
  class(g) <- c("rtweettree_tbl_graph", "tbl_graph", "igraph")
  g
}





#' @export
#' @describeIn rtweettree_tbl_graph return rtweettree_tbl_graph object as is.
rtweettree_tbl_graph.rtweettree_tbl_graph <- function(x, ...) {
  x
}

#' @export
#' @describeIn rtweettree_tbl_graph First run rtweettree_data on the status id `x` and then transform to rtweettree tbl_graph.
rtweettree_tbl_graph.character <- function(x, ...) {
  l <- rtweettree_data(x)
  g <-
    rtweettree_tbl_graph.rtweettree_data(l)
}

#' @export
#' @describeIn rtweettree_tbl_graph First run rtweettree_data on the named list `x` and then transform to rtweettree tbl_graph.
rtweettree_tbl_graph.list <- rtweettree_tbl_graph.character

#' @export
rtweettree_tbl_graph.default <- function(x, ...) {
  stop("rtweettree_tbl_graph() not defined for object of class ", class(x))
}






