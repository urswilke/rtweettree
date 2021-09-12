#' Recursively return all \code{status_id}s of a tweet and its replies
#'
#' @param df_tree data frame returned by rtweet::search_tweets2(main_status_id)
#' @param df0 data frame returned by rtweet::lookup_statuses(main_status_id)
#'
#' @return Edges data frame that can be plotted by tidygraph & ggraph
#' @export
#'
#' @examples
#' main_status_id <- "1234620900386975744"
#'\dontrun{
#' df_main_status <- rtweet::lookup_statuses(main_status_id)
#' df_tree <- search_tree(main_status_id)
#' tree_ids <- df_tree$user_id %>% unique()
#' df_tls <- scrape_timelines(tree_ids)
#'df0 <- df_main_status %>%
#'                        dplyr::filter(status_id == main_status_id) %>%
#'                        dplyr::select(to = status_id, user_id) %>%
#'                        dplyr::mutate(from = "root", type = "root")
#' tweet_edges <-
#' find_connections_rec(dplyr::bind_rows(df_tree, df_tls), df0)
#' }
find_connections_rec <- function(df_tree, df0) {
  df_quote1 <-
    df_tree %>%
    dplyr::filter(.data$quoted_status_id %in% df0$to) %>%
    dplyr::select(to = .data$status_id, from = .data$quoted_status_id, .data$user_id, .data$screen_name) %>%
    dplyr::mutate(type = "quote")
  df_reply1 <-
    df_tree %>%
    dplyr::filter(.data$reply_to_status_id %in% df0$to) %>%
    dplyr::select(to = .data$status_id, from = .data$reply_to_status_id, .data$user_id, .data$screen_name) %>%
    dplyr::mutate(type = "reply")
  res <- list(df0, df_reply1, df_quote1) %>%
    purrr::reduce(dplyr::full_join) %>%
    dplyr::distinct()
  if (nrow(res) == nrow(df0)) {
    return(res# %>%
           # dplyr::filter(.data$from != "root")
           )
  } else {
    find_connections_rec(df_tree, res)
  }
}


#' Create a tidygraph tbl_graph object representing the tree structure of a
#' tweet and all replies, quotes and likes that could be scraped using rtweet.
#'
#' @param df_main_status Data frame resulting of rtweet::lookup_statuses(main_status_id)
#' @param df_tree Data frame resulting of search_tree(main_status_id)
#' @param df_tls Data frame resulting of scrape_timelines(unique(df_tree$user_id))
#' @param df_favs Data frame resulting of find_connections_rec() of the involved user ids (see example)
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
#' }

create_tweet_tbl_graph <- function(df_main_status, df_tree, df_tls, df_favs) {
  df <- list(df_main_status, df_tree, df_tls, df_favs) %>% dplyr::bind_rows() %>% dplyr::distinct(.data$status_id, .keep_all = TRUE)
  df_root <-
    df_main_status %>%
    # df_tree %>%
    # filter(status_id == main_status_id) %>%
    dplyr::select(to = .data$status_id, .data$user_id, .data$screen_name) %>%
    dplyr::mutate(from = "root", type = "root")
  tweet_edges <-
    find_connections_rec(dplyr::bind_rows(df_tree, df_tls, df_favs), df_root)
  user_tweet_edges <-
    tweet_edges %>%
    dplyr::bind_rows(df_root %>% dplyr::mutate(from = .data$screen_name)) %>%
    dplyr::transmute(.data$user_id,
                     .data$screen_name,
              from = .data$to,
              to = .data$user_id,
              type = "by")


  fav_edges <-
    df_favs %>%
    dplyr::filter(.data$status_id %in% tweet_edges$to) %>%
    dplyr::transmute(from = .data$status_id, to = .data$favorited_by, user_id = .data$favorited_by, .data$screen_name) %>%
    dplyr::add_count(.data$to, name = "n_likes") %>%
    dplyr::mutate(type = "like")


  edges <-
    list(
      tweet_edges,
      fav_edges,
      user_tweet_edges
    ) %>%
    purrr::reduce(dplyr::full_join) %>%
    dplyr::mutate(from_chr = .data$from, to_chr = .data$to) %>%
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
  ww <- setdiff(yyy, xxx)
  oo <- edges %>% filter(from %in% ww | to %in% ww)
  edges <- edges %>%
    anti_join(oo)
  nodes <-
    tidygraph::tbl_graph(
      nodes,
      edges
    ) %>%
    dplyr::mutate(dist_to_center = tidygraph::node_distance_from(tidygraph::node_is_source()),
           group = tidygraph::group_infomap()) %>%
    dplyr::group_by(.data$group) %>%
    dplyr::mutate(n_group = dplyr::n()) %>%
    dplyr::ungroup() %>%
    tibble::as_tibble() %>%
    tidyr::drop_na(.data$screen_name)
  g <- tidygraph::tbl_graph(nodes, edges)
  g
}
