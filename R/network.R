#' Recursively return all status ids of a tweet and its replies
#'
#' @param df_tree data frame returned by `rtweet::search_tweets2()`.
#' @param df0 data frame returned by `rtweet::lookup_statuses()`.
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

# TODO: make sure to correctly auto-link (pkgdown) the inline expressions inside "``"!
#' Create a tidygraph tbl_graph object
#'
#' Create a `tidygraph::tbl_graph` object representing the tree structure of a
#' tweet and all replies, quotes and likes that could be scraped using rtweet.
#'
#' @param x rtweet status_id or rtweettree_data object
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
#' g <- create_tweet_tbl_graph(tibble::lst(df_main_status, df_tree, df_tls, df_favs, df_retweets))
#' g %>% ggraph::ggraph() + ggraph::geom_node_point() + ggraph::geom_edge_link()
#' }
create_tweet_tbl_graph <- function(x) {
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
  ww <- setdiff(yyy, xxx)
  oo <- edges %>% filter(from %in% ww | to %in% ww)
  edges <- edges %>%
    anti_join(oo)
  tidygraph::tbl_graph(
      nodes %>%
        # TODO: remove row with root
        tidyr::drop_na(.data$screen_name),
      edges
    )
  # %>%
  #   dplyr::mutate(dist_to_center = tidygraph::node_distance_from(tidygraph::node_is_source()),
  #          group = tidygraph::group_infomap()) %>%
  #   dplyr::group_by(.data$group) %>%
  #   dplyr::mutate(n_group = dplyr::n()) %>%
  #   dplyr::ungroup() %>%
  #   tibble::as_tibble() %>%
  #   tidyr::drop_na(.data$screen_name)
  # g <- tidygraph::tbl_graph(nodes, edges)
  # g
}


get_profile_pic_df <- function(df) {
  df %>%
    dplyr::distinct(
      .data$screen_name,
      .data$profile_image_url
    ) %>%
    dplyr::filter(!stringr::str_detect(profile_image_url, "default_profile_normal\\.png")) %>%
    dplyr::mutate(
      img = .data$profile_image_url %>%
        purrr::map(magick::image_read)
    )

}

#' Create ggraph object with profile pictures added
#'
#' @param g tbl_graph object
#' @param df_profile_pic tibble of profile pictures, generated by `get_profile_pic_df()`
#'
#' @return ggraph object of tweet tree with profile pictures
#' @export
#'
#' @examples
#' \dontrun{
#' g1 <- add_profile_pics_to_tree_ggraph(g, get_profile_pic_df(bind_rows(df_tls, df_main_status)))
#' g1 + ggraph::geom_edge_diagonal(aes(color= type)) + geom_node_point(shape = type))
#' }
add_profile_pics_to_tree_ggraph <- function(g, df_profile_pic) {

  g1 <- g %>% ggraph::ggraph()

  # Hack to put all user nodes on the bottom line of the graph:
  g1$data$y[g1$data$type == "user"] <- min(g1$data$y)

  user_coords <- g1$data %>% dplyr::select(.data$screen_name, .data$x, .data$y) %>% dplyr::filter(.data$y == 1)

  df_profile_pic <- df_profile_pic %>% dplyr::full_join(user_coords, by = "screen_name")
  user_coords <- df_profile_pic %>% dplyr::select(.data$screen_name, .data$x, .data$y)
  add_img <- function(g, user_images, user_coords) {
    g  + ggplot2::annotation_raster(user_images, xmin = user_coords$x[1] - 0.4, ymin = user_coords$y[1] + 0.2, xmax = user_coords$x[1] + 0.4, ymax = user_coords$y[1] + 0.6)
  }
  purrr::reduce2(
    df_profile_pic$img,
    user_coords %>% dplyr::rowwise() %>% dplyr::group_split(),
    add_img,
    .init = g1
  )
}
