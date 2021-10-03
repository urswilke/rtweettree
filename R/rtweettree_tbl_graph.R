# TODO: make sure to correctly auto-link (pkgdown) the inline expressions inside "``"!
#' Create a tidygraph tbl_graph object
#'
#' Create a `tidygraph::tbl_graph` object representing the tree structure of a
#' tweet and all replies, quotes and likes that could be scraped using rtweet.
#'
#' @param x rtweet status_id or rtweettree_data object
#' @param add_profile_pics logical whether to scrape the profile pictures of the
#'   twitter users and add them to the nodes tibble (if available); defaults to
#'   TRUE.
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
rtweettree_tbl_graph <- function(x, add_profile_pics = TRUE, ...) {
  UseMethod("rtweettree_tbl_graph")
}

#' @describeIn rtweettree_tbl_graph Construct rtweettree_tbl_graph object from rtweettree_data.
rtweettree_tbl_graph.rtweettree_data <- function(x, add_profile_pics = TRUE, ...) {
  suppressMessages(df <- x %>% purrr::reduce(dplyr::full_join) %>% dplyr::distinct(.data$status_id, .keep_all = TRUE))
  df_root <-
    x$df_main_status %>%
    dplyr::select(to = .data$status_id, .data$user_id, .data$screen_name)
  tweet_edges <-
    find_connections_rec(dplyr::bind_rows(x$df_tree, x$df_tls, x$df_favs), df_root)
  user_tweet_edges <-
    tweet_edges %>%
    dplyr::transmute(.data$user_id,
                     .data$screen_name,
                     from = .data$to,
                     to = .data$user_id,
                     type = "by")


  fav_edges <-
    x$df_favs %>%
    dplyr::filter(.data$status_id %in% tweet_edges$to) %>%
    dplyr::transmute(from = .data$status_id, to = .data$favorited_by, user_id = .data$favorited_by, .data$screen_name) %>%
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
    dplyr::filter(.data$from != "root") %>%
    dplyr::relocate(.data$from, .data$to)



  user_nodes <- tibble::tibble(
      user_id = c(df_root$user_id, user_tweet_edges$user_id) %>% unique(),
      type = "user"
    ) %>%
    dplyr::left_join(df %>% dplyr::distinct(.data$user_id, .keep_all = TRUE) %>% rtweet::users_data()) %>%
    dplyr::mutate(name = .data$user_id) %>%
    dplyr::mutate(url = glue::glue("https://twitter.com/{screen_name}/"))  %>%
    dplyr::group_by(.data$name, .data$type, .data$screen_name, .data$url) %>%
    tidyr::nest() %>%
    dplyr::ungroup()


  tweet_nodes <- tibble::tibble(
    name = c(tweet_edges$to, tweet_edges$from) %>% unique(),
    type = "tweet") %>%
    dplyr::left_join(
      df %>%
        dplyr::distinct(.data$status_id, .keep_all = TRUE) %>%
        dplyr::group_by(name = .data$status_id, text = .data$text) %>%
        tidyr::nest() %>%
        dplyr::ungroup()

    ) %>%
    dplyr::mutate(url = glue::glue("https://twitter.com/fake_screen_name/status/{name}")) %>%
    dplyr::filter(.data$name != "root")
  # the correct url would be:
  # dplyr::mutate(url = glue::glue("https://twitter.com/{screen_name}/status/{name}"))
  # however, this probably wouldn't be completely inline with the twitter terms of use...
  # (twitter will correct for the correct screen_name if the tweet is still available)
  nodes <-
    dplyr::full_join(user_nodes,
                     tweet_nodes) %>%
    dplyr::mutate(label = dplyr::coalesce(.data$text, .data$screen_name)) %>%
    dplyr::relocate(.data$name)

  if (add_profile_pics) {
    df_profile_pics <- scrape_profile_pics(nodes)
    nodes <- nodes %>% dplyr::left_join(df_profile_pics)
  }

  g <- tidygraph::tbl_graph(nodes, edges)
  class(g) <- c("rtweettree_tbl_graph", "tbl_graph", "igraph")
  g
}

scrape_profile_pics <- function(nodes_df) {
  nodes_df %>%
    dplyr::transmute(
      .data$screen_name,
      profile_image_url = purrr::map_chr(.data$data, "profile_image_url")
    ) %>%
    dplyr::distinct(.data$profile_image_url, .keep_all = TRUE) %>%
    dplyr::filter(!stringr::str_detect(.data$profile_image_url, "default_profile_normal\\.png")) %>%
    dplyr::mutate(
      profile_pic = .data$profile_image_url %>%
        purrr::map(magick::image_read)
    ) %>%
    dplyr::select(-.data$profile_image_url)
}



#' @export
#' @describeIn rtweettree_tbl_graph return rtweettree_tbl_graph object as is.
rtweettree_tbl_graph.rtweettree_tbl_graph <- function(x, add_profile_pics = TRUE, ...) {
  x
}

#' @export
#' @describeIn rtweettree_tbl_graph First run rtweettree_data on the status id `x` and then transform to rtweettree tbl_graph.
rtweettree_tbl_graph.character <- function(x, add_profile_pics = TRUE, ...) {
  l <- rtweettree_data(x, ...)
  rtweettree_tbl_graph.rtweettree_data(l, add_profile_pics, ...)
}

#' @export
rtweettree_tbl_graph.default <- function(x, add_profile_pics = TRUE, ...) {
  stop("rtweettree_tbl_graph() not defined for object of class ", class(x))
}






