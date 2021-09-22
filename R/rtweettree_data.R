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
rtweettree_data.character <- function(x, new_tls = TRUE, ...) {

  df_main_status <- rtweet::lookup_statuses(x)
  df_tree <- search_tree(x)
  tree_ids <- df_tree$user_id %>% unique()
  if (new_tls) {
    df_main_status <- rtweet::lookup_statuses(x)
    main_user_name <- paste0("@", df_main_status$screen_name)
    # df_tls <- scrape_timelines(tree_ids)
    df_tls <- rtweet::search_tweets(main_user_name, n = 10000)
  } else {
    df_tls <- rtweet::get_timelines(tree_ids)
  }
  df0 <- df_main_status %>%
    dplyr::filter(.data$status_id == x) %>%
    dplyr::select(to = .data$status_id, .data$user_id) %>%
    dplyr::mutate(from = "root", type = "root")
  tweet_edges <-
    find_connections_rec(dplyr::bind_rows(df_tree, df_tls), df0)
  ids <- tweet_edges$user_id %>% unique()
  # Scrape the likes of the users scraped:
  df_favs <- scrape_favs2(ids, x)
  tweet_ids <- list(df_tls, df_favs, df_main_status) %>% dplyr::bind_rows() %>% dplyr::pull(.data$status_id) %>% unique()
  df_retweets <- tweet_ids %>% purrr::map_dfr(~rtweet::get_retweets(.x)) %>% tibble::as_tibble()
  l <- tibble::lst(df_main_status, df_tree, df_tls, df_favs, df_retweets)

  class(l) <- c("rtweettree_data", class(l))
  l
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

