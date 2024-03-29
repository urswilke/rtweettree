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
#' rtweettree_data_scraped <- rtweettree_data("1438481824922181635")
#' rtweettree_data_scraped
#' }
rtweettree_data <- function(x, ...) {
  UseMethod("rtweettree_data")
}
#' @export
#' @param new_tls logical whether to use the new method or the old one to scrape
#'   the timelines of users in the tree; defaults to TRUE.
#' @describeIn rtweettree_data Scrape the rtweettree data from a Twitter status id.
rtweettree_data.character <- function(x, new_tls = TRUE, ...) {
  if (length(x) != 1) {
    stop("Only defined for character strings of length one.")
  }

  if (suppressWarnings(is.na(as.numeric(x)))) {
    stop("The status_id has to be a character that can be transformed to a numeric.")
  }

  df_main_status <- rtweet::lookup_statuses(x)
  df_tree <- search_tree(x)
  tree_ids <- df_tree$user_id %>% unique()
  if (new_tls) {
    df_main_status <- rtweet::lookup_statuses(x)
    main_user_name <- paste0("@", df_main_status$screen_name)
    df_tls <- rtweet::search_tweets(main_user_name, n = 10000)
    if (nrow(df_tls) == 0) {
      df_tls <- create_empty_rtweet_tbl()
    }
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
  l <- list(df_main_status, df_tree, df_tls, df_favs, df_retweets) %>%
    purrr::set_names(c("main_status", "tree", "tls", "like", "retweet"))

  rtweettree_data_scraped <- l[purrr::map_lgl(l, ~ nrow(.x) > 0)] %>% dplyr::bind_rows(.id = "type") %>% tibble::as_tibble()

  class(rtweettree_data_scraped) <- c("rtweettree_data", class(rtweettree_data_scraped))
  rtweettree_data_scraped
}

#' @export
#' @describeIn rtweettree_data Return rtweettree_data object as is.
rtweettree_data.rtweettree_data <- function(x, ...) {
  x
}

