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
find_connections_rec <- function(df_tree, df0 = rtweet::lookup_statuses(main_status_id)) {
  df_quote1 <-
    df_tree %>%
    dplyr::filter(quoted_status_id %in% df0$to) %>%
    dplyr::select(to = status_id, from = quoted_status_id, user_id, screen_name) %>%
    dplyr::mutate(type = "quote")
  df_reply1 <-
    df_tree %>%
    dplyr::filter(reply_to_status_id %in% df0$to) %>%
    dplyr::select(to = status_id, from = reply_to_status_id, user_id, screen_name) %>%
    dplyr::mutate(type = "reply")
  res <- list(df0, df_reply1, df_quote1) %>%
    purrr::reduce(dplyr::full_join) %>%
    dplyr::distinct()
  if (nrow(res) == nrow(df0)) {
    return(res %>%
             dplyr::filter(from != "root"))
  } else {
    find_connections_rec(df_tree, res)
  }
}
