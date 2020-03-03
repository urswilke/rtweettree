#' Scrape thread
#'
#' @param main_status_id string of twitter status_id
#' @param save_res logical if file should be saved
#' @param n maximum number of tweets to scrape
#' @param df_main_status data frame returned by rtweet::lookup_statuses(main_status_id)
#'
#' @return data frame like rtweet::search_tweets2, but all subtweets of the
#'   thread added (if available)
#' @export
#'
#' @examples
#' main_status_id <- "1234620900386975744"
#' df_main_status <- rtweet::lookup_statuses(main_status_id)
#' df_thread <- search_thread(main_status_id)
search_thread <- function(main_status_id,
                          df_main_status = rtweet::lookup_statuses(main_status_id),
                          save_res = TRUE,
                          n = 1e6) {

  tweet_text <- paste0("\"", df_main_status$text, "\"")
  df_search_tweet <-
    rtweet::search_tweets2(q = c(main_status_id,
                                 tweet_text),
                           n = n,
                           retryonratelimit = TRUE) %>%
    dplyr::distinct(status_id, .keep_all = TRUE)
  new_ids <-
    setdiff(df_search_tweet$status_id, df_main_status$status_id)
  df_replies <- rtweet::search_tweets2(new_ids,
                                       n = n,
                                       retryonratelimit = T)
  result <- add_thread_level(df_search_tweet, df_replies, n)
  # if (save_res == TRUE) {
  #   save_name <- paste0(df_main_status$screen_name,
  #                       "_",
  #                       str_sub(df_main_status$text, end = 15),
  #                       "_thread.rds")
  #   saveRDS(result, save_name)
  #
  # }
  result
}


#' Title
#'
#' @param df0 data frmae
#' @param df1 data frmae
#' @param n  maximum number of tweets to scrape
#'
#' @return data frame like rtweet::search_tweets2, but laso all direct answers
#'   to the tweet (one level lower; if available).

add_thread_level <- function(df0, df1, n) {
  new_ids <-
    setdiff(df1$status_id, df0$status_id)

  df1 <- rtweet::search_tweets2(new_ids,
                                n = n,
                                retryonratelimit = T)
  res <-
    dplyr::bind_rows(df0, df1) %>%
    dplyr::distinct(status_id, .keep_all = T)

  if (length(new_ids) > 0) {
    new_ids <- setdiff(df1$status_id, df0$status_id)

    add_thread_level(df1, res)
  } else {
    return(res)
  }
}
