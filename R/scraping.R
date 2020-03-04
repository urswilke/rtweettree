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


#' Scrape tweet and all tweets returned by rtweet::search_tweets
#'
#' @param df0 data frame
#' @param df1 data frame
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





#' Scrape the timelines of a thread scraped by \code{search_thread}
#'
#' @param thread_ids \code{user_id}s of a thread scraped by \code{search_thread}
#' @param save_res logical if file should be saved
#'
#' @return Dataframe of all timelines of all \code{thread_ids}
#' @export
#'
#' @examples
#' main_status_id <- "1234620900386975744"
#' df_main_status <- rtweet::lookup_statuses(main_status_id)
#' df_thread <- search_thread(main_status_id)
#' thread_ids <- df_thread$user_id %>% unique()
#' df_tls <- scrape_timelines(thread_ids)

scrape_timelines <- function(thread_ids, save_res = TRUE) {
  safe_tl <- possibly(rtweet::get_timelines, otherwise = NULL)
  l <- vector("list", length(thread_ids))
  for (i in 1:length(l)) {
    rl <- rtweet::rate_limit("get_timeline")
    if (rl[["remaining"]] <= 2) {
      print(paste0("Rate limit reached. Resuming at ", rl[["reset_at"]]))
      Sys.sleep(as.numeric(rl[["reset"]], "secs") + 1)
    }
    l[[i]] <- safe_tl(ids[[i]], n = 3200, since_id = main_status_id)
    print(paste0("Index: ", i,
                 "; Scraped ", nrow(l[[i]]),
                 " tweets. Remaining: ", rl[["remaining"]]))
  }
  df_favs <- l %>% bind_rows()
  # if (save_res == TRUE) {
  #   save_name <- paste0(df_main_status$screen_name,
  #                       "_",
  #                       str_sub(df_main_status$text, end = 15),
  #                       "_favs.rds")
  #   saveRDS(df_favs, save_name)
  # }
  df_favs

  # load_slowly <- function(thread_ids, index) {
  #   rl <- rtweet::rate_limit("get_timeline")
  #   if (rl[["remaining"]] <= 2) {
  #     print(paste0("Rate limit reached. Resuming at ", rl[["reset_at"]]))
  #     Sys.sleep(as.numeric(rl[["reset"]], "secs") + 1)
  #   }
  #
  #   df <- safe_tl(thread_ids,
  #                 since_id = main_status_id,
  #                 n = 3200)
  #   print(paste0("Index: ", index,
  #                "Scraped ", nrow(df),
  #                " tweets. Remaining: ", rl[["remaining"]]))
  #
  #   print(df)
  #   df
  # }
  # spliced_list <- seq(0, length(thread_ids), 179) %>% map(~.x + 1:179)
  #
  # l_tls <- spliced_list %>% map(~thread_ids[.x] %>% na.omit(c(1,NA)) %>% as.character()) %>% imap(~load_slowly(.x, .y))
  #
  # # if (save_res == TRUE) {
  # #   save_name <- paste0(df_main_status$screen_name,
  # #                       "_",
  # #                       str_sub(df_main_status$text, end = 15),
  # #                       "_tls.rds")
  # #   saveRDS(l_tls, save_name)
  # # }
  # l_tls %>% bind_rows()
}



#' Recursively return all \code{status_id}s of a tweet and its replies
#'
#' @param df_thread data frame returned by rtweet::search_tweets2(main_status_id)
#' @param df_main_status data frame returned by rtweet::lookup_statuses(main_status_id)
#'
#' @return Edges data frame that can be plotted by tidygraph & ggraph
#' @export
#'
#' @examples
#' main_status_id <- "1234620900386975744"
#' df_main_status <- rtweet::lookup_statuses(main_status_id)
#' df_thread <- search_thread(main_status_id)
#' thread_ids <- df_thread$user_id %>% unique()
#' df_tls <- scrape_timelines(thread_ids)
#'df0 <- df_main_status %>%
#'                        dplyr::filter(status_id == main_status_id) %>%
#'                        dplyr::select(to = status_id, user_id) %>%
#'                        dplyr::mutate(from = "root", type = "root")
#' tweet_edges <-
#' find_connections_rec(dplyr::bind_rows(df_thread, df_tls), df0)
find_connections_rec <- function(df_thread, df0 = df_main_status) {
  df_quote1 <-
    df_thread %>%
    filter(quoted_status_id %in% df0$to) %>%
    select(to = status_id, from = quoted_status_id, user_id) %>%
    mutate(type = "quote")
  df_reply1 <-
    df_thread %>%
    filter(reply_to_status_id %in% df0$to) %>%
    select(to = status_id, from = reply_to_status_id, user_id) %>%
    mutate(type = "reply")
  res <- list(df0, df_reply1, df_quote1) %>%
    reduce(full_join) %>%
    distinct()
  if (nrow(res) == nrow(df0)) {
    return(res %>%
             filter(from != "root"))
  } else {
    find_connections_rec(df_thread, res)
  }
}



#' Scrape all likes of all users occurring in a thread of a twitter status_id
#'
#' @param ids Vector of all code{user_id}s
#' @param save_res logical if file should be saved
#'
#' @return Dataframe of all timelines of all \code{thread_ids}
#' @export
#'
#' @examples
#' main_status_id <- "1234620900386975744"
#' df_main_status <- rtweet::lookup_statuses(main_status_id)
#' df_thread <- search_thread(main_status_id)
#' thread_ids <- df_thread$user_id %>% unique()
#' df_tls <- scrape_timelines(thread_ids)
#'df0 <- df_main_status %>%
#'                        dplyr::filter(status_id == main_status_id) %>%
#'                        dplyr::select(to = status_id, user_id) %>%
#'                        dplyr::mutate(from = "root", type = "root")
#' tweet_edges <-
#' find_connections_rec(dplyr::bind_rows(df_thread, df_tls), df0)
#' ids <- tweet_edges$user_id %>% unique()
#' df_favs <- scrape_favs2(ids)

scrape_favs2 <- function(ids, save_res = TRUE) {
  safe_fav <- possibly(rtweet::get_favorites, otherwise = tibble())
  l <- vector("list", length(ids))
  for (i in 1:length(l)) {
    rl <- rtweet::rate_limit("get_favorites")
    if (rl[["remaining"]] <= 2) {
      print(paste0("Rate limit reached. Resuming at ", rl[["reset_at"]]))
      Sys.sleep(as.numeric(rl[["reset"]], "secs") + 1)
    }
    l[[i]] <- safe_fav(ids[[i]], n = 320, since_id = main_status_id)
    print(paste0("Index: ", i,
                 "; Scraped ", nrow(l[[i]]),
                 " tweets. Remaining: ", rl[["remaining"]]))
  }
  df_favs <- l %>% bind_rows()
  # if (save_res == TRUE) {
  #   save_name <- paste0(df_main_status$screen_name,
  #                       "_",
  #                       str_sub(df_main_status$text, end = 15),
  #                       "_favs.rds")
  #   saveRDS(df_favs, save_name)
  # }
  df_favs
}




