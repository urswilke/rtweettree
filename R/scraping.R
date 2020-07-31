#' Scrape tree
#'
#' @param main_status_id string of twitter status_id
#' @param save_res logical if file should be saved
#' @param n maximum number of tweets to scrape
#' @param df_main_status data frame returned by rtweet::lookup_statuses(main_status_id)
#'
#' @return data frame like rtweet::search_tweets2, but all subtweets of the
#'   tree added (if available)
#' @export
#'
#' @examples
#' main_status_id <- "1234620900386975744"
#' df_main_status <- rtweet::lookup_statuses(main_status_id)
#' df_tree <- search_tree(main_status_id)
search_tree <- function(main_status_id,
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
  result <- add_tree_level(df_search_tweet, df_replies, n)
  # if (save_res == TRUE) {
  #   save_name <- paste0(df_main_status$screen_name,
  #                       "_",
  #                       str_sub(df_main_status$text, end = 15),
  #                       "_tree.rds")
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

add_tree_level <- function(df0, df1, n) {
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

    add_tree_level(df1, res)
  } else {
    return(res)
  }
}





#' Scrape the timelines of a tree scraped by \code{search_tree}
#'
#' @param tree_ids \code{user_id}s of a tree scraped by \code{search_tree}
#' @param save_res logical if file should be saved
#'
#' @return Dataframe of all timelines of all \code{tree_ids}
#' @export
#'
#' @examples
#' main_status_id <- "1234620900386975744"
#' df_main_status <- rtweet::lookup_statuses(main_status_id)
#' df_tree <- search_tree(main_status_id)
#' tree_ids <- df_tree$user_id %>% unique()
#' df_tls <- scrape_timelines(tree_ids)

scrape_timelines <- function(tree_ids, save_res = TRUE) {
  safe_tl <- purrr::possibly(rtweet::get_timelines, otherwise = tibble())
  # l <- vector("list", length(tree_ids))
  # for (i in 1:length(l)) {
  #   rl <- rtweet::rate_limit("get_timeline")
  #   if (rl[["remaining"]] <= 2) {
  #     print(paste0("Rate limit reached. Resuming at ", rl[["reset_at"]]))
  #     Sys.sleep(as.numeric(rl[["reset"]], "secs") + 1)
  #   }
  #   l[[i]] <- safe_tl(tree_ids[[i]], n = 3200, since_id = main_status_id)
  #   print(paste0("Index: ", i,
  #                "; Scraped ", nrow(l[[i]]),
  #                " tweets. Remaining: ", rl[["remaining"]]))
  # }
  # df_favs <- l %>% dplyr::bind_rows()
  # # if (save_res == TRUE) {
  # #   save_name <- paste0(df_main_status$screen_name,
  # #                       "_",
  # #                       str_sub(df_main_status$text, end = 15),
  # #                       "_favs.rds")
  # #   saveRDS(df_favs, save_name)
  # # }
  # df_favs
  rl <- rtweet::rate_limit()
  if (rl[rl$query == "statuses/user_timeline",][["remaining"]] != 900 |
      rl[rl$query == "favorites/list",][["remaining"]] != 75) {
    Sys.sleep(as.numeric(rl[1,][["reset"]], "secs") + 1)
  }
  spliced_list <-
    seq(0, length(tree_ids), 175) %>%
    map(~.x + 1:175) %>%
    map(~tree_ids[.x] %>%
          na.omit() %>%
          as.character())
  # if (rl[["remaining"]] <= 2) {
  #   print(paste0("Rate limit reached. Resuming at ", rl[["reset_at"]]))
  #   Sys.sleep(as.numeric(rl[["reset"]], "secs") + 1)
  # }

  load_slowly <- function(tree_ids, index) {

    df <- safe_tl(tree_ids,
                  since_id = main_status_id,
                  n = 3200)
    rl <- rtweet::rate_limit("get_timeline")
    print(paste0("Downloaded bulk ", index,
                 " of ", length(spliced_list),
                 "; Scraped ", nrow(df),
                 " tweets. Resuming at: ", rl[["reset_at"]]))
    Sys.sleep(as.numeric(rl[["reset"]], "secs") + 1)

    # print(df)
    df
  }

  l_tls <-
    spliced_list %>%
    imap(~load_slowly(.x, .y))

  # if (save_res == TRUE) {
  #   save_name <- paste0(df_main_status$screen_name,
  #                       "_",
  #                       str_sub(df_main_status$text, end = 15),
  #                       "_tls.rds")
  #   saveRDS(l_tls, save_name)
  # }
  l_tls %>% bind_rows()
}






#' Scrape all likes of all users occurring in a tree of a twitter status_id
#'
#' @param ids Vector of all \code{user_id}s
#' @param save_res logical if file should be saved
#'
#' @return Dataframe of all timelines of all \code{tree_ids}
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
#' ids <- tweet_edges$user_id %>% unique()
#' df_favs <- scrape_favs2(ids)

scrape_favs2 <- function(ids, save_res = TRUE) {
  safe_fav <- purrr::possibly(rtweet::get_favorites, otherwise = tibble())
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
  df_favs <-
    l %>% purrr::compact() %>%
    dplyr::bind_rows()
  # if (save_res == TRUE) {
  #   save_name <- paste0(df_main_status$screen_name,
  #                       "_",
  #                       str_sub(df_main_status$text, end = 15),
  #                       "_favs.rds")
  #   saveRDS(df_favs, save_name)
  # }
  df_favs
}




