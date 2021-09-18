#' Example package dataset storing the result of rtweet::lookup_statuses(main_status_id)
#'
#' @format A data frame with 1 row and 90 variables:
#' \describe{
#'   df_main_status <- rtweet::lookup_statuses(main_status_id)
#' }
"df_main_status"

#' Example package dataset storing the result of search_tree(main_status_id)
#'
#' @format A data frame with 1 row and 91 variables:
#' \describe{
#'   df_tree <- search_tree(main_status_id)
#' }
"df_tree"

#' Example package dataset storing the result of rtweet::get_timelines(tree_ids)
#'
#' @format A data frame with 1 row and 90 variables:
#' \describe{
#'   df_tls <- rtweet::get_timelines(tree_ids)
#' }
"df_tls"

#' Example package dataset storing the result of scrape_favs2(ids, main_status_id)
#'
#' @format A data frame with 1 row and 91 variables:
#' \describe{
#'   df_favs <- scrape_favs2(ids, main_status_id)
#' }
"df_favs"

#' Example package dataset storing the result of df_retweets <- tweet_ids %>% map_dfr(~rtweet::get_retweets(.x))
#'
#' @format A data frame with 1 row and 90 variables:
#' \describe{
#'   df_retweets <- tweet_ids %>% map_dfr(~rtweet::get_retweets(.x))
#' }
"df_retweets"

#' Example package dataset storing the result of rtweettree:::get_profile_pic_df()
#'
#' @format A data frame with 1 row and 90 variables:
#' \describe{
#'   df_profile_pic <- rtweettree:::get_profile_pic_df(bind_rows(df_tls, df_favs, df_main_status))
#' }
"df_profile_pic"

