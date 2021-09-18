#' Example package dataset storing the result of rtweet::lookup_statuses(main_status_id)
#'
#' Example package dataset storing the result of rtweet::lookup_statuses(main_status_id)
#'
#' @examples
#' \dontrun{
#'   df_main_status <- rtweet::lookup_statuses(main_status_id)
#' }
"df_main_status"

#' Example package dataset storing the result of search_tree(main_status_id)
#'
#' Example package dataset storing the result of search_tree(main_status_id)
#'
#' @examples
#' \dontrun{
#'   df_tree <- search_tree(main_status_id)
#' }
"df_tree"

#' Example package dataset storing the result of rtweet::get_timelines(tree_ids)
#'
#' Example package dataset storing the result of rtweet::get_timelines(tree_ids)
#'
#' @examples
#' \dontrun{
#'   df_tls <- rtweet::get_timelines(tree_ids)
#' }
"df_tls"

#' Example package dataset storing the result of scrape_favs2(ids, main_status_id)
#'
#' Example package dataset storing the result of scrape_favs2(ids, main_status_id)
#'
#' @examples
#' \dontrun{
#'   df_favs <- scrape_favs2(ids, main_status_id)
#' }
"df_favs"

#' Example package dataset storing the result of rtweet::get_retweets() for multiple status_ids
#'
#' Example package dataset storing the result of rtweet::get_retweets() for multiple status_ids
#'
#' @examples
#' \dontrun{
#'   df_retweets <- tweet_ids %>% map_dfr(~rtweet::get_retweets(.x))
#' }
"df_retweets"

#' Example package dataset storing the result of get_profile_pic_df()
#'
#' Example package dataset storing the result of get_profile_pic_df()
#'
#' @examples
#' \dontrun{
#'   df_profile_pic <- get_profile_pic_df(bind_rows(df_tls, df_favs, df_main_status))
#' }
"df_profile_pic"

