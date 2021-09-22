#' Included datasets
#'
#' The package includes several datasets resulting of rtweet queries (to make
#' them available for documentation purposes and offline use). All these
#' datasets are included in the result of running `rtweettree_data()` (see
#' examples).
#' @name incl_data
#' @examples
#' \dontrun{
#' main_status_id <- "1438481824922181635"
#' rtweettree_data(main_status_id)
#' }
NULL



#' @rdname incl_data
"df_main_status"

#' @rdname incl_data
"df_tree"

#' @rdname incl_data
"df_tls"

#' @rdname incl_data
"df_favs"

#' @rdname incl_data
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

