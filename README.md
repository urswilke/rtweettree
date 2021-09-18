
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rtweettree

<!-- badges: start -->

[![R-CMD-check](https://github.com/urswilke/rtweettree/workflows/R-CMD-check/badge.svg)](https://github.com/urswilke/rtweettree/actions)
<!-- badges: end -->

The goal of rtweettree is to recursively scrape a twitter tweet and all
replies, quotes and likes (that the API provides) and visualize them in
a network graph. The functionalities to scrape twitter data are heavily
based on the excellent [**rtweet**](https://github.com/ropensci/rtweet)
package. The graph network manipulation functionalities rely on the
amazing [**tidygraph**](https://github.com/thomasp85/tidygraph) package
and are visualized with
[**ggraph**](https://github.com/thomasp85/ggraph).

## Responsible use

**rtweettree** should be used in strict accordance with Twitter’s
[developer
terms](https://developer.twitter.com/en/developer-terms/more-on-restricted-use-cases).

## Installation

To get the current development version from Github:

``` r
## install remotes package if it's not already
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

## install dev version of rtweettree from github
remotes::install_github("UrsWilke/rtweettree")
```

## load rtweettree package

``` r
library(rtweettree)
```

## Usage

In order to use **rtweettree** please refer to [the according section of
**rtweet**](https://github.com/ropensci/rtweet#usage). It is probably
good advice to first feel comfortable with
[**rtweet**](https://github.com/ropensci/rtweet).

## Quick dive-in

To give you a quick understanding of the functionalities of this
package, it can first be used to scrape data related to a twitter status
id `main_status_id` (using `rtweet` functions under the hood):

``` r
main_status_id <- "1438481824922181635"
l <- rtweettree_data(main_status_id)
```

This generates a list of rtweet dataframes:

``` r
l
#> $df_main_status
#> # A tibble: 1 × 90
#>   user_id             status_id  created_at          screen_name text     source
#>   <chr>               <chr>      <dttm>              <chr>       <chr>    <chr> 
#> 1 1438476950746636291 143848182… 2021-09-16 12:36:07 rtweetbird1 this is… Twitt…
#> # … with 84 more variables: display_text_width <dbl>, reply_to_status_id <lgl>,
#> #   reply_to_user_id <lgl>, reply_to_screen_name <lgl>, is_quote <lgl>,
#> #   is_retweet <lgl>, favorite_count <int>, retweet_count <int>,
#> #   quote_count <int>, reply_count <int>, hashtags <list>, symbols <list>,
#> #   urls_url <list>, urls_t.co <list>, urls_expanded_url <list>,
#> #   media_url <list>, media_t.co <list>, media_expanded_url <list>,
#> #   media_type <list>, ext_media_url <list>, ext_media_t.co <list>, …
#> 
#> $df_tree
#> # A tibble: 1 × 91
#>   user_id             status_id  created_at          screen_name text     source
#>   <chr>               <chr>      <dttm>              <chr>       <chr>    <chr> 
#> 1 1438480252003569671 143848428… 2021-09-16 12:45:55 rtweetbird3 this is… Twitt…
#> # … with 85 more variables: display_text_width <dbl>, reply_to_status_id <chr>,
#> #   reply_to_user_id <chr>, reply_to_screen_name <chr>, is_quote <lgl>,
#> #   is_retweet <lgl>, favorite_count <int>, retweet_count <int>,
#> #   quote_count <int>, reply_count <int>, hashtags <list>, symbols <list>,
#> #   urls_url <list>, urls_t.co <list>, urls_expanded_url <list>,
#> #   media_url <list>, media_t.co <list>, media_expanded_url <list>,
#> #   media_type <list>, ext_media_url <list>, ext_media_t.co <list>, …
#> 
#> $df_tls
#> # A tibble: 3 × 90
#>   user_id             status_id  created_at          screen_name text     source
#>   <chr>               <chr>      <dttm>              <chr>       <chr>    <chr> 
#> 1 1438480252003569671 143848428… 2021-09-16 12:45:55 rtweetbird3 this is… Twitt…
#> 2 1438480252003569671 143848356… 2021-09-16 12:43:01 rtweetbird3 @rtweet… Twitt…
#> 3 1438480252003569671 143848345… 2021-09-16 12:42:36 rtweetbird3 @rtweet… Twitt…
#> # … with 84 more variables: display_text_width <dbl>, reply_to_status_id <chr>,
#> #   reply_to_user_id <chr>, reply_to_screen_name <chr>, is_quote <lgl>,
#> #   is_retweet <lgl>, favorite_count <int>, retweet_count <int>,
#> #   quote_count <int>, reply_count <int>, hashtags <list>, symbols <list>,
#> #   urls_url <list>, urls_t.co <list>, urls_expanded_url <list>,
#> #   media_url <list>, media_t.co <list>, media_expanded_url <list>,
#> #   media_type <list>, ext_media_url <list>, ext_media_t.co <list>, …
#> 
#> $df_favs
#> # A tibble: 6 × 91
#>   user_id             status_id  created_at          screen_name text     source
#> * <chr>               <chr>      <dttm>              <chr>       <chr>    <chr> 
#> 1 1438480252003569671 143848428… 2021-09-16 12:45:55 rtweetbird3 this is… Twitt…
#> 2 1438480252003569671 143848356… 2021-09-16 12:43:01 rtweetbird3 @rtweet… Twitt…
#> 3 1438480252003569671 143848345… 2021-09-16 12:42:36 rtweetbird3 @rtweet… Twitt…
#> 4 1438479415550390275 143848243… 2021-09-16 12:38:32 rtweetbird2 @rtweet… Twitt…
#> 5 1438479415550390275 143848230… 2021-09-16 12:38:02 rtweetbird2 @rtweet… Twitt…
#> 6 1438479415550390275 143848243… 2021-09-16 12:38:32 rtweetbird2 @rtweet… Twitt…
#> # … with 85 more variables: display_text_width <dbl>, reply_to_status_id <chr>,
#> #   reply_to_user_id <chr>, reply_to_screen_name <chr>, is_quote <lgl>,
#> #   is_retweet <lgl>, favorite_count <int>, retweet_count <int>,
#> #   quote_count <int>, reply_count <int>, hashtags <list>, symbols <list>,
#> #   urls_url <list>, urls_t.co <list>, urls_expanded_url <list>,
#> #   media_url <list>, media_t.co <list>, media_expanded_url <list>,
#> #   media_type <list>, ext_media_url <list>, ext_media_t.co <list>, …
#> 
#> $df_retweets
#>               user_id           status_id          created_at screen_name
#> 1 1438479415550390275 1438482588222607371 2021-09-16 12:39:09 rtweetbird2
#>                                                            text          source
#> 1 this is a test tweet to illustrate how rtweettree can be used Twitter Web App
#>   display_text_width reply_to_status_id reply_to_user_id reply_to_screen_name
#> 1                 NA                 NA               NA                   NA
#>   is_quote is_retweet favorite_count retweet_count quote_count reply_count
#> 1    FALSE       TRUE              0             1          NA          NA
#>   hashtags symbols urls_url urls_t.co urls_expanded_url media_url media_t.co
#> 1       NA      NA       NA        NA                NA        NA         NA
#>   media_expanded_url media_type ext_media_url ext_media_t.co
#> 1                 NA         NA            NA             NA
#>   ext_media_expanded_url ext_media_type    mentions_user_id
#> 1                     NA           <NA> 1438476950746636291
#>   mentions_screen_name lang quoted_status_id quoted_text quoted_created_at
#> 1          rtweetbird1   en             <NA>        <NA>              <NA>
#>   quoted_source quoted_favorite_count quoted_retweet_count quoted_user_id
#> 1          <NA>                    NA                   NA           <NA>
#>   quoted_screen_name quoted_name quoted_followers_count quoted_friends_count
#> 1               <NA>        <NA>                     NA                   NA
#>   quoted_statuses_count quoted_location quoted_description quoted_verified
#> 1                    NA            <NA>               <NA>              NA
#>     retweet_status_id
#> 1 1438481824922181635
#>                                                    retweet_text
#> 1 this is a test tweet to illustrate how rtweettree can be used
#>    retweet_created_at  retweet_source retweet_favorite_count
#> 1 2021-09-16 12:36:07 Twitter Web App                      2
#>   retweet_retweet_count     retweet_user_id retweet_screen_name retweet_name
#> 1                     1 1438476950746636291         rtweetbird1  rtweetbird1
#>   retweet_followers_count retweet_friends_count retweet_statuses_count
#> 1                       0                     0                      1
#>   retweet_location          retweet_description retweet_verified place_url
#> 1                  i'm just a fake user profile            FALSE      <NA>
#>   place_name place_full_name place_type country country_code geo_coords
#> 1       <NA>            <NA>       <NA>    <NA>         <NA>     NA, NA
#>   coords_coords                    bbox_coords
#> 1        NA, NA NA, NA, NA, NA, NA, NA, NA, NA
#>                                                   status_url        name
#> 1 https://twitter.com/rtweetbird2/status/1438482588222607371 rtweetbird2
#>   location              description url protected followers_count friends_count
#> 1          i'm another fake profile  NA     FALSE               0             0
#>   listed_count statuses_count favourites_count  account_created_at verified
#> 1            0              3                1 2021-09-16 12:26:54    FALSE
#>   profile_url profile_expanded_url account_lang profile_banner_url
#> 1        <NA>                 <NA>           NA                 NA
#>   profile_background_url
#> 1                     NA
#>                                                             profile_image_url
#> 1 http://pbs.twimg.com/profile_images/1438498659759378434/Ua9zpiYm_normal.jpg
```

You can then visualize this data with:

``` r
ggplot2::autoplot(l)
```

<img src="man/figures/README-autoplot-1.png" width="100%" />

A more in-depth example how to create the subtweet network graph from a
tweet status\_id is shown in the [vignette for tree
visualization](articles/visualize_tree.html).

``` r
vignette("visualize_tree", package = "rtweettree")
```

## TODOs:

-   clean up code and refactor
-   make plotting functions more customizable, e.g. with a ggplot2
    autoplot method (?)
