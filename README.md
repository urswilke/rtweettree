
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- The logo was created in the repo https://github.com/urswilke/rtweettree_hex -->

# rtweettree <img src="man/figures/hex_sticker.png" width="160px" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/urswilke/rtweettree/workflows/R-CMD-check/badge.svg)](https://github.com/urswilke/rtweettree/actions)
[![Codecov test
coverage](https://codecov.io/gh/urswilke/rtweettree/branch/master/graph/badge.svg)](https://codecov.io/gh/urswilke/rtweettree?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of rtweettree is to recursively scrape a twitter tweet and all
replies, quotes, retweets and likes (that the API provides, see
[here](https://community.rstudio.com/t/rtweet-package-extract-specific-tweets-from-specific-user/45206/2))
and visualize them in a network graph. The functionalities to scrape
twitter data are heavily based on the excellent
[**rtweet**](https://github.com/ropensci/rtweet) package. The graph
network manipulation functionalities rely on the amazing
[**tidygraph**](https://github.com/thomasp85/tidygraph) package and are
visualized with [**ggraph**](https://github.com/thomasp85/ggraph).

## Responsible use

**rtweettree** should be used in strict accordance with Twitter’s
[developer
terms](https://developer.twitter.com/en/developer-terms/more-on-restricted-use-cases).

## Installation

To get the current development version from Github (with the
[remotes](https://github.com/r-lib/remotes) package):

``` r
## install dev version of rtweettree from github
remotes::install_github("UrsWilke/rtweettree")
```

## Usage

In order to use **rtweettree** please refer to [the according section of
**rtweet**](https://github.com/ropensci/rtweet#usage). It is probably
good advice to first feel comfortable with
[**rtweet**](https://github.com/ropensci/rtweet).

## Quick dive-in

First we’ll load the package.

``` r
library(rtweettree)
```

This package can first scrape data related to a twitter status id
`main_status_id` (The status id is the last number in the url of every
tweet on twitter.) and all the replies (to replies), quotes, retweets
and likes the API provides (using `rtweet` functions under the hood).

``` r
main_status_id <- "1438481824922181635"
rtweettree_data_scraped <- rtweettree_data(main_status_id)
```

This results in a dataframe of rtweet data, which can then be
transformed to a `tidygraph::tbl_graph()` object and finally visualized
with `ggraph`. When you have loaded the rtweettree package, you can also
directly use the `ggplot2::autoplot()` method:

``` r
ggplot2::autoplot(rtweettree_data_scraped)
```

<img src="man/figures/README-autoplot-1.png" width="100%" />

A more in-depth example how to create the subtweet network graph from a
tweet `status_id` is shown in the `vignette("get_started")`.

## Getting help

If you encounter a bug, please file an issue with a minimal reproducible
example on [GitHub](https://github.com/urswilke/rtweettree/issues).
