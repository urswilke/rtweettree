
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
#> [1] "Index: 1; Scraped 5 tweets. Remaining: 63"
#> [1] "Index: 2; Scraped 1 tweets. Remaining: 61"
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
