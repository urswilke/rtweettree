
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rtweettree

<!-- badges: start -->

<!-- badges: end -->

The goal of rtweettree is to scrape a twitter tweet and all replies,
quotes and likes (recursively) and visualize them in a network graph.

## Responsible use

**{{rtweettree}}** should be used in strict accordance with Twitter’s
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
# remotes::install_github("UrsWilke/rtweettree")
```

## load rtweettree package

``` r
library(rtweettree)
```

## Usage

In order to use **{{rtweettree}}** please refer to [the according
section of **{{rtweet}}**](https://github.com/ropensci/rtweet#usage).

An example how to create a subtweet network graph is shown in the
[vignette for tree
visualization](https://github.com/UrsWilke/tree/master/vignettes/visualize_tree.Rmd)

``` r
vignette("visualize_tree", package = "rtweettree")
#> Warning: vignette 'visualize_tree' not found
```
