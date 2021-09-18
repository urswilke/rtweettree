suppressMessages(library(tidygraph))
suppressMessages(library(tidyverse))
suppressMessages(g <- create_tweet_tbl_graph(df_main_status, df_tree, df_tls, df_favs, df_retweets))
g <- g %>% mutate(dist_to_center = node_distance_from(node_is_source()))
test_that("generated tbl graph object still the same", {
  expect_snapshot(g %>% as_tibble())
  expect_snapshot(g %E>% as_tibble())
})
