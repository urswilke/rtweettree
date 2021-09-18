suppressMessages(library(tidygraph))
suppressMessages(g <- create_tweet_tbl_graph(df_main_status, df_tree, df_tls, df_favs, df_retweets))

test_that("generated tbl graph object still the same", {
  expect_snapshot(g %>% as_tibble())
  expect_snapshot(g %E>% as_tibble())
})
