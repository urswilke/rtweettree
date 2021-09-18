suppressMessages(library(tidygraph))
suppressMessages(library(ggraph))
suppressMessages(library(dplyr))
suppressMessages(library(purrr))
suppressMessages(l <- rtweettree_data(tibble::lst(df_main_status, df_tree, df_tls, df_favs, df_retweets)))
suppressMessages(g <- rtweettree_tbl_graph(l))


test_that("generated tbl graph object still the same", {
  expect_snapshot(g %>% as_tibble())
  expect_snapshot(g %E>% as_tibble())
})

test_that("generated ggraph graph object still the same", {
  suppressMessages(ggraph_fun <- autoplot(l))
  vdiffr::expect_doppelganger(
    "tweet ggraph",
    ggraph_fun
  )
})





