suppressMessages(library(tidygraph))
suppressMessages(library(ggraph))
suppressMessages(library(dplyr))
suppressMessages(library(purrr))
suppressMessages(l <- list(df_main_status, df_tree, df_tls, df_favs, df_retweets) %>% set_names(c("main_status", "tree", "tls", "like", "retweet")) %>% bind_rows(.id = "type"))
class(l) <- c("rtweettree_data", class(l))

suppressMessages(g <- rtweettree_tbl_graph(l))


test_that("generated tbl graph object still the same", {
  expect_snapshot(g %>% as_tibble())
  expect_snapshot(g %E>% as_tibble())
})

test_that("generated ggraph graph object still the same", {
  skip_on_os(os = c("windows", "mac"))
  suppressMessages(ggraph_fun <- autoplot(l))
  vdiffr::expect_doppelganger(
    "tweet ggraph",
    ggraph_fun
  )
})





