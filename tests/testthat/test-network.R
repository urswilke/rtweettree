suppressMessages(library(tidygraph))
suppressMessages(library(ggraph))
suppressMessages(library(dplyr))
suppressMessages(library(purrr))

suppressMessages(g <- rtweettree_tbl_graph(rtweettree_data_example))


test_that("generated tbl graph object still the same", {
  expect_snapshot(g %>% as_tibble())
  expect_snapshot(g %E>% as_tibble())
})

test_that("generated ggraph graph object still the same", {
  skip_on_os(os = c("windows", "mac"))
  suppressMessages(ggraph_fun <- autoplot(rtweettree_data_example))
  vdiffr::expect_doppelganger(
    "tweet ggraph",
    ggraph_fun
  )
})





