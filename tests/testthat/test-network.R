suppressMessages(library(tidygraph))
suppressMessages(library(ggraph))
suppressMessages(library(tidyverse))
suppressMessages(g <- create_tweet_tbl_graph(df_main_status, df_tree, df_tls, df_favs, df_retweets))
g <- g %>% mutate(dist_to_center = node_distance_from(node_is_source()))
test_that("generated tbl graph object still the same", {
  expect_snapshot(g %>% as_tibble())
  expect_snapshot(g %E>% as_tibble())
})

test_that("generated ggraph graph object still the same", {
  ggraph_fun <- add_profile_pics_to_tree_ggraph(
      g,
      rtweettree:::get_profile_pic_df(bind_rows(df_tls, df_favs, df_main_status))
    ) +
      geom_edge_diagonal(aes(color= type)) +
      scale_edge_colour_hue(name = "action") +
      geom_node_point(aes(color = dist_to_center,
                          shape = type))
  vdiffr::expect_doppelganger(
    "tweet ggraph",
    ggraph_fun
  )
})





