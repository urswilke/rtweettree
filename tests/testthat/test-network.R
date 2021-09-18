suppressMessages(library(tidygraph))
suppressMessages(library(ggraph))
suppressMessages(library(dplyr))
suppressMessages(library(purrr))
suppressMessages(g <- rtweettree_tbl_graph(tibble::lst(df_main_status, df_tree, df_tls, df_favs, df_retweets)))
g <- g %>% mutate(dist_to_center = node_distance_from(node_is_source()))

df_profile_pic$profile_image_url <- 1:3 %>% map_chr(~system.file("profile_pics", paste0(.x, ".png"), package = "rtweettree"))
df_profile_pic <- df_profile_pic %>%
  dplyr::mutate(
    img = profile_image_url %>%
      purrr::map(magick::image_read)
  )

test_that("generated tbl graph object still the same", {
  expect_snapshot(g %>% as_tibble())
  expect_snapshot(g %E>% as_tibble())
})

test_that("generated ggraph graph object still the same", {
  ggraph_fun <- add_profile_pics_to_tree_ggraph(
      g,
      df_profile_pic
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





