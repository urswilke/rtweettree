library(ggraph)
library(threadnet)
library(ggrepel)
library(tidygraph)
library(tidyverse)
library(zeallot)

df_thread <- readRDS("~/R/fdp_filsdep/data/CSU_Die @AfD ist ge_thread.rds")
main_status_id <- "1234508769649758209"

df_main_status <- df_thread %>% filter(status_id == main_status_id)

df_tls<- readRDS("~/R/fdp_filsdep/data/CSU_Die @AfD ist ge_tls.rds") %>% bind_rows()
df_favs<- readRDS("~/R/fdp_filsdep/data/CSU_Die @AfD ist ge_favs.rds")




df <- list(df_main_status, df_thread, df_tls, df_favs) %>% bind_rows() %>% distinct(status_id, .keep_all = TRUE)
df_root <-
  df_main_status %>%
  # df_thread %>%
  # filter(status_id == main_status_id) %>%
  select(to = status_id, user_id, screen_name) %>%
  mutate(from = "root", type = "root")
tweet_edges <-
  find_connections_rec(dplyr::bind_rows(df_thread, df_tls, df_favs), df_root) %>%
  mutate(alpha = 1)
user_tweet_edges <-
  tweet_edges %>%
  bind_rows(df_root %>% mutate(from = screen_name)) %>%
  transmute(user_id,
            screen_name,
            from = to,
            to = user_id,
            type = "by",
            alpha = .5)


fav_edges <-
  df_favs %>%
  filter(status_id %in% tweet_edges$to) %>%
  transmute(from = status_id, to = favorited_by, user_id = favorited_by, screen_name) %>%
  add_count(to, name = "n_likes") %>%
  mutate(type = "like",
         alpha = .1)


edges <-
  list(
    tweet_edges,
    fav_edges,
    user_tweet_edges
  ) %>%
  reduce(full_join) %>%
  mutate(from_chr = from, to_chr = to)

user_nodes <-
  tibble(name =
           c(df_root$user_id,
             user_tweet_edges$user_id) %>%
           unique(),
         type = "user") %>%
  left_join(df %>% select(name = user_id, screen_name) %>% distinct()) %>%
  mutate(url = glue::glue("https://twitter.com/{screen_name}/"))

tweet_nodes <-
  tibble(name =
           c(tweet_edges$to,
             tweet_edges$from) %>%
           unique(),
         type = "tweet") %>%
  left_join(df %>% select(name = status_id, screen_name, text) %>% distinct()) %>%
  mutate(url = glue::glue("https://twitter.com/{screen_name}/status/{name}"))
nodes <-
  full_join(user_nodes,
            tweet_nodes) %>%
  mutate(label = coalesce(text, screen_name))
# tbl_graph(nodes, edges)





# graph -------------------------------------------------------------------

nodes <-
  tbl_graph(nodes, edges) %>%
  mutate(dist_to_center = node_distance_from(node_is_source()),
         group = group_infomap()) %>%
  group_by(group) %>%
  mutate(n_group = n()) %>%
  ungroup() %>%
  as_tibble()

g <- tbl_graph(nodes, edges)
g %>%
  ggraph('circlepack', circular = T) +
  geom_node_circle(aes(fill = dist_to_center), n = 50) +
  scale_fill_viridis_c(direction = -1) +
  coord_fixed()

# g1 <- g %>% ggraph("dendrogram")

g1 <- g %>% ggraph()
g1$data$onclick  <- glue::glue('window.open("{g1$data$url}")')

g1$data$y[g1$data$type == "user"] <- min(g1$data$y)
g1 +
  geom_edge_diagonal(aes(color= type)) +
  geom_node_point(aes(color = dist_to_center,
                      shape = type)) +
  ggforce::geom_mark_hull(aes(x = g1$data$x,
                              y = g1$data$y,
                              filter = type == 'user',
                              label = "user")
  )
g2 <- g1 +
  geom_edge_diagonal(aes(color= type, alpha = alpha)) +
  ggforce::geom_mark_rect(aes(x = g1$data$x,
                              y = g1$data$y,
                              # filter = type == 'user',
                              color = g1$data$type)
  ) +
  ggiraph::geom_point_interactive(aes(x=g1$data$x,
                                      y = g1$data$y,
                                      color = type,
                                      data_id = g1$data$screen_name,
                                      tooltip = g1$data$label,
                                      onclick = g1$data$onclick),
                                  size = 3)
ggiraph::girafe(ggobj = g2,
                options = list(
                  ggiraph::opts_zoom(min = 0.3, max = 5),
                  ggiraph::opts_hover(ggiraph::girafe_css(css = "stroke:yellow;",
                                                          point = "stroke-width:6px"))
                )
)



