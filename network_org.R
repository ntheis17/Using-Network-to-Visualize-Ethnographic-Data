#load in appropriate packages
library(readr)
library(readxl)
library(tidygraph)
library(dplyr)
library(igraph)
library(ggraph)
library(ggplot2)


culture_net <- read_csv("input/culture.net.csv")[,c("Source", "Target")]
culture_nodes <- read_csv("input/culture.nodes.csv")[,c("name", "Label", "Code")]
doll_edges <- read_excel("input/doll.edges.xlsx")
u_doll_verticles <- read_excel("input/u.doll.verticles.xlsx")
part_nodes <- read_csv("input/part.nodes.csv")[,c("name", "Label", "Code")]
participant_net <- read_csv("input/participant.net.csv")[,c("Source", "Target")]

#make participant-participant network
part_net <- as_tbl_graph(participant_net)
part_net <- part_net %>% 
  activate(nodes) %>% 
  left_join(part_nodes)

part_net <- part_net %>% mutate(Mentions = centrality_degree(weights = NULL, mode = "in"))

node_to2 <- part_net %>%
  as_tibble() %>%
  mutate(to = row_number())

new_part_net <- part_net %>%
  activate(edges) %>%
  left_join(node_to2) 

set.seed(2)
ggraph(new_part_net, layout="fr") + geom_edge_link() + geom_node_point(aes(size = Mentions, color = Code))+
  geom_edge_fan(aes(color = Code), show.legend = F)+ geom_node_text(aes(label = name), size=2,  repel = T)

#make cultural reference network

culture_net <- as_tbl_graph(culture_net)
culture_net <- culture_net %>% 
  activate(nodes) %>% 
  left_join(culture_nodes)

culture_net <- culture_net %>% mutate(Mentions = centrality_degree(weights = NULL, mode = "in"))

node_to1 <- culture_net %>%
  as_tibble() %>%
  mutate(to = row_number())

new_culture_net <- culture_net %>%
  activate(edges) %>%
  left_join(node_to1) 

set.seed(2)
ggraph(new_culture_net, layout="fr") + geom_edge_link() + geom_node_point(aes(size = Mentions, color = Code)) +
  geom_edge_fan(aes(color = Code), show.legend = F) + geom_node_text(aes(label = ifelse(Mentions > 2, name, NA)), size=2,  repel = T)

#make full network
doll_edges_net <- as_tbl_graph(doll_edges)
doll_edges_net <- doll_edges_net %>% 
  activate(nodes) %>% 
  left_join(u_doll_verticles)
doll_edges_net <- doll_edges_net %>% mutate(Mentions = centrality_degree(weights = NULL, mode = "in"))

node_to <- doll_edges_net %>%
  as_tibble() %>%
  mutate(to = row_number())

new_doll_edges <- doll_edges_net %>%
  activate(edges) %>%
  left_join(node_to) 

set.seed(2)
ggraph(new_doll_edges, layout="fr") + geom_edge_link() +
  geom_edge_fan(aes(color = Code), show.legend = F) + geom_node_point(aes(size = Mentions, color = Code)) + geom_node_text(aes(label = ifelse(Mentions > 2, name, NA), size=2,  repel = T))

#appendix: community detection
new_doll_edges <- new_doll_edges %>% activate(nodes) %>% mutate(Community = as.factor(group_optimal(weights=NULL)))

set.seed(2)
ggraph(new_doll_edges, layout="fr") + geom_edge_link() + geom_node_point(aes(size = Mentions, color = Community)) + geom_node_text(aes(label = ifelse(Mentions > 2, name, NA), size=2,  repel = T))
