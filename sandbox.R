library(visNetwork)
library(jsonlite)
library(magrittr)
library(dplyr)
library(igraph)

diagram_layout <- fromJSON('pbit_generated/DiagramLayout.json')
relationship_diagram <- as.data.frame(diagram_layout$diagrams$layout$nodes)

x_ranked <- sort( relationship_diagram$left )
y_ranked <- sort( relationship_diagram$top )

nodes <- relationship_diagram %>%
  mutate(
    id = 1:nrow(relationship_diagram)
  ) %>%
  select(
    id,
    label = nodeIndex,
    x = left,
    y = top
  )

rank_value <- function(x, arr) { match(x, arr) }

nodes$x <- lapply(nodes$x, rank_value, arr=x_ranked)
nodes$y <- lapply(nodes$y, rank_value, arr=y_ranked)
nodes$group <- 'table'

nodes$shape <- 'icon'

edges <- data.frame(
  from = c(1,3,4,5),
  to = c(2,2,2,2),
  length = 350
)

relationships <- get_dmv('TMSCHEMA_RELATIONSHIPS')

visNetwork(nodes = nodes, edges = edges, width = "100%") %>% 
  visGroups(groupname = "table", shape = "icon", 
            icon = list(code = "f0ce", size = 100, color='gray')) %>%
  addFontAwesome() %>%
  visEdges(arrows = 'to', smooth=F)
  
relationships %>%
  mutate(
    FromCardinality = ifelse( FromCardinality == 1, '1', '*'),
    ToCardinality = ifelse( ToCardinality == 1, '1', '*'),
    FromTableID = as.character(FromTableID),
    ToTableID = as.character(ToTableID),
    # FromColumnID = as.character(FromColumnID),
    # ToColumnID = as.character(ToColumnID)
  ) %>%
  left_join(
    select( tables, ID, From = Name ),
    by = c( 'FromTableID' = 'ID' )
  ) %>%
  left_join(
    select( tables, ID, To = Name ),
    by = c( 'ToTableID' = 'ID' )
  ) %>%
  left_join(
    select( columns, ID, FromColumn = ExplicitName),
    by = c( 'FromColumnID' = 'ID')
  ) %>%
  left_join(
    select( columns, ID, ToColumn = ExplicitName),
    by = c( 'ToColumnID' = 'ID')
  ) %>%
  mutate(
    label = paste0(
      '[', FromCardinality, '] ', FromColumn, 
      ' - ', ToColumn, ' [', ToCardinality, ']'
    )
  ) %>%
  select(
    From,
    label,
    To
  )