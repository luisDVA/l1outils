#' Separate edges and nodes from graph object
#'
#' @param PCgrph Network graph to split
#'
#' @return tbl_graph objects for further analysis
#' @export
#'
edgesToNodes <- function(PCgrph) {
  # Separate out edges and node data frames
  tg_nodes <-
    PCgrph %>%
    activate(nodes) %>%
    data.frame() %>%
    tibble::rownames_to_column("rowid") %>%
    mutate(rowid = as.integer(rowid))
  tg_edges <-
    PCgrph %>%
    activate(edges) %>%
    data.frame()
  named_edge_list <-
    tg_edges %>%
    # Rename from nodes
    left_join(tg_nodes, by = c("from" = "rowid")) %>%
    select(-from) %>% # Remove unneeded column
    rename(from = name) %>% # Rename column with names now
    # Rename to nodes
    left_join(tg_nodes, by = c("to" = "rowid")) %>%
    select(-to, -w.y) %>% # Remove unneeded column
    rename(to = name, w = w.x, ntrees = weight)
  named_edge_list %>% as_tbl_graph(directed = FALSE)
}
