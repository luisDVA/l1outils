#' Join with w
#'
#' @param regsobj Object with regimes
#' @param PClab Label for the Principal Component
#'
#' @return Joined data frame
#' @export
#'
makeWgraph <- function(regsobj, PClab) {
  regsobj[[2]] %>%
    activate(nodes) %>%
    mutate(reg = group_components(), PC = PClab) %>%
    left_join(regWs_summary) %>%
    edgesToNodes()
}
