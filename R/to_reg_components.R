#' Consensus from edge list
#'
#' @param PCsummary Edge list in data frame format
#' @param comparison Relational operator for comparison
#' @param qtile Quantile to retain
#' @param wPC Label for PC
#' @param wmtype Measurement type label
#'
#' @return Data frame with regimes in long format
#' @export
#'
to_regComponents <- function(PCsummary,comparison,qtile,wPC,wmtype) {
  fun1 <- match.fun(comparison)
  PCsummary %>%
    filter(fun1(n,quantile(unique(PCsummary$n),qtile))) %>%
    rename(to = 1, from = 2, weight = 3) %>%
    mutate(weight = weight) %>%
    mutate(across(where(is.character),~str_replace(.x,"_"," "))) |>
    as_tbl_graph(directed=FALSE) -> adj_grphPC
  regMembership <- components(adj_grphPC)$membership |> tibble::enframe() |>
    rename(sp=1,regime=2) |> mutate(sp=str_replace(sp," ","_"))
  regMembership$PC <- wPC
  regMembership$mtype <- wmtype
  list(regMembership,adj_grphPC)
}
