#' Summarize shifts and regimes
#'
#' @param model Model object with shifts
#'
#' @return Tibble with summary statistics.
#' @export
#'
regimes_summary <- function(model){
  n_shifts <- model$nShifts
  sc <- model$shift.configuration
  regs <- sort(unique(names(sc)))
  regimesls <- purrr::map(regs,~sc[which(names(sc) == .x)])
  n_regs <- length(regimesls)
  n_conv <- sum(lengths(regimesls)>1)
  tibble(shifts=n_shifts,regimes=n_regs,conv_regimes=n_conv)
}
