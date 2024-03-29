#' Summarize convergent regimes across a list of models
#'
#' @param convRegList List of model objects
#'
#' @return Data frame with summarized regimes
#' @export
#'
summarise_convregs <- function(convRegList) {
  # condense convergent clades
  convtipsPC <- map(convRegList, summarize.lasso.convergence.output)
  convtipsPC <- discard(convtipsPC, ~nrow(.) == 0)
  convtipslongPC <- convtipsPC %>% map_df(bind_rows, .id = "tree")
  justcon_PC <- convtipsPC %>% purrr::map(~ filter(.x, conv_reg != "noshift"))
  justcon_PClong <- justcon_PC %>% map_df(bind_rows, .id = "tree")


  # all species
  allsps_conv <- convtipslongPC %>%
    distinct(sp) %>%
    pull(sp)

  # 'basal' species across all trees
  treesgp <- convtipslongPC %>%
    group_by(tree) %>%
    n_groups()
  noShift_taxa <- convtipslongPC %>%
    group_by(tree) %>%
    filter(conv_reg == "noshift") %>%
    ungroup() %>%
    count(sp) %>%
    filter(n == treesgp) %>%
    pull(sp)

  convtaxa <- setdiff(allsps_conv, noShift_taxa)


  # get all convergent taxa for a focal sp, remove sps from same regime
  get_conv_taxa <- function(modeldf, species) {
    mbros <- modeldf %>% filter(stringr::str_detect(sp, species))
    n_basal <- nrow(mbros[mbros$conv_reg == "noshift", ])
    mbros_convOnly <- mbros %>% filter(conv_reg != "noshift")
    conv_regimeN <- pull(mbros_convOnly, conv_reg)
    og_regimeN <- pull(mbros_convOnly, og_reg)
    convBros <- modeldf %>% filter(conv_reg %in% conv_regimeN)
    conv_sps <-
      convBros %>%
      filter(sp != species) %>%
      filter(!og_reg %in% og_regimeN) %>%
      mutate(focal = species, .before = 1) %>%
      select(focal, sp) %>%
      distinct()
    return(list(noshift = n_basal, convergent_sps = conv_sps))
  }

  #
  count_bros <- function(focal_sp) {
    convtipsPC %>%
      map(~ get_conv_taxa(.x, focal_sp)) %>%
      map_df("convergent_sps") %>%
      add_count(sp)
  }

  PCallbros <- map(convtaxa, count_bros) %>% map_df(bind_rows)
  PCallbros <- PCallbros %>% distinct()
  PCallbros
}
