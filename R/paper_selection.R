# Shared paper dataset selection and grouping.
# Source after R/settings.R.

paper_collection_plot_exclude <- c(
  "Bartos", "Brodeur",
  names(bear_dataset_classes$replication[
    bear_dataset_classes$replication &
      grepl(" original$", names(bear_dataset_classes$replication))
  ])
)

paper_analysis_exclude <- unique(c(paper_do_not_include,
                                   paper_collection_plot_exclude))

paper_collection_group_labels <- c(
  curated_evidence = "Curated sets",
  domain_sample = "Representative domain samples",
  replication_effort = "Replication efforts"
)

paper_collection_group_colors <- c(
  curated_evidence = "#377EB8",
  domain_sample = "#E41A1C",
  replication_effort = "#B8860B",
  original_result = "#4D4D4D",
  replication_result = "#B8860B"
)

paper_dataset_groups <- list(
  curated_evidence = c(
    "ArelBundock", "Askarov", "Cochrane", "CostelloFox",
    "Metapsy", "Nuijten", "psymetadata", "WWC"
  ),
  domain_sample = c(
    "BarnettWren", "Lang", "Chavalarias", "ctgov_euctr",
    "SCORE_claims", "Szucs"
  ),
  replication_effort = c(
    "OSC", "SCORE_replications"
  )
)

paper_selected_datasets <- unlist(paper_dataset_groups, use.names = FALSE)

order_paper_datasets <- function(datasets, psr_table = NULL) {
  datasets <- intersect(datasets, paper_selected_datasets)
  if (is.null(psr_table)) return(datasets)

  psr_table %>%
    dplyr::filter(dataset %in% datasets) %>%
    dplyr::arrange(dplyr::desc(PoS)) %>%
    dplyr::pull(dataset)
}

paper_dataset_group <- function(datasets) {
  group_lookup <- rep(names(paper_dataset_groups), lengths(paper_dataset_groups))
  names(group_lookup) <- paper_selected_datasets
  unname(group_lookup[datasets])
}

paper_dataset_color_group <- function(datasets) {
  paper_dataset_group(datasets)
}
