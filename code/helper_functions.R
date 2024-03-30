get_pairwise_comparisons <- function(fitted_model, .intervention_labels, inference_method = "delta", alpha_level = 0.05, R = 1000, ncpus = 1) {
  marginaleffects::avg_comparisons(
    fitted_model,
    variables = list(Intervention = "pairwise"),
    comparison = "ratio",
    hypothesis = 1
  ) %>%
    marginaleffects::inferences(method = inference_method, R = R, ncpus = ncpus, parallel = ifelse(ncpus > 1, "multicore", "no")) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      comparator_group = purrr::map_chr(contrast, ~ str_split(.x, " / ")[[1]][1]),
      reference_group = purrr::map_chr(contrast, ~ str_split(.x, " / ")[[1]][2]),
      comparison_label = paste0(.intervention_labels[comparator_group], " vs. ", .intervention_labels[reference_group])
    ) %>%
    dplyr::filter(
      reference_group == "preint" | contrast %in% c("dismes / dis", "both / excer", "both / cal"),
      comparator_group != "wash"
    ) %>%
    dplyr::mutate(
      p.adj = p.adjust(p.value, method = "holm"),
      # p.label = ifelse(
      #   p.adj < alpha_level,
      #   rstatix::p_format(p.adj, add.p = TRUE),
      #   "ns"
      # ),
      p.label = rstatix::p_format(p.value, add.p = TRUE),
      p.adj.label = rstatix::p_format(p.adj, add.p = TRUE)
    ) %>%
    dplyr::select(
      comparison_label, contrast,
      estimate, std.error, conf.low, conf.high, contains("p."),
      comparator_group, reference_group
    ) %>%
    dplyr::arrange(forcats::fct_rev(reference_group))
}
