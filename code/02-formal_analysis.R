library(here)
library(tidyverse)
library(lme4)
library(marginaleffects)
theme_set(theme_classic(base_size = 20))

source(here("code/helper_functions.R"))

output_dir <- here("output/formal_analysis")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

.intervention_labels <- c(
  "preint" = "Baseline",
  "dismes" = "Discount and messaging",
  "wash" = "Washout",
  "dis" = "Discount",
  "wash2" = "Washout2",
  "follow" = "Follow-up",
  "cal" = "Caloric content messaging",
  "excer" = "Exercise messaging",
  "both" = "Combination messaging"
)

d <- read_csv(
  here("rawdata/june1data.csv"),
  show_col_types = FALSE
) %>%
  dplyr::mutate(
    Intervention = ifelse(
      Intervention %in% c("wash", "wash2", "follow"), "wash", Intervention
    ),
    Intervention_label = .intervention_labels[Intervention],
    Intervention = factor(
      Intervention,
      levels = c("preint", "dis", "dismes", "cal", "excer", "both", "wash")
    ),
    zero_cal_plus_sugary_sales = ZeroCal + Sugary,
    DofW = factor(DofW),
    study_day = Count,
    weekend = DofW %in% c(6, 7)
  ) %>%
  dplyr::filter(!is.na(Total))

#  Zero calorie sales ----

outcome_name <- "ZeroCal"

for (outcome_name in c("ZeroCal", "Sugary")) {
  ## fit three models
  .formula <- str_glue(
    "{outcome_name} ~ DofW + offset(log(Total)) + (1 | Site)"
  )
  m0 <- glmer.nb(
    as.formula(.formula),
    data = d
  )
  m1 <- update(m0, . ~ . + Intervention)
  m2 <- update(m1, . ~ . - (1 | Site) + (1 + Intervention | Site))

  ## get LRT p values
  lrt_pvals <- p.adjust(anova(m0, m1, m2)[["Pr(>Chisq)"]][2:3], method = "holm")
  write_tsv(
    data.frame(term = c("overall", "interaction"), p.adj = lrt_pvals),
    here(
      str_glue(
        "output/formal_analysis/lrt_{outcome_name}.tsv"
      )
    )
  )

  ## get pairwise comparisons using marginal contrasts

  pairwise_comparisons <- get_pairwise_comparisons(
    fitted_model = m1,
    .intervention_labels = .intervention_labels,
    inference_method = "delta",
    alpha_level = 0.05
  )
  ordered_groups <- c(
    "Discount vs. Baseline",
    "Discount and messaging vs. Baseline",
    "Exercise messaging vs. Baseline",
    "Caloric content messaging vs. Baseline",
    "Combination messaging vs. Baseline",
    "Discount and messaging vs. Discount",
    "Combination messaging vs. Exercise messaging",
    "Combination messaging vs. Caloric content messaging"
  )
  plot_df <- pairwise_comparisons %>%
    mutate(
      comparison_label_formatted = str_replace(
        comparison_label, " vs. ", "\nvs. "
      ),
      comparison_label_formatted = fct_relevel(
        comparison_label_formatted,
        str_replace(ordered_groups, " vs. ", "\nvs. "),
      ),
      comparison_label_formatted = fct_rev(comparison_label_formatted),
      relative_change = paste0(
        round(100 * (estimate - 1), 1), "% (",
        round(100 * (conf.low - 1), 2), "% \u2014 ",
        round(100 * (conf.high - 1), 2), "%)"
      )
    )

  #### save table with effect estimates
  pairwise_comparisons_table <- plot_df %>%
    mutate(
      comparison_label_formatted = fct_relevel(comparison_label, ordered_groups)
    ) %>%
    arrange(comparison_label_formatted) %>%
    select(
      `Comparison` := comparison_label_formatted,
      `Relative change` := relative_change,
      `Unadjusted p-value` := p.label,
      `Adjusted p-value` := p.adj.label
    )
  write_tsv(
    pairwise_comparisons_table,
    here(str_glue("{output_dir}/pairwise_comparisons_{outcome_name}.tsv"))
  )

  #### save plots with effect estimates
  pairwise_comparisons_plot <- plot_df %>%
    ggplot(
      aes(
        comparison_label_formatted, estimate - 1,
        ymin = conf.low - 1, ymax = conf.high - 1
      )
    ) +
    geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
    geom_pointrange() +
    geom_text(
      aes(
        label = paste0("adj. ", rstatix::p_format(p.adj, add.p = T, space = T))
      ),
      data = . %>% filter(p.adj < 0.05),
      vjust = -1
    ) +
    coord_flip(ylim = c(-.55, .55)) +
    scale_y_continuous(
      labels = scales::percent,
      breaks = seq(-.5, .5, .25)
    ) +
    labs(
      x = NULL, y = "Relative change"
    )
  ggsave(
    here(str_glue("{output_dir}/pairwise_comparisons_{outcome_name}.png")),
    pairwise_comparisons_plot,
    width = 11, height = 6.5
  )

  ## adjusted rate predictions -- by site
  predicted_response <- ggeffects::predict_response(
    m2,
    terms = c("Intervention", "Site"),
    type = "random", interval = "confidence",
    condition = c(Total = 1, DofW = 1)
  ) %>%
    as_tibble()
  adjusted_predictions_plot <- predicted_response %>%
    mutate(
      intervention = .intervention_labels[as.character(x)],
      intervention = factor(
        intervention,
        levels = rev(c(
          "Baseline",
          "Discount", "Discount and messaging", "Exercise messaging",
          "Caloric content messaging", "Combination messaging"
        ))
      ),
      site = c("chop" = "CHOP", "HF" = "HF", "NS" = "NS")[as.character(group)]
    ) %>%
    filter(str_detect(x, "wash", negate = TRUE)) %>%
    ggplot(
      aes(
        x = predicted, xmin = conf.low,
        xmax = conf.high, y = intervention, color = site
      )
    ) +
    geom_pointrange(position = position_dodge(width = 0.3)) +
    scale_x_continuous(
      labels = scales::percent
    ) +
    coord_cartesian(xlim = c(0, 0.5)) +
    labs(
      x = ifelse(
        outcome_name == "ZeroCal",
        "Fraction of zero-calorie sales",
        "Fraction of sugary sales"
      ),
      y = NULL,
      color = NULL
    )

  ggsave(
    here(str_glue("{output_dir}/adjusted_predictions_{outcome_name}.png")),
    adjusted_predictions_plot,
    width = 11, height = 6.5
  )
}
