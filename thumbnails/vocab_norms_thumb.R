vocab_curves <- read_rds("../vocab_norms/sample_curves.rds")

colour_values <- n_distinct(vocab_curves$percentile) |>
  langcog::solarized_palette() |>
  rev()

ggplot(vocab_curves, aes(x = x, y = predicted, colour = percentile)) + 
  # facet_wrap(vars(demo_label)) + #, labeller = label_both()) +
  coord_cartesian(clip = "off") +
  # geom_jitter(size = .6, color = pt_color, alpha = .7) +
  geom_line(size = 1.5) +
  directlabels::geom_dl(aes(label = percentile),
                        method = list("last.qp", directlabels::dl.trans(x = x + 0.2),
                                      cex = 1, fontfamily = font)) +
  scale_x_continuous(name = "Age",
                     breaks = NULL,
                     limits = c(min(vocab_curves$x), max(vocab_curves$x)),
                     expand = expansion(mult = 0.01)) +
  scale_y_continuous(name = "Vocabulary size",
                     breaks = NULL,
                     limits = c(0, max(vocab_curves$predicted)),
                     expand = expansion(mult = 0.01)) + 
  scale_color_manual(name = "Quantile", values = colour_values, guide = "none") +
  theme(plot.margin = margin(0))

ggsave("vocab_norms_thumb.png", width = 250*5, height = 180*5, units = "px")
# file.copy("vocab_norms_thumb.png", "~/projects/wordbank/wordbank/static/images/sample_plots", overwrite = TRUE)
