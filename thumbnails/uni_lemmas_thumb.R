source("../common.R")

all_prop_data <- feather::read_feather("../uni_lemmas/all_prop_data.feather")
uni_lemmas <- sort(unique(all_prop_data$uni_lemma))
start_lemma <- "dog"
start_measure <- "produces"
kid_min <- 10
points_min <- 6

input <- list(uni_lemma = "dog", measure = "produces")

uld <- all_prop_data %>%
  group_by(language) %>%
  filter(uni_lemma == input$uni_lemma,
         measure == input$measure) |>
  filter(sum(prop > 0) > points_min, # more than n non-zero points
         sum(n > kid_min) > points_min) |> # more than n points with some data
  ungroup()

words_data <- uld |> distinct(language, measure, words)

demo_langs <- c("Croatian", "Danish", "French (Quebecois)",
                "English (American)", "Italian", "Spanish (European)")

demo_uni_data <- uld |>
  filter(language %in% demo_langs) |>
  mutate(language = str_remove(language, "\\s.*$"))

demo_words_data <- words_data |>
  filter(language %in% demo_langs) |>
  mutate(language = str_remove(language, "\\s.*$"))

ggplot(demo_uni_data, aes(x = age)) +
  facet_wrap(vars(language), ncol = 3) +
             # labeller = labeller(language = label_wrap_gen(width = 10))) +
  # coord_fixed(ratio = ages - 2) +
  # geom_point(aes(y = prop, colour = language)) +
  geom_smooth(aes(y = prop, colour = language, weight = n), se = FALSE,
              method = "glm", method.args = list(family = "binomial"),
              size = 1.2, span = 1) +
  # geom_line(aes(y = fit_prop, colour = language), size = 1.5) +
  geom_text(aes(x = 10, y = 0.8, label = words), family = font,
             data = demo_words_data,
             # label.padding = unit(0.15, "lines"),
             vjust = "inward", hjust = "inward") +
  langcog::scale_colour_solarized(guide = "none") +
  langcog::scale_fill_solarized(guide = "none") +
  scale_y_continuous(name = "Proportion of children",
                     limits = c(0, 1), breaks = NULL) +
  scale_x_continuous(name = "Age", limits = c(8, 30),
                     breaks = NULL) +
  theme(plot.margin = margin(0))

# 250 x 180
ggsave("uni_lemmas_thumb.png", width = 250*5, height = 180*5, units = "px")
# file.copy("uni_lemmas_thumb.png", "~/projects/wordbank/wordbank/static/images/sample_plots", overwrite = TRUE)
