source("../item_trajectories/helper.R")
source("../common.R")

input <- list(language = "English (American)", form = "WS", measure = "produces",
              words = c("dog", "shake", "blue"))

admins <- readRDS("../item_trajectories/admins.rds")
items <- get_item_data(language = input$language, form = input$form)
instruments <- get_instruments()

filtered_instrument_tables <- items |>
  left_join(instruments) |>
  filter(item_kind == "word")  |>
  group_by(language, form, instrument_id) |>
  nest()

filtered_admins <- admins |>
  filter(language == input$language, form %in% input$form)

td <- trajectory_data_fun(filtered_admins, filtered_instrument_tables, 
                          input$measure, input$words)

amin <- 16
amax <- 30

traj <- td |>
  mutate(item = factor(item_definition, levels = input$words)) |>
  filter(age >= amin, age <= amax)

ggplot(traj, aes(x = age, y = prop, colour = item, fill = item, label = item)) +
  geom_smooth(aes(weight = total), method = "loess", se = FALSE, size = 1.2) +
  # geom_point(aes(shape = form)) +
  # scale_shape_manual(name = "", values = c(20, 1), guide = "none") +
  # scale_linetype_discrete(guide = "none") +
  scale_x_continuous(name = "Age",
                     breaks = NULL,
                     limits = c(amin, amax + 1.5)) +
  scale_y_continuous(name = "Proportion of children",
                     # limits = c(-0.01, 1),
                     breaks = NULL) +
  langcog::scale_colour_solarized(guide = "none") +
  langcog::scale_fill_solarized(guide = "none") +
  directlabels::geom_dl(method = list(directlabels::dl.trans(x = x + 0.1),
                                      "last.qp", cex = 1, fontfamily = font)) +
  theme(plot.margin = margin(0))

ggsave("item_trajectories_thumb.png", width = 250*5, height = 180*5, units = "px")
# file.copy("item_trajectories_thumb.png",
#           "~/projects/wordbank/wordbank/static/images/sample_plots",
#           overwrite = TRUE)
