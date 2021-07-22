library(tidyverse)
library(patchwork)
library(haven)

setwd("~/Desktop/pha-margins")

model_1 <- read_dta("./model_1_margins.dta")[5:11,] %>% mutate(model = 1)
model_2 <- read_dta("./model_2_margins.dta")[5:11,] %>% mutate(model = 2)
model_3 <- read_dta("./model_3_margins.dta")[5:11,] %>% mutate(model = 3)

margins <- bind_rows(model_1, model_2, model_3) %>%
  rename(race_eth = v1, pred = v2, se = v3) %>%
  mutate(pred = parse_number(pred),
         se = parse_number(se),
         pred = ifelse(model == 3, pred/100, pred),
         se = ifelse(model == 3, se/100, se),
         race_eth = str_split_fixed(race_eth, "= [0-9]\\,", n = 2)[,2],
         race_eth = str_trim(race_eth),
         race_eth = ifelse(race_eth == "", "Non-Hispanic Black", race_eth),
         race_eth = ifelse(race_eth == "Non-Hispanic Native", "Non-Hispanic American Indian", race_eth),
         race_eth = str_replace_all(race_eth, "Non-Hispanic", "Non-Latine"),
         race_eth = ifelse(race_eth == "Hispanic, Any Race", "Latine, Any Race", race_eth),
         model_name = case_when(
           model == 1 ~ "Model 1: Probability of Moving Each Month",
           model == 2 ~ "Model 2: Isolation Index",
           model == 3 ~ "Model 3: Percentage of People in Tract Living in Poverty"
         )) 

Black, White, Asian, Latine, American Indian, Pacific Islander, Multi-Racial.

margins$race_eth <- factor(margins$race_eth)
margins$race_eth <- factor(margins$race_eth, levels = levels(margins$race_eth)[c(4, 7, 3, 1, 2, 6, 5)])



model_1_plot <- ggplot(margins %>% filter(str_detect(model, "1")), 
                  aes(x = race_eth, y = pred, 
                      ymin = pred - 1.96 * se, ymax = pred + 1.96 * se)) +
  facet_grid(~ model_name, scales = "free_y") +
  geom_errorbar(width = .5) +
  geom_point() +
  scale_x_discrete(position = "top") +
  scale_y_continuous(breaks = c(.006, .007, .008, .009, .01),
                     labels = paste0(seq(.6, 1.0, .1), "%")) +
  labs(x = "", y = "\nPredicted Probability") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 315, hjust = 1, vjust = 0,
                                 size = 12), 
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())

model_2_plot <- ggplot(margins %>% filter(str_detect(model, "2")), 
                  aes(x = race_eth, y = pred, 
                          ymin = pred - 1.96 * se, ymax = pred + 1.96 * se)) +
  facet_grid(~ model_name, scales = "free_y") +
  geom_errorbar(width = .5) +
  geom_point() +
  scale_y_continuous() +
  guides(x = NULL) +
  labs(x = "", y = "\nPredicted Index Score") +
  theme_bw() +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())

model_3_plot <- ggplot(margins %>% filter(str_detect(model, "3")), 
                  aes(x = race_eth, y = pred, 
                      ymin = pred - 1.96 * se, ymax = pred + 1.96 * se)) +
  facet_grid(~ model_name, scales = "free_y") +
  geom_errorbar(width = .5) +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "\nPredicted Poverty Rate") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1,
                                   size = 12))

model_plot <- (model_1_plot / model_2_plot / model_3_plot) 

model_plot

ggsave(filename = "./pha-margins-plot.pdf", model_plot,
         width = 8, height = 10, dpi = 300)

