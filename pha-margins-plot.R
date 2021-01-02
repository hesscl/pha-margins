library(tidyverse)
library(patchwork)

setwd("~/Desktop")

margins <- read_csv("./pha_margins.csv") %>%
  mutate(pred = parse_number(pred),
         pred = ifelse(str_detect(model, "1|3"), pred/100, pred),
         se = ifelse(str_detect(model, "3"), se/100, se))

margins$race_eth <- factor(margins$race_eth)
margins$race_eth <- factor(margins$race_eth, levels = levels(margins$race_eth)[c(7, 2, 5, 6, 1, 4, 3)])

model_1 <- ggplot(margins %>% filter(str_detect(model, "1")), 
                  aes(x = race_eth, y = pred, 
                      ymin = pred - 1.96 * se, ymax = pred + 1.96 * se)) +
  facet_grid(~ model, scales = "free_y") +
  geom_errorbar(width = .5) +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "\nPredicted Likelihood of Moving Homes") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.x = element_blank())

model_2 <- ggplot(margins %>% filter(str_detect(model, "2")), 
                  aes(x = race_eth, y = pred, 
                          ymin = pred - 1.96 * se, ymax = pred + 1.96 * se)) +
  facet_grid(~ model, scales = "free_y") +
  geom_errorbar(width = .5) +
  geom_point() +
  scale_y_continuous() +
  guides(x = NULL) +
  labs(x = "", y = "\nPredicted Z-Score for \n% Same Race/Ethnicity Neighbors in Tract") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.x = element_blank())

model_3 <- ggplot(margins %>% filter(str_detect(model, "3")), 
                  aes(x = race_eth, y = pred, 
                      ymin = pred - 1.96 * se, ymax = pred + 1.96 * se)) +
  facet_grid(~ model, scales = "free_y") +
  geom_errorbar(width = .5) +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "\nPredicted % of People in Poverty Tract") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

(model_1 / model_2 / model_3) + 
  ggsave(filename = "./pha_margins_plot.pdf",
         width = 10, height = 12, dpi = 300)
