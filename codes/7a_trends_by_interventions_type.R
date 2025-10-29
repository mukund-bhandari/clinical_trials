library(readxl)
library(tidyverse)
library(ggplot2)
library(ggprism)

data <- dat
dim(data)

#### Type of Interventions by year ####
data <- data %>%
  dplyr::filter(CT_YEAR != 2025)

data <- data %>%
  mutate(CT_YEAR = as.factor(CT_YEAR)) %>%
  #filter(!DISEASES == "OTHER") %>%
  group_by(CT_YEAR, INTERVENTIONS) %>%
  summarise(count = n())%>%
  arrange(count)

rank <- data %>% 
  group_by(INTERVENTIONS) %>%
  summarise(Total = sum(count))%>%
  arrange(desc(Total))


###### line plot #########
data$INTERVENTIONS <- factor(data$INTERVENTIONS, levels = rank$INTERVENTIONS  )
palette7 <- brewer.pal(n = 7, name = "Set3")

p24<- ggplot(data, aes(x = CT_YEAR, y = count, color = INTERVENTIONS, group =INTERVENTIONS)) +
  geom_line() +
  labs(x = "Clinical Trial Start Year", y = "Number of Clinical Trails", color = "INTERVENTIONS") +
  theme_classic()+
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),  # Set legend text size to 10
    legend.title = element_text(size = 12),
    axis.text.x = element_text(angle = 90, size = 12, color = "black", face = "bold"),
    axis.text.y = element_text(size = 12, color = "black", face = "bold"),
    axis.title.x = element_text(size = 12, color = "black", face = "bold"),
    axis.title.y = element_text(size = 12, color = "black", face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  ) + scale_y_continuous(
    breaks = seq(0, max(data$count), by = 500),  # Major breaks
    minor_breaks = seq(0, max(data$count), by = 250)  # Minor breaks
  ) 



p24



