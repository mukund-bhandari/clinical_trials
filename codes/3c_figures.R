# create new dataframe disease by disease and make bar plot (disease vs count)
data<- dat
data <- data %>% select (1, 11, 14, 16)
data <- data %>%
  mutate(Study.Type = ifelse(is.na(Study.Type) | Study.Type == "", "NA", Study.Type))

data <- data %>% filter(Study.Type == "INTERVENTIONAL")
data <- data %>%
  mutate(Phases = ifelse(is.na(Phases) | Phases == "", "NA", Phases)) %>%
  filter(! Phases == "NA")%>%
  filter (Primary_Completion_Days > 1)

ggplot(data, aes(x=Phases, 
                 y=Primary_Completion_Days))+
  geom_boxplot()+
  labs(y = "Primary_Completion_Days", x = "") +
  theme_classic() +
  #coord_flip() +
  #geom_text(aes(label = Primary_Completion_Days), hjust = -0.1, colour = "black", size = 2.5) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, size = 8, color = "black", face = "bold"),
    axis.text.y = element_text(size = 6, color = "black", face = "bold"),
    axis.title.x = element_text(size = 10, color = "black", face = "bold"),
    axis.title.y = element_text(size = 12, color = "black", face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  )+
  scale_y_continuous(
    breaks = seq(0, 20000, by = 1000),  # Major breaks
    minor_breaks = seq(0, 20000, by = 500)  # Minor breaks
  )


############
# create new dataframe disease by disease and make bar plot (disease vs count)
data<- dat
data <- data %>% select (1,2, 11, 14, 16, 17)
data <- data %>%
  mutate(Study.Type = ifelse(is.na(Study.Type) | Study.Type == "", "NA", Study.Type))

data <- data %>% filter(Study.Type == "INTERVENTIONAL")
data <- data %>%
  mutate(Phases = ifelse(is.na(Phases) | Phases == "", "NA", Phases)) %>%
  filter(! Phases == "NA")%>%
  filter (Primary_Completion_Days > 1)

ggplot(data, aes(x = Phases, y = Primary_Completion_Days)) +
  #geom_point(position = position_jitter(width = 0.2), size = 0.5) +  # Adjust jitter width and point size
  geom_boxplot(width = 0.8, outlier.shape = NA, alpha = 0.9) +  # Adjust width and set outlier.shape to NA
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), vjust = -1, hjust= 0.9, color = "black", size = 4) +  # Add mean text
  labs(y = "Days_to_Primary_Completion (Days)", x = "") +
  theme_classic() +
  theme(
    legend.position = "none",
    legend.text = element_text(size = 10),  # Set legend text size to 10
    legend.title = element_text(size = 12),
    axis.text.x = element_text(angle = 90, size = 12, color = "black", face = "bold"),
    axis.text.y = element_text(size = 12, color = "black", face = "bold"),
    axis.title.x = element_text(size = 10, color = "black", face = "bold"),
    axis.title.y = element_text(size = 12, color = "black", face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  ) +
  scale_y_log10()



