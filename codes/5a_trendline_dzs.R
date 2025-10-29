data<- dat


data <- dat %>%
  mutate(CT_YEAR = as.numeric(CT_YEAR)) %>%
  filter(CT_YEAR > 2004 & CT_YEAR < 2025) %>%
  select(-c(4, 6, 7, 8, 18, 19))


data <- data %>%
  mutate(DISEASE = ifelse(grepl("&", Diseases), "multi_disease", Diseases)) %>%
  select (-(24:36))

######### trend line for diseases ###########

data <- data %>%
  mutate(CT_YEAR = as.factor(CT_YEAR)) %>%
  filter(!DISEASE == "OTHER") %>%
  group_by(CT_YEAR, DISEASE) %>%
  summarise(count = n())%>%
  arrange(count)

rank <- data %>% 
       group_by(DISEASE) %>%
       summarise(Total = sum(count))%>%
        arrange(desc(Total))


###### line plot #########
data$DISEASE <- factor(data$DISEASE, levels = rank$DISEASE  )
palette7 <- brewer.pal(n = 7, name = "Set3")

p24<- ggplot(data, aes(x = CT_YEAR, y = count, color = DISEASE, group =DISEASE)) +
  geom_line() +
  labs(x = "Clinical Trial Start Year", y = "Number of Clinical Trails", color = "Disease") +
  theme_classic()+
  theme(
    legend.position = "right",
    legend.text = element_text(size = 10),  # Set legend text size to 10
    legend.title = element_text(size = 0),
    axis.text.x = element_text(angle = 90, size = 12, color = "black", face = "bold"),
    axis.text.y = element_text(size = 12, color = "black", face = "bold"),
    axis.title.x = element_text(size = 10, color = "black", face = "bold"),
    axis.title.y = element_text(size = 12, color = "black", face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  ) + scale_y_continuous(
    breaks = seq(0, max(data$count), by = 500),  # Major breaks
    minor_breaks = seq(0, max(data$count), by = 250)  # Minor breaks
  ) 


p24



p25<- ggplot(data, aes(x = CT_YEAR, y = count, color = DISEASE, group =DISEASE)) +
  geom_line() +
  labs(x = "Clinical Trial Start Year", y = "Number of Clinical Trails", color = "Disease") +
  theme_classic()+
  theme(
    legend.position = "top",
    legend.text = element_text(size = 7, face = "bold"),  # Set legend text size to 10
    legend.title = element_text(size = 0),
    axis.text.x = element_text(angle = 0, size = 0, color = "white"),
    axis.text.y = element_text(size = 0, color = "white"),
    axis.title.x = element_text(size = 0, color = "white"),
    axis.title.y = element_text(size = 0, color = "white"),
    panel.border = element_rect(colour = "white", fill = NA, size = 0)
  )

p24
p25

library(cowplot)
plot_grid(p24, p25, labels = c('', ''), label_size = 12 , 
          ncol=1, rel_heights = c(1,0.15))