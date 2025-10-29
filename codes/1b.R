################## Figure 2 (study status and number), this should be colored bar plot #######################
data2 <- dat %>%
   rename("Study_Status" = 2)%>%
   mutate(Study_Status = ifelse(is.na(Study_Status) | Study_Status == "", "NA", Study_Status))
table(data2$Study_Status)

data2 <- data2 %>%
  group_by(Study_Status)%>%
  summarise(count = n())%>%
  arrange(count)


data2 <- data2 %>% arrange(count)
data2$Study_Status <- factor(data2$Study_Status, levels = data2$Study_Status)


########### figure 1 #################
f2 <- ggplot(data2, aes(x = Study_Status, y = count)) +
  geom_bar(stat = "identity") +
  labs(y = "Number of Clinical Trial", x = "Study Status") +
  theme_classic() +
  coord_flip() +
  geom_text(aes(label = count),angle = 270, hjust = 0.1, vjust = -0.2, colour = "black", size=3) +
  theme(
    legend.position = "none",
    legend.text = element_text(size = 10),  # Set legend text size to 10
    legend.title = element_text(size = 12),
    axis.text.x = element_text(angle = 0, size = 12, color="black", face="bold"), # Increase x tick size
    axis.text.y = element_text(size = 12, color="black", face ="bold"), # Increase y tick size
    axis.title.x = element_text(size = 10, color="black", face="bold"), # Increase x label size
    axis.title.y = element_text(size = 12, color="black", face="bold"),  # Increase y label size
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  )

f2


