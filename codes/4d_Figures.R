################## Figure 2 (study status and number), this should be colored bar plot #######################
data <- dat 

data2 <- data %>%
  filter(UNIQ_COUNTRY_COUNT==1)  %>%
  #rename("STUDY_TYPE" = 14)%>%
  group_by(UNIQ_COUNTRY)%>%
  summarise(count = n())%>%
  arrange(count)%>%
  top_n(20)


########### figure 1 #################
data2$UNIQ_COUNTRY <- factor(data2$UNIQ_COUNTRY, levels = data2$UNIQ_COUNTRY)
f17 <- ggplot(data2, aes(x = UNIQ_COUNTRY, y = count)) +
  geom_bar(stat = "identity") +
  labs(y = "Number of single country Clinical Trial", x = "COUNTRY") +
  theme_classic() +
  coord_flip() +
  #scale_y_log10()+
  geom_text(aes(label = count),angle = 270, hjust = +0.5, vjust = -0.05, colour = "black", size=2.5) +
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
f17


