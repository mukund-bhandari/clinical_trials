################## Figure 1 #######################

############## number by year ##################
count_data <- dat %>% group_by(CT_YEAR) %>%
                   summarise(count = n())

########### figure 1 #################
f1 <- ggplot(count_data, aes(x = factor(CT_YEAR), y = count)) +
  geom_bar(stat = "identity") +
  labs(y = "Number of Clinical Trial", x = "Clinical Trial Start Year") +
  theme_classic() +
  #coord_flip() +
  geom_text(aes(label = count),angle = 0, hjust = 0.5, vjust= -1, colour = "black", size = 3) +
  theme(
    legend.position = "none",    
    legend.text = element_text(size = 10),  # Set legend text size to 10
    legend.title = element_text(size = 12),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 16), # Increase x tick size
    axis.text.y = element_text(size = 16), # Increase y tick size
    axis.title.x = element_text(size = 18), # Increase x label size
    axis.title.y = element_text(size = 18),  # Increase y label size
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
  )

f1

#ggsave("plot1.pdf", plot=f1, dpi = 300)
#ggsave("plot1.pdf", plot = f1, height = 12, width = 30, units = "in")  



