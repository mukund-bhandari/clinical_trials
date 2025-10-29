data <- dat 

#data <- data %>% filter(!is.na(Phases) )
data <- data %>%
  mutate(Phases = ifelse(is.na(Phases) | Phases == "", "NA", Phases))

data <- data %>%mutate(Phases = fct_relevel(Phases, "NA"))
data <- data %>%
        filter(!Phases =="NA")
#data <- data %>% mutate(Phases = fct_drop(Phases))
table(data$Diseases)
######
p11<-ggplot(data, aes(x = Phases, y = Enrollment)) +
  geom_boxplot() +
  labs(title = "",x = "Phases", y = "Enrollment") +
  scale_y_log10()+
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), vjust = -1, hjust= 0.9, color = "black", size = 4) + 
  theme_classic() +
  #coord_flip() +
  #geom_text(aes(label = count),angle = 0, hjust = -0.1, colour = "black", size=2.5) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, size = 12, color="black", face="bold"), # Increase x tick size
    axis.text.y = element_text(size = 12, color="black", face ="bold"), # Increase y tick size
    axis.title.x = element_text(size = 0, color="black", face="bold"), # Increase x label size
    axis.title.y = element_text(size = 12, color="black", face="bold"),  # Increase y label size
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  )

p11



p11 <- ggplot(data, aes(x = Phases, y = Enrollment)) +
  geom_boxplot() +
  labs(title = "", x = "Phases", y = "Enrollment") +
  scale_y_log10(breaks = c(10, 1000, 100000), labels = c("10", "1,000", "100,000")) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), 
               vjust = -2, hjust= 0.9, color = "black", size = 4) + 
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, size = 12, color="black", face="bold"),
    axis.text.y = element_text(size = 12, color="black", face ="bold"),
    axis.title.x = element_text(size = 0, color="black", face="bold"),
    axis.title.y = element_text(size = 12, color="black", face="bold"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  )

p11

