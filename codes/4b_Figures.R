################## Figure 2 (study status and number), this should be colored bar plot #######################
data <- dat 

data <- data %>%
  mutate(Age = ifelse(is.na(Age) | Age == "", "NA", Age))


data2 <- data %>%
  #rename("Participant_sex" = 9)%>%
  group_by(Age)%>%
  summarise(count = n())%>%
  arrange(count)

data2

data2$Age <- factor(data2$Age, 
                    levels = c("NA", "CHILD", "CHILD, ADULT",
                               "ADULT", "ADULT, OLDER_ADULT",
                               "OLDER_ADULT", "CHILD, ADULT, OLDER_ADULT"
                               ))

########### figure 1 #################
f7 <- ggplot(data2, aes(x = Age, y = count)) +
  geom_bar(stat = "identity") +
  labs(y = "Number of Clinical Trial", x = "Age") +
  theme_classic() +
  coord_flip() +
  geom_text(aes(label = count),angle = 270, hjust = +0.5, vjust = -0.05, colour = "black", size=5) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 0, size = 12, color="black", face="bold"), # Increase x tick size
    axis.text.y = element_text(size = 12, color="black", face ="bold"), # Increase y tick size
    axis.title.x = element_text(size = 12, color="black", face="bold"), # Increase x label size
    axis.title.y = element_text(size = 0, color="black", face="bold"),  # Increase y label size
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  )
f7
