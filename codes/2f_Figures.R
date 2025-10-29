################## Figure 2 (study status and number), this should be colored bar plot #######################
data <- dat 


data2 <- data %>%
  rename("STUDY_TYPE" = 14)%>%
  group_by(Sponsor)%>%
  summarise(count = n())%>%
  arrange(count)%>%
  top_n(20)

data2_O <- data %>%
  rename("STUDY_TYPE" = 14)%>%
  filter(STUDY_TYPE == "OBSERVATIONAL")%>%
  group_by(Sponsor)%>%
  summarise(count = n())%>%
  arrange(count)%>%
  top_n(20)

data2_I <- data %>%
  rename("STUDY_TYPE" = 14)%>%
  filter(STUDY_TYPE == "INTERVENTIONAL")%>%
  group_by(Sponsor)%>%
  summarise(count = n())%>%
  arrange(count)%>%
  top_n(20)

####################################
data2$Sponsor <- factor(data2$Sponsor, levels = data2$Sponsor)
data2_O$Sponsor <- factor(data2_O$Sponsor, levels = data2_O$Sponsor)
data2_I$Sponsor <- factor(data2_I$Sponsor, levels = data2_I$Sponsor)

################ plot 5 ###########

########### figure 1 #################
f5 <- ggplot(data2, aes(x = Sponsor, y = count)) +
  geom_bar(stat = "identity") +
  labs(y = "Number of Clinical Trial", x = "Top 20 Sponsors") +
  theme_classic() +
  coord_flip() +
  geom_text(aes(label = count),angle = 0, hjust = -0.1, colour = "black", size=2.5) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 0, size = 8, color="black", face="bold"), # Increase x tick size
    axis.text.y = element_text(size = 6, color="black", face ="bold"), # Increase y tick size
    axis.title.x = element_text(size = 10, color="black", face="bold"), # Increase x label size
    axis.title.y = element_text(size = 12, color="black", face="bold"),  # Increase y label size
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  )
f5


################ plot 5b ###########
f5_O <- ggplot(data2_O, aes(x = Sponsor, y = count)) +
  geom_bar(stat = "identity") +
  labs(y = "Number of Observational Clinical Trial", x = "Top 20 Sponsors") +
  theme_classic() +
  coord_flip() +
  geom_text(aes(label = count),angle = 0, hjust = -0.1, colour = "black", size=2.5) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 0, size = 8, color="black", face="bold"), # Increase x tick size
    axis.text.y = element_text(size = 6, color="black", face ="bold"), # Increase y tick size
    axis.title.x = element_text(size = 10, color="black", face="bold"), # Increase x label size
    axis.title.y = element_text(size = 12, color="black", face="bold"),  # Increase y label size
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  )
f5_O


################ plot 5b ###########
f5_I <- ggplot(data2_I, aes(x = Sponsor, y = count)) +
  geom_bar(stat = "identity") +
  labs(y = "Number of Interventional Clinical Trial", x = "Top 20 Sponsors") +
  theme_classic() +
  coord_flip() +
  geom_text(aes(label = count),angle = 270, hjust = 0.5,vjust=0, colour = "black", size=3) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 0, size = 8, color="black", face="bold"), # Increase x tick size
    axis.text.y = element_text(size = 12, color="black", face ="bold"), # Increase y tick size
    axis.title.x = element_text(size = 10, color="black", face="bold"), # Increase x label size
    axis.title.y = element_text(size = 12, color="black", face="bold"),  # Increase y label size
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  )
f5_I