################## Figure 2 (study status and number), this should be colored bar plot #######################
data <- dat 

data2 <- data %>%
  rename("Multi_National_Trial" = 29)%>%
  group_by(Multi_National_Trial)%>%
  summarise(count = n())%>%
  arrange(count)

data2
########## make pie chart ################

df <- data2 %>%
  mutate(percentage = count / sum(count) * 100)

# Create the pie chart
ggplot(df, aes(x = "", y = percentage, fill = Multi_National_Trial)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))+
  #labs(title = "Pie Chart of Collaboration") +
  #theme(legend.title = element_blank()) +
  geom_text(aes(label = paste0(round(percentage, 1), "%\n(", count, ")")), 
            position = position_stack(vjust = 0.9), hjust=1.1, angle= 0, size =5)
  #geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5))