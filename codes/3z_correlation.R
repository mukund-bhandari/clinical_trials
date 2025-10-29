# create new dataframe disease by disease and make bar plot (disease vs count)
data<- dat
data <- data %>% select (1,2, 11, 14, 16, 17)
data <- data %>%
  mutate(Study.Type = ifelse(is.na(Study.Type) | Study.Type == "", "NA", Study.Type))

data <- data %>% filter(Study.Type == "INTERVENTIONAL")
data <- data %>%
  mutate(Phases = ifelse(is.na(Phases) | Phases == "", "NA", Phases)) %>%
  filter(! Phases == "NA")%>%
  filter (Primary_Completion_Days > 1)%>%
  filter (Completion_Days > 1)


x<-ggplot(data, aes(x=Primary_Completion_Days, 
                 y=Completion_Days, color = Phases))+
  geom_point(size = 0.2)+
  scale_x_log10()+
  scale_y_log10()


data <- data %>%
  mutate(Difference = Completion_Days - Primary_Completion_Days)

################ plot ####################
x<-ggplot(data, aes(x = Phases, y = Difference)) +
  #geom_point(position = position_jitter(width = 0.2), size = 0.5) +  # Adjust jitter width and point size
  geom_boxplot(width = 0.8, outlier.shape = NA, alpha = 0.9) +  # Adjust width and set outlier.shape to NA
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)), vjust = -0.9, hjust= 0.9, color = "red", size = 3) +  # Add mean text
  labs(y = "Difference (Days)", x = "") +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, size = 8, color = "black", face = "bold"),
    axis.text.y = element_text(size = 6, color = "black", face = "bold"),
    axis.title.x = element_text(size = 10, color = "black", face = "bold"),
    axis.title.y = element_text(size = 12, color = "black", face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  ) +
  scale_y_log10()


########## has study record ##############
################## Figure 2 (study status and number), this should be colored bar plot #######################
data <- dat 
data <- data %>%mutate(STUDY_DOCUMENTS = ifelse(is.na(STUDY_DOCUMENTS) | STUDY_DOCUMENTS == "", "NA", STUDY_DOCUMENTS))
table(data$STUDY_DOCUMENTS)


data2 <- data %>%
  group_by(STUDY_DOCUMENTS)%>%
  summarise(count = n()) #%>%
  #arrange(desc(count))%>%
  #top_n(10)


data2$STUDY_DOCUMENTS <- factor(data2$STUDY_DOCUMENTS, levels = data2$STUDY_DOCUMENTS)


data2
table(data2$STUDY_DOCUMENTS)
# Compute fractions and percentages
data2$fraction <- data2$count / sum(data2$count)
data2$percentage <- data2$fraction * 100
# Compute the cumulative percentages (top of each rectangle)
data2$ymax <- cumsum(data2$fraction)
# Compute the bottom of each rectangle
data2$ymin <- c(0, head(data2$ymax, n=-1))
# Compute label position
data2$labelPosition <- (data2$ymax + data2$ymin) / 2
# Create a label specifically for the percentages
#data2$percentLabel <- data2$count
data2$percentLabel <- sprintf("%.1f%%", data2$percentage)  # format for one decimal place
# Set color values for each category, ensuring order matches factor levels
#colors <- c("USA based" = "#F8766D", "Non-USA based" = "#00BFC4")
# Make the plot
p2 <- ggplot(data2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=STUDY_DOCUMENTS)) +
  geom_rect() +  # creates the sections of the doughnut chart
  #scale_fill_manual(values = colors, name = "Filing") +  # set the colors for each category
  # Add the text (percentage) within the slices. Position them with proper 'x' and 'y' values.
  geom_text(aes(x = 3.5, y = labelPosition, label = percentLabel), color = "black", size=4, angle = 90) +
  coord_polar(theta="y") +  # transforms the bar chart into a doughnut chart
  xlim(c(2, 4)) +  # adjusts the space, creating the doughnut hole
  theme_void() +  # removes axes, background, etc.
  #ggtitle("FDA cleared AI/ML enabled medical Devices") +  # title of the chart
  theme(plot.title = element_text(hjust = 0.5)) +  # centers the title
  theme(legend.position = "bottom") + # places the legend on the right side
  guides(fill = guide_legend(ncol = 2)) 

p2

####################
library(RColorBrewer)
palette7 <- brewer.pal(n = 7, name = "Set3")
library(viridis)
palette7 <- viridis(7)

p2 <- ggplot(data2, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = STUDY_DOCUMENTS)) +
  geom_rect() +  # creates the sections of the doughnut chart
  geom_text(aes(x = 3.5, y = labelPosition, label = percentLabel), color = "black", size = 2, angle = 90) +
  coord_polar(theta = "y") +  # transforms the bar chart into a doughnut chart
  xlim(c(2, 4)) +  # adjusts the space, creating the doughnut hole
  theme_void() +  # removes axes, background, etc.
  scale_fill_manual(
    values = palette7, 
    name = "Filing", 
    guide = guide_legend(ncol = 2)  # Splits the legend into 2 columns
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),  # centers the title
    legend.position = "bottom"  # places the legend at the bottom
  )


