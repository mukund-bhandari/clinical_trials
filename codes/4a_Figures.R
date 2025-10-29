################## Figure 2 (study status and number), this should be colored bar plot #######################
data <- dat 
table(data$Sex)

data <- data %>%mutate(Sex = ifelse(is.na(Sex) | Sex == "", "NA", Sex))
table(data$Sex)


data2 <- data %>%
  #rename("Participant_sex" = 9)%>%
  group_by(Sex)%>%
  summarise(count = n()) #%>%
  #arrange(count)%>%
  #top_n(4)


data2$Sex <- factor(data2$Sex, levels = data2$Sex)


data2
table(data2$Sex)
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
data2$percentLabel <- data2$count
data2$percentLabel <- sprintf("%.1f%%", data2$percentage)  # format for one decimal place

#data2$percentLabel <- sprintf("%.2f%%", data2$percentage)  # format for one decimal place
data2$percentLabel <- sprintf("%d(%.2f%%)", data2$count,data2$percentage) 
# Set color values for each category, ensuring order matches factor levels
#colors <- c("USA based" = "#F8766D", "Non-USA based" = "#00BFC4")
# Make the plot
p2 <- ggplot(data2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Sex)) +
  geom_rect() +  # creates the sections of the doughnut chart
  #scale_fill_manual(values = colors, name = "Filing") +  # set the colors for each category
  # Add the text (percentage) within the slices. Position them with proper 'x' and 'y' values.
  geom_text(aes(x = 3.5, y = labelPosition, label = percentLabel), color = "black", size=4.5, angle = 85, hjust = 0.5, vjust=0.5) +
  coord_polar(theta="y") +  # transforms the bar chart into a doughnut chart
  xlim(c(2, 4)) +  # adjusts the space, creating the doughnut hole
  theme_void() +  # removes axes, background, etc.
  #ggtitle("FDA cleared AI/ML enabled medical Devices") +  # title of the chart
  theme(plot.title = element_text(hjust = 0.5)) +  # centers the title
  theme(legend.position = "right", 
        legend.text = element_text(size = 10),  # Set legend text size to 10
        legend.title = element_text(size = 12)
        )  # places the legend on the right side


p2