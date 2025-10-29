data <- dat 

data <- data %>% 
        rename("Study_Type"=14) %>% 
        #filter (!Study_Type == "OBSERVATIONAL") #%>%
        filter (Study_Type == "INTERVENTIONAL") 

table(data$INT_Allocation)

######### now make ##########
data <- data %>%
  mutate(INT_Allocation = ifelse(is.na(INT_Allocation) | INT_Allocation == "", "NA", INT_Allocation))

data2 <- data %>%
  group_by(INT_Allocation)%>%
  #filter(!is.na(INT_Allocation) )%>%
  summarise(count = n())


################### donought plot ##################
# Compute fractions and percentages

data2$INT_Allocation <- factor(data2$INT_Allocation, levels = c("RANDOMIZED", "NON_RANDOMIZED", "NA"))

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
data2$percentLabel <- sprintf("%.2f%%", data2$percentage)  # format for one decimal place
data2$percentLabel <- sprintf("(%.1f%%)\n%d", data2$percentage, data2$count) 
# Set color values for each category, ensuring order matches factor levels
#colors <- c("USA based" = "#F8766D", "Non-USA based" = "#00BFC4")
# Make the plot
f2c <- ggplot(data2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=INT_Allocation)) +
  geom_rect() +  # creates the sections of the doughnut chart
  #scale_fill_manual(values = colors, name = "Filing") +  # set the colors for each category
  # Add the text (percentage) within the slices. Position them with proper 'x' and 'y' values.
  geom_text(aes(x = 3.5, y = labelPosition, label = percentLabel), color = "black", size=5) +
  coord_polar(theta="y") +  # transforms the bar chart into a doughnut chart
  xlim(c(2, 4)) +  # adjusts the space, creating the doughnut hole
  theme_void() +  # removes axes, background, etc.
  #ggtitle("FDA cleared AI/ML enabled medical Devices") +  # title of the chart
  theme(plot.title = element_text(hjust = 0.5)) +  # centers the title
  theme(legend.position = "right", 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12)
  ) 


f2c


