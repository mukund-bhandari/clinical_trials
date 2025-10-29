######## create dataframe for top 13 diseases #########
data_temp <- dat %>% select(30:42)


# Initialize an empty data frame to store the results
result <- data.frame(Column = character(), Count = numeric(), stringsAsFactors = FALSE)

# Loop through each column to count the occurrences of "HONI"
for (col_name in names(data_temp)) {
  count <- sum(data_temp[[col_name]] == "YES", na.rm = TRUE)
  result <- rbind(result, data.frame(Column = col_name, Count = count, stringsAsFactors = FALSE))
}


####
data_temp2 <- dat %>% select(30:43) %>% 
               filter(Diseases == "OTHER")
      
  
new_row <- data.frame(Column = "OTHER", Count = nrow(data_temp2))
data <- data.frame(rbind(result, new_row))

############## (bar graph) ##########

data <- data %>%
  rename("Diseases" = Column, "Number_of_Clinical_Trials" = Count) %>%
  arrange(Number_of_Clinical_Trials) 

data$Diseases <- factor(data$Diseases, levels = data$Diseases)

data <- data %>%
        mutate(Diseases = fct_relevel(Diseases, "OTHER"))

# Create the plot
f3 <- ggplot(data, aes(x = Diseases, y = Number_of_Clinical_Trials)) +
  geom_col() +
  labs(y = "Number of Clinical Trials", x = "Diseases") +
  theme_classic() +
  coord_flip() +
  geom_text(aes(label = Number_of_Clinical_Trials), hjust = 0.6, vjust=-0.6, colour = "black", size = 3, angle = 270) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 0, size = 10, color = "black", face = "bold"),
    axis.text.y = element_text(size = 10, color = "black", face = "bold"),
    axis.title.x = element_text(size = 12, color = "black", face = "bold"),
    axis.title.y = element_text(size = 12, color = "black", face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  )


f3

