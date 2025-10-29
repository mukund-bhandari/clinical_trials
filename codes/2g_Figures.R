################## Figure 2 (study status and number), this should be colored bar plot #######################
data <- dat 


data <- data %>%
  rename("Funder_Type" = 13)%>%
  mutate(Funder_Type = 
           ifelse(is.na(Funder_Type) | Funder_Type == "", "NA",
                  Funder_Type))


data2 <- data %>%
  mutate(Funder_Type = recode(Funder_Type, 
                              "AMBIG" = "UNKNOWN", 
                              "INDIV" = "INDIVIDUAL",
                              "NA"="UNKNOWN"))%>%
  group_by(Funder_Type)%>%
  summarise(count = n()) %>%
  arrange(count)


data2 <- data2 %>% filter(! Funder_Type %in% c("OTHER", "UNKNOWN"))
data2 <- data2 %>%mutate(Funder_Type = fct_drop(Funder_Type))

data2$Funder_Type <- factor(data2$Funder_Type, 
                    levels = c( "OTHER","UNKNOWN","INDIVIDUAL", "NETWORK", "FED", "NIH",  "OTHER_GOV",
                               "INDUSTRY"
                                
                    ))

########### figure 1 #################
f9 <- ggplot(data2, aes(x = Funder_Type, y = count)) +
  geom_bar(stat = "identity") +
  labs(y = "Number of Clinical Trial", x = "Funder_Type") +
  theme_classic() +
  coord_flip() +
  geom_text(aes(label = count),angle = 270, hjust = +0.8, vjust = -0.2, colour = "black", size=5) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 0, size = 12, color="black", face="bold"), # Increase x tick size
    axis.text.y = element_text(size = 12, color="black", face ="bold"), # Increase y tick size
    axis.title.x = element_text(size = 12, color="black", face="bold"), # Increase x label size
    axis.title.y = element_text(size = 12, color="black", face="bold"),  # Increase y label size
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  )
f9
