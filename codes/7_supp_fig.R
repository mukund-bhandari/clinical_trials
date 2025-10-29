library(readxl)
library(tidyverse)
library(ggplot2)
library(ggprism)

data <- dat
dim(data)

data <- data %>% filter(UNIQ_COUNTRY_COUNT == 1)
dim(data)

#### now add new column for continent #####
uniq_country <- read_xlsx("country_continent_key.xlsx")


df_merged <- merge(data, uniq_country[, c("UNIQ_COUNTRY", "Continent", "Continenet_add")], by = "UNIQ_COUNTRY", all.x = TRUE)
dim(df_merged)

df_merged <- df_merged[c(setdiff(names(df_merged), "UNIQ_COUNTRY"), "UNIQ_COUNTRY")]
dim(df_merged)

df_merged <- df_merged %>%
  mutate(DISEASE = ifelse(grepl("&", Diseases), "multi_disease", Diseases)) 



df_merged$Continent = as.factor(df_merged$Continent)
df_merged$DISEASE = as.factor(df_merged$DISEASE)

df_merged <- df_merged %>%
        filter(DISEASE != "OTHER") %>%
         filter(Continent != "Unknown")
   
df_merged<- df_merged %>% 
  mutate(Continent = fct_drop(Continent))  %>%
  mutate(DISEASE = fct_drop(DISEASE)) 

df1<- as.data.frame(table(df_merged$Continent, df_merged$DISEASE))
df1 <- df1 %>% rename(Continents = 1, DISEASE = 2, Number_of_Trials=3)



rank <- df1 %>% 
  group_by(Continents) %>%
  summarise(Total = sum(Number_of_Trials))%>%
  arrange(desc(Total))

rank2 <- df1 %>% 
  group_by(DISEASE) %>%
  summarise(Total = sum(Number_of_Trials))%>%
  arrange((Total))

df1$Continents <- factor(df1$Continents, levels = rank$Continents)
df1$DISEASE <- factor(df1$DISEASE, levels = rank2$DISEASE)




p3<- ggplot(df1, aes(factor(Continents), as.numeric(Number_of_Trials), 
                     fill=DISEASE))+
  geom_col()+
  #coord_flip()+
  theme_prism()+
  ylab("Number of Clinical Trials") + xlab("Continents")+
  theme_classic() +
  #coord_flip() +
  #scale_y_log10()+
  #geom_text(aes(label = count),angle = 270, hjust = +0.5, vjust = -0.05, colour = "black", size=4) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 10),  # Set legend text size to 10
    legend.title = element_text(size = 0),
    axis.text.x = element_text(angle = 90, size = 12, color="black", face="bold"), # Increase x tick size
    axis.text.y = element_text(size = 12, color="black", face ="bold"), # Increase y tick size
    axis.title.x = element_text(size = 12, color="black", face="bold"), # Increase x label size
    axis.title.y = element_text(size = 12, color="black", face="bold"),  # Increase y label size
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  )


p3
