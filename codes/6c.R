data<- dat
data <- dat %>%
  mutate(CT_YEAR = as.numeric(CT_YEAR)) %>%
  filter(CT_YEAR > 2004 & CT_YEAR < 2025) %>%
  select(-c(4, 6, 7, 8, 18, 19))

table(data$STUDY_DOCUMENTS)

data <- data %>%
  mutate(DISEASE = ifelse(grepl("&", Diseases), "multi_disease", Diseases)) %>%
  select(-c(3, 8, 12:19, 21, 22, 24:38))

######## fill NA and "" values as "NA"
for (i in 1:ncol(data)) {
  data[, i][is.na(data[, i]) | data[, i] == ""] <- "NA"
}

########## now plot the shankey plot ############
df <- data %>%
  rename("Study_Status"=2,
         "Funder_Type"=7,
         "Study_Type"=8)%>%
  select(-1)

table(df$Study_Status)

########## shankey plot #########
df <- df %>%
  mutate(Multi_Nation = ifelse(Multi_Nation == "Yes", 
                               "Multi", "Single"))


library(ggplot2)
library(dplyr)
library(devtools)
library(ggsankey)


set.seed(0809)
TotalCount = nrow(df)


##for ggshankey
df<-df%>%
  make_long(CT_YEAR,
            DISEASE,
            Multi_Nation,
            Multi_Location,
            Age,  
            Sex)

### tally
####
dagg <- df%>%
  dplyr::group_by(node)%>%
  tally()

dagg <- dagg%>%
  dplyr::group_by(node)%>%
  dplyr::mutate(pct = n/TotalCount)

# Step 3
df2 <- merge(df, dagg, by.x = 'node', by.y = 'node', all.x = TRUE)


########plot
pl <- ggplot(df2, aes(x = x
                      , next_x = next_x
                      , node = node
                      , next_node = next_node
                      , fill = factor(node)
                      , label = paste0(node,
                                       #" n=", n #, 
                                      '(',  round(pct* 100,2), '%)' 
                                       ))
)

pl <- pl +geom_sankey(flow.alpha = 0.5,  color = "gray40", show.legend = TRUE)
pl <- pl +geom_sankey_label(size = 3, color = "black", fill= "white", hjust = 0)

pl <- pl +  theme_bw()
pl <- pl + theme(legend.position = "none")
pl <- pl +  theme(axis.title = element_blank()
                  , axis.text.y = element_blank()
                  , axis.ticks = element_blank()  
                  , panel.grid = element_blank())
p23 <- pl + scale_fill_viridis_d(option = "inferno")

p23

