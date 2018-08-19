## Must Run Cleaning and Clustering Fist 

## Getting the data into a bar chart format
bars <- cluster %>% filter(reltrad !=0) %>% 
  mutate(reltrad = as.numeric(reltrad)) %>% 
  mutate(reltrad = recode(reltrad, "1= 'Evangelical'; 2 = 'Mainline'; 3 = 'B. Prot.'; 4 = 'Catholic'; 5 = 'Jewish'; 6 = 'Other Faith'; 7 = 'No Faith'")) %>% 
  mutate(clusters = recode(clusters, "1= 'Cluster 1'; 2 = 'Cluster 2'; 3 ='Cluster 3'; 4= 'Cluster 4'; 5 = 'Cluster 5'; 6 ='Cluster 6'; 7 = 'Cluster 7'")) %>% 
  tabyl(reltrad, clusters) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>% 
  melt()

## Round variables 
bars <- bars %>% mutate(value = round(value, 3))

## Making the Bar Charts 

bars %>% 
  filter(reltrad == "Evangelical") %>% 
  ggplot(., aes(x=variable, y= value)) + geom_col(fill = "azure3", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  labs(x= "", y = "Percent", title = "Evangelical Protestants") +
  geom_text(aes(y = value + .015, label = paste0(value *100, '%')), position = position_dodge(width = .9), size = 16)


pal <- c("azure3", "black", "gray87")


bars %>% 
  filter(reltrad == "Mainline" | reltrad == "Catholic") %>% 
  ggplot(., aes(x=variable, y= value, fill = reltrad)) + geom_col(position= "dodge", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = pal) +
  labs(x= "", y = "Percent", title = "Catholics and Mainline Protestants") +
  geom_text(aes(y = value + .015, label = paste0(value *100, '%')), position = position_dodge(width = .9), size = 16)




bars %>% 
  filter(reltrad == "B. Prot.") %>% 
  ggplot(., aes(x=variable, y= value)) + geom_col(fill = "azure3", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  labs(x= "", y = "Percent", title = "Black Protestants") +
  geom_text(aes(y = value + .015, label = paste0(value *100, '%')), position = position_dodge(width = .9), size = 16)


bars %>% 
  filter(reltrad == "Jewish") %>% 
  ggplot(., aes(x=variable, y= value)) + geom_col(fill = "azure3", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  labs(x= "", y = "Percent", title = "Jewish") +
  geom_text(aes(y = value + .015, label = paste0(value *100, '%')), position = position_dodge(width = .9), size = 16)


bars %>% 
  filter(reltrad == "Other Faith" | reltrad == "No Faith") %>% 
  ggplot(., aes(x=variable, y= value, fill = reltrad)) + geom_col(position= "dodge", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = pal) +
  labs(x= "", y = "Percent", title = "No Faith and Other Faith") +
  geom_text(aes(y = value + .015, label = paste0(value *100, '%')), position = position_dodge(width = .9), size = 16)




