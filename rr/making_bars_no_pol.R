bars <- cluster %>% filter(reltrad !=0) %>% 
  mutate(reltrad = as.numeric(reltrad)) %>% 
  mutate(reltrad = recode(reltrad, "1= 'Evangelical'; 2 = 'Mainline'; 3 = 'B. Prot.'; 4 = 'Catholic'; 5 = 'Jewish'; 6 = 'Other Faith'; 7 = 'No Faith'")) %>% 
  mutate(clusters = recode(clusters, "1= 'Cluster 1'; 2 = 'Cluster 2'; 3 ='Cluster 3'; 4= 'Cluster 4'; 5 = 'Cluster 5'; 6 ='Cluster 6'; 7 = 'Cluster 7'")) %>% 
  tabyl(reltrad, clusters) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>% 
  melt()


bar_rb <- function(base_size = 12, base_family = "Product Sans") 
{theme(legend.position = "bottom", 
       legend.title = element_blank(), 
       legend.spacing.x = unit(1, "cm"),
       legend.spacing.y = unit(1, "cm"),
       panel.background = element_rect(fill = "white"), 
       panel.grid.major.y =  element_line(colour = "gray48", size = .25), 
       panel.grid.minor.y =  element_line(colour = "gray48", size = .25, linetype = "dashed"),
       text = element_text(base_family, size = 12),
       plot.title = element_text(family = "Product Sans", size = 14, vjust =2, face = "bold"),
       plot.subtitle = element_text(family = "Product Sans", size = 12, vjust =-1),
       plot.caption = element_text(family = "Product Sans", size =12),
       axis.title.x =  element_text(family = "Product Sans", size =12),
       axis.title.y =  element_text(family = "Product Sans", size =12), 
       axis.text.x = element_text(family = "Product Sans", size =12, angle = 45, vjust = .95, hjust = 1)
)
  
}

bars <- bars %>% mutate(value = round(value, 3))

a1 <- bars %>% 
  filter(reltrad == "Evangelical") %>% 
  ggplot(., aes(x=variable, y= value)) + geom_col(fill = "azure3", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  bar_rb() +
  labs(x= "", y = "Percent", title = "Making Clusters Without Political Variables", subtitle = "Evangelical Protestants") +
  # geom_text(aes(y = value + .015, label = paste0(value *100, '%')), position = position_dodge(width = .9), size = 16, family = "Product Sans") +
  theme(legend.position = "none")

# ggsave(file="D://deus_ex_machina/rr/evan_cluster_no_pol.png", type = "cairo-png", width = 18, height = 15)


pal <- c("azure3", "black", "gray87")


a2 <- bars %>% 
  filter(reltrad == "Mainline") %>% 
  ggplot(., aes(x=variable, y= value, fill = reltrad)) + geom_col(position= "dodge", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = pal) +
  bar_rb() +
  labs(x= "", y = "Percent", subtitle = "Mainline Protestants") + 
  # geom_text(aes(y = value + .015, label = paste0(value *100, '%')), position = position_dodge(width = .9), size = 9, family = "Product Sans") +
  theme(legend.position = "none")

# ggsave(file="D://deus_ex_machina/rr/cathmain_cluster_no_pol.png", type = "cairo-png", width = 18, height = 15)



a3 <- bars %>% 
  filter(reltrad == "Catholic") %>% 
  ggplot(., aes(x=variable, y= value, fill = reltrad)) + geom_col(position= "dodge", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = pal) +
  bar_rb() +
  labs(x= "", y = "Percent", subtitle = "Catholics") +
  # geom_text(aes(y = value + .015, label = paste0(value *100, '%')), position = position_dodge(width = .9), size = 9, family = "Product Sans") +
  theme(legend.position = "none")



a4 <-  bars %>% 
  filter(reltrad == "B. Prot.") %>% 
  ggplot(., aes(x=variable, y= value)) + geom_col(fill = "azure3", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  bar_rb() +
  labs(x= "", y = "Percent", subtitle = "Black Protestants") +
  # geom_text(aes(y = value + .015, label = paste0(value *100, '%')), position = position_dodge(width = .9), size = 16, family = "Product Sans") +
  theme(legend.position = "none")

# ggsave(file="D://deus_ex_machina/rr/bprot_cluster_no_pol.png", type = "cairo-png", width = 18, height = 15)



a5 <- bars %>% 
  filter(reltrad == "Jewish") %>% 
  ggplot(., aes(x=variable, y= value)) + geom_col(fill = "azure3", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  bar_rb() +
  labs(x= "", y = "Percent", subtitle = "Jewish") +
  # geom_text(aes(y = value + .015, label = paste0(value *100, '%')), position = position_dodge(width = .9), size = 16, family = "Product Sans") +
  theme(legend.position = "none")

# ggsave(file="D://deus_ex_machina/rr/jewish_no_pol.png", type = "cairo-png", width = 18, height = 15)



a6 <- bars %>% 
  filter(reltrad == "No Faith") %>% 
  ggplot(., aes(x=variable, y= value, fill = reltrad)) + geom_col(position= "dodge", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = pal) +
  bar_rb() +
  labs(x= "", y = "Percent", subtitle = "No Faith ") +
  # geom_text(aes(y = value + .015, label = paste0(value *100, '%')), position = position_dodge(width = .9), size = 9, family = "Product Sans") +
  theme(legend.position = "none")


a7 <- bars %>% 
  filter(reltrad == "Other Faith") %>% 
  ggplot(., aes(x=variable, y= value, fill = reltrad)) + geom_col(position= "dodge", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = pal) +
  bar_rb() +
  labs(x= "", y = "Percent", subtitle = "Other Faith") +
  # geom_text(aes(y = value + .015, label = paste0(value *100, '%')), position = position_dodge(width = .9), size = 9, family = "Product Sans") +
  theme(legend.position = "none")

# ggsave(file="D://deus_ex_machina/rr/other_none_no_pol.png", type = "cairo-png", width = 18, height = 15)


all <- a1+a2+a3+a4+a5+a6+a7

ggsave(file = "rr/non_pol.png", type = "cairo-png", all)
