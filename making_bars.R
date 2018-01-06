bars <- cluster %>% filter(reltrad !=0) %>% 
  mutate(reltrad = as.numeric(reltrad)) %>% 
  mutate(reltrad = recode(reltrad, "1= 'Evangelical'; 2 = 'Mainline'; 3 = 'B. Prot.'; 4 = 'Catholic'; 5 = 'Jewish'; 6 = 'Other Faith'; 7 = 'No Faith'")) %>% 
  mutate(clusters = recode(clusters, "1= 'Cluster 1'; 2 = 'Cluster 2'; 3 ='Cluster 3'; 4= 'Cluster 4'; 5 = 'Cluster 5'; 6 ='Cluster 6'; 7 = 'Cluster 7'")) %>% 
  tabyl(reltrad, clusters) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>% 
  melt()


bar_rb <- function(base_size = 25, base_family = "Product Sans") 
{theme(legend.position = "bottom", 
       legend.title = element_blank(), 
       legend.spacing.x = unit(1, "cm"),
       legend.spacing.y = unit(1, "cm"),
       panel.background = element_rect(fill = "white"), 
       panel.grid.major.y =  element_line(colour = "gray48", size = .25), 
       panel.grid.minor.y =  element_line(colour = "gray48", size = .25, linetype = "dashed"),
       text = element_text(base_family, size = 28),
       plot.title = element_text(family = "Product Sans", size = 40, vjust =2, face = "bold"),
       plot.subtitle = element_text(family = "Product Sans", size = 20, vjust =-1),
       plot.caption = element_text(family = "Product Sans", size =20),
       axis.title.x =  element_text(family = "Product Sans", size =32),
       axis.title.y =  element_text(family = "Product Sans", size =32), 
       axis.text.x = element_text(family = "Product Sans", size =24)
)
  
}


bars %>% 
  filter(reltrad == "Evangelical") %>% 
  ggplot(., aes(x=variable, y= value)) + geom_col(fill = "azure3", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  bar_rb() +
  labs(x= "", y = "Percent", title = "Evangelical Protestants")

ggsave(file="D://deus_ex_machina/evan_cluster.png", type = "cairo-png", width = 18, height = 15)


pal <- c("azure3", "black", "gray87")


bars %>% 
  filter(reltrad == "Mainline" | reltrad == "Catholic") %>% 
  ggplot(., aes(x=variable, y= value, fill = reltrad)) + geom_col(position= "dodge", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = pal) +
  bar_rb() +
  labs(x= "", y = "Percent", title = "Catholics and Mainline Protestants")

ggsave(file="D://deus_ex_machina/cathmain_cluster.png", type = "cairo-png", width = 18, height = 15)



bars %>% 
  filter(reltrad == "B. Prot.") %>% 
  ggplot(., aes(x=variable, y= value)) + geom_col(fill = "azure3", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  bar_rb() +
  labs(x= "", y = "Percent", title = "Black Protestants")

ggsave(file="D://deus_ex_machina/bprot_cluster.png", type = "cairo-png", width = 18, height = 15)



bars %>% 
  filter(reltrad == "Jewish") %>% 
  ggplot(., aes(x=variable, y= value)) + geom_col(fill = "azure3", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  bar_rb() +
  labs(x= "", y = "Percent", title = "Jewish")

ggsave(file="D://deus_ex_machina/jewish.png", type = "cairo-png", width = 18, height = 15)



bars %>% 
  filter(reltrad == "Other Faith" | reltrad == "No Faith") %>% 
  ggplot(., aes(x=variable, y= value, fill = reltrad)) + geom_col(position= "dodge", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = pal) +
  bar_rb() +
  labs(x= "", y = "Percent", title = "No Faith and Other Faith")

ggsave(file="D://deus_ex_machina/other_none.png", type = "cairo-png", width = 18, height = 15)


