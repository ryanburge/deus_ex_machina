## Run Cleaning File First 


clust <- gss %>% select(age2, black, inc, ed, att, bible2, prayer,) %>% na.omit()
idclust <- gss %>% select(id, age2, black, inc, ed, att, bible2, prayer, reltrad) %>% na.omit() 


set.seed(90210)

k.max <- 15

wss <- sapply(1:k.max, 
              function(k){kproto(clust, k)$tot.withinss})


elbow <- as.tibble(wss) %>% mutate(cluster = seq(from = 1, to = 15, by= 1))


scatter_rb <- function(base_size = 25, base_family = "Product Sans") 
{theme(legend.position = "bottom", 
       legend.title = element_blank(), 
       legend.spacing.x = unit(1, "cm"),
       legend.spacing.y = unit(1, "cm"),
       panel.background = element_rect(fill = "white"), 
       panel.grid.major.y =  element_line(colour = "gray48", size = .25), 
       # panel.grid.minor.y =  element_line(colour = "gray48", size = .25, linetype = "dashed"),
       panel.grid.major.x =  element_line(colour = "gray48", size = .25, linetype = "dashed"),
       text = element_text(base_family, size = 28),
       plot.title = element_text(family = "Product Sans", size = 40, vjust =2, face = "bold"),
       plot.subtitle = element_text(family = "Product Sans", size = 20, vjust =-1),
       plot.caption = element_text(family = "Product Sans", size =20),
       axis.title.x =  element_text(family = "Product Sans", size =24),
       axis.title.y =  element_text(family = "Product Sans", size =24), 
       axis.text.x = element_text(family = "Product Sans", size =18)
)
  
}


ggplot(elbow, aes(x=cluster, y= value)) + 
  geom_point(size =3.5) + 
  geom_line(size = 1.5) + scatter_rb() + 
  scale_x_continuous(breaks = round(seq(min(elbow$cluster), max(elbow$cluster), by = 1),1)) +
  labs(x= "Number of Clusters", y = "Within Groups Sum of Squares", title = "Elbow Method for Choosing Number of Clusters")

ggsave(file="D://deus_ex_machina/rr/elbow_graph.png", type = "cairo-png", width = 15, height = 15)


set.seed(90210)
k<-kproto(clust, k=6,iter.max=100000)
k$size
k$centers

cluster <- as_tibble(clust) %>% 
  add_column(clusters = factor(k$cluster)) %>% bind_cols(idclust) 


xtab <- cluster %>% filter(reltrad !=0) %>% 
  mutate(reltrad = as.numeric(reltrad)) %>% 
  mutate(reltrad = recode(reltrad, "1= 'Evangelical'; 2 = 'Mainline'; 3 = 'B. Prot.'; 4 = 'Catholic'; 5 = 'Jewish'; 6 = 'Other Faith'; 7 = 'No Faith'")) %>% 
  mutate(clusters = recode(clusters, "1= 'Cluster 1'; 2 = 'Cluster 2'; 3 ='Cluster 3'; 4= 'Cluster 4'; 5 = 'Cluster 5'; 6 ='Cluster 6'; 7 = 'Cluster 7'")) %>% 
  tabyl(reltrad, clusters) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting() %>%
  adorn_ns()

kable(xtab)

means <- clust %>% 
  group_by(cluster = k$cluster) %>% 
  summarise_all(funs(mean)) %>% 
  mutate_if(is.numeric, funs(round(., 2))) %>% 
  ungroup(cluster) %>% 
  rename(`Age` = age2, `Income` = inc, `Black` = black, `Education` = ed, `Church Attendance` = att, `bible` = bible2, `Freq. Prayer` = prayer) %>% 
  select(-Black, -bible)

add <- k$centers %>% select(black, bible2) 

means <- bind_cols(means, add) %>% 
  rename(`Black` = black, `View of Bible` = bible2)

means$cluster <- paste("Cluster", means$cluster, sep = " ")

write.xlsx(means, file = "rr/means_table.xlsx")


clust %>% 
  group_by(cluster = k$cluster) %>% 
  ct(black)
