## Run Cleaning File ####

library(clustMixType)

anes <- anes %>% 
  mutate(black = as.factor(black))


clust <- anes %>% 
  select(black, att, reborn, bible, relguide, discrim_muslim, discrim_xtn, therm_fundie, therm_pope, therm_muslim, therm_jews, therm_xtn, moral) %>% 
  na.omit()

clust <- as.data.frame(clust)

idclust <- anes %>% 
  select(V160001, black, att, reborn, bible, relguide, discrim_muslim, discrim_xtn, therm_fundie, therm_pope, therm_muslim, therm_jews, therm_xtn, moral) %>% 
  na.omit()


## Making the Elbow Graph ####

set.seed(90210)

k.max <- 15

wss <- sapply(1:k.max, 
              function(k){kproto(clust, k)$tot.withinss})

elbow <- as.tibble(wss) %>% mutate(cluster = seq(from = 1, to = 15, by= 1))

ggplot(elbow, aes(x=cluster, y= value)) + 
  geom_point(size =3.5) + 
  geom_line(size = 1.5) + 
  theme_gg("Abel") + 
  scale_x_continuous(breaks = round(seq(min(elbow$cluster), max(elbow$cluster), by = 1),1)) +
  labs(x= "Number of Clusters", y = "Within Groups Sum of Squares", title = "Elbow Method for Choosing Number of Clusters") +
  ggsave("D://deus_ex_machina/v3/elbow_graph.png")

## Actual Clustering Here ####

k<-kproto(clust, k=5,iter.max=100000)

## Then joining it back to the IDs and RELTRADS ####

cluster <- as_tibble(clust) %>% 
  add_column(clusters = factor(k$cluster)) %>% bind_cols(idclust) 

rel <- anes %>% 
  select(V160001, reltrad)

cluster <- left_join(cluster, rel)

## Making the Crosstab Dataset ####

xtab <- cluster %>% filter(reltrad !=0) %>% 
  mutate(reltrad = as.numeric(reltrad)) %>% 
  mutate(reltrad = recode(reltrad, "1= 'Mainline'; 2 = 'Evangelical'; 3 = 'Black Prot.'; 4 = 'Catholic'; 5 = 'Christian'; 6 = 'Jewish'; 7 = 'Other Faith'; 8 = 'No Faith'")) %>% 
  mutate(clusters = recode(clusters, "1= 'Cluster 1'; 2 = 'Cluster 2'; 3 ='Cluster 3'; 4= 'Cluster 4'; 5 = 'Cluster 5'; 6 ='Cluster 6'; 7 = 'Cluster 7'; 8 = 'Cluster 8'; 9 = 'Cluster 9'")) %>% 
  group_by(reltrad) %>%
  ct(clusters) %>%
  complete(clusters, fill = list(count = 0)) %>% 
  replace(is.na(.), 0)

### Heatmapping the Crosstab ####

xtab %>% 
  ggplot(., aes(x= clusters, y = reltrad)) +
  geom_tile(aes(fill = pct), color = "black") +
  scale_fill_gradient(low = "azure3", high = "#E94057") +
  theme_gg("Abel") + 
  scale_x_discrete(position = "top")  +
  geom_text(aes(x= clusters, y = reltrad, label = paste0(pct*100, '%')), size = 4, family = "font") +
  labs(x= "", y = "", title = "Distribution of Religious Groups Across Clusters", subtitle = "", caption = "Data: ANES (2016)") +
  ggsave("D://deus_ex_machina/v3/cluster_heatmap.png", width = 6)

mean <- as.data.frame(k$centers)

write_csv(mean, "D://deus_ex_machina/v3/mean_table.csv")


### This is checking PID ####

id <- cluster %>% 
  select(V160001, clusters)

anes <- left_join(anes, id)

graph <- anes %>% 
  group_by(clusters) %>% 
  filter(V161158x > 0) %>% 
  mean_ci(V161158x, ci = .84) %>% 
  na.omit() %>% 
  mutate(word = "Cluster") %>% 
  mutate(clusters = paste(word, clusters))


graph2 <- anes %>% 
  filter(V161158x > 0) %>% 
  mean_ci(V161158x, ci = .84) %>% 
  na.omit() %>% 
  mutate(clusters = "Entire Sample")


graph %>% 
  ggplot(., aes(y=mean, x= fct_rev(clusters), color = clusters)) +
  geom_point(position=position_dodge(width=0.5), size =4) +
  geom_errorbar(aes(ymin = lower, ymax=upper), position=position_dodge(0.5), size = 1, width = .25) +
  coord_flip() +
  theme_gg("Abel") +
  labs(title = "Mean Partisan Identification for Each Cluster", x = "", y = "", caption = "Data: ANES (2016)") +
  scale_y_continuous(limits = c(0.85,5.25), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Democrat", "Not Very Strong Dem.", "Ind.-Democrat", "Independent", "Ind.- Republican", "Not Very Strong Rep.", "Strong Republican")) +
  scale_color_npg() + 
  geom_hline(yintercept = 3.86, linetype = "dashed") +
  theme(legend.position = "none") +  
  annotate("text", x=3.1, y = 3.6, label = "Sample Mean", size = 6, family = "font") +
  theme(legend.title=element_blank()) +
  theme(text=element_text(size=28, family="font")) +
  ggsave("D://deus_ex_machina/v3/pid_means.png", height = 4, width =16) 




