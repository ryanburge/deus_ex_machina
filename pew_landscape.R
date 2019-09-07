pew14 <- read_dta("D://pew14.dta")

## CLeaning ####
pew14 <- pew14 %>% 
  mutate(reltrad = frcode(reltrad == 1100 ~ "Evangelical",
                          reltrad == 1200 ~ "Mainline",
                          reltrad == 1300 ~ "Black Protestant",
                          reltrad == 10000 ~ "Catholic",
                          reltrad == 50000 ~ "Jewish",
                          reltrad == 100000 ~ "Nones",
                          TRUE ~ "Other Faith")) %>% 
  mutate(right = car::recode(QB2D, "1=1; 2=0; else = NA")) %>% 
  mutate(right = as.factor(right)) %>% 
  mutate(creation = car::recode(QB30, "2=1; 1=0; else =NA")) %>% 
  mutate(creation = as.factor(creation)) %>% 
  mutate(black = car::recode(racecmb, "2=1; else =0")) %>% 
  mutate(black = as.factor(black)) %>%
  mutate(att = car::recode(attend, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1")) %>% 
  mutate(att = att/6) %>% 
  mutate(relimp = car::recode(QF2, "1=4; 2=3; 3=2; 4=1; else = NA")) %>% 
  mutate(relimp = relimp/6) %>% 
  mutate(certain = car::recode(QG1B, "4=1; 3=2; 2=3; 1=4; else = NA")) %>% 
  mutate(certain = certain/6) %>% 
  mutate(bible = case_when(QG7 == 2 ~ 1, 
                           QG7B == 1 ~ 3, 
                           QG7B == 2 ~ 2, 
                           TRUE ~ 0)) %>% 
  mutate(bible = bible/3) %>% 
  mutate(pray = car::recode(QI1, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; else = NA")) %>% 
  mutate(pray = pray/7) %>% 
  mutate(pray_grp = car::recode(QI2A, "5=1; 4=2; 3=3; 2=4; 1=5; else = NA")) %>% 
  mutate(pray_grp = pray_grp/5) %>% 
  mutate(bibread  = car::recode(QI2B, "5=1; 4=2; 3=3; 2=4; 1=5; else = NA")) %>% 
  mutate(bibread = bibread/5) %>% 
  mutate(meditate = car::recode(QI2C, "5=1; 4=2; 3=3; 2=4; 1=5; else = NA")) %>%
  mutate(meditate = meditate/5) %>% 
  mutate(bornagain = car::recode(born, "1=1; else = 0")) %>%
  mutate(bornagain = as.factor(bornagain))


## Making Dataframes ####
clust <- pew14 %>% 
  dplyr::select(right, creation, black, att, relimp, certain, bible, pray, pray_grp, bibread, meditate, bornagain) %>% 
  na.omit()

clust <- as.data.frame(clust)
    
idclust <- pew14 %>% 
  dplyr::select(resp, right, creation, black, att, relimp, certain, bible, pray, pray_grp, bibread, meditate, bornagain) %>% 
  na.omit()


## Elbows ####
set.seed(90210)

k.max <- 15

wss <- sapply(1:k.max, 
              function(k){kproto(clust, k)$tot.withinss})

elbow <- as_tibble(wss) %>% mutate(cluster = seq(from = 1, to = 15, by= 1))


ggplot(elbow, aes(x=cluster, y= value)) + 
  geom_point(size =3.5) + 
  geom_line(size = 1.5) + 
  theme_gg("Abel") + 
  scale_x_continuous(breaks = round(seq(min(elbow$cluster), max(elbow$cluster), by = 1),1)) +
  labs(x= "Number of Clusters", y = "Within Groups Sum of Squares", title = "Elbow Method for Choosing Number of Clusters") +
  ggsave("D://deus_ex_machina/pew_elbow_graph.png")


## Actual Clustering Here ####

k<-kproto(clust, k=6,iter.max=100000)

## Then joining it back to the IDs and RELTRADS ####

cluster <- as_tibble(clust) %>% 
  add_column(clusters = factor(k$cluster)) %>% bind_cols(idclust) 

rel <- pew14 %>% 
  select(resp, reltrad)

cluster <- left_join(cluster, rel)


## Making the Crosstab Dataset ####

xtab <- cluster %>% filter(reltrad !=0) %>% 
  # mutate(reltrad = as.numeric(reltrad)) %>% 
  # mutate(reltrad = recode(reltrad, "1= 'Mainline'; 2 = 'Evangelical'; 3 = 'Black Prot.'; 4 = 'Catholic'; 5 = 'Jewish'; 6 = 'Other Faith'; 7 = 'No Faith'")) %>% 
  mutate(clusters = recode(clusters, "1= 'Cluster 1'; 2 = 'Cluster 2'; 3 ='Cluster 3'; 4= 'Cluster 4'; 5 = 'Cluster 5'; 6 ='Cluster 6'")) %>% 
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
  labs(x= "", y = "", title = "Distribution of Religious Groups Across Clusters", subtitle = "", caption = "Data: Pew Religious Landscape (2014)") +
  ggsave("D://deus_ex_machina/pew_cluster_heatmap.png", width = 6)

mean <- as.data.frame(k$centers)
