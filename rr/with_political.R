## Run Cleaning File First 

## This creates a df called clust which has our relevant variables
clust <- gss %>% select(age2, black, inc, ed, att, bible2, prayer,  tolerance, gaym, abortion, pid) %>% na.omit()

## Creating a good ID variable 
gss <- gss %>% mutate(newid = paste(id, year), sep = "_")

## Creating another df that has the IDs of each respondent, along with RELTRAD 
idclust <- gss %>% select(newid, age2, black, inc, ed, att, bible2, prayer,  tolerance, gaym, abortion, pid, reltrad) %>% na.omit() 

## Random Seed
set.seed(90210)

## Max Number of Clusters for Elbow Method
k.max <- 15

## Calcuating with sum of squares
wss <- sapply(1:k.max, 
              function(k){kproto(clust, k)$tot.withinss})

## Plotting the Output
elbow <- as.tibble(wss) %>% mutate(cluster = seq(from = 1, to = 15, by= 1))

ggplot(elbow, aes(x=cluster, y= value)) + 
  geom_point(size =3.5) + 
  geom_line(size = 1.5) + 
  scale_x_continuous(breaks = round(seq(min(elbow$cluster), max(elbow$cluster), by = 1),1)) +
  labs(x= "Number of Clusters", y = "Within Groups Sum of Squares", title = "Elbow Method for Choosing Number of Clusters")


## To do the actual clustering
set.seed(90210)

## Using six clusters
k<-kproto(clust, k=6,iter.max=100000)


k$size
k$centers

cluster <- as_tibble(clust) %>% 
  add_column(clusters = factor(k$cluster)) %>% bind_cols(idclust) 

## Organizing Data to get a table 
xtab <- cluster %>% filter(reltrad !=0) %>% 
  mutate(reltrad = as.numeric(reltrad)) %>% 
  mutate(reltrad = recode(reltrad, "1= 'Evangelical'; 2 = 'Mainline'; 3 = 'B. Prot.'; 4 = 'Catholic'; 5 = 'Jewish'; 6 = 'Other Faith'; 7 = 'No Faith'")) %>% 
  mutate(clusters = recode(clusters, "1= 'Cluster 1'; 2 = 'Cluster 2'; 3 ='Cluster 3'; 4= 'Cluster 4'; 5 = 'Cluster 5'; 6 ='Cluster 6'; 7 = 'Cluster 7'")) %>% 
  tabyl(reltrad, clusters) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting() %>%
  adorn_ns()


## Make a readable table 
kable(xtab)


## Make the means table 
means <- clust %>% 
  group_by(cluster = k$cluster) %>% 
  summarise_all(funs(mean)) %>% 
  mutate_if(is.numeric, funs(round(., 2))) %>% 
  ungroup(cluster) %>% 
  rename(`Age` = age2, `Income` = inc, `Black` = black, `Education` = ed, `Church Attendance` = att, `bible` = bible2, `Freq. Prayer` = prayer, `Tolerance` = tolerance, `Pro-Gay Marriage` = gaym, `Pro-Choice` = abortion, `Rep. ID` = pid) %>% 
  select(-Black, -bible)

add <- k$centers %>% select(black, bible2) 

means <- bind_cols(means, add) %>% 
  rename(`Black` = black, `View of Bible` = bible2)

means$cluster <- paste("Cluster", means$cluster, sep = " ")

write.xlsx(means, file = "rr/means_table.xlsx")


clust %>% 
  group_by(cluster = k$cluster) %>% 
  ct(black)
