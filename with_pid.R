library(tidyverse)
library(haven)
library(car)
library(janitor)
library(extrafont)
library(knitr)


gss <- read_dta("C:/Users/Ryan Burge/Desktop/gss_reltrad.dta")

gss <- gss %>% filter(year >=2008)

gss <- gss %>% mutate(age2 = age/89)
gss <- gss %>% mutate(male = recode(sex, "1=1; else=0"))
gss <- gss %>% mutate(inc = income/12)
gss <- gss %>% mutate(white = recode(race, "1=1; else=0"), black = recode(race, "2=1; else=0"))
gss <- gss %>% mutate(ed = educ/20)
gss <- gss %>% mutate(att = attend/8)
gss <- gss %>% mutate(literal = recode(bible, "1=1; else=0"))
gss <- gss %>% mutate(inspired = recode(bible, "2=1; else=0"))
gss <- gss %>% mutate(fables = recode(bible, "3=1; else=0"))


gss$atheist <- gss$spkath
gss$atheist <- recode(gss$atheist, "1=1; 2=0; else=0")
gss$atheist2 <- gss$colath
gss$atheist2 <- recode(gss$atheist2, "4=1; 5=0; else=0")
gss$atheist3 <- gss$libath
gss$atheist3 <- recode(gss$atheist3, "2=1; 1=0; else=0")
gss$racist <- gss$spkrac
gss$racist <- recode(gss$racist, "1=1; 2=0; else=0")
gss$racist2 <- gss$colrac
gss$racist2 <- recode(gss$racist2, "4=1; 5=0; else=0")
gss$racist3 <- gss$librac
gss$racist3 <- recode(gss$racist3, "2=1; 1=0; else=0")
gss$mili <- gss$spkmil
gss$mili <- recode(gss$mili, "1=1; 2=0; else=0")
gss$mili2 <- gss$colmil
gss$mili2 <- recode(gss$mili2, "4=1; 5=0; else=0")
gss$mili3 <- gss$libmil
gss$mili3 <- recode(gss$mili3, "2=1; 1=0; else=0")
gss$comm <- gss$spkcom
gss$comm <- recode(gss$comm, "1=1; 2=0; else=0")
gss$comm2 <- gss$colcom
gss$comm2 <- recode(gss$comm2, "5=1; 4=0; else=0")
gss$comm3 <- gss$libcom
gss$comm3 <- recode(gss$comm3, "2=1; 1=0; else=0")
gss$homo <- gss$spkhomo
gss$homo <- recode(gss$homo, "1=1; 2=0; else=0")
gss$homo2 <- gss$colhomo
gss$homo2 <- recode(gss$homo2, "4=1; 5=0; else=0")
gss$homo3 <- gss$libhomo
gss$homo3 <- recode(gss$homo3, "2=1; 1=0; else=0")
gss$tolerance <- gss$atheist + gss$atheist2 + gss$atheist3 + gss$racist + gss$racist2 + gss$racist3 + gss$comm + gss$comm2 + gss$comm3 + gss$mili + gss$mili2 + gss$mili3 + gss$homo + gss$homo2 + gss$homo3
gss$tolerance <- gss$tolerance/15


gss$abortion <- gss$abany
gss$abortion <- recode(gss$abortion, "1=1; else=0")

gss$gaymarriage <- gss$marhomo
gss$gaymarriage <- recode(gss$gaymarriage, "1=5; 2=4; 3=3; 4=2; 5=1; else=0") 
gss <- gss %>% mutate(gaymarriage = gaymarriage/5)

gss$gaysex <- gss$homosex
gss$gaysex <- recode(gss$gaysex, "4=1; else=0")
gss <- gss %>% mutate(pid = recode(partyid, "7:10=3")) %>% mutate(pid = pid/6)

gss$reltrad[is.na(gss$reltrad)] <- 0

clust <- gss %>% select(age2, male, inc, white, ed, att, literal, inspired, fables, white, black, tolerance, gaymarriage, abortion, pid) %>% na.omit()
idclust <- gss %>% select(id, age2, male, inc, white, ed, att, literal, inspired, fables, white, black, tolerance, gaymarriage, abortion, pid, reltrad) %>% na.omit() 


set.seed(62864)

wss <- (nrow(clust)-1)*sum(apply(clust,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(clust,
                                     centers=i,iter.max=1000,algorithm="MacQueen")$withinss)


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
  


ggsave(file="D://deus_ex_machina/elbow_graph.png", type = "cairo-png", width = 15, height = 15)

# plot(1:15, wss, type="b", xlab="Number of Clusters",
#      ylab="Within groups sum of squares")

set.seed(62864)
k<-kmeans(clust, centers=6,iter.max=1000)
k$size
k$centers



cluster <- as.data.frame(k$cluster)

joined <- bind_cols(idclust, cluster) %>% rename(clusters = `k$cluster`)

xtab <- joined %>% filter(reltrad !=0) %>% 
  mutate(reltrad = as.numeric(reltrad)) %>% 
  mutate(reltrad = recode(reltrad, "1= 'Evangelical'; 2 = 'Mainline'; 3 = 'B. Prot.'; 4 = 'Catholic'; 5 = 'Jewish'; 6 = 'Other Faith'; 7 = 'No Faith'")) %>% 
  mutate(clusters = recode(clusters, "1= 'Cluster 1'; 2 = 'Cluster 2'; 3 ='Cluster 3'; 4= 'Cluster 4'; 5 = 'Cluster 5'; 6 ='Cluster 6'; 7 = 'Cluster 7'")) %>% 
  tabyl(reltrad, clusters) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting() %>%
  adorn_ns()

kable(xtab)

clust %>% 
  group_by(cluster = k$cluster) %>% 
  summarise_all(funs(mean)) 
