library(tidyverse)
library(car)
library(extrafont)
library(fst)
library(xlsx)


gss <- read.fst("C://gss.fst")

gss <- gss %>% filter(year >=2008)

gss <- gss %>% mutate(age2 = age - 18)
gss <- gss %>% mutate(age2 = age2/71)

gss <- gss %>% mutate(inc = income06/25)

gss <- gss %>% mutate(black = recode(race, "2=1; else=0")) %>% mutate(black= as.factor(black))
gss <- gss %>% mutate(ed = educ/20)
gss <- gss %>% mutate(att = attend/8)

gss <- gss %>% 
  mutate(bible2 = recode(bible, "1 = 'Literal'; 2 = 'Inspired'; 3 = 'Fables'; 4 = 'Other'; else = 'Other'")) %>% 
  mutate(bible2 = as.factor(bible2))

gss <- gss %>% 
  mutate(prayer = recode(pray, "8:9 = NA; 0 = NA")) %>% 
  mutate(prayer = 6 - prayer) %>% 
  mutate(prayer = prayer/5)


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


gss <- gss %>% 
  mutate(abany1 = recode(abany, "1=1; 2=0; else = NA")) %>%
  mutate(abdefect1 = recode(abdefect, "1=1; 2=0; else = NA")) %>% 
  mutate(abnomore1 = recode(abnomore, "1=1; 2=0; else = NA")) %>% 
  mutate(abhlth1 = recode(abhlth, "1=1; 2=0; else = NA")) %>% 
  mutate(abpoor1 = recode(abpoor, "1=1; 2=0; else = NA")) %>% 
  mutate(abrape1 = recode(abrape, "1=1; 2=0; else = NA")) %>% 
  mutate(absingle1 = recode(absingle, "1=1; 2=0; else = NA"))

gss <- gss %>% 
  mutate(abortion = abany1 +  abdefect1 + abnomore1 + abhlth1 + abpoor1 + abrape1 + absingle1) %>% 
  mutate(abortion = abortion/7)

gss <- gss %>% 
  mutate(gaym = recode(marhomo, "1=1; 2=.75; 3=.5; 4=.25; 5=0; else=NA"))

gss <- gss %>% 
  mutate(pid = recode(partyid, "7:9 = NA")) %>% 
  mutate(pid = pid/6)

gss$reltrad[is.na(gss$reltrad)] <- 0

gss <- gss %>% 
  filter(reltrad != 0)

