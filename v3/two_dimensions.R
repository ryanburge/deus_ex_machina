mean2 <- mean %>% 
  as_tibble() %>% 
  mutate(bb = att + bible + relguide +  moral) %>% 
  mutate(therm = therm_fundie + therm_pope + therm_muslim + therm_jews + therm_xtn) %>% 
  mutate(bb = bb/3.54) %>% 
  mutate(therm = therm/4.14)

mean2$clust <- c(1,2,3,4,5,6,7)

mean2 %>% 
  ggplot(., aes(x = therm, y = bb, label = clust)) + 
  geom_point() +
  geom_text_repel() +
  ggsave("D://deus_ex_machina/v3/2dimensions.png", type = "cairo-png")