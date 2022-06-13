

sec2 <- all_variables %>% 
  select(Group, Black, White, Nat.Am, Asian, Hisp) %>% 
  mutate(Black = ifelse(Black == "Yes", TRUE,FALSE),
         White = ifelse(White == "Yes", TRUE,FALSE),
         Nat.Am = ifelse(Nat.Am == "Yes", TRUE,FALSE),
         Asian = ifelse(Asian == "Yes", TRUE,FALSE),
         Hisp = ifelse(Hisp == "Yes", TRUE,FALSE)) %>% 
  select(Group, Hisp, everything())

#sec2 <- cbind(sec2[1:2], Race = names(sec2)[-(1:2)][max.col(sec2[-(1:2)], 'first')])

sec2_1<- sec2 %>% 
  group_by(Group, Hisp) %>% 
  count() %>% 
  ungroup() %>% 
  filter(Hisp == TRUE) %>% 
  mutate(freq = n/sum(n)) %>%
  mutate(Hispanic = paste0(n, " (", round((freq*100), 1), ")")) %>% 
  select(Group, Hispanic) %>% 
  pivot_wider(names_from = Group,
              values_from = Hispanic) %>% 
  mutate(Characteristic = rep("Hispanic")) %>%
  mutate("P Value" = rep(0.43)) %>% 
  select(Characteristic, everything()) %>% 
  rename("Control Group" = "C", "Treatment Group" = "T")

sec2_2 <- sec2 %>% 
  group_by(Group, Race) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(freq = n/sum(n)) %>%
  mutate(Race = paste0(n, " (", round((freq*100), 1), ")")) %>% 
  select(Group, Hispanic) %>% 
  pivot_wider(names_from = Group,
              values_from = Hispanic) %>% 
  mutate(Characteristic = rep("Hispanic")) %>%
  mutate("P Value" = rep(0.43)) %>% 
  select(Characteristic, everything()) %>% 
  rename("Control Group" = "C", "Treatment Group" = "T")



rownames(table_data) <- c("Age — yr","Race or ethnic group — no. (%) †","Race or ethnic group — no. (%) †","Race or ethnic group — no. (%) †", "Education — no. (%)", "Education — no. (%)", "Education — no. (%)", "Mean gestational age of fetus -- wk")

