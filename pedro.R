

dataset <- dataset %>%
  mutate(Membership = recode(Membership, 
                             `1` = "5",
                             `2` = "1",
                             `3` = "3",
                             `4` = "6",
                             `5` = "2",
                             `6` = "4"))
library(dplyr)
library(brms)
library(tidybayes)
library(ggforce)
library(ggplot2)
library(ggdark)

library(tidyr)

library(stringr)



m_model <- bf(MActiva ~ 1 + x1 + x2 + x3 + x4 + x5 + g + x1:g + x2:g + x3:g + x4:g + x5:g)
y_model <- bf(Externalizacion   ~ 1 + x1 + x2 + x3 + x4 + x5 + MActiva  + Edad + MActiva:Edad)


x1<-as.numeric(dataset$Membership==2)
x2<-as.numeric(dataset$Membership==3)
x3<-as.numeric(dataset$Membership==4)
x4<-as.numeric(dataset$Membership==5)
x5<-as.numeric(dataset$Membership==6)
g <-as.numeric(dataset$Genero) -1 

ma <- dataset$MActiva
e <- dataset$Edad




dataset$x1 <- as.numeric(dataset$Membership == 2)
dataset$x2 <- as.numeric(dataset$Membership== 3)
dataset$x3 <- as.numeric(dataset$Membership== 4)
dataset$x4 <- as.numeric(dataset$Membership == 5)
dataset$x5 <- as.numeric(dataset$Membership == 6)
dataset$g <- as.numeric(dataset$Genero) -1 





model13.1 <-
  brm(data = dataset, 
      family = gaussian,
      m_model + y_model + set_rescor(FALSE),
      chains = 4, cores = 4)




post <- posterior_samples(model13.1)

post %>% 
  pivot_longer(starts_with("b_")) %>% 
  mutate(criterion = ifelse(str_detect(name, "MActiva_"), "criterion: MActiva", "criterion: Externalizacion"),
         criterion = factor(criterion, levels = c("criterion: MActiva", "criterion: Externalizacion")),
         name = str_remove(name, "b_MActiva_"),
         name = str_remove(name, "b_Externalizacion_"),
         name = factor(name, levels = c("Intercept",  "MActiva:Edad", "Edad", "MActiva",  "x5:g", "x4:g", "x3:g", "x2:g", "x1:g", "g" , "x5", "x4", "x3", "x2", "x1"    ))) %>% 

  
  ggplot(aes(x = value, y = name, group = name)) +
  geom_halfeyeh(.width = .95, 
                scale = "width", relative_scale = .75,
                color = "white")  +
  coord_cartesian(xlim = c(-7, 6)) +
  labs(x = NULL, y = NULL) +
  dark_mode() +
  theme(axis.text.y = element_text(hjust = 0),
        axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey20")) +
  facet_wrap(~criterion)



##---------------------

indirect <-
  post %>% 
  expand(nesting(b_x1 + b_x2 + b_x3 + b_x4 + b_x5 + b_MActiva  + b_Edad + `b_MActiva:Edad`),
         sexism = seq(from = 3.5, to = 6.5, length.out = 30)) %>% 
  mutate(`Protest vs. No Protest`            = (b_respappr_d1 + `b_respappr_d1:sexism` * sexism) * b_liking_respappr,
         `Collective vs. Individual Protest` = (b_respappr_d2 + `b_respappr_d2:sexism` * sexism) * b_liking_respappr) %>% 
  pivot_longer(contains("Protest")) %>% 
  select(sexism:value) %>% 
  
  group_by(name, sexism) %>% 
  median_qi(value)

head(indirect)
