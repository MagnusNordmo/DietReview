
## Required packages

library(tidyverse)
library(bayestestR)
library(lspline)



diet <- read.csv2("~/Desktop/Magnus/R Wizard/Vekt/diet.csv", stringsAsFactors=FALSE)

############## Simulate datapoints

#SDs are based on percentage score


#Cooper
id <- 1:99
"Cooper0" <- distribution_normal(99,mean = 100,sd = 9.26)
"Cooper10" <- distribution_normal(99,
                   mean = as.numeric(diet%>% 
                                       filter(TimeMonth == 10 & Study == "Cooper (2010)") %>% 
                                       ungroup() %>% 
                                       select(Percent)),
                   sd = as.numeric(diet%>% 
                                     filter(TimeMonth == 10 & Study == "Cooper (2010)") %>% 
                                     ungroup() %>% 
                                     select(SD)))
"Cooper16" <- distribution_normal(99,
                   mean = as.numeric(diet%>% 
                                       filter(TimeMonth == 16 & Study == "Cooper (2010)") %>% 
                                       ungroup() %>% 
                                       select(Percent)),
                   sd = as.numeric(diet%>% 
                                     filter(TimeMonth == 16 & Study == "Cooper (2010)") %>% 
                                     ungroup() %>% 
                                     select(SD)))
"Cooper22" <- distribution_normal(99,
                    mean = as.numeric(diet%>% 
                                        filter(TimeMonth == 22 & Study == "Cooper (2010)") %>% 
                                        ungroup() %>% 
                                        select(Percent)),
                    sd = as.numeric(diet%>% 
                                      filter(TimeMonth == 22 & Study == "Cooper (2010)") %>% 
                                      ungroup() %>% 
                                      select(SD)))
"Cooper34" <- distribution_normal(99,
                    mean = as.numeric(diet%>% 
                                        filter(TimeMonth == 34 & Study == "Cooper (2010)") %>% 
                                        ungroup() %>% 
                                        select(Percent)),
                    sd = as.numeric(diet%>% 
                                      filter(TimeMonth == 34 & Study == "Cooper (2010)") %>% 
                                      ungroup() %>% 
                                      select(SD)))
"Cooper46" <- distribution_normal(99,
                    mean = as.numeric(diet%>% 
                                        filter(TimeMonth == 46 & Study == "Cooper (2010)") %>% 
                                        ungroup() %>% 
                                        select(Percent)),
                    sd = as.numeric(diet%>% 
                                      filter(TimeMonth == 46 & Study == "Cooper (2010)") %>% 
                                      ungroup() %>% 
                                      select(SD)))
sim <- data.frame(id,Cooper0,Cooper10,Cooper16,Cooper22,Cooper34,Cooper46)
sim <- sim %>% gather("Time","Percent",2:7)
sim$Time <- str_remove(sim$Time, "Cooper")
sim$Time <- as.integer(sim$Time)
sim <- add_column(sim, Study = "Cooper (2010)")
sim <- as_tibble(sim)
sim <- arrange(sim,id)
x <- as_tibble(diet%>% 
                 filter(Study == "Cooper (2010)") %>% 
                 group_by(TimeMonth) %>% 
                 select(Percent))
sim <- add_column(sim, Mean = rep(x$Percent,99))
rm(Cooper0,Cooper10,Cooper16,Cooper22,Cooper34,Cooper46,x)

# Stalonas 
id <- 100:213
"S0" <- distribution_normal(114,mean = 100,sd = 11.63)
"S10" <- distribution_normal(114,
              mean = as.numeric(diet%>% 
                                  filter(TimeMonth == 10 & Study == "Stalonas (1984)") %>% 
                                  ungroup() %>% 
                                  select(Percent)),
              sd = as.numeric(diet%>% 
                                filter(TimeMonth == 10 & Study == "Stalonas (1984)") %>% 
                                ungroup() %>% 
                                select(SD)))
"S13" <- distribution_normal(114,
              mean = as.numeric(diet%>% 
                                  filter(TimeMonth == 13 & Study == "Stalonas (1984)") %>% 
                                  ungroup() %>% 
                                  select(Percent)),
              sd = as.numeric(diet%>% 
                                filter(TimeMonth == 13 & Study == "Stalonas (1984)") %>% 
                                ungroup() %>% 
                                select(SD)))
"S22" <- distribution_normal(114,
               mean = as.numeric(diet%>% 
                                   filter(TimeMonth == 22 & Study == "Stalonas (1984)") %>% 
                                   ungroup() %>% 
                                   select(Percent)),
               sd = as.numeric(diet%>% 
                                 filter(TimeMonth == 22 & Study == "Stalonas (1984)") %>% 
                                 ungroup() %>% 
                                 select(SD)))
"S70" <- distribution_normal(114,
               mean = as.numeric(diet%>% 
                                   filter(TimeMonth == 70 & Study == "Stalonas (1984)") %>% 
                                   ungroup() %>% 
                                   select(Percent)),
               sd = as.numeric(diet%>% 
                                 filter(TimeMonth == 70 & Study == "Stalonas (1984)") %>% 
                                 ungroup() %>% 
                                 select(SD)))
sim2 <- data.frame(id,S0,S10,S13,S22,S70)
sim2 <- sim2 %>% gather("Time","Percent",2:6)
sim2$Time <- str_remove(sim2$Time, "S")
sim2$Time <- as.integer(sim2$Time)
sim2 <- add_column(sim2, Study = "Stalonas (1984)")
sim2 <- as_tibble(sim2)
sim2 <- arrange(sim2,id)
x <- as_tibble(diet%>% 
                 filter(Study == "Stalonas (1984)") %>% 
                 group_by(TimeMonth) %>% 
                 select(Percent))
sim2 <- add_column(sim2, Mean = rep(x$Percent,114))
sim <- bind_rows(sim,sim2)
rm(sim2,S0,S10,S13,S22,S70,id,x)

### Hensrud
id <- 214:237
"H0" <- distribution_normal(24,mean = 100,sd = 14.2)
"H10" <- distribution_normal(24,
              mean = as.numeric(diet%>% 
                                  filter(TimeMonth == 10 & Study == "Hensrud (1994)") %>% 
                                  ungroup() %>% 
                                  select(Percent)),
              sd = as.numeric(diet%>% 
                                filter(TimeMonth == 10 & Study == "Hensrud (1994)") %>% 
                                ungroup() %>% 
                                select(SD)))
"H22" <- distribution_normal(24,
               mean = as.numeric(diet%>% 
                                   filter(TimeMonth == 22 & Study == "Hensrud (1994)") %>% 
                                   ungroup() %>% 
                                   select(Percent)),
               sd = as.numeric(diet%>% 
                                 filter(TimeMonth == 22 & Study == "Hensrud (1994)") %>% 
                                 ungroup() %>% 
                                 select(SD)))
"H58" <- distribution_normal(24,
               mean = as.numeric(diet%>% 
                                   filter(TimeMonth == 58 & Study == "Hensrud (1994)") %>% 
                                   ungroup() %>% 
                                   select(Percent)),
               sd = as.numeric(diet%>% 
                                 filter(TimeMonth == 58 & Study == "Hensrud (1994)") %>% 
                                 ungroup() %>% 
                                 select(SD)))
sim2 <- data.frame(id,H0,H10,H22,H58)
sim2 <- sim2 %>% gather("Time","Percent",2:5)
sim2$Time <- str_remove(sim2$Time, "H")
sim2$Time <- as.integer(sim2$Time)
sim2 <- add_column(sim2, Study = "Hensrud(1994)")
sim2 <- as_tibble(sim2)
sim2 <- arrange(sim2,id)
x <- as_tibble(diet%>% 
                 filter(Study == "Hensrud (1994)") %>% 
                 group_by(TimeMonth) %>% 
                 select(Percent))
sim2 <- add_column(sim2, Mean = rep(x$Percent,24))
sim <- bind_rows(sim,sim2)
rm(sim2,H0,H10,H22,H58,id,x)


### Olszanecka - Glinianowicz
id <- 238:267
"OG0" <- distribution_normal(30,mean = 100,sd = 14.8)
"OG10" <- distribution_normal(30,
                             mean = as.numeric(diet%>% 
                                                 filter(TimeMonth == 10 & Study == "Olszanecka - Glinianowicz (2012)") %>% 
                                                 ungroup() %>% 
                                                 select(Percent)),
                             sd = as.numeric(diet%>% 
                                               filter(TimeMonth == 10 & Study == "Olszanecka - Glinianowicz (2012)") %>% 
                                               ungroup() %>% 
                                               select(SD)))
"OG70" <- distribution_normal(30,
                             mean = as.numeric(diet%>% 
                                                 filter(TimeMonth == 70 & Study == "Olszanecka - Glinianowicz (2012)") %>% 
                                                 ungroup() %>% 
                                                 select(Percent)),
                             sd = as.numeric(diet%>% 
                                               filter(TimeMonth == 70 & Study == "Olszanecka - Glinianowicz (2012)") %>% 
                                               ungroup() %>% 
                                               select(SD)))
sim2 <- data.frame(id,OG0,OG10,OG70)
sim2 <- sim2 %>% gather("Time","Percent",2:4)
sim2$Time <- str_remove(sim2$Time, "OG")
sim2$Time <- as.integer(sim2$Time)
sim2 <- add_column(sim2, Study = "Olszanecka - Glinianowicz (2012)")
sim2 <- as_tibble(sim2)
sim2 <- arrange(sim2,id)
x <- as_tibble(diet%>% 
                 filter(Study == "Olszanecka - Glinianowicz (2012)") %>% 
                 group_by(TimeMonth) %>% 
                 select(Percent))
sim2 <- add_column(sim2, Mean = rep(x$Percent,30))
sim <- bind_rows(sim,sim2)
rm(sim2,OG0,OG10,OG70,id,x)


#### Pekkarinnen 1997
id <- 268:326
"P0" <- distribution_normal(59,mean = 100,sd = 14.2)
"P10" <- distribution_normal(59,
              mean = as.numeric(diet%>% 
                                  filter(TimeMonth == 10 & Study == "Pekkarinen (1997)") %>% 
                                  ungroup() %>% 
                                  select(Percent)),
              sd = as.numeric(diet%>% 
                                filter(TimeMonth == 10 & Study == "Pekkarinen (1997)") %>% 
                                ungroup() %>% 
                                select(SD)))
"P70" <- distribution_normal(59,
               mean = as.numeric(diet%>% 
                                   filter(TimeMonth == 70 & Study == "Pekkarinen (1997)") %>% 
                                   ungroup() %>% 
                                   select(Percent)),
               sd = as.numeric(diet%>% 
                                 filter(TimeMonth == 70 & Study == "Pekkarinen (1997)") %>% 
                                 ungroup() %>% 
                                 select(SD)))
sim2 <- data.frame(id,P0,P10,P70)
sim2 <- sim2 %>% gather("Time","Percent",2:4)
sim2$Time <- str_remove(sim2$Time, "P")
sim2$Time <- as.integer(sim2$Time)
sim2 <- add_column(sim2, Study = "Pekkarinen (1997)")
sim2 <- as_tibble(sim2)
sim2 <- arrange(sim2,id)
x <- as_tibble(diet%>% 
                 filter(Study == "Pekkarinen (1997)") %>% 
                 group_by(TimeMonth) %>% 
                 select(Percent))
sim2 <- add_column(sim2, Mean = rep(x$Percent,59))
sim <- bind_rows(sim,sim2)
rm(sim2,P0,P10,P70,id,x)


#### Vogels 2005
id <- 327:417
"V0" <- distribution_normal(91,mean = 100,sd = 10.5)
"V10" <- distribution_normal(91,
              mean = as.numeric(diet%>% 
                                  filter(TimeMonth == 10 & Study == "Vogels (2005)") %>% 
                                  ungroup() %>% 
                                  select(Percent)),
              sd = as.numeric(diet%>% 
                                filter(TimeMonth == 10 & Study == "Vogels (2005)") %>% 
                                ungroup() %>% 
                                select(SD)))
"V46" <- distribution_normal(91,
               mean = as.numeric(diet%>% 
                                   filter(TimeMonth == 46 & Study == "Vogels (2005)") %>% 
                                   ungroup() %>% 
                                   select(Percent)),
               sd = as.numeric(diet%>% 
                                 filter(TimeMonth == 46 & Study == "Vogels (2005)") %>% 
                                 ungroup() %>% 
                                 select(SD)))
sim2 <- data.frame(id,V0,V10,V46)
sim2 <- sim2 %>% gather("Time","Percent",2:4)
sim2$Time <- str_remove(sim2$Time, "V")
sim2$Time <- as.integer(sim2$Time)
sim2 <- add_column(sim2, Study = "Vogels (2005)")
sim2 <- as_tibble(sim2)
sim2 <- arrange(sim2,id)
x <- c(100,90.9,100.8)
sim2 <- add_column(sim2, Mean = rep(x,91))
sim <- bind_rows(sim,sim2)
rm(sim2,id,V0,V10,V46,x)

#### Schwarzfuchs 2012
id <- 418:739
"S0" <- distribution_normal(322,mean = 100,sd = 13.2)
"S10" <- distribution_normal(322,
              mean = as.numeric(diet%>% 
                                  filter(TimeMonth == 10 & Study == "Schwarzfuchs (2012)") %>% 
                                  ungroup() %>% 
                                  select(Percent)),
              sd = as.numeric(diet%>% 
                                filter(TimeMonth == 10 & Study == "Schwarzfuchs (2012)") %>% 
                                ungroup() %>% 
                                select(SD)))
"S34" <- distribution_normal(322,
               mean = as.numeric(diet%>% 
                                   filter(TimeMonth == 34 & Study == "Schwarzfuchs (2012)") %>% 
                                   ungroup() %>% 
                                   select(Percent)),
               sd = as.numeric(diet%>% 
                                 filter(TimeMonth == 34 & Study == "Schwarzfuchs (2012)") %>% 
                                 ungroup() %>% 
                                 select(SD)))
"S58" <- distribution_normal(322,
               mean = as.numeric(diet%>% 
                                   filter(TimeMonth == 58 & Study == "Schwarzfuchs (2012)") %>% 
                                   ungroup() %>% 
                                   select(Percent)),
               sd = as.numeric(diet%>% 
                                 filter(TimeMonth == 58 & Study == "Schwarzfuchs (2012)") %>% 
                                 ungroup() %>% 
                                 select(SD)))
sim2 <- data.frame(id,S0,S10,S34,S58)
sim2 <- sim2 %>% gather("Time","Percent",2:5)
sim2$Time <- str_remove(sim2$Time, "S")
sim2$Time <- as.integer(sim2$Time)
sim2 <- add_column(sim2, Study = "Schwarzfuchs (2012)")
sim2 <- as_tibble(sim2)
sim2 <- arrange(sim2,id)
x <- c(100,93.8,95.6,98.0)
sim2 <- add_column(sim2, Mean = rep(x,322))
sim <- bind_rows(sim,sim2)
rm(sim2,id,S0,S10,S34,S58,x)

#### Wadden 1989
id <- 740:815
"W0" <- distribution_normal(76,mean = 100,sd = 10)
"W10" <- distribution_normal(76,
                             mean = as.numeric(diet%>% 
                                                 filter(TimeMonth == 10 & Study == "Wadden (1989)") %>% 
                                                 ungroup() %>% 
                                                 select(Percent)),
                             sd = as.numeric(diet%>% 
                                               filter(TimeMonth == 10 & Study == "Wadden (1989)") %>% 
                                               ungroup() %>% 
                                               select(SD)))
"W22" <- distribution_normal(76,
                             mean = as.numeric(diet%>% 
                                                 filter(TimeMonth == 22 & Study == "Wadden (1989)") %>% 
                                                 ungroup() %>% 
                                                 select(Percent)),
                             sd = as.numeric(diet%>% 
                                               filter(TimeMonth == 22 & Study == "Wadden (1989)") %>% 
                                               ungroup() %>% 
                                               select(SD)))
"W70" <- distribution_normal(76,
                             mean = as.numeric(diet%>% 
                                                 filter(TimeMonth == 70 & Study == "Wadden (1989)") %>% 
                                                 ungroup() %>% 
                                                 select(Percent)),
                             sd = as.numeric(diet%>% 
                                               filter(TimeMonth == 70 & Study == "Wadden (1989)") %>% 
                                               ungroup() %>% 
                                               select(SD)))
sim2 <- data.frame(id,W0,W10,W22,W70)
sim2 <- sim2 %>% gather("Time","Percent",2:5)
sim2$Time <- str_remove(sim2$Time, "W")
sim2$Time <- as.integer(sim2$Time)
sim2 <- add_column(sim2, Study = "Wadden (1989)")
sim2 <- as_tibble(sim2)
sim2 <- arrange(sim2,id)
x <- as_tibble(diet%>% 
                 filter(Study == "Wadden (1989)") %>% 
                 group_by(TimeMonth) %>% 
                 select(Percent))
sim2 <- add_column(sim2, Mean = rep(x$Percent,76))
sim <- bind_rows(sim,sim2)
rm(sim2,id,W0,W10,W22,W70,x)


## Asses that mean Pretreatment is 100
sim %>% filter(Time == 0) %>% summarise(mean = mean(Percent))

## Assess mean weightloss at Posttreatment
sim %>% filter(Time == 10) %>% summarise(mean = mean(Percent))


############## Model linear spline regression
# Convert x and y values for the lspline procedure
sim$x <- sim$Time
sim$y <- sim$Percent
m1 <- lm(y ~ lspline(x, 10,marginal = F), data=sim)

sim$pred <- predict(m1)

# Spline Regression
summary(m1)

# Plot data
ggplot(sim,aes(x = Time, y = Percent)) + 
  scale_y_continuous(limits = c(80, 110)) +
  scale_x_continuous(breaks = c(0,10,22,34,46,58,70),
                     labels = c("Pre-\ntreatment","Post-\ntreatment","1","2","3","4","5")) + 
  geom_point(aes(x = Time, y = Mean, fill = Study), shape = 23, size = 7,color = "black") + 
  xlab("Years After Intervention") +
  ylab("Mean Change in Weight (Percent)") + 
  geom_abline(aes(intercept = 100,slope = 0), color = "orangered4", linetype = "dashed") + 
  geom_line(aes(x = Time, y = pred), color = "blue", size = 1.1) + 
  theme_classic() 

# Save plot 
ggsave("spline.png", dpi = 320, width = 30, units = "cm")


diet %>% 
  ggplot(aes(x = TimeMonth, y = Percent, color = Study)) +
  geom_point(size = 2) + 
  geom_line() + 
  scale_x_continuous(minor_breaks = F, breaks = c(0,10,22,34,46,58,70),
                     labels = c("Pre-\ntreatment","Post-\ntreatment","1","2","3","4","5")) + 
  scale_y_continuous(minor_breaks = F, limits = c(75,110)) + 
  geom_abline(aes(intercept = 100,slope = 0), color = "orangered4", linetype = "dashed") +
  xlab("Years After Intervention") +
  ylab("Mean Change in Weight (Percent)") + 
  annotate("rect", xmin=0, xmax=10, ymin=-Inf,  ymax=Inf, fill="black", alpha=0.3) +
  theme_classic()

ggsave("individual.png", dpi = 320, width = 30, units = "cm")

# Drop Pekkarinnen
simdrop <- sim %>% filter(!Study == "Pekkarinen (1997)")


simdrop$x <- simdrop$Time
simdrop$y <- simdrop$Percent
m2 <- lm(y ~ lspline(x, 10,marginal = F), data=simdrop)

simdrop$pred <- predict(m2)

# Spline Regression
summary(m2)

