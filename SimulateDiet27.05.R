
## Load packages

library(tidyverse)

library(lspline)

############## Simulate datapoints #############
set.seed(249)
#Cooper 2010
id <- seq(1:150)
"Cooper0" <- rnorm(id,mean = 100,sd = 9.26)
"Cooper1" <- rnorm(id,
                   mean = as.numeric(data%>% 
                                       filter(TimeMonth == 1 & Study == "Cooper2010") %>% 
                                       ungroup() %>% 
                                       select(Percent)),
                   sd = as.numeric(data%>% 
                                     filter(TimeMonth == 1 & Study == "Cooper2010") %>% 
                                     ungroup() %>% 
                                     select(SDpercent)))
"Cooper6" <- rnorm(id,
                   mean = as.numeric(data%>% 
                                       filter(TimeMonth == 6 & Study == "Cooper2010") %>% 
                                       ungroup() %>% 
                                       select(Percent)),
                   sd = as.numeric(data%>% 
                                     filter(TimeMonth == 6 & Study == "Cooper2010") %>% 
                                     ungroup() %>% 
                                     select(SDpercent)))
"Cooper12" <- rnorm(id,
                    mean = as.numeric(data%>% 
                                        filter(TimeMonth == 12 & Study == "Cooper2010") %>% 
                                        ungroup() %>% 
                                        select(Percent)),
                    sd = as.numeric(data%>% 
                                      filter(TimeMonth == 12 & Study == "Cooper2010") %>% 
                                      ungroup() %>% 
                                      select(SDpercent)))
"Cooper24" <- rnorm(id,
                    mean = as.numeric(data%>% 
                                        filter(TimeMonth == 24 & Study == "Cooper2010") %>% 
                                        ungroup() %>% 
                                        select(Percent)),
                    sd = as.numeric(data%>% 
                                      filter(TimeMonth == 24 & Study == "Cooper2010") %>% 
                                      ungroup() %>% 
                                      select(SDpercent)))
"Cooper36" <- rnorm(id,
                    mean = as.numeric(data%>% 
                                        filter(TimeMonth == 36 & Study == "Cooper2010") %>% 
                                        ungroup() %>% 
                                        select(Percent)),
                    sd = as.numeric(data%>% 
                                      filter(TimeMonth == 36 & Study == "Cooper2010") %>% 
                                      ungroup() %>% 
                                      select(SDpercent)))
sim <- data.frame(id,Cooper0,Cooper1,Cooper6,Cooper12,Cooper24,Cooper36)
sim <- sim %>% gather("Time","Percent",2:7)
sim$Time <- str_remove(sim$Time, "Cooper")
sim$Time <- as.integer(sim$Time)
sim <- add_column(sim, Study = "Cooper(2010)")
sim <- as.tibble(sim)
sim <- arrange(sim,id)
x <- as.tibble(data%>% 
                 filter(Study == "Cooper2010") %>% 
                 group_by(TimeMonth) %>% 
                 select(Percent))
sim <- add_column(sim, Mean = rep(x$Percent,150))
rm(Cooper0,Cooper1,Cooper12,Cooper6,Cooper24,Cooper36,id,x)
# Stalonas (1984)
id <- c(151:264)
"S0" <- rnorm(id,mean = 100,sd = 11.63)
"S1" <- rnorm(id,
              mean = as.numeric(data%>% 
                                  filter(TimeMonth == 1 & Study == "Stalonas1984") %>% 
                                  ungroup() %>% 
                                  select(Percent)),
              sd = as.numeric(data%>% 
                                filter(TimeMonth == 1 & Study == "Stalonas1984") %>% 
                                ungroup() %>% 
                                select(SDpercent)))
"S3" <- rnorm(id,
              mean = as.numeric(data%>% 
                                  filter(TimeMonth == 3 & Study == "Stalonas1984") %>% 
                                  ungroup() %>% 
                                  select(Percent)),
              sd = as.numeric(data%>% 
                                filter(TimeMonth == 3 & Study == "Stalonas1984") %>% 
                                ungroup() %>% 
                                select(SDpercent)))
"S12" <- rnorm(id,
               mean = as.numeric(data%>% 
                                   filter(TimeMonth == 12 & Study == "Stalonas1984") %>% 
                                   ungroup() %>% 
                                   select(Percent)),
               sd = as.numeric(data%>% 
                                 filter(TimeMonth == 12 & Study == "Stalonas1984") %>% 
                                 ungroup() %>% 
                                 select(SDpercent)))
"S60" <- rnorm(id,
               mean = as.numeric(data%>% 
                                   filter(TimeMonth == 60 & Study == "Stalonas1984") %>% 
                                   ungroup() %>% 
                                   select(Percent)),
               sd = as.numeric(data%>% 
                                 filter(TimeMonth == 60 & Study == "Stalonas1984") %>% 
                                 ungroup() %>% 
                                 select(SDpercent)))
sim2 <- data.frame(id,S0,S1,S3,S12,S60)
sim2 <- sim2 %>% gather("Time","Percent",2:6)
sim2$Time <- str_remove(sim2$Time, "S")
sim2$Time <- as.integer(sim2$Time)
sim2 <- add_column(sim2, Study = "Stalonas(1984)")
sim2 <- as.tibble(sim2)
sim2 <- arrange(sim2,id)
x <- as.tibble(data%>% 
                 filter(Study == "Stalonas1984") %>% 
                 group_by(TimeMonth) %>% 
                 select(Percent))
sim2 <- add_column(sim2, Mean = rep(x$Percent,114))
sim <- bind_rows(sim,sim2)
rm(sim2,S0,S1,S3,S12,S60,id,x)
# Henserud 1994
id <- c(265:288)
"H0" <- rnorm(id,mean = 100,sd = 14.2)
"H1" <- rnorm(id,
              mean = as.numeric(data%>% 
                                  filter(TimeMonth == 1 & Study == "Henserud1994") %>% 
                                  ungroup() %>% 
                                  select(Percent)),
              sd = as.numeric(data%>% 
                                filter(TimeMonth == 1 & Study == "Henserud1994") %>% 
                                ungroup() %>% 
                                select(SDpercent)))
"H12" <- rnorm(id,
               mean = as.numeric(data%>% 
                                   filter(TimeMonth == 12 & Study == "Henserud1994") %>% 
                                   ungroup() %>% 
                                   select(Percent)),
               sd = as.numeric(data%>% 
                                 filter(TimeMonth == 12 & Study == "Henserud1994") %>% 
                                 ungroup() %>% 
                                 select(SDpercent)))
"H50" <- rnorm(id,
               mean = as.numeric(data%>% 
                                   filter(TimeMonth == 50 & Study == "Henserud1994") %>% 
                                   ungroup() %>% 
                                   select(Percent)),
               sd = as.numeric(data%>% 
                                 filter(TimeMonth == 50 & Study == "Henserud1994") %>% 
                                 ungroup() %>% 
                                 select(SDpercent)))
sim2 <- data.frame(id,H0,H1,H12,H50)
sim2 <- sim2 %>% gather("Time","Percent",2:5)
sim2$Time <- str_remove(sim2$Time, "H")
sim2$Time <- as.integer(sim2$Time)
sim2 <- add_column(sim2, Study = "Henserud(1994)")
sim2 <- as.tibble(sim2)
sim2 <- arrange(sim2,id)
x <- as.tibble(data%>% 
                 filter(Study == "Henserud1994") %>% 
                 group_by(TimeMonth) %>% 
                 select(Percent))
sim2 <- add_column(sim2, Mean = rep(x$Percent,24))
sim <- bind_rows(sim,sim2)
rm(sim2,H0,H1,H12,H50,id,x)
# Pekkarinnen 1997
id <- c(289:316)
"P0" <- rnorm(id,mean = 100,sd = 14.2)
"P1" <- rnorm(id,
              mean = as.numeric(data%>% 
                                  filter(TimeMonth == 1 & Study == "Pekkarinen1997") %>% 
                                  ungroup() %>% 
                                  select(Percent)),
              sd = as.numeric(data%>% 
                                filter(TimeMonth == 1 & Study == "Pekkarinen1997") %>% 
                                ungroup() %>% 
                                select(SDpercent)))
"P60" <- rnorm(id,
               mean = as.numeric(data%>% 
                                   filter(TimeMonth == 60 & Study == "Pekkarinen1997") %>% 
                                   ungroup() %>% 
                                   select(Percent)),
               sd = as.numeric(data%>% 
                                 filter(TimeMonth == 60 & Study == "Pekkarinen1997") %>% 
                                 ungroup() %>% 
                                 select(SDpercent)))
sim2 <- data.frame(id,P0,P1,P60)
sim2 <- sim2 %>% gather("Time","Percent",2:4)
sim2$Time <- str_remove(sim2$Time, "P")
sim2$Time <- as.integer(sim2$Time)
sim2 <- add_column(sim2, Study = "Pekkarinen(1997)")
sim2 <- as.tibble(sim2)
sim2 <- arrange(sim2,id)
x <- as.tibble(data%>% 
                 filter(Study == "Pekkarinen1997") %>% 
                 group_by(TimeMonth) %>% 
                 select(Percent))
sim2 <- add_column(sim2, Mean = rep(x$Percent,28))
sim <- bind_rows(sim,sim2)
rm(sim2,P0,P1,P60,id,x)
# Wadden 1988
id <- c(317:366)
"W0" <- rnorm(id,mean = 100,sd = 10)
"W1" <- rnorm(id,
              mean = as.numeric(data%>% 
                                  filter(TimeMonth == 1 & Study == "Wadden1988") %>% 
                                  ungroup() %>% 
                                  select(Percent)),
              sd = as.numeric(data%>% 
                                filter(TimeMonth == 1 & Study == "Wadden1988") %>% 
                                ungroup() %>% 
                                select(SDpercent)))
"W12" <- rnorm(id,
               mean = as.numeric(data%>% 
                                   filter(TimeMonth == 12 & Study == "Wadden1988") %>% 
                                   ungroup() %>% 
                                   select(Percent)),
               sd = as.numeric(data%>% 
                                 filter(TimeMonth == 12 & Study == "Wadden1988") %>% 
                                 ungroup() %>% 
                                 select(SDpercent)))
"W36" <- rnorm(id,
               mean = as.numeric(data%>% 
                                   filter(TimeMonth == 36 & Study == "Wadden1988") %>% 
                                   ungroup() %>% 
                                   select(Percent)),
               sd = as.numeric(data%>% 
                                 filter(TimeMonth == 36 & Study == "Wadden1988") %>% 
                                 ungroup() %>% 
                                 select(SDpercent)))
sim2 <- data.frame(id,W0,W1,W12,W36)
sim2 <- sim2 %>% gather("Time","Percent",2:5)
sim2$Time <- str_remove(sim2$Time, "W")
sim2$Time <- as.integer(sim2$Time)
sim2 <- add_column(sim2, Study = "Wadden(1988)")
sim2 <- as.tibble(sim2)
sim2 <- arrange(sim2,id)
x <- as.tibble(data%>% 
                 filter(Study == "Wadden1988") %>% 
                 group_by(TimeMonth) %>% 
                 select(Percent))
sim2 <- add_column(sim2, Mean = rep(x$Percent,50))
sim <- bind_rows(sim,sim2)
rm(sim2,id,W0,W1,W12,W36,x)
# Vogels 2005
id <- c(367:457)
"V0" <- rnorm(id,mean = 100,sd = 10.5)
"V1" <- rnorm(id,
              mean = as.numeric(data%>% 
                                  filter(TimeMonth == 1 & Study == "Vogels2005") %>% 
                                  ungroup() %>% 
                                  select(Percent)),
              sd = as.numeric(data%>% 
                                filter(TimeMonth == 1 & Study == "Vogels2005") %>% 
                                ungroup() %>% 
                                select(SDpercent)))
"V45" <- rnorm(id,
               mean = as.numeric(data%>% 
                                   filter(TimeMonth == 48 & Study == "Vogels2005") %>% 
                                   ungroup() %>% 
                                   select(Percent)),
               sd = as.numeric(data%>% 
                                 filter(TimeMonth == 48 & Study == "Vogels2005") %>% 
                                 ungroup() %>% 
                                 select(SDpercent)))
sim2 <- data.frame(id,V0,V1,V45)
sim2 <- sim2 %>% gather("Time","Percent",2:4)
sim2$Time <- str_remove(sim2$Time, "V")
sim2$Time <- as.integer(sim2$Time)
sim2 <- add_column(sim2, Study = "Vogels(2005)")
sim2 <- as.tibble(sim2)
sim2 <- arrange(sim2,id)
x <- c(100,90.9,100.8)
sim2 <- add_column(sim2, Mean = rep(x,91))
sim <- bind_rows(sim,sim2)
rm(sim2,id,V0,V1,V45,x)
# Schwarzfuchs 2012
id <- c(458:779)
"S0" <- rnorm(id,mean = 100,sd = 13.2)
"S1" <- rnorm(id,
              mean = as.numeric(data%>% 
                                  filter(TimeMonth == 1 & Study == "Schwarzfuchs2012") %>% 
                                  ungroup() %>% 
                                  select(Percent)),
              sd = as.numeric(data%>% 
                                filter(TimeMonth == 1 & Study == "Schwarzfuchs2012") %>% 
                                ungroup() %>% 
                                select(SDpercent)))
"S24" <- rnorm(id,
               mean = as.numeric(data%>% 
                                   filter(TimeMonth == 24 & Study == "Schwarzfuchs2012") %>% 
                                   ungroup() %>% 
                                   select(Percent)),
               sd = as.numeric(data%>% 
                                 filter(TimeMonth == 24 & Study == "Schwarzfuchs2012") %>% 
                                 ungroup() %>% 
                                 select(SDpercent)))
"S46" <- rnorm(id,
               mean = as.numeric(data%>% 
                                   filter(TimeMonth == 48 & Study == "Schwarzfuchs2012") %>% 
                                   ungroup() %>% 
                                   select(Percent)),
               sd = as.numeric(data%>% 
                                 filter(TimeMonth == 48 & Study == "Schwarzfuchs2012") %>% 
                                 ungroup() %>% 
                                 select(SDpercent)))
sim2 <- data.frame(id,S0,S1,S24,S46)
sim2 <- sim2 %>% gather("Time","Percent",2:5)
sim2$Time <- str_remove(sim2$Time, "S")
sim2$Time <- as.integer(sim2$Time)
sim2 <- add_column(sim2, Study = "Schwarzfuchs(2012)")
sim2 <- as.tibble(sim2)
sim2 <- arrange(sim2,id)
x <- c(100,93.8,95.6,98.0)
sim2 <- add_column(sim2, Mean = rep(x,322))
sim <- bind_rows(sim,sim2)
rm(sim2,id,S0,S1,S24,S46,x)
## Asses that mean Pretreatment is 100
sim %>% filter(Time == 0) %>% summarise(mean = mean(Percent))
############# Make a Pre and Post Before Follow-up Measures
sim$Time <- sim$Time+10
sim$Time[sim$Time == 10] <- 0
sim$Time[sim$Time == 11] <- 10
############## Model linear spline regression
# Convert x and y values for the lspline procedure
sim$x <- sim$Time
sim$y <- sim$Percent
m1 <- lm(y ~ lspline(x, 10,marginal = F), data=sim)
sim$pred <- predict(m1)
m1

# Plot data
ggplot(sim,aes(x = Time, y = Percent)) + 
  scale_y_continuous(limits = c(80, 110)) +
  scale_x_continuous(breaks = c(0,10,22,34,46,58,70),
                     labels = c("Pre-\ntreatment","Post-\ntreatment","1","2","3","4","5")) + 
  geom_point(aes(x = Time, y = Mean, fill = Study), shape = 23, size = 7,color = "black") + 
  xlab("Years After Intervention") +
  geom_abline(aes(intercept = 100,slope = 0), color = "orangered4", linetype = "dashed") + 
  geom_line(aes(x = Time, y = pred), color = "blue", size = 1.1) + 
  theme_classic() 

# Save plot
ggsave("spline.png", dpi = 320)


### Plot individual studies
sim2 <- data
sim2$TimeMonth[sim2$TimeMonth == -1]<- -10
sim2$TimeMonth[sim2$TimeMonth == 1]<- 0


sim2 %>% 
  ggplot(aes(x = TimeMonth, y = Percent, color = Study)) +
  geom_point(size = 2) + 
  geom_line() + 
  scale_x_continuous(minor_breaks = F, breaks = c(-10,0,12,24,36,48,60),
                     labels = c("Pre-\ntreatment","Post-\ntreatment","1","2","3","4","5")) + 
  scale_y_continuous(minor_breaks = F, limits = c(75,110)) + 
  geom_abline(aes(intercept = 100,slope = 0), color = "orangered4", linetype = "dashed") +
  xlab("Years After Intervention") +
  annotate("rect", xmin=-10, xmax=0, ymin=-Inf,  ymax=Inf, fill="black", alpha=0.3) +
  theme_classic()

# Save plot
ggsave("individ.png", dpi = 320)
