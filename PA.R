library(tidyverse)
library(psych)
library(nFactors)


dat <- readRDS("N:/durable/projects/scq_efa/CFA/data_dx_NPRupdate.rds") 

dat_split <- dat %>%
   filter(split50 == 1) #EFA sample

#tetrachoric cor matrix 
cor.obj <- dat_split %>%
  select(starts_with("NN")) %>%
  tetrachoric(.) 

cor.mat <- cor.obj$rho

#Getting eigen values and running parallel analysis 
ev <- eigen(cor.mat)

ap <- parallel(subject = nrow(dat_split),
               var = 38,
               rep = 500, 
               quantile = 0.95)

#scree plot 
scree <- nScree(x = ev$values,
                aparallel = ap$eigen$mevpea)

plotnScree(scree)

scree$Analysis %>% 
  mutate(Components = as.numeric(row.names(.))) %>%
  as.data.frame(.) %>%
  ggplot(aes(x = Components, y = Eigenvalues)) + 
  geom_point(size = 4) +
  geom_line(size = 0.8) + 
  geom_point(aes(x = Components, y = Par.Analysis, ),
             shape = 2,
             size = 4) +
  geom_line(aes(x = Components, y = Par.Analysis), 
            color = "red", 
            size = 0.7) + 
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.text = element_text(size = 16, color = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 18,face = "bold"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 18, face = "bold"))

  