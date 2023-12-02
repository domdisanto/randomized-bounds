
library(pacman)
p_load(dplyr, ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

.df = .df_naive = data.frame(Iter = NULL, CI.L = NULL, CI.H = NULL, .valid = NULL)

.n = 1e3
B = 2000 
.alpha = 0.05
k1 = 20
k2 = 100
k3 = 1000 

for(b in 1:B){
  X = rnorm(.n)
  U = runif(1)
  Uk1 = min(runif(k1))
  Uk2 = min(runif(k2))
  Uk3 = min(runif(k3))
  
  .test = .test + as.integer(sqrt(U) > sqrt(Uk1)*sqrt(k1))
  tot = tot + 1
  
  CI.n = c(mean(X) - 1 / sqrt(.n * .alpha)
         , mean(X) + 1 / sqrt(.n * .alpha)
  )
  CI = c(mean(X) - sqrt(U) / sqrt(.n * .alpha)
         , mean(X) + sqrt(U) / sqrt(.n * .alpha)
  )
  CI.k1 = c(mean(X) - sqrt(Uk1)*sqrt(k1) / sqrt(.n * .alpha)
           , mean(X) + sqrt(Uk1)*sqrt(k1) / sqrt(.n * .alpha)
  )
  CI.k2 = c(mean(X) - sqrt(Uk2)*sqrt(k2) / sqrt(.n * .alpha)
           , mean(X) + sqrt(Uk2)*sqrt(k2) / sqrt(.n * .alpha)
  )
  CI.k3 = c(mean(X) - sqrt(Uk3)*sqrt(k3) / sqrt(.n * .alpha)
           , mean(X) + sqrt(Uk3)*sqrt(k3) / sqrt(.n * .alpha)
  )
  
  .valid = CI[1]<=0 & CI[2]>=0
  .valid.n = CI.n[1]<=0 & CI.n[2]>=0
  .valid.k1 = CI.k1[1]<=0 & CI.k1[2]>=0
  .valid.k2 = CI.k2[1]<=0 & CI.k2[2]>=0
  .valid.k3 = CI.k3[1]<=0 & CI.k3[2]>=0
  
  .df = rbind(.df, c(CI, .valid, "Randomized (k=1)"))
  .df = rbind(.df, c(CI.n, .valid.n, "Naive"))
  .df = rbind(.df, c(CI.k1, .valid.k1, "Randomized (k=20)"))
  .df = rbind(.df, c(CI.k2, .valid.k2, "Randomized (k=100)"))
  .df = rbind(.df, c(CI.k3, .valid.k3, "Randomized (k=1000)"))
}


colnames(.df) = c('CI.L', 'CI.U', 'Coverage', "Type")
.df$CI.L = as.numeric(.df$CI.L)
.df$CI.U = as.numeric(.df$CI.U)
.df$Coverage = as.logical(.df$Coverage)
.df = .df %>% mutate(width = CI.U - CI.L)


.ci_plot = .df %>% 
  arrange(CI.L) %>% 
  ggplot(aes(x = as.numeric(rownames(.))
             # , color=Type 
         )) +
  geom_point(aes(y = CI.L)) + 
  geom_point(aes(y = CI.U)) + 
  facet_grid(cols = vars(Type)) + 
  theme_minimal() + 
  xlab("Iteration") + 
  ylab("95% CI")


.width_plot = .df %>% 
  arrange(CI.L) %>% 
  ggplot(aes(y=width)) + 
  geom_boxplot() + 
  facet_grid(cols = vars(Type)) + 
  theme_minimal() + 
  # xlab("Iteration") + 
  ylab("95% CI")

.df %>% group_by(Type) %>% 
  summarize(mean(Coverage))

ggsave("GaussMean_CI_Plot.png", .ci_plot)
ggsave("GaussMean_WidthPlot.png", .width_plot)


# quick beta test 
x = 1:1e3
test = pbeta(1/x, 1, x, lower.tail = T)
summary(test)

plot(x, test, "l")

