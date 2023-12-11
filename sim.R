
library(pacman)
p_load(dplyr, ggplot2, xtable)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
set.seed(235)

.df = .df_naive = data.frame(Iter = NULL, CI.L = NULL, CI.H = NULL, .valid = NULL)

.n = 1000
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
  
  # .test = .test + as.integer(sqrt(U) > sqrt(Uk1)*sqrt(k1))
  # tot = tot + 1
  
  CI.n = c(mean(X) - 1 / sqrt(.n * .alpha)
         , mean(X) + 1 / sqrt(.n * .alpha)
  )
  
  CI.g = mean(X) + c(-1, 1)*qnorm(0.025, lower.tail=F) / sqrt(.n)
  
  .hoefB = sqrt(2*log(2/.alpha) / .n) + log(U) / sqrt(2*.n*log(2/.alpha))
  CI.H = mean(X) + c(-1, 1)*.hoefB
  
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
  .valid.g = CI.g[1]<=0 & CI.g[2]>=0
  .valid.H = CI.H[1]<=0 & CI.H[2]>=0
  .valid.k1 = CI.k1[1]<=0 & CI.k1[2]>=0
  .valid.k2 = CI.k2[1]<=0 & CI.k2[2]>=0
  .valid.k3 = CI.k3[1]<=0 & CI.k3[2]>=0
  
  .df = rbind(.df, c(CI, .valid, "Randomized (k=1)"))
  .df = rbind(.df, c(CI.n, .valid.n, "Naive"))
  .df = rbind(.df, c(CI.H, .valid.H, "Randomized Hoeffding"))
  .df = rbind(.df, c(CI.g, .valid.g, "Gaussian"))
  .df = rbind(.df, c(CI.k1, .valid.k1, "Randomized (k=20)"))
  .df = rbind(.df, c(CI.k2, .valid.k2, "Randomized (k=100)"))
  .df = rbind(.df, c(CI.k3, .valid.k3, "Randomized (k=1000)"))
}


colnames(.df) = c('CI.L', 'CI.U', 'Coverage', "Type")
.df$CI.L = as.numeric(.df$CI.L)
.df$CI.U = as.numeric(.df$CI.U)
.df$Coverage = as.logical(.df$Coverage)
.dfw = .df %>% mutate(width = abs(CI.U - CI.L))


.ci_plot = .dfw %>% 
  arrange(CI.L) %>% 
  group_by(Type) %>% 
  mutate(Iteration = row_number()) %>% 
  ungroup() %>% 
  ggplot(aes(x = Iteration
             # , color=Type 
         )) +
  geom_point(aes(y = CI.L)) + 
  geom_point(aes(y = CI.U)) + 
  scale_x_continuous(breaks = c(0, 1000, 2000)) + 
  facet_grid(cols = vars(Type)) + 
  theme_minimal() + 
  xlab("Iteration") + 
  ylab("95% CI") #+ 
  # labs(title = "Comparison of Confidence Intervals by Tail-Bound"
  # , subtitle = "Gaussian Mean 95% Confidence Interval, n=1000")
  

.width_plot = .dfw %>% 
  arrange(CI.L) %>% 
  ggplot(aes(y=width)) + 
  geom_boxplot() + 
  facet_grid(cols = vars(Type)) + 
  theme_minimal() + 
  # xlab("Iteration") + 
  ylab("95% CI") #+ 
  # labs(title = "Comparison of Confidence Interval Width by Tail-Bound"
     # , subtitle = "Gaussian Mean 95% Confidence Interval, n=1000")


# .test = .dfw %>% group_by(Type) %>%
#   summarize(mean(Coverage))
# utils::writeClipboard
# print(xtable(.test), include.rownames=F)



ggsave("GaussMean_CI_Plot.png", .ci_plot, height = 4, width=12)
ggsave("GaussMean_WidthPlot.png", .width_plot, height = 4, width=12)



# Non subGaussian for Hoeffding "counter"example
# .n = 1000
# B = 2000
# .alpha = 0.05
# k = 5
# 
# .dfnsubG = data.frame(Iter = NULL, CI.L = NULL, CI.H = NULL, .valid = NULL)
# 
# for(b in 1:B){
#   X = (rchisq(.n, df=k)-k) / sqrt(2*k)
#   U = runif(1)
#   
#   CI.n = c(mean(X) - 1 / sqrt(.n * .alpha)
#            , mean(X) + 1 / sqrt(.n * .alpha)
#   )
#   
#   CI.g = mean(X) + c(-1, 1)*qnorm(0.025, lower.tail=F) / sqrt(.n)
#   
#   .hoefB = sqrt(2*log(2/.alpha) / .n) + log(U) / sqrt(2*.n*log(2/.alpha))
#   CI.H = mean(X) + c(-1, 1)*.hoefB
#   
#   CI = c(mean(X) - sqrt(U) / sqrt(.n * .alpha)
#          , mean(X) + sqrt(U) / sqrt(.n * .alpha)
#   )
# 
#   .valid = CI[1]<=0 & CI[2]>=0
#   .valid.n = CI.n[1]<=0 & CI.n[2]>=0
#   .valid.g = CI.g[1]<=0 & CI.g[2]>=0
#   .valid.H = CI.H[1]<=0 & CI.H[2]>=0
# 
#   .dfnsubG = rbind(.dfnsubG, c(CI.n, .valid.n, "Naive"))
#   .dfnsubG = rbind(.dfnsubG, c(CI, .valid, "Randomized Markov"))
#   .dfnsubG = rbind(.dfnsubG, c(CI.H, .valid.H, "Randomized Hoeffding"))
#   .dfnsubG = rbind(.dfnsubG, c(CI.g, .valid.g, "Gaussian"))
# }
# 
# 
# colnames(.dfnsubG) = c('CI.L', 'CI.U', 'Coverage', "Type")
# .dfnsubG$CI.L = as.numeric(.dfnsubG$CI.L)
# .dfnsubG$CI.U = as.numeric(.dfnsubG$CI.U)
# .dfnsubG$Coverage = as.logical(.dfnsubG$Coverage)
# .dfnsubGw = .dfnsubG %>% mutate(width = abs(CI.U - CI.L))
# 
# 
# # .tbl_nsubG = 
#   .dfnsubGw %>% group_by(Type) %>%
#   summarize(mean(Coverage))
# # utils::writeClipboard
# # print(xtable(.test), include.rownames=F)
# 
# 
# 
# 
# .dfnsubGw %>% 
#   arrange(CI.L) %>% 
#   group_by(Type) %>% 
#   mutate(Iteration = row_number()) %>% 
#   ungroup() %>% 
#   ggplot(aes(x = Iteration
#              # , color=Type 
#   )) +
#   geom_point(aes(y = CI.L)) + 
#   geom_point(aes(y = CI.U)) + 
#   scale_x_continuous(breaks = c(0, 1000, 2000)) + 
#   facet_grid(cols = vars(Type)) + 
#   theme_minimal() + 
#   xlab("Iteration") + 
#   ylab("95% CI") #+ 
# # labs(title = "Comparison of Confidence Intervals by Tail-Bound"
# # , subtitle = "Gaussian Mean 95% Confidence Interval, n=1000")
# 
# 
# .dfnsubGw %>% 
#   arrange(CI.L) %>% 
#   ggplot(aes(y=width)) + 
#   geom_boxplot() + 
#   facet_grid(cols = vars(Type)) + 
#   theme_minimal() + 
#   # xlab("Iteration") + 
#   ylab("95% CI") #+ 
# # labs(title = "Comparison of Confidence Interval Width by Tail-Bound"
# # , subtitle = "Gaussian Mean 95% Confidence Interval, n=1000")
