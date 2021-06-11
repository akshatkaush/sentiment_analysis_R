tyre1 <- c(32.998,36.435,32.777,37.637,36.304)
tyre2 <- c(33.523,31.995,35.006,27.879,31.297)
tyre3 <- c(34.445,32.806,33.414,36.861,36.972)
tyre4 <- c(39.596,38.937,36.124,37.695,36.586)
combined_g <-data.frame(cbind(tyre1,tyre2,tyre3,tyre4))
combined_g
summary(combined_g)
stacked_groups <- stack(combined_g)
stacked_groups
Anova_Results <- aov(values ~ ind, data
                     = stacked_groups)
summary(Anova_Results)
qf(.95,3,16)

