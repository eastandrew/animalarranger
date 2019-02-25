liz <- data.frame(lizard_id = c(11, 15, 23, 29, 33, 35, 36, 37,
               45, 50, 8, 30, 34, 43, 53, 55, 62, 65, 70, 74, 5, 6, 10, 25, 
               39, 42, 63, 71, 79, 80, 16, 26, 28, 32, 40, 44, 57, 72, 76, 81,
               13, 22, 24, 27, 49, 58, 66, 77, 78, 82),
  liz_mass_g = c(5.25, 5.05, 8.55, 5.65, 7.5, 6.55, 7, 6,
                 6.25, 5.5, 6.75, 7.9, 7.15, 6.05, 5.55, 5.9, 6.45, 4.9, 5.15, 
                 5.35, 5.95, 6.9, 7.25, 8.1, 5.35, 6.45, 6.15, 4.95, 5.6, 5.2, 
                 5.55, 6.6, 7, 7.75, 5.8, 6.35, 5.35, 4.8, 6.05, 5.1, 5.65, 8.2, 
                 6.5, 5.95, 6.25, 7.3, 5.45, 7, 5.25, 5))
liz

liz2 <- liz[order(liz$liz_mass_g),]

liz2$treat <- sample(c("cont","low","med","high","highhigh"),prob=c(rep((1/5),5)))

boxplot(liz_mass_g~treat, data=liz2)
dotchart(liz2$liz_mass_g, labels=liz2$treat)  # see how the treatment order is random, but it is in order (what does this mean?)
summary(aov(liz_mass_g~factor(treat), data=liz2))

plot(density(liz2$liz_mass_g[liz2$treat=="cont"],bw=0.4),ylim=c(-0.1,0.4))
points(density(liz2$liz_mass_g[liz2$treat=="low"],bw=0.4),type="l", col="darkgreen")
points(density(liz2$liz_mass_g[liz2$treat=="med"],bw=0.4),type="l", col="blue")
points(density(liz2$liz_mass_g[liz2$treat=="high"],bw=0.4),type="l", col="orange")
points(density(liz2$liz_mass_g[liz2$treat=="high"],bw=0.4),type="l", col="red")
rug(liz2$liz_mass_g[liz2$treat=="cont"],line=-5)
rug(liz2$liz_mass_g[liz2$treat=="low"], col="darkgreen",line=-4)
rug(liz2$liz_mass_g[liz2$treat=="med"], col="blue",line=-3)
rug(liz2$liz_mass_g[liz2$treat=="high"], col="orange",line=-2)
rug(liz2$liz_mass_g[liz2$treat=="highhigh"], col="red",line=-1)
