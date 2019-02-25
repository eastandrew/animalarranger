############################################ for single room, just testing for treatment effects ####################

x2 <- abs(rnorm(50,3.5,1))  #fake lizard masses, n=50, mean=3.5, sd=1
plot(density(x2))           # density distribution of fake lizard masses
hist(x2, breaks=10)         # histogram of distribution of fake lizard masses broken into 10 bins
summary(x2)                 # quartiles/max/min/mean/median of fake lizard masses

n <- 1000                 # number of samples to take

testframe2 <- data.frame(replicate(n,sample(x2,length(x2), replace=F)))  #build dataframe from sampled (without replacement) lizard masses.  This creates the shuffling of lizards.
testframe2$treat <- factor(c(rep("cont",10),rep("low",10),rep("med",10),rep("high",10),rep("highhigh",10)), levels=c("cont","low","med","high","highhigh"))  #creates a vector in dataframe of 5 treatments, 10 lizards each that are "paired" with the random masses

summary(aov(x2~testframe2$treat))   # this is the ANOVA output for the fake lizard masses against the treatments in order for an example.  Singular values can be extracted from this using the [[]] and $ as the structure of the summary(aov()) output is a list of vectors

outputfullp2 <- matrix(mapply(function(x,y) summary(aov(y~x))[[1]]$'Pr(>F)'[[1]], testframe2[n+1],testframe2[1:n]))  # create a matrix of values extracted from a function's output applied across the dataframe
answerp2 <- which.max(outputfullp2)          # The above finds the p-value of the ANOVA, this line finds the largest of the distribution of p-values
outfullss2 <- matrix(mapply(function(x,y) summary(aov(y~factor(x)))[[1]]$'Sum Sq'[[2]], testframe2[n+1],testframe2[1:n]))
answers2 <- which.max(outfullss2)            # The above finds the sum of squares of the ANOVA, this line finds the largest of the distribution of sum of squares
outfulleff2 <- matrix(mapply(function(x,y) summary(aov(y~factor(x)))[[1]]$'Sum Sq'[[1]]/summary(aov(y~factor(x)))[[1]]$'Sum Sq'[[2]], testframe2[n+1],testframe2[1:n]))
answereff2 <- which.min(outfulleff2)        # The above finds the effect size of the ANOVA, this line finds the smallest of the distribution of effect sizes

# look at the ANOVA output for each of the best cases
summary(aov(testframe2[,answerp2]~testframe2$treat))         
summary(aov(testframe2[,answers2]~testframe2$treat))
summary(aov(testframe2[,answereff2]~testframe2$treat))
#

# plot the distribution of p-values, sum squares, and effect sizes
plot(density(outputfullp2))                                   
plot(density(outfullss2))
plot(density(outfulleff2))

# create boxplots for each of the masses by treatments for the best case
boxplot(testframe2[,answerp2]~testframe2$treat, main=bquote("max p-value of treat"~.(answerp2)))
boxplot(testframe2[,answers2]~testframe2$treat, main=bquote("max SS of treat"~.(answers2)))
boxplot(testframe2[,answereff2]~testframe2$treat, main=bquote("min effect size of treat"~.(answereff2)))

# create boxplot for each of the masses by treatments for the best case
dotchart(testframe2[,answerp2],labels=row.names(testframe2$treat),pch=16,groups= testframe2$treat, xlab="mass")




#### optional way more simple but slightly less robust/random method ####
x2ord <- sort(x2)   #rank the masses
x2ord
df <- data.frame(mass=x2ord)   #build dataframe holding masses
df$treat <- sample(c(rep(c("cont","low","med","high","highhigh"),10)),prob=c(rep((1/5),50)))  # add vector to dataframe sampling 50 treatments with equal probability (1/5)
df

boxplot(mass~treat, data=df)
dotchart(df$mass, labels=df$treat)
summary(aov(mass~factor(treat), data=df))

##### or additionally simpler route

x2ord <- sort(x2)   #rank the masses
x2ord
df <- data.frame(mass=x2ord)   #build dataframe holding masses
df$treat <- sample(c("cont","low","med","high","highhigh"),prob=c(rep((1/5),5)))  # add vector to dataframe sampling 5 treatments with equal probability (1/5)
df

boxplot(mass~treat, data=df)
dotchart(df$mass, labels=df$treat)  # see how the treatment order is random, but it is in order (what does this mean?)
summary(aov(mass~factor(treat), data=df))

####


#####################################################################################################################
