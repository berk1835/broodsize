##### R script for visualising and analysing brood size data with a treatment and control
## 10/08/2022 RW, Weadick lab, University of Exeter


# Set up ------------------------------------------------------------------


## Set working directory 
setwd("C:/Users/rw617/OneDrive - University of Exeter/rw617/05_Lifespan_Assay/09_Cherry-worms/hip_acid-brood")


## Load in worm data and packages

if (!require("rstatix", quietly = TRUE))
  install.packages("rstatix", INSTALL_opts = '--no-lock')
library(rstatix)

alr <- read.csv("hip_brood_all_stats.csv")

mut1 <- read.csv("hip_brood_mut.csv")






# Mutate data -------------------------------------------------------------


## Mutate all data

eal <- alr %>%
  group_by(trt) %>%
  get_summary_stats(type = "mean_sd")

eal1 <- eal[-9,]
eal2 <- eal1[-17,]

## Add 'day' column
day <- c(2,3,4,5,6,7,8,9,2,3,4,5,6,7,8,9)
eal2$day <- day

eal2



# Create subsets: treatment and control

tdf <-  subset(eal2, trt == "hippuric acid",
           select=c(trt, variable, mean, sd, day))

cdf <-  subset(eal2, trt == "control",
               select=c(trt, variable, mean, sd, day))








# Create plot -------------------------------------------------------------


## Parse plots

plot(cdf$day, cdf$mean, main = "P. pacificus n = 30", type = "l", col = "lightblue", ylim = c(0, 80), xlim = c(2, 9), xaxs = "i", lwd = 2, ylab = "Number of juveniles", xlab = "Days since hatching")
par(new = TRUE)
plot(tdf$day, tdf$mean, type = "l", col = "coral", ylim = c(0, 80), xlim = c(2, 9), xaxs = "i", lwd = 2, ylab = "", xlab = "")



## Add legend 

legend(7, 60, legend=c("Control", "Hippuric\nacid"), col=c("lightblue", "coral"), lty=1:2, cex=0.8, bty="n")


## Add significance values (from aov later on in script)

text(2.8, 14, "ns")
text(3.8, 40, "ns")
text(5, 72.5, "**")
text(6.2, 20, "ns")
text(7.1, 5.5, "ns")


# Statistics --------------------------------------------------------------




## ANOVA

#Outlier detection

mut1 %>%
  group_by(trt, day) %>%
  identify_outliers(juveniles)



# Normality checking with Q-Q plot

if (!require("ggpubr", quietly = TRUE))
  install.packages("ggpubr", INSTALL_opts = '--no-lock')
library(ggpubr)

ggqqplot(mut1, "juveniles", ggtheme = theme_bw()) +
  facet_grid(day ~ trt, labeller = "label_both")



#Anova - Sphericity Assumption 
# If the interaction is not significant then execute pairwise t test comparison (end of script)

res.aov <- anova_test(data = mut1, dv = juveniles, wid = worm_ID,within = c(trt, day))
get_anova_table(res.aov)



## Run one-way repeated measures anova

myData.mean <- aggregate(mut1$juveniles,
                         by = list(mut1$worm_ID, mut1$day,
                                   mut1$trt),
                         FUN = 'mean')
colnames(myData.mean) <- c("worm_ID","day","trt","juveniles")
myData.mean <- myData.mean[order(myData.mean$worm_ID), ]
head(myData.mean)



stress.aov <- with(myData.mean,
                   aov(juveniles ~ day * trt +
                         Error(worm_ID / (day * trt)))
)


summary(stress.aov)







# only if needed: basic aov
res1 <- aov(juveniles ~ day * trt, data = mut1)
summary(res1)


## Only if needed: t-test
t.test(mut1$day, mut1$juveniles ~ mut1$trt)


## AOV day 5
mut5 <-  subset(mut1, day == 5,
                 select=c(Study, worm_ID, trt, day, juveniles))

mut5anova <- aov(juveniles ~ trt, data = mut5)

summary(mut5anova)


## AOV day 6
mut6 <-  subset(mut1, day == 6,
                select=c(Study, worm_ID, trt, day, juveniles))

mut6anova <- aov(juveniles ~ trt, data = mut6)

summary(mut6anova)




