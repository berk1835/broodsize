##### R script for visualising and analysing brood size data with a treatment and control - mini version
## 20/10/2022 RW, Weadick lab, University of Exeter


# Set up ------------------------------------------------------------------


## EDIT ME: Set working directory 
## Find yours by clicking ctrl+shift+H, and navigate to where your data is. Copy the folder path into the speech marks below.

setwd("C:/Users/rw617/OneDrive - University of Exeter/rw617/05_Lifespan_Assay/09_Cherry-worms/hip_acid-brood")


## Load in packages

if (!require("rstatix", quietly = TRUE))
  install.packages("rstatix", INSTALL_opts = '--no-lock')
library(rstatix)



## EDIT ME: Load in data
# Change csv file names to your file names

alr <- read.csv("hip_brood_all_stats.csv")

mut1 <- read.csv("hip_brood_mut.csv")






# Mutate data -------------------------------------------------------------


## Find mean and sd for data and turn it into a table

eal <- alr %>%
  group_by(trt) %>%
  get_summary_stats(type = "mean_sd")



## EDIT ME: If any rows in eal should not be present (e.g., a variable called 'study'), use below to remove them.
# If it all looks okay, ignore.

#eal1 <- eal[-9,]
#eal <- eal1[-17,]




## Add 'day' column
# This is in addition to the J4+n column to make a nice plot. 
## May need amending if your eal file is a bit different.

day <- c(2,3,4,5,6,7,8,9,2,3,4,5,6,7,8,9)
eal$day <- day

eal



## Create subsets: treatment and control
# Split data up by treatment for plot

tdf <-  subset(eal, trt == "hippuric acid",
               select=c(trt, variable, mean, sd, day))

cdf <-  subset(eal, trt == "control",
               select=c(trt, variable, mean, sd, day))





# Create plot -------------------------------------------------------------


## Parse plots

plot(cdf$day, cdf$mean, main = "P. pacificus n = 30", type = "l", col = "lightblue", ylim = c(0, 80), xlim = c(2, 9), xaxs = "i", lwd = 2, ylab = "Number of juveniles", xlab = "Days since hatching")
par(new = TRUE)
plot(tdf$day, tdf$mean, type = "l", col = "coral", ylim = c(0, 80), xlim = c(2, 9), xaxs = "i", lwd = 2, ylab = "", xlab = "")



## Add legend 

legend(7, 60, legend=c("Control", "Hippuric\nacid"), col=c("lightblue", "coral"), lty=1:2, cex=0.8, bty="n")



## Add significance values 
# From aov later on in script; not accurate at first.

#text(5, 72.5, "**")





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
# If the interaction is not significant then execute pairwise t test comparison 

res.aov <- anova_test(data = mut1, dv = juveniles, wid = worm_ID,within = c(trt, day))
get_anova_table(res.aov)





## Only if needed: t-test
#t.test(mut1$day, mut1$juveniles ~ mut1$trt)






# Basic aov - overall treatment vs. control (would expect no difference in our case)
res1 <- aov(juveniles ~ day * trt, data = mut1)
summary(res1)



#### Anova for individual timepoints 

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


