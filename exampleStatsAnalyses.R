# load up any libraries you might need - you can also select them from the Packages window!
library(tidyr)
library(rstatix)
library(ggpubr)
library(datasets)
library(emmeans)

# some basic R commands

variable <- 1 # use the <- to assign
variable1 <- variable + 1

data(ChickWeight, package = "datasets")
head(ChickWeight, 20)

chickdata.1 <- subset(ChickWeight, Chick == "1")
chickdata.1$Chick <- NULL

chickPlot <- plot(chickdata.1$Time, chickdata.1$weight,
                  ylab = "Weight", xlab = "Time (Days)",
                  )

# what about linear modeling?
scatter.smooth(chickdata.1$Time, chickdata.1$weight,
                  ylab = "Weight", xlab = "Time (Days)",
)

lin <- lm(formula = weight ~ Time, data = chickdata.1)
print(lin)
summary(lin)

#####################
# trying out some correlation (with data not about chicks)
# use the training.csv data

# check if there is correlation between the subscales: Pearson correlation
corplot <- ggscatter(training, x = "bais.v", y = "bais.c", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "BAIS V", ylab = "BAIS C")

# we can't just stop here - we need to make sure our data actually fits the test assumptions
# is the data normally distributed?: Shapiro-Wilk Test
shapiro.test(training$bais.v)
shapiro.test(training$bais.c)
# confirm visually with QQ plots - show the correlation between given points and a normal distribution
ggqqplot(training$bais.v, ylab = "BAIS V")
ggqqplot(training$bais.c, ylab = "BAISC")

# if they are NOT normal - we need to use a different test (here, ranked tests such as Spearman or Kendall)

# let's check out the correlation:
res <- cor.test(training$bais.v, training$bais.c, 
                method = "pearson")
res

# t = 4.319
# df = 14
# p = 0.0007
# correlation coefficient = 0.76

# t(14) = 4.319, p < 0.05

# maybe we then want to take an averaged BAIS score for future tests - add this to the dataset
training$bais.agg <- rowMeans(training[c('bais.v', 'bais.c')])

######################
# the t-test and friends

# t-test is used to compare the means of two-groups
# one-sample: compare your data to a known standard or theoretical mean
# independent sample: compare two groups
# paired sample: look data from one group at two different points (data is linked/paired)

# let R know about our groups
pitcherror %>%
  group_by(Group) %>%
  get_summary_stats(AvgABSError, type = "mean_sd")

# what does our data look like?
boxplot_t <- ggboxplot(
  pitcherror, x = "Group", y = "AvgABSError", 
  ylab = "Error (Cents)", xlab = "Group", add = "jitter"
)
boxplot_t

# need to check our assumptions before we go on:
# 1) check for outliers in the data
pitcherror %>%
  group_by(Group) %>%
  identify_outliers(AvgABSError)

# 2) check normal distribution of data - Shapiro wilk test by goups
pitcherror %>%
  group_by(Group) %>%
  shapiro_test(AvgABSError)
ggqqplot(pitcherror, x = "AvgABSError", facet.by = "Group")

# 3) check the variance is equal
pitcherror %>% levene_test(AvgABSError ~ Group) # it's not, so we use Welch's t-test

# ok cool, so let's actually do the t-test
welch_t <- pitcherror %>% 
  t_test(AvgABSError ~ Group) %>%
  add_significance()
welch_t

# how strong is this effect?
pitcherror %>%  cohens_d(AvgABSError ~ Group, var.equal = FALSE)

welch_t <- welch_t %>% add_xy_position(x = "Group")
boxplot_t + 
  stat_pvalue_manual(welch_t, tip.length = 0) +
  labs(subtitle = get_test_label(welch_t, detailed = TRUE))

# t(17.94) = -5.24, p < 0.0001

# what if my data isn't parametric?
# Wilcoxon testing (aka Mann-Whitney) - the test is harsher

wilcox <- pitcherror %>% 
  wilcox_test(AvgABSError ~ Group) %>%
  add_significance()
wilcox
pitcherror %>% wilcox_effsize(AvgABSError ~ Group)

#####################

# what if I like groups and so I have more than two?
# ANOVA (analyis of variance)

ratings %>%
  group_by(Gender, Instr) %>%
  get_summary_stats(Rating, type = "mean_sd")

# focal variable is Instr, moderator variable is Gender
boxplot_a <- ggboxplot(
  ratings, x = "Instr", y = "Rating",
  color = "Gender", palette = "jco"
)
boxplot_a 

# check for outliers - what do we do with them?
ratings %>%
  group_by(Instr, Gender) %>%
  identify_outliers(Rating)

ratings.alt <- ratings[-c(12,35), ]
ratings.alt %>%
  group_by(Instr, Gender) %>%
  identify_outliers(Rating)

# build a linear model (ANOVA is basically linear modeling in disguise - more on that later)
model <- lm(Rating ~ Instr*Gender,data = ratings)
# check that normality distribution
shapiro_test(residuals(model))
ggqqplot(residuals(model))

# now we check it by group
ratings %>%
  group_by(Instr, Gender) %>%
  shapiro_test(Rating)
# you can confirm visually if you like (I like)
ggqqplot(ratings, "Rating", ggtheme = theme_bw()) +
  facet_grid(Instr ~ Gender)

# Levine's to check our variances are the same
ratings %>% levene_test(Rating ~ Instr*Gender)

# okay, all good - let's look at those interactions
res.aov <- ratings %>% anova_test(Rating ~ Instr * Gender)
res.aov

# what about if we remove the outliers?
model_alt <- lm(Rating ~ Instr*Gender,data = ratings.alt)
shapiro_test(residuals(model_alt))
ggqqplot(residuals(model_alt))
ratings.alt %>%
  group_by(Instr, Gender) %>%
  shapiro_test(Rating)
ggqqplot(ratings.alt, "Rating", ggtheme = theme_bw()) +
  facet_grid(Instr ~ Gender)
ratings.alt %>% levene_test(Rating ~ Instr*Gender)

res.aov <- ratings.alt %>% anova_test(Rating ~ Instr * Gender)
res.aov
# results are similar - it's up to you to decide. In this case, we'd probably just leave them in

# let's check our main effect (Instr) by Gender
model_main <- lm(Rating ~ Gender * Instr, data = ratings)
ratings %>%
  group_by(Gender) %>%
  anova_test(Rating ~ Instr, error = model_main)
# the effect of Instrument was significant for both males, F(2,41) = 18.2, p < 0.05, and females F(2,41) = 46.2, p < 0.05,
# with principal instruments of voice, strings, or winds.

# so now we check pairwise comparisons (how these two variables interact depending on Instr)
pairwise <- ratings %>% 
  group_by(Gender) %>%
  emmeans_test(Rating ~ Instr, p.adjust.method = "bonferroni") 
pairwise

# the effect was significant for all principal instruments on rating, except for female wind players and vocalists