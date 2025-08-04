###############################################################################
### Script: FordKa_Extended.R
### Version: 1.0
### Copyright: (c) 2024 by Alan Montgomery. Distributed using license CC BY-NC 4.0
###   To view this license see https://creativecommons.org/licenses/by-nc/4.0/
### Notes:
###   R script to create customer segments for ford ka using k-Means
###   Requires the excel spreadsheet with the data (FordKaData.xlsx).
###   This script creates clusters using both the psychographic and demographic
###   datasets using k-means analysis with varying clusters.
###   Changes that you need to make have "!!!"
### Input Files:
###   FordKaData.xlsx          Excel dataset
### Output Files:
###   FordKa_Results.xlsx      Tables from the cluster analyses
### Variables:
###   df_forddemo              Demographic data (data frame)
###   df_fordpsyc              Psycholographic data (data frame)
###   df_fordquest             Psychographic questionnaire data (data frame)
###   df_fordseg               Segments data (data frame)
###   df_ford                  Main dataset (data frame)
###   df_forddemoStd           Demographic data in standardized form (data frame)
###   df_fordpsycStd           Psycholographic data in standardized form (data frame)
###   df_fordStd               Main dataset in standardized form (data frame)
###   cvec_fordquest           Truncated psychographic questionnaire data (character vector)
###   cvec_qlist               Question list (character vector)
###   nvec_qlistShort          Short question list (numerical vector)
###   cvec_qlistShort          Short question list (character vector)
###   cvec_qnameShort          Short question name (character vector)
###   cvec_qdlist              Question - demographic list (character vector)
###   nvec_qdlist              Question - demographic list (numerical vector)
###   nvec_vars               Numeric variables list (numerical vector)
###   cvec_dlist               Demographic variables list (character vector)
###   nvec_kclust              Parameter k list(numerical vector)
###   mtx_qcor                 Correlation matrix of question (matrix)
###   mtx_dist                 Dissimilarity matrix (matrix)
###   mtx_distsq               Squared of dissimilarity matrix (matrix)
###   mtx_cdistB               Cdist matrix for demographic data (matrix)
###############################################################################



###############################################################################
### setup the environment
###############################################################################

# load in additional packages to extend R functionality
if (!require(lattice)) {install.packages("lattice"); library(lattice)}
if (!require(gplots)) {install.packages("gplots"); library(gplots)}
if (!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}
if (!require(reshape2)) {install.packages("reshape2"); library(reshape2)}
if (!require(openxlsx)) {install.packages("openxlsx"); library(openxlsx)}
if (!require(gmodels)) {install.packages("gmodels"); library(gmodels)}
if (!require(factoextra)) {install.packages("factoextra"); library(factoextra)}

# set to your correct working directory
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # only works in Rstudio scripts
# alternatively set the working directory manually
setwd("~/Documents/class/analytical marketing/cases/ford ka/data")

###############################################################################
### alternative input of data from CSV files
### use this if you have trouble importing directly from Excel files
###############################################################################

# if you have problems with read.xlsx you can read in the data from CSV files (make sure you uncomment lines below and download CSV files)
# df_forddemo=read.csv("FordKaDemographicData.csv",row.names=1)
# df_fordpsyc=read.csv("FordKaPsychographicData.csv",row.names=1)
# cvec_fordquest=scan("FordKaQuestions.txt",what='a',sep='\n')  # question list, which is read as a vector
# df_fordseg=read.csv("FordKaSegmentData.csv")


###############################################################################
### input data
###############################################################################

# read data from excel spreadsheet
df_forddemo <- read.xlsx("FordKaData.xlsx", sheet = "Demographic Data", startRow = 7, cols = 2:10)
df_fordpsyc <- read.xlsx("FordKaData.xlsx", sheet = "Psychographic Data", startRow = 7, cols = 2:63)
df_fordquest <- read.xlsx("FordKaData.xlsx", sheet = "Psychographic questionnaire", startRow = 7, cols = 2)
df_fordseg <- read.xlsx("FordKaData.xlsx", sheet = "Demographic Data", startRow = 7, cols = 11:12)

# transform the data to make it easier to use
cvec_fordquest <- paste0(1:62, ",", df_fordquest$Statement) # convert the question list into a character string to make it easier to work with
cvec_fordquest <- strtrim(cvec_fordquest, 30) # truncate the strings to the first 30 characters since some questions are quite long
df_fordseg$SegName <- as.factor(df_fordseg$SegmentName) # convert the segment names into a factor for easier use as a classification variable
df_fordseg$SegmentName <- NULL # remove this variable
df_ford <- cbind(df_forddemo, df_fordpsyc) # create a new dataframe with both demogrpahic and psychographic data

# create some lists of variables which we will use later in the script
(cvec_qlist <- paste0("Q", 1:62)) # create sequence of strings like Q1, ... Q62

# let's try to cluster our questions by transposing the question data
nvec_qlistShort <- c(30, 57, 53, 1, 4, 12) # short list of questions
cvec_qlistShort <- paste0("Q", nvec_qlistShort) # append Q in front of the numbers to generate a list of questions to match variable names
cvec_qnameShort <- strtrim(cvec_fordquest[nvec_qlistShort], 30) # the first 30 characters of the strings
nvec_vars <- match(cvec_qlist, colnames(df_ford)) # define list of numeric variables

# define a list of demographic variables that we will use later in the script
cvec_dlist <- c("Age", "MaritalStatus", "Gender", "NumberChildren", "IncomeCategory", "FirstTimePurchase")

# create new standardized datasets using the scale function (set the mean of the new variable to 0 and stddev to 1)
df_forddemoStd <- scale(df_forddemo)
df_fordpsycStd <- scale(df_fordpsyc)
df_fordStd <- scale(df_ford)



###############################################################################
### initial exploratory analysis of the data
###############################################################################

# to list the variables in each data frame
ls(df_forddemo)
ls(df_fordpsyc)
ls(df_ford)

# remember these data sets are made up of lists of objects
typeof(df_ford) # notice that ford is a list
names(df_ford) # this is the list of object names within ford
class(df_ford) # the ford object itself is a special type of list known as a data.frame
attributes(df_ford) # this prints an objects attributes -- which usually has names of columns and rows
str(df_ford) # a more verbose way of checking information about an object is with structure

# descriptive statistics for all the variables
summary(df_ford)

# to print an individual variable, enter it by itself
df_ford$Age

# to print a record use the selector for the first record
df_ford[1, ]

# check the scaled data
df_fordStd[1, ]





###############################################################################
######### Demographic Cluster Analysis
###############################################################################



###############################################################################
### step 1) exploratory analysis of the data
###############################################################################

# create tables to describe the data
xtabs(~Age, data = df_ford)
xtabs(~AgeCategory, data = df_ford)
xtabs(~ChildrenCategory, data = df_ford)
xtabs(~FirstTimePurchase, data = df_ford)
xtabs(~Gender, data = df_ford)
xtabs(~IncomeCategory, data = df_ford)
xtabs(~MaritalStatus, data = df_ford)
xtabs(~NumberChildren, data = df_ford)
xtabs(~PreferenceGroup, data = df_ford)

# to see the relationship between two variables do a cross-tab
xtabs(~ PreferenceGroup + AgeCategory, data = df_ford)
# here is a more visualize representation of a table with a BalloonPlot (uncomment the line if you want to see it)
balloonplot(table(df_ford$PreferenceGroup, df_ford$AgeCategory), xlab = "PreferenceGroup", ylab = "AgeCategory")

# let's plot all pairs of data in a matrix plot
pairs(~ Age + MaritalStatus + Gender + NumberChildren + IncomeCategory + FirstTimePurchase, data = df_ford)
pairs(~ jitter(Age) + jitter(MaritalStatus) + jitter(Gender) + jitter(NumberChildren)
  + jitter(IncomeCategory) + jitter(FirstTimePurchase), data = df_ford)



###############################################################################
## step 2) examine various cluster solutions with different values of k
## let's determine how many clusters to use by creating kmeans solutions
## with k from 2 to 30 and then we can plot the sum of square errors to
## understand how much variation each solution explains
###############################################################################

# set the random number seed so the samples will be the same if regenerated
set.seed(1234)

# compute multiple cluster solutions
grpB2 <- kmeans(df_fordStd[, cvec_dlist], centers = 2)
grpB3 <- kmeans(df_fordStd[, cvec_dlist], centers = 3)
grpB4 <- kmeans(df_fordStd[, cvec_dlist], centers = 4)
grpB5 <- kmeans(df_fordStd[, cvec_dlist], centers = 5)
grpB6 <- kmeans(df_fordStd[, cvec_dlist], centers = 6)
grpB7 <- kmeans(df_fordStd[, cvec_dlist], centers = 7)
grpB8 <- kmeans(df_fordStd[, cvec_dlist], centers = 8)
grpB9 <- kmeans(df_fordStd[, cvec_dlist], centers = 9)
grpB10 <- kmeans(df_fordStd[, cvec_dlist], centers = 10)
grpB15 <- kmeans(df_fordStd[, cvec_dlist], centers = 15)
grpB20 <- kmeans(df_fordStd[, cvec_dlist], centers = 20)
grpB30 <- kmeans(df_fordStd[, cvec_dlist], centers = 30)

# compute between and within SS
nvec_kclust <- c(2:10, 15, 20, 30)
bssB <- c(
  grpB2$betweenss,
  grpB3$betweenss, grpB4$betweenss, grpB5$betweenss, grpB6$betweenss,
  grpB7$betweenss, grpB8$betweenss, grpB9$betweenss, grpB10$betweenss,
  grpB15$betweenss, grpB20$betweenss, grpB30$betweenss
)
wssB <- c(
  grpB2$tot.withinss,
  grpB3$tot.withinss, grpB4$tot.withinss, grpB5$tot.withinss, grpB6$tot.withinss,
  grpB7$tot.withinss, grpB8$tot.withinss, grpB9$tot.withinss, grpB10$tot.withinss,
  grpB15$tot.withinss, grpB20$tot.withinss, grpB30$tot.withinss
)
# plot the results and look for the "Hockey-Stick" effect
par(mfrow = c(1, 1))
plot(nvec_kclust, bssB, type = "l", main = "Between SS for k-means")
points(nvec_kclust, bssB)
plot(nvec_kclust, wssB, type = "l", main = "Within SS for k-means")
points(nvec_kclust, wssB)
plot(nvec_kclust, bssB / (wssB + bssB), type = "l", main = "R-Squared for k-means")
points(nvec_kclust, bssB / (wssB + bssB))



###############################################################################
## step 3) analysis with demographics
## use the value of k from the previous analysis
###############################################################################

# set the random number seed so the samples will be the same if regenerated
set.seed(1248765792)

# compute a k-means cluster with specified number of k using just the demographics
k <- 3 # !!! change from 3 to whatever value you decide !!!
(grpB <- kmeans(df_fordStd[, cvec_dlist], centers = k))

# plot the solutions against the Age and NumberChildren
# since the data is categorical most of the plots will overlay one another,
# so instead we jitter the points -- which adds a small random number to each
par(mfrow = c(1, 1), mar = c(5, 4, 4, 1) + .1)
plot(df_fordStd[, "Age"], df_fordStd[, "NumberChildren"], xlab = "Age", ylab = "NumberChildren", col = grpB$cluster)
points(grpB$centers[, c("Age", "NumberChildren")], col = 1:k, pch = 8, cex = 2)
legend("topright", pch = 8, bty = "n", col = 1:k, as.character(1:k))

# compare the cluster solutions with the PreferenceGroup
xtabs(~ df_ford$PreferenceGroup + grpB$cluster)
CrossTable(df_ford$PreferenceGroup, grpB$cluster) # slightly nicer cross tabulation
# here is a more visualize representation of a table with a BalloonPlot
balloonplot(table(df_ford$PreferenceGroup, grpB$cluster), xlab = "PreferenceGroup", ylab = "Cluster")

# summarize the centroids
grpBcenter <- t(grpB$centers) # create variable with the transpose of the centroids
rownames(grpBcenter) <- cvec_dlist # add the variable names
print(round(grpBcenter, 2)) # print the centroid values for each question
parallelplot(t(grpBcenter), auto.key = list(text = as.character(1:k), space = "top", columns = 3, lines = T)) # create a parallel plot to visualize the centroid values



###############################################################################
######### Psychographic Cluster Analysis
###############################################################################



###############################################################################
### step 1) exploratory analysis of the data
###############################################################################

# create tables to describe the data
xtabs(~PreferenceGroup, data = df_ford)
xtabs(~Q1, data = df_ford)

# to see the relationship between two variables do a cross-tab
xtabs(~ PreferenceGroup + Q1, data = df_ford)
# here is a more visualize representation of a table with a BalloonPlot (uncomment the line if you want to see it)
balloonplot(table(df_ford$PreferenceGroup, df_ford$Q1), xlab = "PreferenceGroup", ylab = "Q1")

# optional part of script to create boxplots -- you can skip to "step 2"
# create boxplots of the questions
# notice that instead of accessing entering each variable as
# df_ford$Q1 we instead use the format df_ford[,"Q1"], R understands
# that we want the column of the object df_ford named Q1
# in this example we will refer to a list of ten questions
par(mfrow = c(4, 1), mar = c(4, 3, 1, 1))
boxplot(df_ford[, c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9")])
boxplot(df_ford[, c("Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19")])
boxplot(df_ford[, c("Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28", "Q29")])
boxplot(df_ford[, c("Q30", "Q31", "Q32", "Q33", "Q34", "Q35", "Q36", "Q37", "Q38", "Q39")])
boxplot(df_ford[, c("Q40", "Q41", "Q42", "Q43", "Q44", "Q45", "Q46", "Q47", "Q48", "Q49")])
boxplot(df_ford[, c("Q50", "Q51", "Q52", "Q53", "Q54", "Q55", "Q56", "Q57", "Q58", "Q59")])
boxplot(df_ford[, c("Q60", "Q61", "Q62")])

# to compute the correlation matrix
mtx_qcor <- cor(df_ford[, cvec_qlist])
# print out the correlations with just 2 digits, since it is a huge matrix
print(mtx_qcor, digit = 1)
# here is a better visualization of the correlation matrix using a heatmap
qplot(x = Var1, y = Var2, data = melt(cor(df_ford[, cvec_qlist], use = "p")), fill = value, geom = "tile") + scale_fill_gradient2(limits = c(-1, 1))



###############################################################################
## step 2) let's determine how many clusters to use by creating kmeans solutions
## with k from 2 to 30 and then we can plot the sum of square errors to
## understand how much variation each solution explains
###############################################################################

# set the random number seed so the samples will be the same if regenerated
set.seed(24895792)

# compute multiple cluster solutions
grpA2 <- kmeans(df_fordStd[, cvec_qlist], centers = 2)
grpA3 <- kmeans(df_fordStd[, cvec_qlist], centers = 3)
grpA4 <- kmeans(df_fordStd[, cvec_qlist], centers = 4)
grpA5 <- kmeans(df_fordStd[, cvec_qlist], centers = 5)
grpA6 <- kmeans(df_fordStd[, cvec_qlist], centers = 6)
grpA7 <- kmeans(df_fordStd[, cvec_qlist], centers = 7)
grpA8 <- kmeans(df_fordStd[, cvec_qlist], centers = 8)
grpA9 <- kmeans(df_fordStd[, cvec_qlist], centers = 9)
grpA10 <- kmeans(df_fordStd[, cvec_qlist], centers = 10)
grpA15 <- kmeans(df_fordStd[, cvec_qlist], centers = 15)
grpA20 <- kmeans(df_fordStd[, cvec_qlist], centers = 20)
grpA30 <- kmeans(df_fordStd[, cvec_qlist], centers = 30)

# compute between and within SS
kclust <- c(2:10, 15, 20, 30)
bss <- c(
  grpA2$betweenss,
  grpA3$betweenss, grpA4$betweenss, grpA5$betweenss, grpA6$betweenss,
  grpA7$betweenss, grpA8$betweenss, grpA9$betweenss, grpA10$betweenss,
  grpA15$betweenss, grpA20$betweenss, grpA30$betweenss
)
wss <- c(
  grpA2$tot.withinss,
  grpA3$tot.withinss, grpA4$tot.withinss, grpA5$tot.withinss, grpA6$tot.withinss,
  grpA7$tot.withinss, grpA8$tot.withinss, grpA9$tot.withinss, grpA10$tot.withinss,
  grpA15$tot.withinss, grpA20$tot.withinss, grpA30$tot.withinss
)
# plot the results and look for the "Hockey-Stick" effect
par(mfrow = c(1, 1))
plot(kclust, bss, type = "l", main = "Between SS for k-means")
points(kclust, bss)
plot(kclust, wss, type = "l", main = "Within SS for k-means")
points(kclust, wss)
plot(kclust, bss / (wss + bss), type = "l", main = "R-Squared for k-means")
points(kclust, bss / (wss + bss))



###############################################################################
### step 3) cluster analysis with psychographics and 3 clusters
### k=3 may be a good value based upon the previous analysis, but you may
### want to try having more or fewer clusters, and also change the set of
### variables that you use in the analysis
###############################################################################

# set the random number seed so the samples will be the same if regenerated
set.seed(1248765792)

# set the value of k
k <- 3 # !!! change this value to 2, 3, ... !!!

# compute a k-means cluster with specified k using just the psychographics
(grpA <- kmeans(df_fordStd[, cvec_qlist], centers = k))

# plot the solutions against the Q1 and Q2
# since the data is categorical most of the plots will overlay one another,
# so instead we jitter the points -- which adds a small random number to each
par(mfrow = c(1, 1), mar = c(5, 4, 4, 1) + .1)
plot(jitter(df_fordStd[, "Q1"]), jitter(df_fordStd[, "Q2"]), xlab = cvec_fordquest[1], ylab = cvec_fordquest[2], col = grpA$cluster)
points(grpA$centers[, c("Q1", "Q2")], col = 1:k, pch = 8, cex = 2)
legend("topleft", pch = 8, bty = "n", col = 1:k, as.character(1:k))

# let's do a pairwise plot with the short list of questions
par(mfrow = c(length(cvec_qlistShort), length(cvec_qlistShort)), mar = c(4.5, 4.5, 0, 0))
for (j in 1:length(cvec_qlistShort)) {
  for (i in 1:length(cvec_qlistShort)) {
    plot(jitter(df_fordStd[, cvec_qlistShort[i]]), jitter(df_fordStd[, cvec_qlistShort[j]]),
      xlab = cvec_qlistShort[i], ylab = cvec_qlistShort[j], col = grpA$cluster
    )
    points(grpA$centers[, c(cvec_qlistShort[i], cvec_qlistShort[j])], col = 1:5, pch = 8, cex = 2)
    if (i == j) legend("topleft", pch = 8, bty = "n", col = 1:k, as.character(1:k))
  }
}

# list out the short list of questions
cvec_fordquest[nvec_qlistShort]

# compare the cluster solutions with the PreferenceGroup
# preference group #1 are "Ka Chooser (top 3)"
#                  #2 are Ka Non-Chooser (bottom 3)"
#                  #3 is "Middle (Middle 4)"
xtabs(~ df_ford$PreferenceGroup + grpA$cluster)
CrossTable(df_ford$PreferenceGroup, grpA$cluster) # slightly nicer cross tabulation
# here is a more visualize representation of a table with a BalloonPlot (uncomment the line if you want to see it)
par(mfrow = c(1, 1)) # reset graphics to one panel
balloonplot(table(df_ford$PreferenceGroup, grpA$cluster), xlab = "PreferenceGroup", ylab = "Cluster")

# summarize the centroids
grpAcenter <- t(grpA$centers) # create variable with the transpose of the centroids
rownames(grpAcenter) <- cvec_fordquest # add the question names
print(grpAcenter) # print the centroid values for each question
parallelplot(t(grpAcenter)) # create a parallel plot to visualize the centroid values
print(round(grpAcenter[nvec_qlistShort, ], 2)) # print the centroid values for short list of questions
parallelplot(t(grpAcenter[nvec_qlistShort, ]), auto.key = list(text = as.character(1:k), space = "top", columns = 3, lines = T)) # a parallel plot with just a few questions

# to save the data to an Excel spreadsheet (one sheet with the centroids and another with cluster assignments)
write.xlsx(
  list(
    "A Centroids" = grpA$centers, "A Assignment" = grpA$cluster,
    "B Centroids" = grpB$centers, "B Assignment" = grpB$cluster
  ),
  file = "FordKa_Results.xlsx", colnames = T, rownames = T, overwrite = T
)



###############################################################################
######### compare Psychographic and Demographic
###############################################################################

# compare the cluster solutions with the each other
xtabs(~ grpA$cluster + grpB$cluster)
# here is a more visualize representation of a table with a BalloonPlot
balloonplot(table(grpA$cluster, grpB$cluster), xlab = "Demo Cluster", ylab = "Psych Cluster")



###############################################################################
### silhouette and gap statistic methods for determing K
### see https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/
### for a nice blog entry that discussses the statistics
###############################################################################

# silhouette is within the cluster package
if (!require(cluster)) {
  install.packages("cluster")
  library(cluster)
}
if (!require(factoextra)) {
  install.packages("factoextra")
  library(factoextra)
}

# the silhouette concept builds on dissimilarities
# to build the silhouette we first need to compute distances for the demographic data
mtx_dist <- daisy(df_fordStd[, cvec_dlist]) # compute the dissimilarity matrix (e.g., all pairwise distances)
mtx_distsq <- mtx_dist^2 # square the euclidean distances since kmeans usings square distances
sil.grpB <- silhouette(grpB$cluster, mtx_dist) # compute the silhouette scores now

# For completeness let's do all three methods (elbow, silhouette, and gap statistic)
# using the factoextra package (which has nicer clustering visualizations)
# to determine the "optimal" k-value. The graphics all use ggplot2
set.seed(123) # set the seed so we can repeat following methods to get same results
# first plot the clusters again the first two principal components
fviz_cluster(grpB, data = df_fordStd[, cvec_dlist], ellipse.type = "euclid")
# *elbow method* -- look for kink in the scree plot
# the vertical line is determined by you, and values of 3 to 5 seem reasonable
fviz_nbclust(df_fordStd[, cvec_dlist], kmeans, nstart = 100, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2) + labs(subtitile = "Elbow Method")
# *silhouette* plots a score for each observation that measures whether the cluster
# for an observation is different from the next closest cluster. negative values are
# probably in the wrong cluster, while large values indicate good solutions
# Plot the silhouette below, and if these three things hold you have a good solution:
# (1) high average silhouette scores
# (2) no clusters where the max silhouette score is less than the average score
# (3) silhouettes should have similar size and similar silhouettes within each cluster
# plot(sil.grpB)
fviz_silhouette(sil.grpB, label = FALSE, print.summary = TRUE)
# we can try many different k values and compare with the average silhouette width
# to find the optimal number of clusters
fviz_nbclust(df_fordStd[, cvec_dlist], kmeans, nstart = 100, method = "silhouette") +
  labs(subtitle = "Silhouette method")
# *gap statistic*: compare total within intra-cluster variation for different k values
# to their expected values under the null hypothesis. Uses bootstrapping to compute
# the standard errors. Look for the value of k that maximizes the gap statistic
fviz_nbclust(df_fordStd[, cvec_dlist], kmeans, nstart = 100, method = "gap_stat", nboot = 50) +
  labs(subtitle = "Gap statistic method")

# try again for the psychographic data
mtx_dist <- daisy(df_fordStd[, cvec_qlist]) # compute the dissimilarity matrix (e.g., all pairwise distances)
mtx_distsq <- mtx_dist^2 # square the euclidean distances since kmeans usings square distances
sil.grpA <- silhouette(grpA$cluster, distsq)
plot(sil.grpA) # plot the silhouette

# for completeness let's redo all three methods (elbow, silhouette, and gap statistic)
fviz_nbclust(df_fordStd[, cvec_qlist], kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) + labs(subtitile = "Elbow Method")
fviz_nbclust(df_fordStd[, cvec_qlist], kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method") # notice silhouette suggest 4 clusters!
set.seed(123)
fviz_nbclust(df_fordStd[, cvec_qlist], kmeans, nstart = 100, method = "gap_stat", nboot = 50) +
  labs(subtitle = "Gap statistic method")



###############################################################################
### find prototypes of users in each cluster
###
### One of the challenges that you face is making sense out of your clusters
### (or validating or interpretting your clusters). In class I talk about using
### the table of means to understand each cluster.  Another option is to look
### at observations that are close to the centroid, and use these as prototypes.
### Prototypes have the advantage of being actual observations/people in our
### sample.  In contrast, an average may not be meaningful.  For example, if
### a cluster has an average gender of 1.5 -- there is no individual that has
### this gender -- either a user is male or female in our dataset.
###
### This code will find the closest observation to your cluster centroids.
###############################################################################

# to compute distances we use the proxy package
if (!require(proxy)) {
  install.packages("proxy")
  library(proxy)
}

# compute distances between data used for the kmeans and centroids for grpB
# cdist is a matrix between original observation and each of the cluster centroids in the columns
# for example cdist[10,3] would be the distance between observation 10 and cluster #3
# need to use dist package from proxy since it can compute distances between variables
mtx_cdistB <- proxy::dist(df_fordStd[, cvec_dlist], grpB$centers)
# there is another distance function, so the "proxy::dist" forces R to use dist from proxy package

# find the closest observations to each centroid
cprototypeB <- apply(mtx_cdistB, 2, which.min)
# print the indices of the observations
print(cprototypeB)
# print the prototypes for each centroid
print(df_fordStd[cprototypeB, cvec_dlist])
# or perhaps easier to look at the original data
print(df_ford[cprototypeB, cvec_dlist])

# print the centroids
print(grpB$centers)
# count the number of observations in each centroid
table(grpB$cluster)

