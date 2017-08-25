# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
df <- scale(wine[-1])

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	               wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(df)

# Exercise 2:
#   * How many clusters does this method suggest?

# 3 clusters since this is where there is a major bend in the graph. 

#   * Why does this method work? What's the intuition behind it?

# The reason we choose the point with major bend is because this point 
# is equidistant from above and below areas. Reasoning similar to choosing number of
# clusters in dendogram. This is the equilibrium point where we neither have too many nor too few clusters.

#   * Look at the code for wssplot() and figure out how it works

# The code works as follows. nc sets maximum number of clusters.seed ensures
# reproducibility. In wss, apply() takes data, number 2 which stands for column
# and function var (variance). Variance is applied on all columns of data dataframe.
# Now, we have a dataframe with 1 row that only contains variance of respective column.
# sum() sums up variance of all columns. But we only want summation of squared differences
# from mean. We only have variance here. So, the sum is multiplied by nrow - 1. 
# Next, a for loop is initiated from 2 to maximum clusters. 1 is ommitted since this has already been
# calculated in wss, where nrow(data) -1, sum, apply() were used.
# wss[i] performs same function but for i=2 to maximum clusters.
# When i=2, a k-means algorithm with 2 centers is initiated on data. Withinss generates
# vector of within-cluster sum of squares, one component per cluster. So, sum() function
# adds up individual components and outputs for total of 2 clusters, 3 clusters upto nc clusters.
# Finally, number of clusters and within group sum of squares are plotted.

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")

# Exercise 3: How many clusters does this method suggest?
# 3

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(df,centers=3)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

table(wine$Type,fit.km$cluster)
# No. Only 2,2 has 65 true positives (TP). 1,1 and 3,3 have 0 TP`s. Model accuracy is only 36%

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
library(cluster)
par(mfrow=c(1,1))
clusplot(wine,fit.km$cluster)
# Plot shows total of 3 clusters on 2 principal components. Principal components are
# those that show maximum variability. Plot says that these 2 components only explain
# 57.38% of the variability. So, adding more components will improve model.