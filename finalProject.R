#  library("dplyr")
#  library("stats")
#  library(ggplot2)
#  library(factoextra)
#  library(ggfortify)
#  library(NbClust)
#  library(gtools)
#  library(arules)
#  library("arulesViz")
#  library(Matrix)
#  library(FSelector)
#  library(rpart)
#  library(rpart.plot)
#  library(caret)
#  library(data.tree)
#  library(caTools)

Itemset <- read.csv("/home/amro/Documents/College/Fall-22ClassMaterial/Data Science + R/Final Project/grcDataset-master/grc.csv")
Itemset <- select(Itemset, items, count, customer, age, city, total, paymentType)

#prep samples
#for total spending of each age
compAges <- group_by(Itemset, age)
compAges <- summarize(compAges, total=sum(total))

#for each governorate's total spendings
compGov <- group_by(Itemset, city)
compGov <- summarize(compGov, total=sum(total)) # nolint # nolint
compGov <- arrange(compGov, desc(total))

#combine in one dashboard

par(mfrow=c(2,2))
pie(   #compare payment type by total spending
  x= table(Itemset$paymentType), main = "Comparing Cash & Credit Total Spendings")
boxplot(    #distribute total spendings
  x= Itemset$total, main = "Distribution of Total Spending", xlab = "Total")
plot(   #compare total spending of each age within
  x= compAges$age, y= compAges$total, main = "Age VS. Spendings",
  xlab = "Age", ylab = "Total")
barplot(    #compare each governorate's total spendings
  height = compGov$total, names = compGov$city, col = "light blue", main = "Compare each Governorate Total Spendings", xlab = "City", ylab = "Total", horiz = T, axes = T, las = 1)

summary(Itemset) #so you can explain specifically the spendigs of each one



#Kmeans cluster analysis for age and total spendings

#prep vribls
#for filtering the data set and compine it with group number
grpdItemset <- select(Itemset, customer, age, total)
grpdItemset <- group_by(grpdItemset, customer, age)
grpdItemset <- summarize(grpdItemset, total=sum(total), .groups = "keep")
#for Kmeans
clusteringItemset <- grpdItemset
clusteringItemset$customer <- NULL
as.double(clusteringItemset$total)

#to choose the optimal number of centers
scaledItemset <- scale(clusteringItemset)
fviz_nbclust(scaledItemset, kmeans, method = "wss") + labs(subtitle = "Elbow")

#clusturing
KMout <- kmeans(scaledItemset, centers =3)
KMclstrs <- KMout$cluster
rownames(scaledItemset) <- paste(grpdItemset$customer, 1:dim(grpdItemset)[1], sep = "_")

#visualize
fviz_cluster(list(data = scaledItemset, clusters = KMclstrs))

#assign groups
grpdItemset$group <- KMclstrs
View(grpdItemset)


#Association Rules in Items
#Apriori Algorithm

arItemset <- select(Itemset, items)

#explore the data
transactions <- strsplit(as.vector(arItemset$items), ',')
transactions.count <- length(transactions)
unique.items <- unique(unlist(transactions))

#start the algorithm with 0.2% min support & 80 confidence, then sort by confidence
rules <- apriori(transactions, parameter = list(supp=0.002, conf=0.8), control = list(verbose=F))
rules.sorted <- sort(rules, by="confidence")
inspect(rules.sorted)

#redundant rules
#find
subset.matrix <- is.subset(rules.sorted, rules.sorted, sparse = F)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- (colSums(subset.matrix, na.rm=T) >= 1)
which(redundant)
#prun
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

#visualize
plot(rules.pruned)


#build the prediction tree
basket <- select(Itemset, paymentType, city, age)
basket <- mutate(basket, paymentType=factor(paymentType), city=factor(city), age=as.numeric(age))

#splitting into training $ testing sets
set.seed(1234)
sample = sample.split(basket, SplitRatio = .30)
train = subset(basket, sample==T)
test = subset(basket, sample==F)

#decision tree classifier
tree <- rpart(paymentType ~.,data = train)

#predictions
tree.payType.prdct <- predict(tree, test, type = 'class')

#Confusion matrix
confusionMatrix(tree.payType.prdct, test$paymentType)

#VizViz
prp(tree)
rpart.plot(tree)
