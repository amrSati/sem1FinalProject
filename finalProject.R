#library("dplyr")
#library("stats")

Itemset<- read.csv("/home/amro/Documents/College/Fall-22 Class Material/Data Science + R/Final Project/grcDataset-master/grc.csv")
Itemset<- select(Itemset, items, count, customer, age, city, total, paymentType)

#compare cash & credit totals
pie(
  x= table(Itemset$paymentType),
  main = "Compare Cash & Credit Total Spending"
)
#compare totals by age
compAges<- group_by(Itemset, age)
compAges<- summarize(comAges, total=sum(total))
plot(
  x= totalAge$age,
  y= totalAge$total,
  main = "Age VS. Total Spending",
  xlab = "Age",
  ylab = "Total"
)
#compare each city's total
compCities<- group_by(Itemset, city)
compCities<- summarize(comCities, total=sum(total))
compCities<- arrange(comCities, desc(total))
barplot(
  height = comCities$total,
  names = comCities$city,
  col = "light blue",
  main = "Compare Cities Total Spending",
  xlab = "City",
  ylab = "Total",
  horiz = F,
  axes = T,
  las = 1
)
#distribute total
boxplot(
  x= Itemset$total,
  main = "Distribution of Total Spending",
  xlab = "Total"
)
summary(Itemset)

#combine in one dashboard
par(mfrow=c(2,2))
pie(
  x= table(Itemset$paymentType),
  main = "Compare Cash & Credit Total Spending"
)
plot(
  x= totalAge$age,
  y= totalAge$total,
  main = "Age VS. Total Spending",
  xlab = "Age",
  ylab = "Total"
)
barplot(
  height = totalCity$total,
  names = totalCity$city,
  beside = F,
  col = "light green",
  main = "Compare Cities Total Spending",
  xlab = "City",
  ylab = "Total",
  horiz = F,
  axes = T,
  las = 1
)
boxplot(
  x= Itemset$total,
  main = "Distribution of Total Spending",
  xlab = "Total"
)



#do Kmeans
#library(ggplot2)
#library(factoextra)
#library(ggfortify)
#library(NbClust)
#prep vribls
grpdItemset <- select(Itemset, customer, age, total)
grpdItemset <- group_by(grpdItemset, customer, age)
grpdItemset <- summarize(grpdItemset, total=sum(total), .groups = "keep")

clusteredItemset <- select(Itemset, customer, age, total)
clusteredItemset <- group_by(clusteredItemset, customer, age)
clusteredItemset <- summarize(clusteredItemset, total=sum(total), .groups = "keep")
clusteredItemset$customer<- NULL
as.double(clusteredItemset$total)
                                                                                            #wssplot <- function(data, nc=12, seed=1234){
                                                                                            #  wss <- (nrow(data)-1)*sum(apply(data,c(1,2),var))
                                                                                            # for (i in 1:nc){
                                                                                            #  set.seed(seed)
                                                                                            # wss[i] <- sum(kmeans(data, centers=i)$withinss)}
                                                                                            #plot(1:nc, wss, type="b", xlab="Number of Clusters",
                                                                                            #    ylab="Within groups sum of squares")
                                                                                            #}
                                                                                            #wssplot()
#to choose the optimal number of centers
scaledItemset<- scale(clusteredItemset)
fviz_nbclust(scaledItemset, kmeans, method = "wss") + labs(subtitle = "Elbow Method")

KMout<- kmeans(scaledItemset, centers =3, iter.max = 10000, nstart = 4)
KMclstrs<- KMout$cluster
rownames(scaledItemset)<- paste(grpdItemset$customer, 1:dim(grpdItemset)[1], sep = "_")

fviz_cluster(list(data = scaledItemset, clusters = KMclstrs))

grpdItemset$group <- KMclstrs
View(grpdItemset)


#do Apriori Algorithm
#library(gtools)
#library(arules)
#library("arulesViz")
#library(Matrix)

arItemset<- read.csv("/home/amro/Documents/College/Fall-22 Class Material/Data Science + R/Final Project/grcDataset-master/grc.csv")
arItemset<- select(arItemset, items)

transactions<- strsplit(as.vector(arItemset$items), ',') #split items as vector
transactions.count<- length(transactions)
unique.items<- unique(unlist(transactions)) #get all items in items set

rules<- apriori(transactions, parameter = list(supp=0.002, conf=0.8), control = list(verbose=F))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)
#f#ck redundant rules
#find
subset.matrix <- is.subset(rules.sorted, rules.sorted, sparse = F)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- (colSums(subset.matrix, na.rm=T) >= 1)
which(redundant)
#kill
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

#VizViz
#plot(rules.pruned)