rm(list = ls())
load("/Users/huzhizhou/Desktop/MKT Research/Final Case - Toy Horse Conjoint/MKT412R - Final Analysis Case Data.Rdata")
##Regression Analysis for getting part-worth estimates
ls()
colnames(desmat) = atts[2:5]
##regressions
##aggregate
summary(lm(ratings~desmat))
# ------------------------------------------------------#
##by individual
desmatf = cbind(rep(1,nrow(desmat)),desmat); ##add column for constant
partworths = matrix(nrow=sampsize,ncol=ncol(desmatf))
for(i in 1:sampsize){ #for each individual run the regression
        partworths[i,]=lm(ratings~desmat,subset=ID==i)$coef
}
        colnames(partworths) = atts

# segmenting individuals - see code from last week for details
library(cluster)
library(fpc)
set.seed(123456)   # set random number seed before doing cluster analysis


toclust = partworths;
pm1 = pamk(toclust,scaling=TRUE)

wss <- (nrow(toclust)-1)*sum(apply(toclust,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(toclust,
                                     centers=i,nstart=2)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
# 3 groups

km1 = kmeans(toclust,2,iter.max = 20, nstart=2)
km2 = kmeans(toclust,3,iter.max = 20, nstart=2) #???????????????????????
km3 = kmeans(toclust,4,iter.max = 20, nstart=2)
percsize = paste(1:3," = ",format(km1$size/sum(km1$size)*100,digits=2),"%",sep="")

km2
km2$size

km1
km1$size

#"1 = 25%" "2 = 26%" "3 = 50%"
pie(km1$size,labels=percsize)
pie(km2$size,labels=percsize)  # best
pie(km3$size,labels=percsize)

clusplot(toclust, km1$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0); #plot clusters against principal components
clusplot(toclust, km2$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0);
clusplot(toclust, km3$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0);

plotcluster(toclust, km1$cluster); #plot against discriminant functions ()
plotcluster(toclust, km2$cluster);
plotcluster(toclust, km3$cluster);

plotClust = function(km,discPlot=FALSE){
        nc = length(km$size)
        if(discPlot){par(mfrow=c(2,2))}
        else {par(mfrow=c(3,1))}
        percsize = paste(1:nc," = ",format(km$size/sum(km$size)*100,digits=2),"%",sep="")
        pie(km$size,labels=percsize,col=1:nc)
        
        if (nc > 2) {
                clusplot(toclust, km$cluster, color=TRUE, shade=TRUE,
                         labels=2, lines=0,col.clus=nc:1); #plot clusters against principal components
        } else {
                clusplot(toclust, km$cluster, color=TRUE, shade=TRUE,
                         labels=2, lines=0,col.clus=1:nc); #plot clusters against principal components    
        }
        if(discPlot){
                plotcluster(toclust, km$cluster,col=km$cluster); #plot against discriminant functions ()
        }
        rng = range(km$centers)
        dist = rng[2]-rng[1]
        locs = km$centers+.05*dist*ifelse(km$centers>0,1,-1)
        bm = barplot(km$centers,beside=TRUE,col=1:nc,main="Cluster Means",ylim=rng+dist*c(-.1,.1))
        text(bm,locs,formatC(km$centers,format="f",digits=1))
}

plotClust(km1)
plotClust(km2)
plotClust(km3)

# ------------------------------------------------------#
##by apriori segment age
summary(lm(ratings~desmat*ageD))
summary(lm(ratings~desmat*genderD)); ##run the regression with interactions for segment dummies
##note if significant. can run separately for two categories
summary(lm(ratings~desmat,subset=ageD==1)) # young kids
summary(lm(ratings~desmat,subset=ageD==0)) # old kids
summary(lm(ratings~desmat,subset=genderD==1)) # female
summary(lm(ratings~desmat,subset=genderD==0)) # male

# ------------------------------------------------------#
##predicting missing cells (preparing for market simulation)
##repeat individual level partworths for multiplication
partworths.full = matrix(rep(partworths,each=16),ncol=5)
pratings = rowSums(desmatf*partworths.full)
finalratings = ifelse(is.na(ratings),pratings,ratings); #combining actual when available and predicted ratings

# ---------------------select scenarios first---------------------------------#
##market simulation
scen0 = c(4,7,14,16)

##market simulations
#tranform final ratings into matrix
simDecInput = matrix(finalratings,nrow=nprofiles); ##this has 16 rows for profiles and sampsize columns

##inputmat is the ratings matrix with rows as profiles and cols as ratings
##scen is the list of products in the market for the scenario (these are rows in inputmat)
simDec = function(inputmat,scen){
        inmkt = inputmat[scen,]
        max = apply(inmkt,2,max)
        firstChoices = (inmkt>rep(max,each=length(scen))-.000000000001)
        shares = firstChoices/rep(colSums(firstChoices),each=length(scen))
        rowMeans(shares)
}
simDec0 = simDec(simDecInput,scen0)
simDec1 = simDec(simDecInput,scen1)
simDec2 = simDec(simDecInput,scen2)

##inputmat and scen is as above. myprods are indicators of which prods are the firms,
## prices are the prices for all products, vcosts are the variable costs
## fcosts are the fixed costs for the firm (need to calculate in already the number of products)
simProfit = function(inputmat,scen, myProds, prices, vcosts,fcosts,mktsize=1){
        mktshr = simDec(inputmat,scen);
        vprofit = mktshr * (prices-vcosts)*mktsize;
        sum(vprofit[myProds])-fcosts
}
simProf0 = simProfit(simDecInput,scen0,c(1,2,3),c(95.99,95.99,95.99),c(29,33,41),60000,4000)



# -------------------table cluster with demo-----------------------------------#
table(rep(km1$cluster, each = 16), genderD)
table(rep(km2$cluster, each = 16), genderD)
clust1 <- rep(km2$cluster, each = 16)
clust2 <- rep(km1$cluster, each = 16)
t1 <- aggregate(ID ~ genderD + ageD + clust1, FUN = length)
t2 <- aggregate(ID ~ genderD + ageD + clust2, FUN = length)
t1/16
t2/16
