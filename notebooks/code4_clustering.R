library(data.table)
library(dbplyr)
setwd("/Users/tjmask/Desktop/Courses/health care/HW4")

## data manipulation
{
## loading data
# layout_code = fread("FILE_LAYOUT_and_CODES.csv")
revcode = fread("VTREVCODE16.csv")
inp = fread("VTINP16_upd.csv")
head(revcode)
head(inp)

## filtering inp and revcode data
inp_important = inp[DRG %in% c(20:977)]
revcode_filter = revcode[!REVCHRGS<100]

## merging two tables
revcode_inp = merge(inp_important, revcode_filter, by.x="UNIQ", by.y = "Uniq")
head(revcode_inp)

## selecting useful columns from merged table
pccr_drg = revcode_inp[,.(total_charge = sum(REVCHRGS)), by = .(UNIQ, DRG, PCCR)][order(PCCR,DRG,decreasing = FALSE)]
nrow(pccr_drg)
setkey(pccr_drg, DRG, PCCR)

## getting the avarage charge
avg_charge = pccr_drg[,.(avg_charge = mean(total_charge)), by = .(DRG, PCCR)]
avg_charge= na.omit(avg_charge)

## cross tabulation
tabulation = xtabs(avg_charge ~ DRG + PCCR, avg_charge)

## transfering the tabulation into data table
write.csv(tabulation, 'tabulation.csv')
table_tabulation = fread('tabulation.csv', header= TRUE)
head(table_tabulation)

## combining a new cost category
table_tabulation$PCCR_OR_and_Anesth_Costs = table_tabulation$`3700`+table_tabulation$`4000`

## rename columns 
pccr_names = fread("pccr_names.csv")
for (i in 1:length(colnames(table_tabulation))){
  for (j in 1:length(pccr_names$PCCR)){
    if (colnames(table_tabulation)[i] == as.character(pccr_names$PCCR[j])){
      colnames(table_tabulation)[i] = pccr_names$PCCR_NAME[j]
    }
  }
}

##  save table table_tabulation
write.csv(table_tabulation, 'table_tabulation.csv')
table_tabulation = read.csv('table_tabulation.csv', header= TRUE, row.names=1)

head(table_tabulation)
nrow(table_tabulation)

## getting the data for clustering
table_cluster = table_tabulation[c('PCCR_OR_and_Anesth_Costs')]

head(table_cluster)
}

## clustering analysis
{
#install.packages("fpc")
#install.packages("factoextra")
library(factoextra)
library(cluster)
library(fpc)
library(tidyverse)

# To standarize the variables
table_standarized  = scale(table_cluster) 
head(table_standarized)

## getting the Calinski-Harabasz F-statistics for k in (2,20)
f_20 = c()
for (i in 2:13){
  k.means.fit = kmeans(table_cluster, i)
  f_20[length(f_20)+1] = calinhara(table_cluster, k.means.fit$cluster)
}
f_20 = data.frame(f_20)
rownames(f_20) = c(2:13)
colnames(f_20) = "F-statistic"
## drawing the picture of optimal number of clusters
fviz_nbclust(table_standarized, kmeans, k.max=20, method = "wss") + 
  geom_vline(xintercept = 11, linetype = 2)


## getting the Calinski-Harabasz F-statistics for k in (2,5)
f_5 = c()
for (i in 2:5){
  k.means.fit = kmeans(table_cluster, i)
  f_5[length(f_5)+1] = calinhara(table_cluster, k.means.fit$cluster)
}
f_5 = data.frame(f_5)
rownames(f_5) = c(2:5)
colnames(f_5) = "F-statistic"
## visualizing the SSE 
fviz_nbclust(table_cluster, kmeans, method = "wss") + 
  geom_vline(xintercept = 5, linetype = 2)

nrow(table_cluster)
## clustering when k = 3
k.means.fit = kmeans(table_cluster, 3) # k = 3 
attributes(k.means.fit)
## getting the center of the 3 clusters
k.means.fit$centers
k.means.fit$cluster
## getting the size of each cluster
table(k.means.fit$cluster)
## data frame the clusters 
df_cluster = data.frame(k.means.fit$cluster)
## getting the specific rows of each cluster
cluster1 = rownames(df_cluster)[k.means.fit$cluster==1]
cluster2 = rownames(df_cluster)[k.means.fit$cluster==2]
cluster2
cluster3 = rownames(df_cluster)[k.means.fit$cluster==3]
cluster3

## adding the detail description of each cluster
drg_detail = fread("DRG_detail.csv")
drg_detail = as.data.frame(drg_detail)

cluster11 = as.data.frame(cluster1)
cluster11$drg_descp = drg_detail[cluster11$cluster1,]$DRG_DESC

cluster22 = as.data.frame(cluster2)
cluster22$drg_descp = drg_detail[cluster22$cluster2,]$DRG_DESC

cluster33 = as.data.frame(cluster3)
cluster33$drg_descp = drg_detail[cluster33$cluster3,]$DRG_DESC


write.csv(table_cluster, "cluster_cost.csv")
write.csv(cluster11, "cluster11.csv")
write.csv(cluster22, "cluster22.csv")
write.csv(cluster33, "cluster33.csv")
}

## visualization of the results
{
library(plotly)
## rearranging the data
cluster_cost_num = as.data.frame(k.means.fit$centers)
cluster_cost_num$number_of_drgs = c(407, 234,61)
cluster_cost_num$clusters = c("cluster1", "cluster2", "cluster3")
cluster_cost_num
write.csv(cluster_cost_num, 'cluster_cost_num.csv')

## plotting
p <- plot_ly(data = cluster_cost_num) %>%
  add_trace(x = ~clusters, y = ~number_of_drgs, type = 'bar', 
            name="numbers", 
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
  add_trace(x = ~clusters, y = ~PCCR_OR_and_Anesth_Costs,type = 'scatter',
            name="costs", yaxis='y2',
            marker = list(color = 'red',
                          line = list(color = 'red', width = 1.5))) %>%
  layout(title = "numbers and cost of each cluster",
         barmode = 'group',
         xaxis = list(title = "clusters"),
         yaxis = list(title = ""), 
         yaxis2=list(anchor='x', overlaying='y', side='right'))
p

library(plotly)
table_clusters = read.csv("cluster_cost.csv")
head(table_clusters)
nrow(table_clusters)
table_clusters$clusters = 0
table_clusters$clusters[which(table_clusters$X %in% as.numeric(cluster1))] = "cluster1"
table_clusters$clusters[which(table_clusters$X %in% as.numeric(cluster2))] = "cluster2"
table_clusters$clusters[which(table_clusters$X %in% as.numeric(cluster3))] = "cluster3"
colnames(table_clusters) =c("DRG","Costs","Clusters")
head(table_clusters)
p2 <- plot_ly(data = table_clusters, x = ~DRG, y = ~Costs, color = ~Clusters)
p2
}

## further analysis
{
head(pccr_drg)
length(unique(pccr_drg$UNIQ))
length(unique(pccr_drg$DRG))
head(revcode_inp)
nrow(revcode_inp)

## getting all the info for each cluster
which(colnames(revcode_inp)=="DRG")
cluster1_all = unique(revcode_inp[DRG %in% as.numeric(cluster1)][,c(2, 10,31:50,76)])
cluster2_all = unique(revcode_inp[DRG %in% as.numeric(cluster2)][,c(2, 31:50,76)])
cluster3_all = unique(revcode_inp[DRG %in% as.numeric(cluster3)][,c(2, 31:50,76)])


## getting all the services info for each cluster
cluster1_px = data.table(unique(gather(cluster1_all[,3:22]))[2])
cluster2_px = data.table(unique(gather(cluster2_all[,3:22]))[2])
cluster3_px = data.table(unique(gather(cluster3_all[,3:22]))[2])

## find the top 30 services for each cluster
### top 30 services for cluster 1
top_service_cluster1 = cluster1_px[,.(total_num = .N), by =.(value)]
top_service_cluster1 = top_service_cluster1[order(top_service_cluster1[,2], 
                                                  decreasing = T),][-1]
top30_cluster1  = top_service_cluster1[1:30]

### top 30 services for cluster 2
top_service_cluster2 = cluster2_px[,.(total_num = .N), by =.(value)]
top_service_cluster2 = top_service_cluster2[order(top_service_cluster2[,2], 
                                                  decreasing = T),][-1]
top30_cluster2  = top_service_cluster2[1:30]

### top 30 services for cluster 3
top_service_cluster3 = cluster3_px[,.(total_num = .N), by =.(value)]
top_service_cluster3 = top_service_cluster3[order(top_service_cluster3[,2], 
                                                  decreasing = T),][-1]
top30_cluster3  = top_service_cluster3[1:30]

## export top 30 services of each cluster
write.csv(top30_cluster1, "top30_cluster1.csv")
write.csv(top30_cluster2, "top30_cluster2.csv")
write.csv(top30_cluster3, "top30_cluster3.csv")

## getting the hosipital frequency for each cluster
hp_cluster1 =data.table(table(cluster1_all$hnum2.x))
hp_cluster1 = hp_cluster1[order(hp_cluster1[,2],decreasing=T),]
names(hp_cluster1) =  c("hnum","num_treatment")

hp_cluster2 =data.table(table(cluster2_all$hnum2.x))
hp_cluster2 = hp_cluster2[order(hp_cluster2[,2],decreasing=T),]
names(hp_cluster2) =  c("hnum","num_treatment")

hp_cluster3 =data.table(table(cluster3_all$hnum2.x))
hp_cluster3 = hp_cluster3[order(hp_cluster3[,2],decreasing=T),]
colnames(hp_cluster3) =  c("hnum","num_treatment")

hp_cluster1$hnum
hp_cluster2$hnum
hp_cluster3$hnum

## export hospital treatment frequency of each cluster
write.csv(hp_cluster1, "hp_cluster1.csv")
write.csv(hp_cluster2, "hp_cluster2.csv")
write.csv(hp_cluster3, "hp_cluster3.csv")


## getting drg frequency of each cluster 
#cluster 1
freq_cluster1 = inp_important[inp_important$DRG %in% as.numeric(cluster1)][,.(freq =.N), by = .(DRG)]
freq_cluster1 = as.data.frame(freq_cluster1)
freq_cluster1 = freq_cluster1[order(freq_cluster1[,2], decreasing=T),]
head(freq_cluster1,20)

#cluster 2
freq_cluster2 = inp_important[inp_important$DRG %in% as.numeric(cluster2)][,.(freq =.N), by = .(DRG)]
freq_cluster2 = as.data.frame(freq_cluster2)
freq_cluster2 = freq_cluster2[order(freq_cluster2[,2], decreasing=T),]
head(freq_cluster2)

#cluster 3
freq_cluster3 = inp_important[inp_important$DRG %in% as.numeric(cluster3)][,.(freq =.N), by = .(DRG)]
freq_cluster3 = as.data.frame(freq_cluster3)
freq_cluster3 = freq_cluster3[order(freq_cluster3[,2], decreasing=T),]
head(freq_cluster3)

## export drg frequency of each cluster
write.csv(freq_cluster1, "freq_cluster1.csv")
write.csv(freq_cluster2, "freq_cluster2.csv")
write.csv(freq_cluster3, "freq_cluster3.csv")


## average admission days of each cluster
# cluster 1
length(unique(pdays_cluster1$DRG))
pdays_cluster1 = inp_important[inp_important$DRG %in% freq_cluster1$DRG][,.(DRG,pdays)]
pdays_cluster1 = pdays_cluster1[order(pdays_cluster1[,2], decreasing = T)][5:nrow(pdays_cluster1)]
pdays_cluster1 = pdays_cluster1[,.(sum(pdays)/.N)]

# cluster 2
pdays_cluster2 = inp_important[inp_important$DRG %in% freq_cluster2$DRG][,.(DRG,pdays)]
pdays_cluster2 = pdays_cluster2[order(pdays_cluster2[,2], decreasing = T)]
pdays_cluster2 = pdays_cluster2[,.(sum(pdays)/.N)]

# cluster 3
pdays_cluster3 = inp_important[inp_important$DRG %in% freq_cluster3$DRG][,.(DRG,pdays)]
pdays_cluster3 = pdays_cluster3[order(pdays_cluster3[,2], decreasing = T)]
pdays_cluster3_higher = pdays_cluster3[pdays>=7]
pdays_cluster3_higher = pdays_cluster3_higher[,.(sum(pdays)/.N)]
pdays_cluster3 = pdays_cluster3[,.(sum(pdays)/.N)]



## admission type
# cluster 1
type_cluster1 = inp_important[inp_important$DRG %in% freq_cluster1$DRG][,.(DRG,ATYPE)]
type_cluster1 = type_cluster1[,.(type_num=.N), by=.(ATYPE)]
type_cluster1 = type_cluster1[order(type_cluster1[,2], decreasing = T)]
type_cluster1_ratio = type_cluster1[,.(ATYPE, ratio = round((type_num)/sum(type_num),2))]

# cluster 2
type_cluster2 = inp_important[inp_important$DRG %in% freq_cluster2$DRG][,.(DRG,ATYPE)]
type_cluster2 = type_cluster2[,.(type_num=.N), by=.(ATYPE)]
type_cluster2 = type_cluster2[order(type_cluster2[,2], decreasing = T)]
type_cluster2_ratio = type_cluster2[,.(ATYPE, ratio = round((type_num)/sum(type_num),2))]

# cluster 3
type_cluster3 = inp_important[inp_important$DRG %in% freq_cluster3$DRG[1:30]][,.(DRG,ATYPE)]
type_cluster3 = type_cluster3[,.(type_num=.N), by=.(ATYPE)]
type_cluster3 = type_cluster3[order(type_cluster3[,2], decreasing = T)]
type_cluster3_ratio = type_cluster3[,.(ATYPE, ratio = round((type_num)/sum(type_num),2))]

}










