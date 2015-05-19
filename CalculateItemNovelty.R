itemChoiceNovelty<-function(path="~/Lataukset/datasets/QueryResults.csv"){
  dataset <- read.csv(file=path,head=FALSE)
  totalActivities = sum(dataset$V3)
  colnames(dataset)[1]<-"i"
  aggregatedItems <-aggregate(V3~i, data=dataset, FUN = sum)
  results <- transform(aggregated_items, popularity=activity/totalActivities, novelty=-log2(activity/totalActivities))
  return (results)