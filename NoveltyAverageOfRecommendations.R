evaluateNovelty<-function(item_novelty, userId=3,path){
  a<-read.table(path)
  a<-a[,1]
  noveltySum =0
  
  n= length(a)
  print(paste("USER", userId,sep = ":"))
  discarded = 0
  for(i in 1:n){
    
    #if(length(item_novelty[item_novelty$i == a[i],]) ==0){
    
    #}
    #It should be noted that if the item is not found from the list it will be discarded
    #This will occure if user is example is -1 or the post type is not parent post.
    if(nrow(item_novelty[item_novelty$i == a[i],]) >0){
      print(a[i])
      
      print(item_novelty[item_novelty$i == a[i],])
      noveltySum = noveltySum+item_novelty[item_novelty$i == a[i],'novelty']
    }else{
      discarded=discarded+1
    }
  }
  n=n-discarded
  nov_avg = noveltySum/n
  newRow <- c(row.names=userId, n=n, nov_sum=noveltySum, now_avg=nov_avg)
  print(newRow)
  return (newRow)
}
evaluateAll<-function(item_novelty,class="PearsonCorrelationSimilarity", path){
  fullPath <-paste(path, class,sep = "")
  files <- list.files(fullPath)
  
  
  
  df <- data.frame(User=character(),
                   n=integer(), 
                   nov_sum=numeric(),
                   nov_avg=numeric(), 
                   stringsAsFactors=FALSE)
  for(i in 1:length(files)){
    user <- sub("^([^.]*).*", "\\1", files[i])
    evaluation<-evaluateNovelty(item_novelty,user, paste(fullPath, files[i],sep="/"))
    print(str(evaluation))
    
    if(evaluation[2] > 0){
      df[nrow(df)+1,]<-evaluation
    }
  }
  return(df)
}