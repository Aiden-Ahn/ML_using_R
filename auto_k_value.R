
knn_fun<-function(){
  
  x <- readline("분석 할 csv 파일명을 입력하시오 :") 
  y <-readline("라벨 컬럼 명을 입력하시오 :")
  
  wbcd <- read.csv(x, stringsAsFactors=FALSE)
  
  normalize<-function(x) {
    return( (x-min(x))/ ( max(x)-min(x)))
  }
  
  wbcd <- wbcd[-1] 
  ncol1 <- which(colnames(wbcd)==y)
  wbcd_n <- as.data.frame(lapply(wbcd[,-ncol1], normalize) ) 
  
  mm<-round(nrow(wbcd_n)*2/3)
  
  wbcd_train <- wbcd_n[1:mm, ]
  wbcd_test  <- wbcd_n[(mm+1):nrow(wbcd_n), ]
  
  wbcd_train_label <- wbcd[1:mm,y]
  wbcd_test_label  <- wbcd[(mm+1):nrow(wbcd_n),y]
  
  library(class)
  
  ## k값에 따른 정확도 비교
  valid_k <- data.frame( k= 1, accuracy= 2, error= 3)
  
  for (i in seq(from=1, to=100, by=2)){
    knnpredic <- knn(wbcd_train, wbcd_test, wbcd_train_label, k=i, prob = T)
    for_cbind <-as.vector( prop.table( table( ifelse(wbcd_test_label==knnpredic,"O","X"))) ) 
    for_rbind <-c( i ,for_cbind)
    valid_k<-rbind(valid_k, for_rbind)
  }
  
  valid_k<-valid_k[-1,]
  suitable_k<-valid_k[valid_k$accuracy == max(valid_k$accuracy) & valid_k$k >1,"k"]
  
  knnpredic <- NULL
  
  ## 출력
  knnpredic <- knn(train=wbcd_train, test=wbcd_test, cl= wbcd_train_label, k = suitable_k )
  cat('Training Set :',nrow(wbcd_train),'건','\n',
      'Testing Set :', nrow(wbcd_test),'건','\n',
      '적합한 K값 :', suitable_k,'\n',
      '분류기 정확도 :', valid_k[valid_k$accuracy == max(valid_k$accuracy) & valid_k$k >1,2], '\n') 
  cat('Warning : 첫번째 컬럼이 유효하지 않은 Dataset 에 대해서만 정확함')
}