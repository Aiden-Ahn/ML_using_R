# 유성 머신러닝 r 자동화 코드

################ 1. knn  함수 ######################
knn <- function(){
  
  x <- readline("분석할 csv 파일명을 입력하세요~ ") 
  y <-readline("라벨의 컬럼의 이름을 입력하세요~ ")
  #입력값받는 함수
  k_n<-readline("k값을 입력하시오~ ")
  
  wbcd <- na.omit(read.csv(x, stringsAsF5actors=FALSE) )
  
  wbcd <- wbcd[sample(nrow(wbcd)), ] 
  
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
  
  result1 <- knn(train=wbcd_train, test=wbcd_test, cl= wbcd_train_label, k = k_n )
  
  prop.table( table(ifelse(wbcd[(mm+1):nrow(wbcd_n),y]==result1,"o","x" )))
  
}
################나이브 베이즈 모듈######################
#install.packages('tm')
naviebayes <- function(){
  library(tm)
  library(gmodels)
  
  
  input_table <- readline('csv파일을 입력하세요. ex) emp.csv : ')
  input_header <- readline('컬럼명이 있습니까? ex)T OR F : ')
  input_label <- readline('라벨 컬럼의 번호를 입력하세요. ex)N (N>=1) : ')
  input_laplace <- readline('라플라스를 입력하세요. ex)n (0<n<1) : ')
  
  
  ### 헤더 유무 검사
  if (input_header == 'T'){
    table_name <- read.csv(input_table, stringsAsFactors = F, header=T) 
  }
  else {
    table_name <- read.csv(input_table, stringsAsFactors = F, header=F)
  }
  
  
  ### factor로 변환
  for (i in 1:ncol(table_name)){
    table_name[,i] <- factor(table_name[,i])
  }
  
  
  ### 라벨 번호, 라플라스 값 숫자화
  input_label <- as.integer(input_label)
  input_laplace <- as.numeric(input_laplace)
  
  
  set.seed(1)
  train_cnt <- round(0.75*dim(table_name[1]))
  train_index <- sample(1:dim(table_name)[1], train_cnt,replace=F)
  
  
  train_data <- table_name[train_index,]
  test_data <- table_name[-train_index,]
  
  
  table_name[,input_label]
  
  
  model <- naiveBayes(train_data[,input_label]~., data=train_data, laplace = input_laplace)
  result <- predict(model, test_data[,-input_label])
  
  
  CrossTable(result, test_data[,input_label])
}

################# 3.의사 결정트리 ##########################
################ 1) 정보획득량 ############################
information <- function(){
  packages <- c("FSelector")
  
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
  }
  
  library(FSelector)
  
  input_table <- readline('csv파일을 입력하세요. ex) emp.csv : ')
  table_name <- read.csv(input_table, stringsAsFactors = F, header=T)
  print(colnames(table_name))
  input_header <- readline('컬럼명이 있습니까? ex)T OR F : ')
  input_label_num <- readline('라벨이 위치한 번호을 입력하세요. ex)N : ') 
  input_rm_num <- readline('배제할 컬럼이 있다면 컬럼 위치번호를 입력하세요. ex) n,n,n ..., 없을 시 0 : ')
  
  input_label_num <- as.integer(input_label_num)
  
  ### 헤더 유무 검사
  if (input_header == 'T'){
    table_name <- read.csv(input_table, stringsAsFactors = T, header=T)  
  }else {
    table_name <- read.csv(input_table, stringsAsFactors = T, header=F)
  }
  
  ### factor로 변환
  for (i in 1:ncol(table_name)){
    table_name[,i] <- factor(table_name[,i])
  }
  
  ### na값 제거
  table_name <- na.omit(table_name)
  
  ### 제거할 컬럼 제거, 라벨 컬럼도 같이 지운다(라벨컬럼 마지막 열로 옮기기 위해 마지막에 붙여줌)
  
  ## 라벨 값 추출
  table_label_col<-table_name[, input_label_num]
  
  ### 제거할 컬럼 제거
  split_num<-strsplit(input_rm_num, ',')
  split_num <- sort(as.integer(c(unlist(split_num), input_label_num)), decreasing = T)
  if (0 %in% split_num ){
    table_name <- table_name[,-as.integer(input_label_num)]
  }else{    
    for(i in split_num){
      table_name <- table_name[,-as.integer(i)]
    }
  }
  
  table2 <- cbind(table_name, lb=factor(table_label_col))
  
  ## 결과
  weights <- information.gain( lb ~ ., table2)
  print(weights)
}


################# 3.의사 결정트리 ##########################
################ 2) 의사결정 트리로 시각화 ############################

decision <- function(){
  
  packages <- c("rpart.plot","rattle")
  
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
  }
  
  library(rattle)
  library(rpart.plot)
  
  input_table <- readline('csv파일을 입력하세요. ex) emp.csv : ')
  table_name <- read.csv(input_table, stringsAsFactors = F, header=T)
  print(colnames(table_name))
  input_header <- readline('컬럼명이 있습니까? ex)T OR F : ')
  input_label_num <- readline('라벨이 위치한 번호을 입력하세요. ex)N : ') 
  input_rm_num <- readline('배제할 컬럼이 있다면 컬럼 위치번호를 입력하세요. ex) n,n,n ..., 없을 시 0 : ')
  
  input_label_num <- as.integer(input_label_num)
  
  ### 헤더 유무 검사
  if (input_header == 'T'){
    table_name <- read.csv(input_table, stringsAsFactors = T, header=T)  
  }else {
    table_name <- read.csv(input_table, stringsAsFactors = T, header=F)
  }
  
  ### na값 제거
  table_name <- na.omit(table_name)
  
  ### 제거할 컬럼 제거, 라벨 컬럼도 같이 지운다(라벨컬럼 마지막 열로 옮기기 위해 마지막에 붙여줌)
  
  ## 라벨 값 추출
  table_label_col<-table_name[, input_label_num]
  
  ### 제거할 컬럼 제거
  split_num<-strsplit(input_rm_num, ',')
  split_num <- sort(as.integer(c(unlist(split_num), input_label_num)), decreasing = T)
  if (0 %in% split_num ){
    table_name <- table_name[,-as.integer(input_label_num)]
  }else{    
    for(i in split_num){
      table_name <- table_name[,-as.integer(i)]
    }
  }
  
  
  table2 <- cbind(table_name, lb=factor(table_label_col))
  
  tree <- rpart(lb~. , data=table2, control=rpart.control(minsplit=2) )
  
  fancyRpartPlot(tree)
}

################# 3.의사 결정트리 ##########################
################ 3) 규칙 기반의 리퍼 알고리즘을 사용 ############################

riper <- function(){
  
  packages <- c("RWeka","gmodels","tm")
  
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
  }
  
  library(RWeka)
  library(tm)
  library(gmodels)
  
  input_table <- readline('csv파일을 입력하세요. ex) emp.csv : ')
  table_name <- read.csv(input_table, stringsAsFactors = F, header=T)
  print(colnames(table_name))
  input_header <- readline('컬럼명이 있습니까? ex) T or F : ')
  input_label_num <- readline('라벨이 위치한 번호을 입력하세요. ex)N : ') 
  input_rm_num <- readline('배제할 컬럼이 있다면 컬럼 위치번호를 입력하세요. ex) n,n,n ..., 없을 시 0 : ')
  
  input_label_num <- as.integer(input_label_num)
  
  ###헤더유무검사
  if(input_header == 'T'){
    table_name <- read.csv(input_table, stringsAsFactors = F , header = T)
  }else{
    table_name <- read.csv(input_table, stringsAsFactors = F, header=F)
  }
  
  ### na값 제거
  table_name <- na.omit(table_name)
  
  ### 라벨 값 추출
  table_label_col <- table_name[ , input_label_num]
  
  ### 제거할 컬럼 제거
  split_num <- strsplit(input_rm_num, ',')
  split_num <- sort(as.integer( c(unlist(split_num), input_label_num) ), decreasing = T )
  if (0 %in% split_num ){
    table_name <- table_name[,-as.integer(input_label_num)]
  }else{
    for(i in split_num){
      table_name <- table_name[,-as.integer(i)]
    }
  }
  
  table2 <- cbind(table_name, lb=factor(table_label_col))
  
  ### 전체데이터를 factor로 변환
  for(i in 1:ncol(table2)){
    table2[, i] <- factor(table2[ ,i])
  }
  
  ### 훈련데이터와 테스트데이터를 3:1 비율로 나눔
  set.seed(1)
  
  train_cnt <- round(0.75*nrow(table2))
  
  train_index <- sample(1:nrow(table2), train_cnt, replace = F)
  
  data_train <- table2[train_index, ]
  data_test <- table2[-train_index, ]
  
  ### ripper 알고리즘으로 모델 생성
  model <- JRip(lb~., data = data_train)
  
  print <- summary(model)
  
  ### 결과 예측
  result <- predict(model, data_test[ , -ncol(data_test)] )
  
  print(result)
  
  ### 이원교차표 생성
  CrossTable(result, data_test[ , ncol(data_test)])
}

################# 4.회귀분석 ##########################

multi_reg <- function(){
  graphics.off()
  
  input_table <- readline('csv파일을 입력하세요. ex) emp.csv : ')
  table_name <- read.csv(input_table, stringsAsFactors = F, header=T)
  
  print(colnames(table_name))
  cat('\n','Data Summary','\n')
  print(head(table_name)[1:3,])
  
  input_header <- readline('컬럼명이 있습니까? ex)T OR F : ')
  input_y_num <- readline('종속변수의 위치번호를 입력하세요. ex)N (N>=1) : ') 
  input_y_num <- as.integer(input_y_num)
  
  input_x_num <- readline('독립변수의 위치번호를 입력하세요. ex)n OR n,n,n ... (n>=1) : ')
  
  ### 헤더 유무 검사
  if (input_header == 'T'){
    table_name <- read.csv(input_table, stringsAsFactors = F, header=T)  
  }else {
    table_name <- read.csv(input_table, stringsAsFactors = F, header=F)
  }
  
  ### 종속변수 값 추출
  table_y_col<-table_name[, input_y_num]
  table_y_col
  
  ### 독립변수 값 추출
  split_num<-data.frame(strsplit(input_x_num, ','))
  names(split_num) <- 'num'
  split_num <- as.integer(as.character(split_num$num))
  tmp_table <- data.frame(table_name[,split_num])
  
  final_table <- cbind(lb = table_y_col, tmp_table)
  
  model <- lm(lb~., data=final_table)
  
  split_num <- data.table(num=split_num)
  if(nrow(split_num) == 1){
    names(final_table) <- c('lb','xv')
    
    input_x_num <- as.integer(input_x_num)
    yname <- colnames(table_name[input_y_num])
    xname <- colnames(table_name[input_x_num])
    
    tmp<-round(coef(model),2)
    title <- paste('y = ', tmp[2],'* x + ',tmp[1])
    
    plot(lb~xv, data=final_table, xlab = xname , ylab = yname, col='blue', main=title)
    abline(model,col='red')
  }
  
  print(model)
}

################# 4.회귀분석 ##########################
#####################3.회귀트리######################

reg_tree <- function(){
  graphics.off()
  
  input_table <- readline('csv파일을 입력하세요. ex) emp.csv : ')  
  table_name <- read.csv(input_table, stringsAsFactors = F, header=T)
  
  print(colnames(table_name))
  cat('\n','Data Summary','\n')
  print(head(table_name)[1:3,])
  
  input_header <- readline('컬럼명이 있습니까? ex)T OR F : ')
  input_y_num <- readline('종속변수의 위치번호를 입력하세요. ex)N (N>=1) : ') 
  input_y_num <- as.integer(input_y_num)
  
  input_x_num <- readline('독립변수의 위치번호를 입력하세요. ex)n OR n,n,n ... (n>=1) : ')
  
  ### 헤더 유무 검사
  if (input_header == 'T'){
    table_name <- read.csv(input_table, stringsAsFactors = F, header=T)  
  }else {
    table_name <- read.csv(input_table, stringsAsFactors = F, header=F)
  }
  
  ### 종속변수 값 추출
  table_y_col<-table_name[, input_y_num]
  
  table2 <- cbind(table_name, lb=table_y_col)
  
  ### 훈련데이터와 테스트데이터를 3:1 비율로 나눔
  set.seed(1)
  
  train_cnt <- round(0.75*nrow(table2))
  data_train <- table2[1:train_cnt, ]
  data_test <- table2[(train_cnt+1):nrow(table2), ]
  table_y_col <- table_y_col[1:train_cnt]
  
  ### 독립변수 값 추출
  split_num<-data.frame(strsplit(input_x_num, ','))
  names(split_num) <- 'num'
  split_num <- as.integer(as.character(split_num$num))
  tmp_table <- data.frame(data_train[,split_num])
  
  final_table <- cbind(lb = table_y_col, tmp_table)
  
  ### 모델트리 생성
  model <- rpart(lb~., data=final_table)
  
  ### 모델트리 그래프
  rpart.plot(model,digits=3, fallen.leaves = TRUE, type = 3, extra = 101)
  
  ### 모델로 테스트 데이터를 테스트의 라벨을 예측
  result <- predict(model, data_test)
  
  ### 테스트 데이터의 실제 라벨과 비교
  cbind(round(result), data_test$lb)
  
  ### 위의 예측 결과와 테스트 라벨의 상관관계가 어떻게 되는지 확인
  cor(result, data_test$lb)
  
  ### 두 데이터 간의 오차율 확인(모델 성능 측정)
  MAE <- function(actual, predicted){
    mean(abs(actual - predicted))
  }
  print(MAE(result, data_test$lb))
}

################# 5.신경망 ##########################
###############1. neuralnet 패키지####################

ann_neuralnet <- function(){
  packages <- c('neuralnet')
  
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
  }
  
  library(neuralnet)
  
  input_table <- readline('csv파일을 입력하세요. ex) emp.csv : ')
  table_name <- read.csv(input_table, stringsAsFactors = F, header=T)
  
  print(colnames(table_name))
  cat('\n','Data Summary','\n')
  print(head(table_name)[1:3,])
  
  input_header <- readline('컬럼명이 있습니까? ex)T OR F : ')
  input_label_num <- readline('라벨의 위치번호를 입력하세요. ex)N (N>=1) : ') 
  input_label_num <- as.integer(input_label_num)
  input_hidden_node <- readline('hidden node의 수를 입력하세요. ex) 5 ')
  #input_hidden_layer <- readline('hidden layer의 수를 입력하세요. ex) 노드수 : 숫자, 계층수 : , ex) 2 ')
  input_rm_num <- readline('배제할 컬럼이 있다면 컬럼 위치번호를 입력하세요. ex) n,n,n ..., 없을 시 0 : ')
  
  ### 헤더 유무 검사
  if (input_header == 'T'){
    table_name <- read.csv(input_table, stringsAsFactors = T, header=T)  
  }else {
    table_name <- read.csv(input_table, stringsAsFactors = T, header=F)
  }
  
  ### na값 제거
  table_name <- na.omit(table_name)
  
  ## 라벨 값 추출
  table_label_col<-table_name[, input_label_num]
  
  ### 제거할 컬럼 제거
  split_num<-strsplit(input_rm_num, ',')
  split_num <- sort(as.integer(c(unlist(split_num), input_label_num)), decreasing = T)
  if (0 %in% split_num ){
    table_name <- table_name[,-as.integer(input_label_num)]
  }else{
    for(i in split_num){
      table_name <- table_name[,-as.integer(i)]
    }
  }
  final_table <- cbind(table_name, lb=table_label_col)
  
  # 정규화 함수
  normalize <- function(x) {
    return((x - min(x)) / (max(x) - min(x)))
  }
  
  # 전체 데이터 프레임에 정규화 적용
  table_norm <- as.data.frame(lapply(final_table, normalize))
  
  # 훈련, 테스트 데이터 생성
  size<-nrow(table_norm)
  set.seed(100)
  index <- c(sample(1:size,size*0.7))
  train_data <- table_norm[index,]
  test_data <- table_norm[-index,]
  
  # fomula 생성
  ncol(train_data)
  allVars <- colnames(train_data)
  predictorVars<-allVars[-ncol(train_data)]
  predictorVars<-paste(predictorVars, collapse = "+")
  form = as.formula(paste("lb ~", predictorVars))
  model <- neuralnet( formula = form , train_data, hidden=input_hidden_node)
  
  plot(model)
  
  result <- compute(model, test_data[,-ncol(test_data)])
  predicted <- result$net.result
  cor(predicted, test_data[,ncol(test_data)])
}

################# 6.서포트 벡터 머신 ###################
################ 1. e1071 패키지 ####################

svm_e1071 <- function(){
  
  packages <- c('e1071','kernlab')
  
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
  }
  
  library(e1071)
  library(kernlab)
  
  input_table <- readline('csv파일을 입력하세요. ex) emp.csv : ')
  table_name <- read.csv(input_table, stringsAsFactors = F, header=T)
  
  print(colnames(table_name))
  cat('\n','Data Summary','\n')
  print(head(table_name)[1:3,])
  
  input_header <- readline('컬럼명이 있습니까? ex)T OR F : ')
  input_label_num <- readline('라벨의 위치번호를 입력하세요. ex)N (N>=1) : ') 
  input_label_num <- as.integer(input_label_num)
  input_kernel <- readline('원하는 커널명을 입력하세요. ex) linear(선형), polynomial(다항), radial basis(방사형 기저), sigmoid(시그모이드) : ')
  
  ### 헤더 유무 검사
  if (input_header == 'T'){
    table_name <- read.csv(input_table, stringsAsFactors = T, header=T)  
  }else {
    table_name <- read.csv(input_table, stringsAsFactors = T, header=F)
  }
  
  ### na값 제거
  table_name <- na.omit(table_name)
  
  ## 라벨 값 추출
  table_label_col<-table_name[, input_label_num]
  
  ### 제거할 컬럼 제거
  table_name <- table_name[,-as.integer(input_label_num)]
  
  final_table <- cbind(table_name, lb=factor(table_label_col))
  
  # 랜덤시드 생성
  set.seed(12345)
  
  # 셔플
  table_ran <- final_table[order(runif(nrow(final_table))), ]
  
  # 트레이닝셋 80%
  index <- round(0.8*nrow(table_ran))
  train_data <- table_ran[1:index, ]
  
  # 테스트셋 20%
  test_data <- table_ran[(index+1):nrow(table_ran), ]
  
  # 선형SVM 훈련
  table_svm <- svm(lb~., data = train_data, kernel=input_kernel)
  
  # 모델 테스트
  p <- predict(table_svm, test_data)
  
  table(p, test_data$lb)
  
  # 분류 결과 확인
  mean(p == test_data$lb)
  
}


################# 6.서포트 벡터 머신 ###################
################ 2. kernlab 패키지 ####################

svm_kernlab <- function(){
  
  packages <- c('e1071','kernlab')
  
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
  }
  
  library(e1071)
  library(kernlab)
  
  input_table <- readline('csv파일을 입력하세요. ex) emp.csv : ')
  table_name <- read.csv(input_table, stringsAsFactors = F, header=T)
  
  print(colnames(table_name))
  cat('\n','Data Summary','\n')
  print(head(table_name)[1:3,])
  
  input_header <- readline('컬럼명이 있습니까? ex)T OR F : ')
  input_label_num <- readline('라벨의 위치번호를 입력하세요. ex)N (N>=1) : ') 
  input_label_num <- as.integer(input_label_num)
  input_kernel <- readline('원하는 커널명을 입력하세요. ex) rbfdot(방사형 기저), polydot(다항), tanhdot(하이퍼볼릭 탄젠트 시그모이드), vanilladot(선형) : ')
  ### 헤더 유무 검사
  if (input_header == 'T'){
    table_name <- read.csv(input_table, stringsAsFactors = T, header=T)  
  }else {
    table_name <- read.csv(input_table, stringsAsFactors = T, header=F)
  }
  
  ### na값 제거
  table_name <- na.omit(table_name)
  
  ## 라벨 값 추출
  table_label_col<-table_name[, input_label_num]
  
  ### 제거할 컬럼 제거
  table_name <- table_name[,-as.integer(input_label_num)]
  final_table <- cbind(table_name, lb=factor(table_label_col))
  
  # 랜덤시드 생성
  set.seed(12345)
  
  # 셔플
  table_ran <- final_table[order(runif(nrow(final_table))), ]
  
  
  # 트레이닝셋 80%
  index <- round(0.8*nrow(table_ran))
  train_data <- table_ran[1:index, ]
  
  # 테스트셋 20%
  test_data <- table_ran[(index+1):nrow(table_ran), ]
  
  # 비선형SVM 훈련
  lb_classifier <- ksvm(lb ~ ., data = train_data, kernel = "vanilladot")
  
  # 모델 테스트
  lb_predictions <- predict(lb_classifier, test_data)
  
  table(lb_predictions, test_data$lb)
  
  # 분류 결과 확인
  agreement <- lb_predictions == test_data$lb
  
  prop.table(table(agreement))
  
}

################# 7.k-means ##########################

kmeans <- function(){
  packages <- c('factoextra')
  
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
  }
  
  library(factoextra)
  
  input_table <- readline('csv파일을 입력하세요. ex) emp.csv : ')
  
  table_name <- read.csv(input_table, header=T)
  
  print(colnames(table_name))
  cat('\n','Data Summary','\n')
  print(head(table_name)[1:3,])
  
  input_header <- readline('컬럼명이 있습니까? ex)T OR F : ')
  
  input_x_num <- readline('x축의 위치번호를 입력하세요. ex)N (N>=1) : ') 
  input_y_num <- readline('y축의 위치번호를 입력하세요. ex)N (N>=1) : ') 
  input_x_num <- as.integer(input_x_num)
  input_y_num <- as.integer(input_y_num)
  
  input_k_num <- readline('k의 값을 입력하세요. ex)N (N>=1) : ')
  input_k_num <- as.integer(input_k_num)
  
  ### 헤더 유무 검사
  if (input_header == 'T'){
    table_name <- read.csv(input_table, header=T)  
  }else {
    table_name <- read.csv(input_table, header=F)
  }
  
  final_table <- table_name[,c(input_x_num,input_y_num)]
  km <- kmeans(final_table, input_k_num)
  fviz_cluster(km, data=final_table ,stand=F)
}

################# 8.연관규칙 ##########################

association_rule <- function(){
  packages <- c("igraph","visNetwork","arulesViz","arules")
  
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
  }
  
  library(arules)
  library(arulesViz)
  library(visNetwork)
  library(igraph)
  
  input_table <- readline('csv파일을 입력하세요. ex) emp.csv : ')
  table_name <- read.csv(input_table, stringsAsFactors = F, header=T)
  print(colnames(table_name))
  input_header <- readline('컬럼명이 있습니까? ex)T OR F : ')
  
  input_support <- readline('지지도를 입력하세요. ex)N : ')
  input_support <- as.numeric(input_support)
  
  input_confidence <- readline('신뢰도를 입력하세요. ex)N : ')
  input_confidence <- as.numeric(input_confidence)
  
  input_rm_num <- readline('배제할 컬럼이 있다면 컬럼 위치번호를 입력하세요. ex) n,n,n ..., 없을 시 0 : ')
  
  
  ### 헤더 유무 검사
  if (input_header == 'T'){
    table_name <- read.csv(input_table, stringsAsFactors = T, header=T)  
  }else {
    table_name <- read.csv(input_table, stringsAsFactors = T, header=F)
  }
  
  ### na값 0으로 변경
  table_name[is.na(table_name)] <- 0
  
  ### 제거할 컬럼 제거
  split_num<-strsplit(input_rm_num, ',')
  split_num <- sort(as.integer(unlist(split_num)), decreasing = T)
  if (0 %in% split_num ){
    table_name <- table_name
  }else{    
    for(i in split_num){
      table_name <- table_name[,-as.integer(i)]
    }
  }
  
  
  trans <- as.matrix(table_name, "Transaction")
  
  rules <- apriori(trans, parameter=list(support=input_support, confidence=input_support))
  
  subrules2 <- head(sort(rules, by="lift"), 10)
  
  ig <- plot( subrules2, method="graph", control=list(type="items") )
  
  # saveAsGraph seems to render bad DOT for this case
  tf <- tempfile( )
  saveAsGraph( subrules2, file = tf, format = "dot" )
  
  # clean up temp file if desired
  #unlink(tf)
  
  
  
  # let's bypass saveAsGraph and just use our igraph
  ig_df <- get.data.frame( ig, what = "both" )
  visNetwork(
    nodes = data.frame(
      id = ig_df$vertices$name
      ,value = ig_df$vertices$support # could change to lift or confidence
      ,title = ifelse(ig_df$vertices$label == "",ig_df$vertices$name, ig_df$vertices$label
      )
      ,ig_df$vertices
    )
    , edges = ig_df$edges
  ) %>%
    visEdges( ig_df$edges ) %>%
    visOptions( highlightNearest = T )
}

################함수 종합 모듈######################
machine_learning <- function() {
  ml_cho<-menu(c("knn 알고리즘", "나이브 베이즈","의사 결정트리","회귀 분석","신경망","서포트 벡터 머신","k-mean","연관분석"), title="What do you want to do?")
  
  if (ml_cho == 1){
    knn()
  }
  else if (ml_cho == 2 ) {
    naviebayes()
  }
  else if (ml_cho == 3 ) {
    cho3<-menu(c("정보획득량 출력","의사결정 트리로 시각화","규칙 기반의 리퍼 알고리즘을 사용"))
    
    if (cho3 == 1){
      information()
    }
    else if (cho3 == 2 ) {
      decision()
    }
    else if (cho3 == 3 ) {
      riper()
    }
  }
  else if (ml_cho == 4 ) {
    cho4<-menu(c("단순 선형 회귀 그래프","다중 선형 회귀 그래프","회귀트리"))
    
    if (cho4 == 1){
      multi_reg()
    }
    else if (cho4 == 2 ) {
      multi_reg()
    }
    else if (cho4 == 3 ) {
      reg_tree()
    }
  }
  else if (ml_cho == 5 ) {
    cho5<-menu(c("neuralnet 패키지","nnet 패키지"))
    
    if (cho5 == 1){
      ann_neuralnet()
    }
    else if (cho5 == 2 ) {
      ann_nnet()
    }
  }
  else if (ml_cho == 6 ) {
    cho6<-menu(c("e1071 패키지","kernlab 패키지"))
    
    if (cho6 == 1){
      svm_e1071()
    }
    else if (cho6 == 2 ) {
      svm_kernlab()
    }
  }
  else if (ml_cho == 7 ) {
    kmeans()
  }
  else if (ml_cho == 8 ) {
    association_rule()
  }
}

machine_learning()
