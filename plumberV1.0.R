#disease prediction using bayesian classification V1.0

library(plumber)
library(naivebayes)
library(tm)
library(magrittr)
library(RMariaDB)
library(dplyr)
#database related data
sql.conf.file <- './healthpray_feedback.cnf'
database.name <- 'healthpray_feedback'

#global variable and intitialization
num_cluster <- 5 # maximum number of cluster to create
threshold <- 0.20 # minimum probablity to predict final disease name
laplace <- 1 #laplace value for mnb classifier
session_id <- 1 # 1 session per user
cluster_id <- 0 
id <- 0
intermediate.flag <- FALSE
final.flag <- FALSE

cluster.list <- list() # info about each cluster
symptom.list <- list() # symptoms data user selected
symptom.list.na <- list() # symptoms data user not selected
cluster.df <- data.frame(NA,NA) # dataframe contain mapping of cluster id and ref cluster id
names(cluster.df) <- c("id","ref_id")
global.cluster.symptom <- vector("list")

#global file data
dictionary <- read.csv("disease_symptom_dictionary.csv") #name and code mapping
train_file <- read.csv("disease_symptom_mapping_training.csv") #training data
symptoms.select.df <- read.csv("symptoms_code_mapping.csv")
symptom.discription <- read.csv("symptom_discription.csv")
symptom.discription.file <- read.csv("symptoms_code_mapping_detailed.csv")
logfile <- "log.txt"
code.value <- "067 111 100 101 032 098 121 032 075 097 114 116 104 105 107"
  #get all symptoms which need test
test.positive.symptoms.index <- which(symptom.discription.file$require_test.Y.N.. %in% "Y")
count <- 1
test.positive.symptoms <- list()
for (i in test.positive.symptoms.index) {
  test.positive.symptoms[count] <- toString(symptom.discription.file$code[i])
  count <- count + 1
}

#create mnb and dtm
file.symptoms <- as.vector(train_file$symptom)
val <- Corpus(VectorSource(file.symptoms))

#create document term matrix
dtm <- DocumentTermMatrix(val)
matrix.symptoms <- as.matrix(dtm)
colnames(matrix.symptoms) <- toupper(colnames(matrix.symptoms))

#fetch disease name vector
disease <- as.vector(train_file$disease)

#create multinomial naive bayes classifier
mnb <- multinomial_naive_bayes(x = matrix.symptoms, y = disease, laplace = laplace)

#api code begins

# #* @filter checkAuth
# function(req, res){
#   #print(ls(req))
#   #print(req$HEADERS["username"])
#   if (is.na(req$HEADERS["username"])){
#     res$status <- 401 # Unauthorized
#     return(list(error="Authentication required"))
#   } else {
#     session_id <<- digest(req$HEADERS["username"],algo = "md5")
#     print(paste("current session_id",session_id))
#     plumber::forward()
#   }
# }

#* Log some information about the incoming request
#* @filter logger
function(req){
  cat(as.character(Sys.time()), "-", 
      req$REQUEST_METHOD, req$PATH_INFO, "-",
      req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n",
      file = "healthpray/Questionnaires/logger.txt",append = TRUE)
  plumber::forward()
}


#' @apiTitle Disease Classifier

#' Echo the parameter that was sent in
#' @param msg The message to echo back.
#'  @get /
function() {
  "welcome"
}

#' Will fetch all the disease name require no parmaeters
#' @get /alldisease
function() {
  list(unique(dictionary$name))
}

#' Return disease/symptom name given UMCL code
#' @param UMCLcode UMCL code of disease/symptom
#' @get /getname
function(UMCLcode) {
  list(disease_name = dictionary$name[match(UMCLcode,dictionary$code)])
}

#' Step 1 of disease prediction
#' @param symptoms comma seperated symptoms code
#' @get /predict
main_predict <- function(symptoms="" ,param.symptom.list.na = list(),res) {
  
  #check null value for symptoms
  if(nchar(symptoms) == 0) {
    msg <- "need minimum of one symptom"
    res$status <- 400 # Bad request
    return(list(error=jsonlite::unbox(msg)))
  }
  
  print("debug : in main_predict")
  #create test data, set colnames and fetch symptoms value
  testdata <- matrix(sample(0,1 * ncol(dtm),replace = TRUE),nrow = 1,ncol = ncol(dtm),byrow = TRUE)
  colnames(testdata) <- colnames(matrix.symptoms)
  symtoms.list <- strsplit(symptoms,split = ",") 
  symtoms.list <- as.vector(symtoms.list[[1]]) %>% trimws(.)
  
  #initialize test data with given symptoms 
  for(i in 1:length(symtoms.list)) {
    if(is.na(testdata[1,match(symtoms.list[i],colnames(testdata))])) {
      stop( paste("please provide proper UMCL code; error in",
                  symtoms.list[i],sep = " : "))
      
    }
    else {
      testdata[1,match(symtoms.list[i],colnames(testdata))] <- 1
    }
  }
  
  #set testdata
  data <- testdata
  
  prob.data <- predict(mnb, newdata = data, type = "prob")
  
  #create cluster based on the prob.data
  cluster <- cluster.local(prob.data,4)
  #check if cluster is dense if so change rounding digit to 6
  if(length(unlist(cluster)) > 120) {
    cluster <- cluster.local(prob.data,6)
  }
  #add values to global variable
  symptom.list[[session_id]] <<- symtoms.list
  symptom.list.na[[session_id]] <<- param.symptom.list.na
  #create symptoms for each cluster
  cluster.symptom <- create_fuction(cluster)
  global.cluster.symptom <<- cluster.symptom
  cluster_id <<- cluster_id + 1
  print(paste("cluster id",cluster_id,sep = " : "))
  cluster.list[[cluster_id]] <<- cluster
  temp.df <- data.frame(cluster_id,NA)
  names(temp.df) <- c("id","ref_id")
  cluster.df <<- bind_rows(cluster.df,temp.df)
  # print("df")
  # print(cluster.df)
  # print("cluster list")
  # print(cluster.list)
  # print("symptom list")
  # print(symptom.list)
  # print("symptom list na")
  # print(symptom.list.na)
  list(ID = cluster_id,return_question_object(cluster.symptom = cluster.symptom))
  
}

#' Set threshold
#' @get /setthreshold/<thresholdvalue:double>
function(thresholdvalue) {
  threshold <<- thresholdvalue
  list(paste("threshold set to",threshold,sep = " : "))
}

#' Fecth questions to be validated
#' @get /getfeeback
function(req) {
  
  storiesDb<-dbConnect(RMariaDB::MariaDB(),default.file=sql.conf.file,group=database.name)
  query <- "select * from feedback_question"
  result <- dbSendQuery(storiesDb,query)
  questions.data.frame <- data.frame(dbFetch(result))
  dbClearResult(result)
  for(i in 1:length(questions.data.frame$id)) {
    questions.data.frame$yes[i] <- paste('http://',req$SERVER_NAME,':',req$SERVER_PORT,'/answerquestion/',
                                         questions.data.frame$id[i],'/answer/T',sep = "")
    questions.data.frame$no[i] <- paste('http://',req$SERVER_NAME,':',req$SERVER_PORT,'/answerquestion/',
                                        questions.data.frame$id[i],'/answer/F',sep = "")
    questions.data.frame$Na[i] <- paste('http://',req$SERVER_NAME,':',req$SERVER_PORT,'/answerquestion/',
                                        questions.data.frame$id[i],'/answer/NA',sep = "")
  }
  
  # list(disease = questions.data.frame$answer,symptoms = questions.data.frame$question, yes = yes,
  #      no = no, Na = Na)
  dbDisconnect(storiesDb)
  questions.data.frame
  
}

#' answer feedback questions
#' @get /answerquestion/<questionval:int>/answer/<answerval>
function(questionval,answerval) {
  
  storiesDb<-dbConnect(RMariaDB::MariaDB(),default.file=sql.conf.file,group=database.name)
  fetch.query <- paste("SELECT * FROM feedback_question WHERE id =",questionval,sep = " ")
  rsread <- dbSendQuery(storiesDb,fetch.query)
  intermediate.data <- dbFetch(rsread)
  symptom <- intermediate.data$question
  disease <- intermediate.data$answer
  final.query <- paste("INSERT INTO feedback_data(disease_name,symptoms,validataion) VALUES(",
                       "'",disease,"'",",","'",symptom,"'",",","'",answerval,"'",")",sep = "")
  print(final.query)
  dbClearResult(rsread)
  rsInsert <- dbSendQuery(storiesDb,final.query)
  dbClearResult(rsInsert)
  dbDisconnect(storiesDb)
  
  list(paste("thanks for feedback",answerval))
}


#' pridict stage two
#' @post /question-reply/
#' @param question_id id of the question
#' @param answer responce to given question it should contain symptom code
intermediate_predict <- function(question_id,answer = "") {
  
  print("debug : in intermediate_predict")
  question_id <- as.numeric(question_id)
  
  answer <- strsplit(answer,split = ",")
  answer <- as.vector(answer)
  
  symptom.list[[session_id]] <<- c(symptom.list[[session_id]],answer[[1]])
  print(symptom.list[[session_id]])
  #fetch cluster
  cluster <- cluster.list[[question_id]]
  previous.cluster.symptom <- global.cluster.symptom
  #check highest prob - backtracking
  highest_prob = high_prob(symptom.list[[session_id]])
  for(dis in highest_prob) {
    if(is.na(match(dis,unlist(cluster)))) {
      temp.cluster <- find_cluster(dis,cluster_id)
      cluster[[length(cluster) + 1]] <- temp.cluster
      print(paste("added",temp.cluster,sep = " : "))
    }
  }
  
  #fetch cluster symptoms
  
  temp <- list()
  cluster.symptom.sub <- list()
  for(i in 1:length(cluster)) {
    for(j in 1:length(cluster[[i]])){
      temp[j] <- train_file$symptom[match(cluster[[i]][j],train_file$disease)] %>% toString() %>%
        strsplit(.,split = " ")
    }
    cluster.symptom.sub[[i]] <- unique(unlist(temp))
  }
  #update symptoms answered as no
  symptom.list.na[[session_id]] <<- c(symptom.list.na[[session_id]],setdiff(unlist(previous.cluster.symptom),
                                                                            unlist(symptom.list[[session_id]])))
  
  #rank cluster - top cluster is used for next process
  if(length(answer) == 0) {
    cluster.rank <- rank_cluster(cluster.symptom.sub,symptom.list[[session_id]],symptom.list.na[[session_id]],na = TRUE)
  }else{
    cluster.rank <- rank_cluster(cluster.symptom.sub,symptom.list[[session_id]],symptom.list.na[[session_id]])
  }
  #fetch top cluster index
  cluster.rank.index <- which(cluster.rank %in% max(unlist(cluster.rank)))
  print(cluster.rank.index)
  if(length(cluster.rank.index) > 1) {
    cluster.temp = list()
    for(i in cluster.rank.index) {
      cluster.temp = c(cluster.temp, cluster[[i]])
    }
  } else {
    cluster.temp = cluster[[cluster.rank.index]]
  }
  #debug msg
  # print("cluster rank")
  # print(cluster.rank)
  
  #call prob_data to fetch probability where rank.cluster is set of disease
  cluster.prob.data <- prob_data(rank.cluster = cluster.temp,selected.symptoms = symptom.list[[session_id]])
  #cluster again from top rank cluster
  cluster.new <- cluster.local(cluster.prob.data,2)
  print(intersect(cluster.new,cluster.list[[cluster_id]]))
  cluster_id <<- cluster_id + 1
  
  #cluster has only one disease then it will be final cluster
  if(length(cluster.new) == 1 && length(cluster.new[[1]]) == 1) {
    cluster.symptom <- as.vector(train_file$symptom[match(cluster.new[[1]][1],
                                                          train_file$disease)])
    cluster.symptom <- strsplit(cluster.symptom,split = " ") %>% as.vector(.)
    cluster.symptom <-setdiff(unlist(cluster.symptom),symptom.list[[session_id]])
    cluster.symptom <- setdiff(unlist(cluster.symptom),unlist(symptom.list.na[[session_id]]))
    # responce_link <- paste('http://',req$SERVER_NAME,':',req$SERVER_PORT,'/finalcall/',
    #                        cluster_id,sep = "")
    global.cluster.symptom <<- cluster.symptom
    cluster.list[[cluster_id]] <<- cluster.new
    # print("symptom list")
    # print(symptom.list)
    # print("symptom list na")
    # print(symptom.list.na)
    list(ID = cluster_id,is.next = 1,
         return_question_object(cluster.symptom))
    
  } else {
    #create symptom set for new cluster
    cluster.symptom.new <- create_fuction(cluster.new)
    global.cluster.symptom <<- cluster.symptom.new
    #debug msg
    print(paste("cluster id",cluster_id,sep = " : "))
    print("mapping symptom's for cluster")
    #update global variable
    cluster.list[[cluster_id]] <<- cluster.new
    temp.df <- data.frame(cluster_id,question_id)
    names(temp.df) <- c("id","ref_id")
    cluster.df <<- bind_rows(cluster.df,temp.df)
    # print("df")
    # print(cluster.df)
    # print("cluster list")
    # print(cluster.list)
    # print("symptom list")
    # print(symptom.list)
    # print("symptom list na")
    # print(symptom.list.na)
    question_object <- return_question_object(cluster.symptom.new)
    if(is.na(question_object) || is.null(question_object)) list(ID = cluster_id,is.next = 1,question_object)
    else list(ID = cluster_id,is.next = 0,question_object)
    
  }
}

#' final step
#' @get /finalcall
#' @param question_id 
#' @param symptoms list of all symptoms present for final disease
final_predict <- function(question_id,symptoms = "") {
  
  print("debug : in final_predict")
  question_id <- as.numeric(question_id)
  
  symptoms <- strsplit(symptoms,split = ",")
  symptoms <- as.vector(symptoms)
  
  #update symptoms and symptoms.na
  if(symptoms != "") {
    symptom.list[[session_id]] <<- c(symptom.list[[session_id]],symptoms[[1]])
  }
  
  symptom.list.na[[session_id]] <<- c(symptom.list.na[[session_id]],setdiff(unlist(global.cluster.symptom),
                                                                            unlist(symptom.list[[session_id]])))
  
  
  #create new test data and add colnames
  testdata <- matrix(sample(0,1 * ncol(dtm),replace = TRUE),nrow = 1,ncol = ncol(dtm),byrow = TRUE)
  colnames(testdata) <- colnames(matrix.symptoms)
  
  #update test data
  for(i in 1:length(symptom.list[[session_id]])) {
    if(is.na(testdata[1,match(symptom.list[[session_id]][i],colnames(testdata))])) {
      stop( paste("please provide proper UMCL code; error in",
                  symtoms.list[i],sep = " : "))
      
    }
    else {
      testdata[1,match(symptom.list[[session_id]][i],colnames(testdata))] <- 1
    }
  }
  
  #set testdata
  data <- testdata
  #check probablity of data
  prob.data <- predict(mnb, newdata = data, type = "prob")
  #check if maximum probable disease meet the threshold value
  if(max(prob.data) < threshold) {
    question_object <- main_predict(symptoms = toString(symptom.list[[session_id]]),
                                    param.symptom.list.na = symptom.list.na)
    print(question_object)
    if(is.na(question_object[[2]]) || is.null(question_object[[2]])) {
      return(list(complete = 1,
      cause = "couldn't find the disease sorry",
      symptoms_entered = symptom.list[[session_id]]))
    }else {
      return(list(complete = 0,
                  question_object))
    }
    
  }else {
    names <- colnames(prob.data)
    #test
    
    p1 <- round(prob.data,1)
    data <- vector("list")
    j <- 1
    val <- which(p1 %in% max(p1))
    name <- colnames(p1)
    for (i in val) {
      data[[j]] <- data_frame(disease_name = dictionary$name[match(name[i],dictionary$code)],
                              probablity = prob.data[i] * 100)
      cat(file = logfile,Sys.time(),"\nsymptoms entered",symptom.list[[session_id]],"predicted disease",
          toString(dictionary$name[match(name[i],dictionary$code)]),
          "with probablity",prob.data[i],append = TRUE)
      j <- j + 1
    }
    data.df <- bind_rows(data)
    #test
    return(list(complete = 1,data = data.df,
                max_prob_disease = dictionary$name[match(names[which(prob.data %in% max(prob.data))],dictionary$code)],
                max_prob = max(prob.data)))
  }
}

#' get symptoms description
#' @get /getSymptomDescription
#' @param id symptom_id
getSymptomDescription <- function(id) {
  row_num <- match(id,symptom.discription.file$code)
  return(list(name = toString(symptom.discription.file$name[row_num]),
              user_friendly_name = toString(symptom.discription.file$X.user_friendly_name..[row_num]),
              description = toString(symptom.discription.file$description.[row_num]),
              image_url = toString(symptom.discription.file$image_url.[row_num])))
  #toString(symptom.discription.file$X.user_friendly_name[392])
  
}

#' get symptoms which require test for given disease
#' @get /getTestData
#' @param disease_id disease_id
getTestData <- function(disease_id) {
  symptoms <- train_file$symptom[match(disease_id,unlist(train_file$disease))]
  symptoms.split <- strsplit(toString(symptoms)," ")
  test.names <- list()
  count <- 1
  index <- which(symptoms.split[[1]] %in% test.positive.symptoms)
  for(t in index) {
    test.names[count] <- toString(symptoms.split[[1]][t])
    count <- count + 1
  }
  if(length(test.names) == 0) return(NA)
  else return(test.names)
}
#end of api

#supporting functions

#clustering fuction
cluster.local <- function(prob.data,roundval) {
  #round of the prob.data to given decimal place
  prob.data <- round(prob.data,roundval)
  prob.data.unique <- sort(prob.data,decreasing = TRUE) %>% unique(.)
  
  
  if(num_cluster > length(prob.data.unique)) num_cluster <- length(prob.data.unique)
  
  #create maximum of num_cluster of clusters
  cluster <- list()
  for(i in 1:num_cluster) {
    #print(paste("creating cluster",i,sep = " : "))
    temp <- which(prob.data %in% prob.data.unique[i])
    cluster.disease <- vector()
    j <- 1
    names <- colnames(prob.data)
    for(k in temp) {
      cluster.disease[j] <- as.vector(names[k])
      j <- j + 1
    }
    cluster[[i]] <-  cluster.disease
  }
  return(cluster)
}


#create symptoms for cluster 
create_fuction <- function(cluster) {
  
  cluster.symptom <- vector("list")
  cluster.symptom.count <- 1
  
  for(cluster.len in 1:length(cluster)) {
    #print(paste("going with cluster",cluster.len,sep = " : "))
    j <- 1
    cluster_1.symptom <- vector()
    cluster_1.disease <- cluster[[cluster.len]]
    
    
    #make list of symptom for each disease
    for(i in cluster_1.disease) {
      cluster_1.symptom[j] <- as.vector(train_file$symptom[match(i,
                                                                 train_file$disease)])
      j <- j + 1
    }
    
    #filter disease with empty symptom
    na.symptom <- which(cluster_1.symptom %in% "") %>% as.vector()
    if(length(na.symptom) != 0) {
      cluster_1.disease <- cluster_1.disease[-na.symptom]
      cluster_1.symptom <- cluster_1.symptom[-na.symptom]
    }
    
    symptom <- cluster_1.symptom
    
    #dtm
    val <- Corpus(VectorSource(symptom))
    dtm <- DocumentTermMatrix(val)
    top.frequency <- sort(termFreq(symptom),decreasing = TRUE) %>% unique()
    top.frequency.index<- 1
    
    #fetch most frequent symptoms which is present in all disease
    k <- 1
    symptom.quetion <- list()
    disease.temp <- vector()
    disease.temp <- cluster_1.disease
    symptom.temp <- cluster_1.symptom
    
    while(length(disease.temp) != 0) {
      symptom.by.frequency <- findFreqTerms(dtm,top.frequency[top.frequency.index]) %>% 
        strsplit(.," ") %>% toupper(.)
      check <- FALSE
      disease.name <- disease.temp[1]
      symptom.name <- strsplit(symptom.temp[1],split = " ") %>% unlist(.)
      for(i in 1:length(symptom.by.frequency)) {
        #check if highest frequency term occur in the disease.name symptom
        if(symptom.by.frequency[i] %in% symptom.name) {
          #check if the selected symptom all ready present in the cluster.symptom
          if(is.na(match(symptom.by.frequency[i],unlist(cluster.symptom)))) {
            #check if the symptom already present in symptom.list
            if(is.na(match(symptom.by.frequency[i],unlist(symptom.list[[session_id]])))) {
              #check if the symptom already present in symptom.list.na
              if(is.na(match(symptom.by.frequency[i],unlist(symptom.list.na[[session_id]])))) {
                check <- TRUE
                disease.temp <- disease.temp[-1]
                symptom.temp <- symptom.temp[-1]
                symptom.quetion[k] <- symptom.by.frequency[i]
                k <- k + 1
                top.frequency.index <- 1
                break
              }
            }
          }
        }
      }
      
      if(!check) {
        top.frequency.index <- top.frequency.index + 1
      }
      if(top.frequency.index > length(top.frequency)) {
        break
      }
      
    }
    if(length(symptom.quetion) > 0) {
      cluster.symptom[[cluster.symptom.count]] <- unique(symptom.quetion)
      cluster.symptom.count <- cluster.symptom.count + 1
    }
    
  }  
  return(cluster.symptom)
}

#return readable question data frame which contain symptom name and code and answer type
return_question_object <- function(cluster.symptom) {
  if(length(cluster.symptom) == 0) {
    return(NA)
  }else if(length(cluster.symptom) == 1 && cluster.symptom == "") {
    return(NA)
  }else {
    k <- 1
    symptom.name <- vector("list")
    for(i in 1:length(cluster.symptom)) {
      cluster = cluster.symptom[i]
      
      for(j in 1:length(cluster[[1]])) {
        if(cluster[[1]][j] %in% symptom.discription$code) {
          index <- match(cluster[[1]][j],symptom.discription$code)
          symptom.name[[k]] <- data_frame(question = paste(symptom.discription$name[index],"(",symptom.discription$discription[index],")",sep = ""),
                                          question_code = unlist(cluster[[1]][j]))
        }else{
          symptom.name[[k]] <- data_frame(question = dictionary$name[which(dictionary$code %in% cluster[[1]][j])],
                                          question_code = unlist(cluster[[1]][j]))
        }
        
        k <- k + 1
      }
    }
    return(unique(bind_rows(symptom.name)))
  }
  
}

#rank cluster 
#cluster.symptom is a list of cluster where answer.symptom should be vector
rank_cluster <- function(cluster.symptom,answer.symptom,answer.symptom.na,na = FALSE) {
  #answer.symptom <- c("D000006","D001416","D004417")
  length.cluster <- list()
  for(i in 1:length(cluster.symptom)) {
    match <- which(unlist(cluster.symptom[[i]]) %in% answer.symptom) %>% length(.)
    length.cluster[i] <- match
  }
  #negetive marking
  if(na) {
    for(i in 1:length(cluster.symptom)) {
      match.na <- which(unlist(cluster.symptom[[i]]) %in% answer.symptom.na) %>% length(.)
      length.cluster[i] <- (as.numeric(length.cluster[i]) - as.numeric(match.na))
    }
  }
  return(length.cluster)
}


#create prob.data for given cluster
prob_data <- function(rank.cluster,selected.symptoms) {
  cluster.symptom <- vector()
  cluster.disease <- unlist(rank.cluster)
  j <- 1
  
  for(i in cluster.disease) {
    cluster.symptom[j] <- as.vector(train_file$symptom[match(i,
                                                             train_file$disease)])
    j <- j + 1
  }
  
  cluster.symptom <- as.vector(cluster.symptom) %>% gsub(","," ",.)
  symptom.corpus <- Corpus(VectorSource(cluster.symptom))
  
  dtm.sub <- DocumentTermMatrix(symptom.corpus)
  matrix.symptoms.sub <-matrix()
  matrix.symptoms.sub <- as.matrix(dtm.sub)
  
  colnames(matrix.symptoms.sub) <- toupper(colnames(matrix.symptoms.sub))
  
  disease.sub <- as.vector(cluster.disease)
  
  mnb.sub <- multinomial_naive_bayes(x = matrix.symptoms.sub, y = disease.sub, laplace = laplace)
  summary(mnb.sub)
  
  testdata <- matrix(sample(0,1 * ncol(dtm.sub),replace = TRUE),nrow = 1,ncol = ncol(dtm.sub),byrow = TRUE)
  
  #length( colnames(matrix.symptoms))
  colnames(testdata) <- colnames(matrix.symptoms.sub)
  
  #values <- c("D004244","D012585 ")
  for(i in 1:length(selected.symptoms)) {
    testdata[1,match(selected.symptoms[i],colnames(testdata))] <- 1
  }
  match(1,testdata)
  data <- testdata
  
  prob.data <- predict(mnb.sub, newdata = data, type = "prob")
  return(prob.data)
}

#fetch highest probality disease code 
high_prob <- function(symptoms) {
  testdata <- matrix(sample(0,1 * ncol(dtm),replace = TRUE),nrow = 1,ncol = ncol(dtm),byrow = TRUE)
  colnames(testdata) <- colnames(matrix.symptoms)
  for(i in 1:length(symptoms)) {
    testdata[1,match(symptoms[i],colnames(testdata))] <- 1
  }
  
  data <- testdata
  prob.data <- predict(mnb, newdata = data, type = "prob")
  names <- colnames(prob.data)
  max_prob_disease = dictionary$code[match(names[which(prob.data %in% max(prob.data))],dictionary$code)]
  return(unlist(max_prob_disease))
}

#fetch all the cluster id's associated with given id
get_cluster_df_id <- function(id) {
  result <- list()
  result[1] <- id
  i <- 2
  val <- id
  while(!is.na(val)){
    val <- cluster.df$ref_id[match(val,cluster.df$id)]
    result[i] <- val
    i <- i + 1
  }
  result <- result[-length(result)]
  return(result)
}

#check if the cluster has given disease
find_cluster <- function(disease_id,cluster_id) {
  cluster_ids <- get_cluster_df_id(cluster_id)
  for(i in cluster_ids) {
    cluster <- cluster.list[[i]]
    for(j in 1:length(cluster)) {
      if(!is.na((match(disease_id,cluster[[j]])))) {
        return(cluster[[j]])
      }
    }
  }
  return(disease_id)
}
