
library(caret)
library(e1071)

# load("C:/Users/Alex H/Dropbox (Nova SA)/Archive_Alex/Eskom Offset Study 2014 - Team/R/EOP/Rdas/df_ibutton_data_classified.Rda")

# load()

df_ibutton_data_classified <- df_ibutton_data_classified[which(df_ibutton_data_classified$stand_number != "2142"),]
df_ibutton_data_classified <- df_ibutton_data_classified[which(df_ibutton_data_classified$stand_number != "3185"), ]

df_ibutton_data_classified <- df_ibutton_data_classified[complete.cases(df_ibutton_data_classified),] 


mindate <- min(df_ibutton_data_classified[which(df_ibutton_data_classified$date > as.POSIXct("2014-12-31 23:59:59")), "date"])
maxdate <- max(df_ibutton_data_classified$date)

splitdate <- as.POSIXct("2015-07-01 00:00:00", tz = "Africa/Johannesburg")

trainidx <- which(df_ibutton_data_classified$date < splitdate & df_ibutton_data_classified$date > as.POSIXct("2014-12-31 23:59:59"))
testidx <- which(df_ibutton_data_classified$date >= splitdate)
                       
trainset <- df_ibutton_data_classified[trainidx, c("C", "W", "sdif", "vuur")]
rownames(trainset) <- paste(df_ibutton_data_classified$date[trainidx], df_ibutton_data_classified$stand_number[trainidx], sep = "_")

testset <- df_ibutton_data_classified[testidx, c("W", "C", "sdif", "vuur")]
rownames(testset) <- paste(df_ibutton_data_classified$date[testidx], df_ibutton_data_classified$stand_number[testidx], sep = "_")

# X <- trainset[,c("C", "W", "sdif")]
# Y <- as.factor(trainset[,"vuur"])

fit <- svm(factor(vuur) ~ C + W + sdif, data = trainset)
ant <- testset[ ,"vuur", drop = FALSE]

pred <- predict(fit, newdata = testset[, 1:3])

matches <- match(x = names(pred), table = rownames(ant))
pred <- pred[matches]
df <- data.frame(pred, ant)

merk<- df$pred == df$vuur


# createDataPartition

stands <- unique(as.character(df_ibutton_data_classified$stand_number))
training_stands <- sample(x = stands, size = 40)
trainset <- df_ibutton_data_classified[which(as.character(df_ibutton_data_classified$stand_number) %in% training_stands), c("C", "W", "sdif", "vuur")]
trainset <- trainset[complete.cases(trainset),]
testset <- df_ibutton_data_classified[which(!(as.character(df_ibutton_data_classified$stand_number) %in% training_stands)), c("C", "W", "sdif")]
testset <- testset[complete.cases(testset),]


teststand <- df_ibutton_data_classified[which(as.character(df_ibutton_data_classified$stand_number) == "2009"), c("C", "W", "sdif", "vuur")]
trainidxx <- createDataPartition(y = teststand$vuur, p = .70, list = FALSE)

trainset <- teststand[trainidxx,]
testset <- teststand[- trainidxx,]


# ctrl <- trainControl(method = "boot", 
#                      number = 4, 
#                      classProbs = TRUE, 
#                      summaryFunction = twoClassSummary)

trainres <- train(vuur ~ ., 
                  data = trainset,
                  method = "pls") #, 
                 # trControl = ctrl,
                 # metric = "ROC")
