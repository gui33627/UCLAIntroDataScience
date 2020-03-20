# Final Project
# Name: Junhui Yang

library(dplyr)
library(plotrix)
library(ggplot2)
library(randomForest)
library(rpart)
library(rpart.plot)

setwd("C:/Users/gui33/Desktop/Intrduction to Data Science/finalproject")
df <- read.csv("LS_2.0.csv")
colnames(df)
# [1] "STATE"                                    "CONSTITUENCY"                            
# [3] "NAME"                                     "WINNER"                                  
# [5] "PARTY"                                    "SYMBOL"                                  
# [7] "GENDER"                                   "CRIMINAL.CASES"                          
# [9] "AGE"                                      "CATEGORY"                                
# [11] "EDUCATION"                                "ASSETS"                                  
# [13] "LIABILITIES"                              "GENERAL.VOTES"                           
# [15] "POSTAL.VOTES"                             "TOTAL.VOTES"                             
# [17] "OVER.TOTAL.ELECTORS..IN.CONSTITUENCY"     "OVER.TOTAL.VOTES.POLLED..IN.CONSTITUENCY"
# [19] "TOTAL.ELECTORS"                          
summary(df)

# remove missing values
df1 <- df[which(df$NAME != "NOTA"),]
df1$PARTY <- as.factor(as.character(df1$PARTY))
summary(df1)
#            STATE          CONSTITUENCY                 NAME          WINNER           PARTY                         SYMBOL   
# Uttar Pradesh :251   AURANGABAD :  13   Ajay Kumar       :   2   Min.   :0.0000   BJP    :420   Lotus                  :420  
# Bihar         :218   GAYA (SC)  :  11   ATUL KUMAR SINGH :   2   1st Qu.:0.0000   INC    :413   Hand                   :413  
# Tamil Nadu    :189   CHATRA     :   8   Rahul Gandhi     :   2   Median :0.0000   IND    :201   Elephant               :166  
# Maharashtra   :175   MAHARAJGANJ:   8   SANJAY KUMAR     :   2   Mean   :0.2671   BSP    :163   Bicycle                : 65  
# West Bengal   :173   SHEOHAR    :   8   SURENDRA RAM     :   2   3rd Qu.:1.0000   CPI(M) :100   Hammer, Sickle and Star: 63  
# Andhra Pradesh:101   SUPAUL     :   8   A N RADHAKRISHNAN:   1   Max.   :1.0000   AITC   : 47   Cup & Saucer           : 52  
# (Other)       :911   (Other)    :1962   (Other)          :2007                    (Other):674   (Other)                :839  
# GENDER     CRIMINAL.CASES      AGE           CATEGORY                    EDUCATION                                ASSETS    
# :   0   0      :1242   Min.   :25.00          :   0   Post Graduate        :502   Not Available                   :  22  
# FEMALE: 258   1      : 313   1st Qu.:43.25   GENERAL:1392   Graduate             :441   Nil                             :   3  
# MALE  :1760   2      : 119   Median :52.00   SC     : 383   Graduate Professional:336   Rs 1,75,000\n ~ 1 Lacs+         :   2  
# 3      : 104   Mean   :52.27   ST     : 243   12th Pass            :256   Rs 1,93,54,59,756\n ~ 193 Crore+:   2  
# 4      :  64   3rd Qu.:61.00                  10th Pass            :196   Rs 11,95,43,561\n ~ 11 Crore+   :   2  
# 5      :  42   Max.   :86.00                  8th Pass             : 78   Rs 15,88,77,063\n ~ 15 Crore+   :   2  
# (Other): 134                                  (Other)              :209   (Other)                         :1985  
# LIABILITIES    GENERAL.VOTES      POSTAL.VOTES    TOTAL.VOTES      OVER.TOTAL.ELECTORS..IN.CONSTITUENCY
# Rs 0\n ~               : 634   Min.   :   1339   Min.   :    0   Min.   :   1342   Min.   : 0.09794                    
# Not Available          :  22   1st Qu.:  30476   1st Qu.:   97   1st Qu.:  30744   1st Qu.: 1.95362                    
# Rs 5,00,000\n ~ 5 Lacs+:  10   Median : 284630   Median :  463   Median : 285525   Median :18.03686                    
# Rs 1,00,000\n ~ 1 Lacs+:   8   Mean   : 291190   Mean   : 1105   Mean   : 292295   Mean   :17.59681                    
# Rs 50,000\n ~ 50 Thou+ :   8   3rd Qu.: 505862   3rd Qu.: 1546   3rd Qu.: 507618   3rd Qu.:30.70811                    
# Rs 3,00,000\n ~ 3 Lacs+:   7   Max.   :1066824   Max.   :19367   Max.   :1068569   Max.   :51.95101                    
# (Other)                :1329                                                                                           
# OVER.TOTAL.VOTES.POLLED..IN.CONSTITUENCY TOTAL.ELECTORS   
# Min.   : 1.000                           Min.   :  55189  
# 1st Qu.: 2.871                           1st Qu.:1530404  
# Median :27.750                           Median :1679891  
# Mean   :25.808                           Mean   :1660261  
# 3rd Qu.:44.350                           3rd Qu.:1823404  
# Max.   :74.412                           Max.   :3150313  
# EDA


unique(df$STATE)
#  [1] Telangana                 Uttar Pradesh             Maharashtra               Gujarat                  
#  [5] Rajasthan                 Kerala                    West Bengal               Uttarakhand              
#  [9] Andhra Pradesh            Haryana                   Punjab                    Jammu & Kashmir          
# [13] Andaman & Nicobar Islands Tamil Nadu                Bihar                     Arunachal Pradesh        
# [17] Odisha                    Assam                     Karnataka                 Madhya Pradesh           
# [21] Chhattisgarh              Chandigarh                NCT OF Delhi              Jharkhand                
# [25] Dadra & Nagar Haveli      Daman & Diu               Himachal Pradesh          Manipur                  
# [29] Lakshadweep               Mizoram                   Nagaland                  Goa                      
# [33] Puducherry                Meghalaya                 Sikkim                    Tripura                  
# 36 Levels: Andaman & Nicobar Islands Andhra Pradesh Arunachal Pradesh Assam Bihar Chandigarh ... West Bengal

### visulization ###

df_win <- df1[which(df1$WINNER==1),]
df_lose <- df1[which(df1$WINNER == 0),]

# 1. Party
par(las=2)
party_impact <- rbind(table(df_win$PARTY), table(df1$PARTY) - table(df_win$PARTY))
bar <- barplot(party_impact,main = "Candidates by Party", col = c('red', 'yellow'), 
               ylim = c(0,max(table(df1$PARTY))+10),cex.names =0.5, las=2, 
               xlab = "Party", ylab = "Number of Candidates")
text(bar,table(df1$PARTY)+5,labels=as.character(table(df1$PARTY)),cex = 0.5)
legend("topright",legend = c("win","lose"),lty = c(1,1),lwd = 2,col = c('red',"yellow"),cex = 0.9)
win_rate <- table(df_win$PARTY)/table(df1$PARTY)
plot(sort(win_rate), cex.axis = 0.6, xlab = "Party",ylab = "Winning Rate")
abline(h = 0.75)
abline(h = 0.25)
high_party <- names(win_rate)[win_rate > 0.75]
med_party <- names(win_rate)[win_rate > 0.25 & win_rate <= 0.75]
low_party <- names(win_rate)[win_rate <= 0.25]
df1$party_wintype <- rep('low', nrow(df1))
df1$party_wintype[df1$PARTY %in% high_party] <- 'high'
df1$party_wintype[df1$PARTY %in% med_party] <- 'med'
table(df1$party_wintype)

# 2. constitency
uni_state <- unique(df1$STATE)
num_cont <- sapply(uni_state, function(x){
  idx_temp <- which(df1$STATE == x)
  length(unique(df1$CONSTITUENCY[idx_temp]))
})
names(num_cont) <- uni_state
cols <- heat.colors(length(levels(as.factor(num_cont))), rev = TRUE)
cols_idx <- sapply(as.factor(num_cont), function(x){
  as.numeric(x)
})
par(mai=c(1.62,0.82,0.82,0.42), mgp=c(5,1,0))
bar1 <- barplot(num_cont,cex.names = 0.6,las=2, col=cols[cols_idx], ylim = c(0,max(num_cont)+2),
                xlab = "State",
                main = "Number of Constituencies in Each State") 
text(bar1,num_cont+1,labels=as.character(num_cont),cex = 0.5)
mtext("Number of Constituencies", side=2, line=3)

# 3. education
par(mai=c(1.02,0.82,0.82,0.42), mgp=c(3,1,0)) # default
par(las=1)
table(df1$EDUCATION)
#                           10th Pass             12th Pass              5th Pass 
#             0                   196                   256                    28 
#      8th Pass             Doctorate              Graduate Graduate Professional 
#            78                    73                   441                   336 
#    Illiterate              Literate         Not Available                Others 
#             5                    30                    22                    50 
# Post Graduate       Post Graduate\n 
#           502                     1 
df1$EDUCATION <- gsub("\n","",df1$EDUCATION)
df_win$EDUCATION <- gsub("\n","",df_win$EDUCATION)
df_lose$EDUCATION <- gsub("\n","",df_lose$EDUCATION)

edu <- as.data.frame(table(df1$EDUCATION), stringsAsFactors = FALSE)
edu_win <- as.data.frame(table(df_win$EDUCATION), stringsAsFactors = FALSE)
edu_lose <- as.data.frame(table(df_lose$EDUCATION), stringsAsFactors = FALSE)
edu <- left_join(edu,edu_win, by = 'Var1')
edu <- left_join(edu,edu_lose, by = 'Var1')
edu[is.na(edu[,3]),3] <- 0
edu$degper <- edu$Freq.y/edu$Freq.x

edu_prop <- as.numeric(t(as.matrix(edu[,5])))
barp(edu_prop, names.arg = edu$Var1,
     col = c('orange', 'orange', 'purple', 'purple', 'blue', 'blue',
             'blue', 'purple', 'purple', 'purple', 'pink', 'blue'),
     xlab = "Education Levels", ylab = "Winning Rate in Each Level",
     main = "Winning Rate and Distribution of Education Levels")
points(edu$Freq.y/sum(edu$Freq.y), col = 'red', pch = 19, cex = 2)
lines(edu$Freq.y/sum(edu$Freq.y), col = 'red', lwd = 4)
points(edu$Freq/sum(edu$Freq), col = 'green', pch = 19, cex = 2)
lines(edu$Freq/sum(edu$Freq), col = 'green', lwd = 4)
legend("topleft",legend = c("win","lose"),lty = c(1,1),lwd = 2,col = c('red','green'),cex = 0.9)

Adv_edu <- edu$Var1[c(5,6,7,12)]
High_edu <- edu$Var1[c(1,2)]
Low_edu <- edu$Var1[c(3,4,8,9,10)]
Others_edu <- edu$Var1[11]
df1$edu_level <- rep('Adv_edu', nrow(df1))
df1$edu_level[df1$EDUCATION %in% High_edu] <- 'High_edu'
df1$edu_level[df1$EDUCATION %in% Low_edu] <- 'Low_edu'
df1$edu_level[df1$EDUCATION %in% Others_edu] <- 'Others_edu'
table(df1$edu_level)

# 4.symbols
sym <- as.data.frame(table(as.character(df1$SYMBOL)), stringsAsFactors = FALSE)
sym_lose <- as.data.frame(table(as.character(df_lose$SYMBOL)), stringsAsFactors = FALSE)
sym_win <- as.data.frame(table(as.character(df_win$SYMBOL)), stringsAsFactors = FALSE)
sym <- left_join(sym,sym_lose, by = "Var1")
sym <- left_join(sym,sym_win, by = "Var1")
colnames(sym) <- c("Var1","total","lose","win")
sym$lose[is.na(sym$lose)] <- 0
sym$win[is.na(sym$win)] <- 0
idx <- sort(sym$win,index.return = TRUE)
sym <- sym[idx$ix,]

sym$prop <- sym$win/sym$total
sym_prop <- t(as.matrix(sym[,5]))
colnames(sym_prop) <- sym$Var1
par(mai=c(2.42,0.82,0.82,0.42), mgp=c(9,1,0))
par(las=2)
barp(sym_prop,names.arg = colnames(sym_prop), col = "grey70", cex.axis = 0.7, 
     xlab = "Symbols", main = "WInning Rate and Distribution of Election Symbols")
points(sym$win/sum(sym$win), col = 'red', pch = 19)
lines(sym$win/sum(sym$win), col = 'red', lwd = 2)
points(sym$lose/sum(sym$lose), col = 'green', pch = 19)
lines(sym$lose/sum(sym$lose), col = 'green', lwd = 2)
legend("topleft",legend = c("win","lose"),lty = c(1,1),lwd = 2,col = c('red','green'),cex = 0.9)

# 5.Age
par(mai=c(1.02,0.82,0.82,0.42), mgp=c(3,1,0)) # default
par(las = 1)
par(mfrow = c(1,1))

age <- as.data.frame(table(df_win$AGE))
age_lose <- as.data.frame(table(df_lose$AGE))
plot(table(df_lose$AGE), type = "l",xlab = "Age", ylab = "Frequency", main = "Age Distribution of Candidates Winning and Losing")
lines(as.numeric(names(table(df_win$AGE))), as.numeric(table(df_win$AGE)), col='red', pch=19)
legend("topright",legend = c("win","lose"),lty = c(1,1),lwd = 2,col = c('red','black'),cex = 0.9)

par(mfrow = c(1,2))
plot(df1$AGE, df1$WINNER, xlab = "Age", ylab = "Winning Rate")
boxplot(df1$AGE ~ as.factor(df1$WINNER),
        col=c("blue","orange"),names=c("lose","win"),xlab = "", ylab = "Age",varwidth=TRUE)

# 6.Category
cat <- as.data.frame(table(as.character(df1$CATEGORY)), stringsAsFactors = FALSE)
cat_win <- as.data.frame(table(as.character(df_win$CATEGORY)), stringsAsFactors = FALSE)
cat_lose <- as.data.frame(table(as.character(df_lose$CATEGORY)), stringsAsFactors = FALSE)
cat <- left_join(cat,cat_win,by = "Var1")
cat <- left_join(cat,cat_lose,by = "Var1")
colnames(cat) <- c("Var1","total","win","lose")

cat$winper <- round(cat$win/sum(cat$win),3)
cat$loseper <- round(cat$lose/sum(cat$lose),3)
cat$prop <- round(cat$win/cat$total,3)
par(las=1)
par(mfrow = c(1,1))
bar2 <- barp(t(as.matrix(cat[,5:6])),names.arg=cat$Var1, col = c('red','green'),xlab = "Category", 
             ylab = "Proportion", main = "Winning and Losing Candiadtes by Categories")
points(bar2$x[1,]+0.2,t(as.matrix(cat[,7])),col = 'blue',pch = 19, cex = 2)
lines(bar2$x[1,]+0.2,t(as.matrix(cat[,7])),col = 'blue', lwd = 4)
text(bar2$x[1,]+0.2,t(as.matrix(cat[,7]))+0.07,labels=as.character(cat$prop),cex = 1)
legend("topright",legend = c("win","lose"),lty = c(1,1),lwd = 2,col = c('red','green'),cex = 1)

# 7. gender
gen <- as.data.frame(table(as.character(df1$GENDER)), stringsAsFactors = FALSE)
gen_win <- as.data.frame(table(as.character(df_win$GENDER)), stringsAsFactors = FALSE)
gen <-left_join(gen, gen_win, by = "Var1")
colnames(gen) <- c("Var1","total","win")

bar3 <- barplot(t(as.matrix(gen[,2:3])), names.arg = gen$Var1, beside = T, col = c("blue","red"),
                ylab = "Number",main = "Total and Winning Candidates by Gender")
text(bar3[2,], gen[,3]+50,labels = as.character(round((gen$win/gen$total),2)),cex=0.7)
legend("topleft",legend = c("total", "win"),lty = c(1,1),lwd = 2,col = c('blue','red'),cex = 0.9 )

# 8. Criminal Cases
table(df1$CRIMINAL.CASES)
df1$crim_class <- ifelse(df1$CRIMINAL.CASES == 0, "Good","Bad")
cri <- as.data.frame(cbind(table(df1$crim_class), table(df1[which(df1$WINNER == 1),]$crim_class)))
gwinrate <- length(which(df1$crim_class == 'Good' & df1$WINNER == 1))/length(which(df1$crim_class == 'Good'))
bwinrate <- length(which(df1$crim_class == 'Bad' & df1$WINNER == 1))/length(which(df1$crim_class == 'Bad'))

bar4 <- barplot(t(as.matrix(cri)), beside = T, col = c("blue","red"),
                ylab = "Number of candidates",main = "Criminal Status of Candidates")
legend("topleft",legend = c("total", "win"),lty = c(1,1),lwd = 2,col = c('blue','red'),cex = 0.9 )
text(bar4[2,], cri[,2]+50,labels = c(as.character(round(bwinrate,2)),as.character(round(gwinrate,2))),cex=1)

uni_party <- unique(as.character(df1$PARTY))
num_cases <- sapply(uni_party, function(x){
  idx_temp <- which(as.character(df1$PARTY) == x)
  sum(as.numeric(df1$CRIMINAL.CASES[idx_temp]))
})
names(num_cases) <- uni_party
cols <- heat.colors(length(levels(as.factor(num_cases))), rev = TRUE)
cols_idx <- sapply(as.factor(num_cases), function(x){
  as.numeric(x)
})
par(mai=c(1.62,0.82,0.82,0.42), mgp=c(5,1,0))
barplot(num_cases,cex.names = 0.6,las=2, col=cols[cols_idx], ylim = c(0,max(num_cases)+2),
        xlab = "Party", ylab = "Number of Criminal Cases",
        main = "Criminal Cases by Party") 

# Decision Tree
idx_win <- which(df1$WINNER == 1)
idx_lose <- which(df1$WINNER == 0)
train_rate <- 0.6
set.seed(1)
train_idx <- c(sample(idx_win, size = floor(length(idx_win)*train_rate), replace = FALSE),
               sample(idx_lose, size = floor(length(idx_lose)*train_rate), replace = FALSE))
df1_train <- df1[train_idx,]
df1_test <- df1[-train_idx,]

df1_fac <- df1_train %>% mutate_if(is.character, as.factor)
df1_fac$WINNER <- as.factor(df1_fac$WINNER)
df1_fac <- df1_fac[,c(4,10,20,21,22)]
df1_fact <- df1_test %>% mutate_if(is.character, as.factor)
df1_fact$WINNER <- as.factor(df1_fact$WINNER)
df1_fact <- df1_fact[,c(4,10,20,21,22)]

fit <- rpart(WINNER~., data = df1_fac, method = 'class')
rpart.plot(fit, extra = 106)
predict_unseen <-predict(fit, df1_fact, type = 'class')
table_mat <- table(df1_fact$WINNER, predict_unseen)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test
# [1] 0.8539604

# Random Forest
set.seed(37)
rf <- randomForest(WINNER ~ . , 
                   data=df1_fac, ntree=500, 
                   mtry=2,importance=TRUE)

prediction <- predict(rf, newdata=df1_fact, type="class")

table(prediction, df1_fact$WINNER)
# calculate misclassification error rate
misclassification_error_rate <- sum(df1_fact$WINNER != prediction) / 
  nrow(df1_fact)*100
misclassification_error_rate 
# [1] 14.72772

importance(rf)
print(rf)
# Call:
#  randomForest(formula = WINNER ~ ., data = df1_fac, ntree = 500,      mtry = 2, importance = TRUE) 
#                Type of random forest: classification
#                      Number of trees: 500
# No. of variables tried at each split: 2
# 
#         OOB estimate of  error rate: 12.73%
# Confusion matrix:
#     0   1 class.error
# 0 785 102   0.1149944
# 1  52 271   0.1609907

varImpPlot(rf)
varUsed(rf, by.tree=FALSE, count=TRUE)
# [1] 3324 2296 4013 2151

# logistic regression
glm1 <- glm(WINNER ~ crim_class + edu_level + party_wintype + GENDER + CATEGORY, 
            data = df1_train, 
            family = binomial)
summary(glm1)

prob <- predict(glm1, newdata = df1_test, type = "response")

calc_ROC <- function(probabilities, known_truth, model.name=NULL)
{
  outcome <- as.numeric(factor(known_truth))-1
  pos <- sum(outcome) # total known positives
  neg <- sum(1-outcome) # total known negatives
  pos_probs <- outcome*probabilities # probabilities for known positives
  neg_probs <- (1-outcome)*probabilities # probabilities for known negatives
  true_pos <- sapply(probabilities,
                     function(x) sum(pos_probs>=x)/pos) # true pos. rate
  false_pos <- sapply(probabilities,
                      function(x) sum(neg_probs>=x)/neg)
  if (is.null(model.name))
    result <- data.frame(true_pos, false_pos)
  else
    result <- data.frame(true_pos, false_pos, model.name)
  result %>% arrange(false_pos, true_pos)
}

ROC1 <- calc_ROC(probabilities=glm1$fitted.values, 
                 known_truth=df1_train$WINNER ,      
                 model.name="train") 

ROC2 <- calc_ROC(probabilities=prob, 
                 known_truth=df1_test$WINNER ,      
                 model.name="test")

ggplot(data=NULL, aes(x=false_pos, y=true_pos)) +
  geom_line(data=ROC1, aes(color=model.name)) +
  geom_line(data=ROC2, aes(color=model.name))

# Combine all ROCs into one big table
ROCs <- rbind(ROC1, ROC2)

# Calculate AUCs
ROCs %>% group_by(model.name) %>% 
  mutate(delta=false_pos-lag(false_pos)) %>%
  summarize(AUC=sum(delta*true_pos, na.rm=T)) %>%
  arrange(desc(AUC))
# # A tibble: 2 x 2
# model.name   AUC
# <fct>      <dbl>
# 1 train      0.905
# 2 test       0.876
