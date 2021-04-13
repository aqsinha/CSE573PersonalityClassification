install.packages("Matrix")
install.packages("irlba")
install.packages("topicmodels")
install.packages("corrplot")
install.packages("ggplot2", "reshape2")
install.packages("reshape2")
install.packages('ROCR')

# Read in files
users <- read.csv(file = "C:/Users/pkondur1/OneDrive - Arizona State University/Desktop/SWM/sample_dataset/users.csv")
likes <- read.csv(file = "C:/Users/pkondur1/OneDrive - Arizona State University/Desktop/SWM/sample_dataset/likes.csv")
users_likes <- read.csv(file = "C:/Users/pkondur1/OneDrive - Arizona State University/Desktop/SWM/sample_dataset/users-likes.csv")

# Match entries in users_likes with users and likes dictionaries
users_likes$user_row<-match(users_likes$userid,users$userid)
users_likes$like_row<-match(users_likes$likeid,likes$likeid)

# Load Matrix library
require(Matrix)

# Construct the sparse User-Like(user footprint) Matrix ufp
ufp <- sparseMatrix(i = users_likes$user_row, j = users_likes$like_row, x = 1)

# Save user IDs as row names in M
rownames(ufp) <- users$userid

# Save Like names as column names in M
colnames(ufp) <- likes$name

# Retain only these rows/columns that meet the threshold
repeat {                                       
  i <- sum(dim(ufp))                             
  ufp <- ufp[rowSums(ufp) >= 50, colSums(ufp) >= 150] 
  if (sum(dim(ufp)) == i) break                  
}

users <- users[match(rownames(ufp), users$userid), ]

#SVD 
library(irlba)

#Perform SVD for 10 terms 
Msvd<-irlba(M,nv=10);

#SVD scores of users 
u<-Msvd$u

#SVD scores of the likes
v<-Msvd$v

plot(Msvd$d)

# Obtain rotated V matrix:
v_rot <- unclass(varimax(Msvd$v)$loadings)

# The cross-product of M and v_rot gives u_rot:
u_rot <- as.matrix(M %*% v_rot)

#LDA
library(topicmodels)

# Perform LDA analysis, for details on setting alpha and delta parameters. 
Mlda <- LDA(M, control = list(alpha = 10, delta = .1, seed=68), k = 10, method = "Gibbs")

# Extract user LDA cluster memberships
gamma <- Mlda@gamma

# Extract Like LDA clusters memberships
# betas are stored as logarithms, convert logs to probabilities
beta <- exp(Mlda@beta) 

# Correlate user traits and their SVD scores
# users[,-1] is used to exclude the column with IDs
cor(u_rot, users[,-1], use = "pairwise")

# Correlate user traits and their LDA scores
cor(gamma, users[,-1], use = "pairwise")

# Plot the correlation matrix
library(corrplot)

library(ggplot2)
library(reshape2)

# Get correlations
x<-round(cor(u_rot, users[,-1], use="p"),2)

# Reshape it in an easy way using ggplot2
y<-melt(x)
colnames(y)<-c("SVD", "Trait", "r")

# Produce the plot
qplot(x=SVD, y=Trait, data=y, fill=r, geom="tile") +
  scale_fill_gradient2(limits=range(x), breaks=c(min(x), 0, max(x)))+
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=14,face="bold"),
        panel.background = element_rect(fill='white', colour='white'))+
  labs(x=expression('SVD'[rot]), y=NULL)

x<-round(cor(gamma, users[,-1], use="p"),2)
y<-melt(x)
colnames(y)<-c("LDA", "Trait", "r")

# Produce the plot
qplot(x=LDA, y=Trait, data=y, fill=r, geom="tile") +
  scale_fill_gradient2(limits=range(x), breaks=c(min(x), 0, max(x)))+
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=14,face="bold"),
        panel.background = element_rect(fill='white', colour='white'))+
  labs(x=expression('LDA'), y=NULL)

# Split users into 10 groups(k-fold)
folds <- sample(1:10, size = nrow(users), replace = T)

# Take users from group 1 and assign them to the TEST subset
test <- folds == 1
library(irlba)

# Extract SVD dimensions from the TRAINING subset
Msvd <- irlba(ufp[!test, ], nv = 50)

# Rotate Like SVD scores (V)
v_rot <- unclass(varimax(Msvd$v)$loadings)

# Rotate user SVD scores *for the entire sample*
u_rot <- as.data.frame(as.matrix(ufp %*% v_rot))

# Build linear regression model for openness
# using TRAINING subset
fit_o <- glm(users$ope~., data = u_rot, subset = !test)

# Do the same for gender
# use family = "binomial" for logistic regression model
fit_g <- glm(users$gender~.,data = u_rot, subset = !test, family = "binomial")

# Compute the predictions for the TEST subset
pred_o <- predict(fit_o, u_rot[test, ])
pred_g <- predict(fit_g, u_rot[test, ], type = "response")

# Correlate predicted and actual values for the TEST subset
r <- cor(users$ope[test], pred_o)

# Compute Area Under the Curve for gender
library(ROCR)
temp <- prediction(pred_g, users$gender[test])
auc <- performance(temp,"auc")@y.values

#Loading 150 SVD dimensions 
Msvd <- irlba(ufp[!test, ], nv = 150)

# Start predictions 
set.seed(seed=68)
n_folds<-10                # set number of folds
kvals<-c(10,30,60,90,120,150)      # set k
vars<-colnames(users)[-1]  # choose variables to predict

folds <- sample(1:n_folds, size = nrow(users), replace = T)

results<-list()
accuracies<-c()

for (k in kvals){
  print(k)
  for (fold in 1:n_folds){ 
    print(paste("Cross-validated predictions, fold:", fold))
    test <- folds == fold
    
    # If you want to use SVD:
    Msvd <- irlba(ufp[!test, ], nv = k)
    v_rot <- unclass(varimax(Msvd$v[, 1:k])$loadings)
    predictors <- as.data.frame(as.matrix(ufp %*% v_rot))
    
    
    for (var in vars){
      print(var)
      results[[var]]<-rep(NA, n = nrow(users))
      # check if the variable is dichotomous
      if (length(unique(na.omit(users[,var]))) ==2) {    
        fit <- glm(users[,var]~., data = predictors, subset = !test, family = "binomial")
        results[[var]][test] <- predict(fit, predictors[test, ], type = "response")
      } else {
        fit<-glm(users[,var]~., data = predictors, subset = !test)
        results[[var]][test] <- predict(fit, predictors[test, ])
      }
      print(paste(" Variable", var, "done."))
    }
  }
  
  compute_accuracy <- function(ground_truth, predicted){
    if (length(unique(na.omit(ground_truth))) ==2) {
      f<-which(!is.na(ground_truth))
      temp <- prediction(predicted[f], ground_truth[f])
      return(performance(temp,"auc")@y.values)
    } else {return(cor(ground_truth, predicted,use = "pairwise"))}
  }
  
  for (var in vars) accuracies <- c(accuracies,compute_accuracy(users[,var][test], results[[var]][test]))
  
}
print(accuracies)

traits <- c('Gender','Age','Political-Factor','Openness','Conscientiousness','Extroversion','Agreeableness','Neuroticism')

k <- c(10,10,10,10,10,10,10,10,30,30,30,30,30,30,30,30,60,60,60,60,60,60,60,60,90,90,90,90,90,90,90,90,120,120,120,120,120,120,120,120,150,150,150,150,150,150,150,150)
names <- c("SVD","SVD","SVD","SVD","SVD","SVD","SVD","SVD","LDA","LDA","LDA","LDA","LDA","LDA","LDA","LDA")

data_val<-data.frame(People_Personality_Traits=traits, accuracies=as.numeric(accuracies),k=k)
# plot SVD vs K
ggplot(data_val,aes(x = People_Personality_Traits, y = accuracies, group = k, color = k)) + 
  geom_line()+
  theme_light() +
  ggtitle("Predition-Accuracy with Change in Dimensions")+
  ylab(label="Reported Accuracies") + 
  xlab("People_Personality_Traits")+       
  geom_point()

data_val<-data.frame(People_Personality_Traits=traits, accuracies=as.numeric(accuracies_1),k=names)
accuracies_1 <- c(0.93,0.60,0.85,0.44,0.21,0.28,0.22,0.30,0.87,0.67,0.80,0.41,0.20,0.25,0.17,0.25)
# plot SVD vs LDA
library(ggplot2)
ggplot(data_val,aes(x = People_Personality_Traits, y = accuracies_1, group = names, color = names)) + 
  geom_line()+
  theme_light() +
  ggtitle("SVD vs LDA")+
  ylab(label="Reported Accuracies") + 
  xlab("People_Personality_Traits")+       
  geom_point()
ggsave("test1.tiff", width = 30, height = 20 , units = "cm")

# Choose which k are to be included in the analysis
ks<-c(2:10,15,20,30,50,75,100)

# Preset an empty list to hold the results
rs <- list()

# Run the code below for each k in ks
for (k in ks){
  # Varimax rotate Like SVD dimensions 1 to k
  v_rot <- unclass(varimax(Msvd$v[, 1:k])$loadings)
  
  # This code is exactly like the one discussed earlier
  u_rot <- as.data.frame(as.matrix(ufp %*% v_rot))
  fit_o <- glm(users$ope~., data = u_rot, subset = !test)
  pred_o <- predict(fit_o, u_rot[test, ])
  
  # Save the resulting correlation coefficient as the
  # element of R called k
  rs[[as.character(k)]] <- cor(users$ope[test], pred_o)
}

# Convert rs into the correct format
data<-data.frame(k=ks, r=as.numeric(rs))

# plot!
ggplot(data=data, aes(x=k, y=r, group=1)) + 
  theme_light() +
  stat_smooth(colour="red", linetype="dashed", size=1,se=F) + 
  geom_point(colour="red", size=2, shape=21, fill="white") +
  scale_y_continuous(breaks = seq(0, .5, by = 0.05))

