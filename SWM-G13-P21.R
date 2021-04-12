# Read in files
users <- read.csv(file = "C:/Users/pkondur1/OneDrive - Arizona State University/Desktop/SWM/sample_dataset/users.csv")
likes <- read.csv(file = "C:/Users/pkondur1/OneDrive - Arizona State University/Desktop/SWM/sample_dataset/likes.csv")
users_likes <- read.csv(file = "C:/Users/pkondur1/OneDrive - Arizona State University/Desktop/SWM/sample_dataset/users-likes.csv")

# Match entries in users_likes with users and likes dictionaries
users_likes$user_row<-match(users_likes$userid,users$userid)
users_likes$like_row<-match(users_likes$likeid,likes$likeid)

install.packages("Matrix")

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

install.packages('irlba')
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

# Log-likelihood of the model :
Mlda@loglikelihood

# Correlate user traits and their SVD scores
# users[,-1] is used to exclude the column with IDs
cor(u_rot, users[,-1], use = "pairwise")

# Correlate user traits and their LDA scores
cor(gamma, users[,-1], use = "pairwise")

# Plot the correlation matrix
library(corrplot)
corrplot(x, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# Load these libraries
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

