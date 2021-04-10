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

#SVD and PCA 

install.packages('irlba')

library(irlba)
Msvd<-irlba(M,nv=5);

#SVD scores of users 
head(Msvd$d)
u<-Msvd$u

#SVD scores of the likes
v<-Msvd$v

head(u)
dim(u)

head(v)
dim(v)

#PCA for users 

#Construct the user matrix for PCA


pca <- prcomp(u, scale=TRUE) 

library(ggplot2)



pca.data <- u.frame(Sample=rownames(pca$x),
                       X=pca$x[,1],
                       Y=pca$x[,2])
pca.data

ggplot(data=pca.datapca, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("My PCA Graph")
