## Exemple
library(mvtnorm)
library(MixSim)


x.points <- seq(-5,5,length.out=100)
y.points <- x.points
z <- matrix(0,nrow=100,ncol=100)
mu <- c(0,0)
sigma <- matrix(c(7,0,
                  0,1),nrow=2)
for (i in 1:100) {
  for (j in 1:100) {
    z[i,j] <- dmvnorm(c(x.points[i],y.points[j]),
                      mean=mu,sigma=sigma)
  }
}

contour(x.points,y.points,z)


## 2eme partie##############
############################

#generation du permier jeu de données
repeat{
  Q1 <- MixSim(BarOmega = 0, K = 2, p = 2, sph = T)
  if (Q1$fail == 0) break
}

A1 <- simdataset(n = 500, Pi = Q1$Pi, Mu = Q1$Mu, S = Q1$S, n.out = 0)
colors <- c("red", "green")
plot(A1$X, xlab = "x1", ylab = "x2", type = "n")
for (k in 0:2){
  points(A1$X[A1$id == k, ], col = colors[k], pch = 19, cex = 0.5)
}

###################

#generation du permier jeu de données
repeat{
  Q2 <- MixSim(BarOmega = 0.1, K = 3, p = 2, sph = T)
  if (Q2$fail == 0) break
}

A2 <- simdataset(n = 500, Pi = Q2$Pi, Mu = Q2$Mu, S = Q2$S, n.out = 0)
colors <- c("red", "green", "blue")
plot(A2$X, xlab = "x1", ylab = "x2", type = "n")
for (k in 0:3){
  points(A2$X[A2$id == k, ], col = colors[k], pch = 19, cex = 0.5)
}

########################

#generation du permier jeu de données
repeat{
  Q3 <- MixSim(BarOmega = 0.1, K = 3, p = 2, sph = F)
  if (Q3$fail == 0) break
}

A3 <- simdataset(n = 500, Pi = Q3$Pi, Mu = Q3$Mu, S = Q3$S, n.out = 0)
colors <- c("red", "green", "blue")
plot(A3$X, xlab = "x1", ylab = "x2", type = "n")
for (k in 0:3){
  points(A3$X[A3$id == k, ], col = colors[k], pch = 19, cex = 0.5)
}


# la fonction de decision
G.function <- function(X, Mu, Sigma, Pj){
  return(-1/2* t(X - Mu) %*% solve(Sigma) %*% (X - Mu) - 1/2 * log(det(Sigma)) + log(Pj))
}

# foction de classification des données
Classification <- function(Distribution, Data){
  classifications <- c()
  for (i in c(1:dim(Data$X)[1])){
    class_prob <- c()
    for (j in c(1:dim(Distribution$Mu)[1])){
      class_prob <- append(class_prob, G.function(X= Data$X[i,], 
                                                  Mu= Distribution$Mu[j,], 
                                                  Sigma= Distribution$S[,,j], 
                                                  Pj= Distribution$Pi[j]))
    }
  classifications <- append(classifications, which.max(class_prob)) 
  }
  return(classifications)
}


# classifications des trois jeu de données
pred1 <- Classification(Q1, A1)
pred2 <- Classification(Q2, A2)
pred3 <- Classification(Q3, A3)

# les matrices de confusion des trois classifications
cm1 = table(pred1,A1$id)
cm2 = table(pred2,A2$id)
cm3 = table(pred3,A3$id)

# accuracy
sum(diag(cm1))/sum(cm1)
sum(diag(cm2))/sum(cm2)
sum(diag(cm3))/sum(cm3)


# foction utilisé pour le calcul de la precision et le rappel
Precision.recall <- function(cm){
  precisions <- c()
  recalls <- c()
  for (i in c(1:dim(cm)[1])){
    precisions <- append(precisions, diag(cm)[i]/sum(cm[i,]))
    recalls <- append(recalls, diag(cm)[i]/sum(cm[,i]))
  }
  A = list(precisions, recalls)
  return(A)
}

# precsion rappel des trois classification
p1 = Precision.recall(cm1)[1]
r1 = Precision.recall(cm1)[2]

p2 = Precision.recall(cm2)[1]
r2 = Precision.recall(cm2)[2]

p3 = Precision.recall(cm3)[1]
r3 = Precision.recall(cm3)[2]
