library(dagitty)
library(ggdag)
library(gridExtra)
library(ggplot2)
#Exercise 1 - ResidentSleeper
# No edge
dag1 = dagify(A~A, B~B, C~C)

#One edge
dag2 = dagify(A~C, B~B, C~C)
dag3 = dagify(C~A, B~B, C~C)
dag4 = dagify(A~B, B~B, C~C)
dag5 = dagify(B~A, B~B, C~C)
dag6 = dagify(A~A, B~C, C~C)
dag7 = dagify(A~A, C~B, C~C)

#two edges
# A in center
dag8 = dagify(A~B, B~B, C~A)
dag9 = dagify(A~B, B~B, A~C)
dag10 = dagify(B~A, B~B, A~C)
dag11 = dagify(B~A, B~B, C~A)

#B in center
dag12 = dagify(A~B, B~B, C~B)
dag13 = dagify(A~B, B~B, B~C)
dag14 = dagify(B~A, B~B, B~C)
dag15 = dagify(B~A, B~B, C~B)

#C in center
dag16 = dagify(A~C, B~B, C~B)
dag17 = dagify(A~C, B~B, B~C)
dag18 = dagify(C~A, B~B, B~C)
dag19 = dagify(C~A, B~B, C~B)

#Three edges

dag20 = dagify(B~A, C~B, C~A)
dag21 = dagify(B~A, B~C, C~A)
dag22 = dagify(B~A, B~C, A~C)
dag23 = dagify(A~B, B~C, A~C)
dag24 = dagify(A~B, C~B, A~C)
dag25 = dagify(A~B, C~B, C~A)

#Turn into plots 
p1 = ggdag(dag1)
p2 = ggdag(dag2)
p3 = ggdag(dag3)
p4 = ggdag(dag4)
p5 = ggdag(dag5)
p6 = ggdag(dag6)
p7 = ggdag(dag7)
p8 = ggdag(dag8)
p9 = ggdag(dag9)
p10 = ggdag(dag10)
p11 = ggdag(dag11)
p12 = ggdag(dag12)
p13 = ggdag(dag13)
p14 = ggdag(dag14)
p15 = ggdag(dag15)
p16 = ggdag(dag16)
p17 = ggdag(dag17)
p18 = ggdag(dag18)
p19 = ggdag(dag19)
p20 = ggdag(dag20)
p21 = ggdag(dag21)
p22 = ggdag(dag22)
p23 = ggdag(dag23)
p24 = ggdag(dag24)
p25 = ggdag(dag25)

#Arrange for a)

grid.arrange(p1,
             p2,p3,p4,p5,p6,p7,
             p8,p9,p10,p11,
             p12,p13,p14,p15,
             p16,p17,p18,p19,
             p20,p21,p22,p23,p24,p25, ncol = 5) 

#Arannge for b)

grid.arrange(p1,p2,p3,p5,p6,p19, ncol = 3) 

#Arrange for c)

grid.arrange(p6,p7,p8,p10,p11,
             p12,p13,p14,p15,
             p16,p17,p18,p19,
             p20,p21,p22,p23,p24,p25, ncol = 3) 

#Arrange for d)

grid.arrange(p2,p3,p8,p9,p10,p11,
             p12,p13,p15,
             p16,p17,p18,p19,
             p20,p21,p22,p23,p24,p25, ncol = 3) 
#Arrange for e)
p19

#Arrange for f)
p20

#Arrange for h)
p14

#Exercise 4

#a
par(mfrow=c(1, 3))
plot1 = plot( graphLayout( dagitty( "pdag { A--B A--C A--D B--C B--D C--D } " ) ) )
plot2 = plot( graphLayout( dagitty( "pdag { A--B A--C A--D B--D C--D } " ) ) )
plot3 = plot( graphLayout( dagitty( "pdag { A--B A--C B--D C--D } " ) ) )
par(mfrow=c(1, 1))
#b
plot4 = plot( graphLayout( dagitty( "pdag { A--B A--C B->D C->D } " ) ) )
#c
ggdag(dagitty( "dag { A->B A->C B->D C->D } "))

plot5 = ggdag(dagitty( "dag { A->B A->C B->D C->D } "))
plot6 = ggdag(dagitty( "dag { A->B A<-C B->D C->D } " ))
plot7 = ggdag(dagitty( "dag { A<-B A->C B->D C->D } " )) 
grid.arrange(plot5,plot6,plot7, ncol = 3)
#d2
#dag1_ex4 = dagitty( "dag { A->B A->C B->D C->D } " ) 
#dag2_ex4 = dagitty( "dag { A->B A<-C B->D C->D } " )
#dag3_ex4 = dagitty( "dag { A<-B A->C B->D C->D } " )

#adjustmentSets(dag1_ex4, exposure = c("A"), outcome = c("D"), type = "all", effect = "total")
#adjustmentSets(dag2_ex4, exposure = c("A"), outcome = c("D"), type = "all", effect = "total")
#adjustmentSets(dag3_ex4, exposure = c("A"), outcome = c("D"), type = "all", effect = "total")
#ONLY USED IN ICP-type argument

#Exercise 5

### a) setup
library(readr)
X <- t(as.matrix(read_csv('ExamData_ICA.csv')))
dim(X)
par(mfrow=c(1,1))
image(t(apply(matrix(X[2,], 50, 50), 1, rev)),
      col=paste("gray", 1:99, sep=""))
### b) 
#Whiten function
whiten <- function(X) {
  Xc <- X - rowMeans(X)
  Sigma <- Xc %*% t(Xc) / dim(Xc)[2]
  eig <- eigen(Sigma)
  W <- diag(1 / sqrt(eig$values)) %*% t(eig$vectors)
  Z <- W %*% Xc
}
Xc <- X - rowMeans(X)
Sigma <- Xc %*% t(Xc) / dim(Xc)[2]
eig <- eigen(Sigma)
W <- diag(1 / sqrt(eig$values)) %*% t(eig$vectors)
Z <- W %*% Xc
### Report
rowMeans(X)
W

### c)
rotMat <- function(a) {matrix(c(cos(a), -sin(a), sin(a), cos(a)), 2, 2)}
NN = 100
alphas  <- seq(0, pi/2, length.out=NN)
#Kurtosis values:
kurtosisValues = sapply(alphas, function(a) {
  temp = rotMat(a) %*% W %*% (X-rowMeans(X))
  result = abs(1/2500*sum(temp[1,]^4)-3)
})
### Report
alphas[which.max(kurtosisValues)]
V <- rotMat(alphas[which.max(kurtosisValues)]) %*% W
V

# Plot for correctness
Z = whiten(X)
ICA.X = rotMat(alphas[which.max(kurtosisValues)]) %*% Z
image(t(apply(matrix(ICA.X[1,], 50, 50), 1, rev)),
      col=paste("gray", 1:99, sep=""))

### d)
H = solve(V)
H = matrix(c(-2*H[,2],2*H[,1]), nrow = 2)-diag(2)
H
round(H,3)      
