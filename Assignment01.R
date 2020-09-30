## Assignment 01 :: STAT 5353 :: Fall 2017
## Md Abul Hayat :: Date 12/11/2017
#______________________________________________________#
rm(list=ls()) # Remove All Variables
## Take Data Input
data_in = read.table("Pollution.txt",header = TRUE)
dim(data_in) # Answer to (a)
n = nrow(data_in) # of Rows = # of Factories = 30 = n
p = ncol(data_in) # of Columns = # of Pollutants = 799 = p
#______________________________________________________#
## Standardizing Data Matrix
X = matrix(0,n,p) # Standardized Matrix : Each Row = Observation
for (i in 1:p)
{
  X[,i] = (data_in[,i]-mean(data_in[,i]))/sqrt(var(data_in[,i])) 
}
X_bar = colMeans(X) # Row Vector = 0
## Finding Correlation Matrix with S matrix : Sigma = S/(n-1)
W = matrix(0,p,n) # Each Column = Observation
for (i in 1:n)
{
  W[,i] = X[i,]-X_bar # X_bar = 0
}
S = t(W)%*%(W) # As n < p
## Find out the Eigenvalues
lambda = (eigen(S,T)$values)/(n-1)
#__________________________________________________________#
## Parallel Analysis (Horn)
simulations = 250
eigenval_stored = matrix(0,n,simulations) # Eigenvalues from each iteration in columns
for (i in 1:simulations)
{
  rand_data = matrix(rnorm(n*p),n,p) # Create a matrix of size(n,p)
  mu = colMeans(rand_data)
  rr = matrix(0,p,n)
  for (j in 1:n)
  {
    rr[,j] = rand_data[j,]-mu # As they have variance = 1
  }
  corr_mat = t(rr)%*%(rr)
  eigenval_stored[,i] = eigen(corr_mat)$values/(n-1)
}
summary_crit = rowMeans(eigenval_stored)
k = sum(!summary_crit > lambda)
print(k) # Answer to (b) 
##__________________________________________________##
## Comparison Table
compare = cbind(lambda[0:k+1],summary_crit[0:k+1])
print(compare)
## Eigenvalues and Eigenvectors : Reduced Dimentions
temp_eig_vectors_k = eigen(S,T)$vectors[,1:k]
eig_values_k = (eigen(S,T)$values[1:k]) # equal to previous lambda[1:k]
eig_vectors_k = matrix(0,p,k) # Eigenvectors of Dispersion Matrix 
## Get Real Eigenvectors
for (i in 1:k)
{
  eig_vectors_k[,i] = c(1/sqrt(eig_values_k[i]))*(W%*%matrix(temp_eig_vectors_k[,i],ncol=1))
}
eig_values_k = eig_values_k/(n-1) # These are eigenvalues of Dispersion Matrix
## Multiply each eigenvector with sqrt(eigenvalue)
L_first = (eig_vectors_k)%*%diag(sqrt(eig_values_k))
## Varimax Criterion
L = varimax(L_first)$loading
##### All set for answers
var_before = apply(L_first^2,2,var) # variance in each column of Loading Matrix
var_after = apply(L^2,2,var)
print(apply(L_first^2,2,var)) # Column-wise Variance : Before
print(apply(L^2,2,var)) # Column-wise Variance : After
var_improvement = var_after/var_before # Improvement in each factor
print(var_improvement)
sum(var_after)/sum(var_before) # Overall Improvement of Variance
##### Answer to (d)
D = diag(1-rowSums(L^2)) # Dispersion of Error Matrix
length(which(diag(D)< 0.1))
length(which(rowSums(L^2)>0.9))
## Percentage of Variability Captured by 2nd Factor
sum(L[,2]^2)/p
##### Answer to (e)
## Factor score of 3rd Factory
solve(t(L)%*%solve(D)%*%L)%*%t(L)%*%solve(D)%*%W[,3]
##### Answer to (f)
maxvarcap1 = which.max(rowSums(L^2)) # Using Loading Matrix
print(maxvarcap1)
maxvarcap2 = which.min(diag(D)) # Using Dispersion of Error Matrix
print(maxvarcap2)
#####__________________k = 10_______________________________#####
k = 10
## Getting prepared with k = 10
temp_eig_vectors_k = eigen(S,T)$vectors[,1:k]
eig_values_k = (eigen(S,T)$values[1:k]) # equal to previous lambda[1:k]
print(eig_values_k)
eig_vectors_k = matrix(0,p,k)
for (i in 1:k)
{
  eig_vectors_k[,i] = c(1/sqrt(eig_values_k[i]))*(W%*%matrix(temp_eig_vectors_k[,i],ncol=1))
}
eig_values_k = eig_values_k/(n-1)
L_first = eig_vectors_k%*%diag(sqrt(eig_values_k))
L = varimax(L_first)$loading
## Answer to (d) with k = 10
D = diag(1-rowSums(L^2)) # Dispersion of Error Matrix
length(which(diag(D)< 0.1))
length(which(rowSums(L^2)>0.9))
sum(L[,2]^2)/p
## Answer to (f) with k = 10
maxvarcap1 = which.max(rowSums(L^2)) # Using Loading Matrix
print(maxvarcap1)
maxvarcap2 = which.min(diag(D)) # Using Dispersion of Error Matrix
print(maxvarcap2)
##----------------------------------------------## 






