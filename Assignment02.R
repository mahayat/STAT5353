## Assignment 02 :: STAT 5353 :: Fall 2017
## Md Abul Hayat :: Date 12/13/2017
#______________________________________________________#
rm(list=ls()) # Remove All Variables
## Take Data Input
rm(list=ls())
data_in= read.table("Genetic.txt") # No Header
dim(data_in)
n = nrow(data_in) # Rows = n = 33 = data from one tissue
p = ncol(data_in) # Columns = p = 1200 = number of genes
# n = 32 < 1200 = p
#______________________________________________________#
## Standardizing Data Matrix
X = matrix(0,n,p) # Standardized Matrix : Each Row = Observation
for (i in 1:p)
{
  X[,i] = (data_in[,i]-mean(data_in[,i]))/sqrt(var(data_in[,i])) 
}
X_bar = colMeans(X) # Row Vector = 0
## Finding Correlation Matrix with S matrix
W = matrix(0,p,n) # Each Column = Observation
for (i in 1:n)
{
  W[,i] = X[i,]-X_bar # X_bar = 0
}
S = t(W)%*%(W) # As n < p
## Find out the Eigenvalues of S vector
lambda = (eigen(S,T)$values)/(n-1)
## Parallel Analysis (Horn)
simulations = 150
eigenval_stored = matrix(0,n,simulations) # Eigenvalues from each iteration in columns
for (i in 1:simulations)
{
  rand_data = matrix(rnorm(n*p),n,p) # Create a matrix of size(n,p)
  mu = colMeans(rand_data)
  rr = matrix(0,p,n)
  for (j in 1:n)
  {
    rr[,j] = rand_data[j,]-mu
  }
  corr_mat = t(rr)%*%(rr)
  eigenval_stored[,i] = eigen(corr_mat)$values/(n-1)
}
summary_crit = apply(eigenval_stored,1,quantile,p=0.95)
k = sum(!summary_crit > lambda)
print(k)
## 
compare = cbind(lambda[0:k+1],summary_crit[0:k+1])
print(compare)
## Eigenvalues and Eigenvectors : Reduced Dimentions
temp_eig_vectors_k = eigen(S,T)$vectors[,1:k]
eig_values_k = (eigen(S,T)$values[1:k]) # equal to previous lambda[1:k]
eig_vectors_k = matrix(0,p,k)
## Get Real Eigenvectors
for (i in 1:k)
{
  eig_vectors_k[,i] = c(1/sqrt(eig_values_k[i]))*(W%*%matrix(temp_eig_vectors_k[,i],ncol=1))
}
eig_values_k = eig_values_k/(n-1) # These are eigenvalues of Dispersion Matrix
## Multiply each eigenvector with sqrt(eigenvalue)
L_first = eig_vectors_k%*%diag(sqrt(eig_values_k))
L = varimax(L_first)$loading
##
var_before = apply(L_first^2,2,var) # variance in each column of Loading Matrix
var_after = apply(L^2,2,var)
print(var_before) # Column-wise Variance : Before
print(var_after) # Column-wise Variance : After
var_improvement = var_after-var_before
print(var_improvement)
which.max(var_improvement)
which.min(var_improvement)
##### Answer to (d)
D = diag(1-rowSums(L^2)) # Dispersion of Error Matrix
length(which(diag(D) > 0.75))
length(which(rowSums(L^2) < 0.25))
##
minvarcap1 = which.min(rowSums(L^2)) # Using Loading Matrix
print(minvarcap1)
minvarcap2 = which.max(diag(D)) # Using Dispersion of Error Matrix
print(minvarcap2)
## Answer to (e)
L[3,]%*%L[17,]
L[5,3]
## Answer to (f)
sum(L[14,]^2)-sum(L[40,]^2)
##________________________________________________##
k=8
## Eigenvalues and Eigenvectors : Reduced Dimentions
temp_eig_vectors_k = eigen(S,T)$vectors[,1:k]
eig_values_k = (eigen(S,T)$values[1:k]) # equal to previous lambda[1:k]
eig_vectors_k = matrix(0,p,k)
## Get Real Eigenvectors
for (i in 1:k)
{
  eig_vectors_k[,i] = c(1/sqrt(eig_values_k[i]))*(W%*%matrix(temp_eig_vectors_k[,i],ncol=1))
}
eig_values_k = eig_values_k/(n-1) # These are eigenvalues of Dispersion Matrix
## Multiply each eigenvector with sqrt(eigenvalue)
L_first = eig_vectors_k%*%diag(sqrt(eig_values_k))
L = varimax(L_first)$loading
##### Answer to (d)
D = diag(1-rowSums(L^2)) # Dispersion of Error Matrix
length(which(diag(D) > 0.75))
length(which(rowSums(L^2) < 0.25))
##
minvarcap1 = which.min(rowSums(L^2)) # Using Loading Matrix
print(minvarcap1)
minvarcap2 = which.max(diag(D)) # Using Dispersion of Error Matrix
print(minvarcap2)
## Answer to (f)
sum(L[14,]^2)-sum(L[40,]^2)
##----------------------------------------------##



