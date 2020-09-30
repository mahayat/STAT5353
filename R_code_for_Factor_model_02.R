## Read the data first

data_mat = as.matrix(read.table("Pollution.txt",header=T))

## first standardize the data

n = nrow(data_mat)
p = ncol(data_mat)

for (i in 1:p)
data_mat[ ,i ] = (data_mat[,i] - mean(data_mat[,i]))/sqrt(var(data_mat[,i]))

## first use the previous code to determine the number of factors based on PA method

## then , decide about the value of k from that

k =

## Now, construct the loading matrix using PCA method of factor analysis


##first compute the EIGENVALUES OF THE CORRELATION MATRIX


############################################
############################################

x_bar = colMeans(data_mat)

mu_hat = x_bar

W = matrix(0,p,n)

for (i in 1:n)
{
    W[,i] =  data_mat[i, ] - x_bar  ## since the data is already centered , x_bar is basically 0
}

## if n < p, use the following:

B = t(W)%*%W

eigen_results = eigen(B,only.values=F)


eigen_kvalues = eigen_results$values[1:k]
eigen_kvectors = eigen_results$vectors[,1:k]

eigvec_Sigma_hat_k = matrix(0,p,k)
for (i in 1:k) eigvec_Sigma_hat_k[ ,i] = c(1/sqrt(eigen_kvalues[i]))*(W%*%matrix(eigen_kvectors[,i],ncol=1))

eigval_Sigma_hat_k = eigen_kvalues/(n-1)

## if n > p, use this:

B = W%*%t(W)

eigen_results = eigen(B,only.values=F)

sum_eigs = sum(diag(B))

eigvec_Sigma_hat_k = eigen_results$vectors[,1:k]
eigval_Sigma_hat_k = eigen_results$values[1:k]/(n-1)



############################################
############################################

## Construct the loading matrix L

L_initial = eigvec_Sigma_hat_k%*%diag(sqrt(eigval_Sigma_hat_k))

## But this L is NOT final.  You need to rotate L to find the best choice using a varimax criterion:

L = varimax(L_initial)$loadings

## print(L)

print(L,cutoff = 0.01)

## How much did we gain by varimax rotation

## individual factor-wise assessment

apply(L_initial^2,2,var)

apply(L^2,2,var)

## Overall assessment for all factors combined

sum(apply(L_initial^2,2,var))

sum(apply(L^2,2,var))

## Determine D, the diagonal dispersion matrix of errors

D = diag( 1 - rowSums(L^2.0) )

## Can answer all sorts of questions regarding the variables and factors

## Communality of variable i

sum(L[i,]^2.0)

## specific variance of variable i

D[i,i]

## which varaibles can be explained by factor model below or above a certain threshold

which(diag(D)> some_threshold)
which(diag(D)< some_threshold)

which.max(diag(D))
which.min(diag(D))

## proprtion of varaibility for all variables explained by factor j

sum(L[,j]^2.0)/p

## correlation between variable i and variable j

sum(L[i,]*L[j,])

## Correlation between varaible i and factor j

L[i,j]

######################
#####################


## estimate the factor score for a new observation

D_inv = diag(1/diag(D))

fhat_new = solve(t(L)%*%D_inv%*%L)%*%t(L)%*%D_inv %*% matrix(data_new , ncol = 1)

fhat_new















	


