## Read the data first

data_mat = as.matrix(read.table("Pollution.txt",header=T))

## first standardize the data

n = nrow(data_mat) # Rows = Observations = n = 30
p = ncol(data_mat) # Columns = Variables = p = 799

for (i in 1:p)
data_mat[ ,i ] = (data_mat[,i] - mean(data_mat[,i]))/sqrt(var(data_mat[,i]))

##Now compute the EIGENVALUES OF THE CORRELATION MATRIX


############################################
############################################

x_bar = colMeans(data_mat)

W = matrix(0,p,n)

for (i in 1:n)
{
    W[,i] =  data_mat[i, ] - x_bar  ## since the data is already centered , x_bar is basically 0
}

## if n < p, use the following:

B = t(W)%*%W

eigenvalues_data = eigen(B,only.values=TRUE)$values/(n-1)


## if n > p, use this:

B = W%*%t(W)

eigenvalues_data = eigen(B,only.values=TRUE)$values/(n-1)


############################################
############################################

## start the PA method

## now generate data of random numbers

n_chains =   ## how manu simulated  datasets we want to use

eigenvalue_store = matrix(0,min(n,p),n_chains)

for (j in 1:n_chains)
{
	data_sim = matrix(rnorm(n*p),n,p)
	
	## standardize
	
	for (i in 1:p)  data_sim[ ,i ] = (data_sim[,i] - mean(data_sim[,i]))/sqrt(var(data_sim[,i])) 
	
##Now compute the EIGENVALUES OF THE CORRELATION MATRIX

############################################
############################################

x_bar_sim = colMeans(data_sim)

W = matrix(0,p,n)

for (i in 1:n)
{
    W[,i] =  data_sim[i, ] - x_bar_sim  ## since the data is already centered , x_bar is basically 0
}

## if n < p, use the following:

B = t(W)%*%W

eigenvalues_sim = eigen(B,only.values=TRUE)$values/(n-1)


## if n > p, use this:

B = W%*%t(W)

eigenvalues_sim = eigen(B,only.values=TRUE)$values/(n-1)


############################################
############################################
	
	eigenvalue_store[,j] = eigenvalues_sim
	}
	
	
summary_mean = rowMeans(eigenvalue_store)

quantile_choice =    ## which quantile do you want to use ?
summary_quantile = apply(eigenvalue_store,1,quantile,p=quantile_choice)

k_choice_mean = which(summary_mean>eigenvalues_data)[1] - 1
k_choice_quantile = which(summary_quantile>eigenvalues_data)[1] - 1

## show some of initial eigenvalues

show_comparison = cbind(eigenvalues_data,summary_mean,summary_quantile)[1:(max(k_choice_mean,k_choice_quantile)+1),]
colnames(show_comparison) = c("Actual data", "  Simulated data: mean", paste("  Simulated data: ", quantile_choice, " quantile",sep=""))

print(show_comparison)
	
	
	


