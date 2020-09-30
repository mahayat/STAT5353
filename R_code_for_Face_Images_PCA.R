## Read the all images from the folder
rm(list=ls()) # Remove All Variables
library(rgdal)
library(ppls)

training_images_folder = "C:/Users/abulh/Desktop/STAT 5353/R Assignment/Faceimage_data/yalefaces_train/"

list_of_files = list.files(training_images_folder)

n = length(list_of_files) # 120 Images

images_store = list()

# Storing image in each entry
for (i in 1:n) images_store[[i]] = readGDAL(paste(training_images_folder,list_of_files[i],sep=""))$band1

## you need note the number of rows and number of columns printed by the readGDAL command


image_size_rows = 243


##Suppose you want to plot any particular image

x11()
imagedata =  matrix(images_store[[40]],nrow= image_size_rows,byrow=T)
image(t(imagedata[nrow(imagedata):1,]),col = grey(seq(0,1, length=256)))


p = length(images_store[[40]])  ## write the above number

x_data = matrix(0,n,p)

for (i in 1:n) x_data[i, ] = images_store[[i]]

## Compute the estimate of mu and Sigma

## estimate the mean

x_bar = colMeans(x_data)
mu_hat = x_bar

## mu_hat is called the average face
## if you want to plot mu_hat

x11()
imagedata =  matrix(mu_hat,nrow= image_size_rows,byrow=T)
image(t(imagedata[nrow(imagedata):1,]),col = grey(seq(0,1, length=256)),xlab="Average Face")


## Since n < p, you cannot compute the eigenvalues of Sigma_hat in staright forward manner, instead define W such that Sigma_hat = W%*%W_transpose/(n-1)

W = matrix(0,p,n)

for (i in 1:n)
{
    W[,i] =  x_data[i, ] - x_bar
}


## Finding eigenvectors of Sigma_hat

############################################
############################################

## As n < p, use the following:

B = t(W)%*%W

eigen_results = eigen(B,only.values=F)

sum_eigs = sum(diag(B))

ELOI = array(0,n)

for (k in 1:n)
{
    ELOI[k] = 1 - sum(eigen_results$values[1:k])/sum_eigs
    print(paste(k," ",ELOI[k],sep=""))
}

## Choose k as the minimum integer for which ELOI < threshold

k  =

eigen_kvalues = eigen_results$values[1:k]
eigen_kvectors = eigen_results$vectors[,1:k]

eigvec_Sigma_hat_k = matrix(0,p,k)
for (i in 1:k) eigvec_Sigma_hat_k[ ,i] = c(1/sqrt(eigen_kvalues[i]))*(W%*%matrix(eigen_kvectors[,i],ncol=1))

U_hat = t(eigvec_Sigma_hat_k)

## if you want to plot any of the eigenfaces, plot corresponding row of U_hat

x11()
imagedata =  matrix(U_hat[Appropriate number, ],nrow= image_size_rows,byrow=T)
image(t(imagedata[nrow(imagedata):1,]),col = grey(seq(0,1,length=256)), xlab="Eigenface Number: (put appropriate number here)")

## read a new image X^(new) from a file

New_image = readGDAL("File_path/File_name")$band1

## plot this image file

x11()
imagedata =  matrix(New_image,nrow= image_size_rows,byrow=T)
image(t(imagedata[nrow(imagedata):1,]),col = grey(seq(0,1,length=256)), xlab= "Test image")

## Construct the PC representation of this image

PC_representation = U_hat%*%( matrix( New_image - mu_hat, ncol=1) )

## Reconstruct the original observations

Reconstructed_observations = mu_hat + t(U_hat)%*%matrix(PC_representation,ncol=1)

## How good were the reconstructed observations: plot those reconstruced images

x11()
imagedata =  matrix(Reconstructed_observations,nrow= image_size_rows,byrow=T)
image(t(imagedata[nrow(imagedata):1,]),col = grey(seq(0,1,length=256)), xlab="Reconstructed test image with k = (put appropriate number here)  eigenfaces")








