(library(rgdal) 
library(ppls)

# Pixel Count
# Horizontal = 320 #of columns
# Vertical = 243 #of rows
# Total Pixels = 320*243 = 77760

training_images_folder = "C:/Users/abulh/Desktop/R Codes/PCA/Faceimage_data/yalefaces_train"
list_of_files = list.files(training_images_folder)

n = length(list_of_files)
images_store = list()

for (i in 1:n) 
  images_store[[i]] = readGDAL(paste(training_images_folder,list_of_files[i],sep="/"))$band1

image_size_rows = 243
## put_no_from_R_terminal showing how many rows the images have

## suppose you want to plot any particular image

# abul = 40
# 
# x11()
# imagedata = matrix(images_store[[abul]],nrow=image_size_rows,byrow=TRUE)
# image(t(imagedata[nrow(imagedata):1,]),col = grey(seq(0,1,length=256)))
# 
# p = length(images_store[[abul]]) # Total Number of Pixels = 77760
# x_data = matrix(0,n,p)
# for (i in 1:n) x_data[i,]=images_store[[i]]
# dim(x_data)

# Compute the estimate of mu and sigma
# estimate the mean

x_bar = colMeans(x_data)
mu_hat = x_bar

# mu_hat is called the average face
# following snippet is for plotting mu_hat

x11()
imagedata = matrix(mu_hat,nrow = image_size_rows,byrows=T)
image(t(imagedata[nrow(imagedata):1,]),col=grey(seq(0,1,length=256)),xlab="Average Face")


##

A = matrix(0,p,n)
for (i in 1:n)
{ 
  A[,i]=x_data[i,]-x_bar
}







