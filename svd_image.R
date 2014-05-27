install.packages("jpeg")
library(jpeg)

# array, width x height x 3
img <- readJPEG("/Users/jselby/Downloads/kittens.jpg")
dim(img)
typeof(img)
head(img)
tail(img)

# display image
plot.size <- max(dim(img)) * sqrt(2)
plot(c(1, plot.size), c(1, plot.size), type='n')
#rasterImage(img, 1, 1, plot.size, plot.size)

# split color channels
img.red <- img[,,1]
img.blue <- img[,,2]
img.green <- img[,,3]

trans <- function(orig, trans.mat) {
     orig.rows <- dim(orig)[1]
     orig.cols <- dim(orig)[2]
     new.size <- max(orig.rows, orig.cols) * sqrt(2) * 2
     new <- matrix(1.0, nrow=new.size, ncol=new.size)
     for (row in 1:orig.rows) {
         for (col in 1:orig.cols) {
             coords <- trans.mat %*% matrix(c(row, col), ncol=1)
             new.row <- as.integer(coords[1] + new.size/2)
             new.col <- as.integer(coords[2] + new.size/2)
             new[new.row, new.col] <- orig[row, col]
         }}
     return (new)
}

rot90 <- matrix(c(0, -1, 1, 0), ncol=2)
img.rot90 <- trans(img.red, rot90)
#rasterImage(img.rot90, 1, 1, plot.size, plot.size)

rot180 <- matrix(c(-1, 0, 0, -1), ncol=2)
img.rot180 <- trans(img.red, rot180)
#rasterImage(img.rot180, 1, 1, plot.size, plot.size)

rot45 <- matrix(c(cos(pi/4), -sin(pi/4), sin(pi/4), cos(pi/4)), ncol=2)
img.rot45 <- trans(img.red, rot45)
#rasterImage(img.rot45, 1, 1, plot.size, plot.size)

rot270 <- matrix(c(0, 1, -1, 0), ncol=2)
img.rot270 <- trans(img.red, rot270)
#rasterImage(img.rot270, 1, 1, plot.size, plot.size)

shear <- matrix(c(1, 0, .5, 1), ncol=2)
img.shear <- trans(img.red, shear)
#rasterImage(img.shear, 1, 1, plot.size, plot.size)
shear.svd <- svd(shear)
img.shear.u <- trans(img.red, shear.svd$u)
#rasterImage(img.shear.u, 1, 1, plot.size, plot.size)
img.shear.d <- trans(img.red, diag(shear.svd$d))
#rasterImage(img.shear.d, 1, 1, plot.size, plot.size)
img.shear.v <- trans(img.red, t(shear.svd$v))
#rasterImage(img.shear.v, 1, 1, plot.size, plot.size)
