install.packages("jpeg")
library(jpeg)

# array, width x height x 3
img <- readJPEG("/Users/jselby/Downloads/kittens.jpg")
dim(img)
typeof(img)
head(img)
tail(img)

# display image
plot_size <- max(dim(img)) * sqrt(2)
plot(c(1, plot_size), c(1, plot_size), type='n')
#rasterImage(img, 1, 1, plot_size, plot_size)

# split color channels
img_red <- img[,,1]
img_blue <- img[,,2]
img_green <- img[,,3]

trans <- function(orig, trans.mat) {
     orig_rows <- dim(orig)[1]
     orig_cols <- dim(orig)[2]
     new_size <- max(orig_rows, orig_cols) * sqrt(2)
     new <- matrix(1.0, nrow=new_size, ncol=new_size)
     row.offset <- as.integer(orig_rows/2)
     col.offset <- as.integer(orig_cols/2)
     for (row in (-row.offset+1):row.offset) {
         for (col in (-col.offset+1):col.offset) {
             coords <- trans.mat %*% matrix(c(row, col), ncol=1)
             new.row <- as.integer(coords[1] + row.offset)
             new.col <- as.integer(coords[2] + col.offset)
             new[new.row, new.col] <- orig[row + row.offset, col + col.offset]
         }}
     return (new)
}

rot90 <- matrix(c(0, -1, 1, 0), ncol=2)
img_rot90 <- trans(img_red, rot90)
#rasterImage(img_rot90, 1, 1, plot_size, plot_size)

rot180 <- matrix(c(-1, 0, 0, -1), ncol=2)
img_rot180 <- trans(img_red, rot180)
#rasterImage(img_rot180, 1, 1, plot_size, plot_size)

rot45 <- matrix(c(cos(pi/4), -sin(pi/4), sin(pi/4), cos(pi/4)), ncol=2)
img_rot45 <- trans(img_red, rot45)
#rasterImage(img_rot45, 1, 1, plot_size, plot_size)

# TODO: doesn't work
#rot270 <- matrix(c(0, 1, -1, 0), ncol=2)
#img_rot270 <- trans(img_red, rot270)
#rasterImage(img_rot270, 1, 1, plot_size, plot_size)

shear <- matrix(c(1, 0, .5, 1), ncol=2)
img_shear <- trans(img_red, shear)
#rasterImage(img_shear, 1, 1, plot_size, plot_size)
shear.svd <- svd(shear)
img_shear_u <- trans(img_red, shear.svd$u)
#rasterImage(img_shear_u, 1, 1, plot_size, plot_size)
img_shear_d <- trans(img_red, diag(shear.svd$d))
#rasterImage(img_shear_d, 1, 1, plot_size, plot_size)
img_shear_v <- trans(img_red, t(shear.svd$v))
#rasterImage(img_shear_v, 1, 1, plot_size, plot_size)
