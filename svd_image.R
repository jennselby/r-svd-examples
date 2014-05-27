install.packages("jpeg")
library(jpeg)

# array, width x height x 3
img <- readJPEG("/Users/jselby/Downloads/kittens.jpg")
dim(img)
typeof(img)
head(img)
tail(img)

# display image
plot(c(0, 327), c(0, 482), type='n')
#rasterImage(img, 0, 0, 327, 482)

# split color channels
img_red <- img[,,1]
img_blue <- img[,,2]
img_green <- img[,,3]

trans <- function(orig, trans.mat) {
    new <- matrix(1.0, nrow=dim(orig)[1]*4, ncol=dim(orig)[2]*4)
    for (x in 1:dim(orig)[1]) {
        for (y in 1:dim(orig)[2]) {
            coords <- trans.mat %*% matrix(c(x, y), ncol=1)
            new.x <- as.integer(coords[1] + dim(orig)[1])
            new.y <- as.integer(coords[2] + dim(orig)[2])
            new[new.x, new.y] <- orig[x,y]
    }}
    return (new)
}

rot90 <- matrix(c(0, -1, 1, 0), ncol=2)
img_rot90 <- trans(img_red, rot90)
#rasterImage(img_rot90, 0, 0, 327, 482)

rot180 <- matrix(c(-1, 0, 0, -1), ncol=2)
img_rot180 <- trans(img_red, rot180)
#rasterImage(img_rot180, 0, 0, 327, 482)

rot45 <- matrix(c(cos(pi/4), -sin(pi/4), sin(pi/4), cos(pi/4)), ncol=2)
img_rot45 <- trans(img_red, rot45)
#rasterImage(img_rot45, 0, 0, 327, 482)

# TODO: doesn't work
#rot270 <- matrix(c(0, 1, -1, 0), ncol=2)
#img_rot270 <- trans(img_red, rot270)
#rasterImage(img_rot270, 0, 0, 327, 482)

shear <- matrix(c(1, 0, .5, 1), ncol=2)
img_shear <- trans(img_red, shear)
#rasterImage(img_shear, 0, 0, 327, 482)
shear.svd <- svd(shear)
img_shear_u <- trans(img_red, shear.svd$u)
#rasterImage(img_shear_u, 0, 0, 327, 482)
img_shear_d <- trans(img_red, diag(shear.svd$d))
#rasterImage(img_shear_d, 0, 0, 327, 482)
img_shear_v <- trans(img_red, t(shear.svd$v))
#rasterImage(img_shear_v, 0, 0, 327, 482)
