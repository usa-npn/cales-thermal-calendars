#From Dave Miller: https://gitlab.bioss.ac.uk/dmiller/ncv-examples/-/blob/main/raster.R

# raster with short-range autocorrelation
# based on the example in https://arxiv.org/html/2404.16490v1#S6

library(mgcv)
library(ggplot2)
library(patchwork)

set.seed(3141)

# one side of the grid
n_grid <- 50
# make the grid
gr <- expand.grid(x = seq(0, 1, length.out=n_grid),
                  y = seq(0, 1, length.out=n_grid))

# two gaussians, code from mgcv::gamSim
test1 <- function(x, z, sx = 0.3, sz = 0.4) {
  (pi^sx * sz) * (1.2 * exp(-(x - 0.2)^2/sx^2 - (z - 0.3)^2/sz^2)
                  + 0.8 * exp(-(x - 0.7)^2/sx^2 - (z - 0.8)^2/sz^2))
}

# make the response without error
gr$z <- test1(gr$x, gr$y)

ggplot(gr, aes(x=x, y=y, fill=z)) +
  # geom_tile(color = NA) +
  geom_raster() +
  geom_contour(aes(z = z), color = "grey50") +
  scale_fill_viridis_c() +
  theme_minimal()

# make independent errors
# need padding around the matrix for the next step
err <- matrix(rnorm((n_grid+2)^2), n_grid+2, n_grid+2)

image(err, main="N(0,1) errors")

err2 <- matrix(NA, n_grid, n_grid)
# use the same weightings as used in the paper 
#TODO: where in the paper are these weightings?
wt_mat <- matrix(c(0.5, 0.3, 0.5,
                   0.3, 1, 0.3,
                   0.5, 0.3, 0.5), nrow=3, ncol=3)
# autocorrelate that - taking a weighed average of the 8-neighbourhood
# around each cell in the raster
for(i in 1:n_grid){
  for(j in 1:n_grid){
    # this indexing looks horrendous because of the extension to matrix err
    # in the above code, sorry (+1s left in for "clarity" lol)
    err2[i, j] <- sum(err[c(i-1 +1, i +1, i+1 +1),
                          c(j-1 +1, j +1, j+1 +1)] * wt_mat)
  }
}

# more correlate-y
image(err2, main="correlated errors (8 neighbours)")

# unwrap the matrix to a data.frame (the t() makes things row-wise)
gr$err2 <- as.vector(t(err2))

# build the response
# fac is the factor to amplify the "signal" by, so we can fiddle
# with the signal-to-noise ratio and see how that effects things
fac <- 2
gr$resp <- fac*gr$z + gr$err2
# what is the signal to noise ratio?
mean(gr$resp)^2/var(gr$resp)^2

# what does the response look like now?
ggplot(gr, aes(x=x, y=y, fill=resp)) +
  # geom_tile() +
  geom_raster() +
  geom_contour(aes(z = resp), color = "grey50") +
  scale_fill_viridis_c() +
  theme_minimal()


#TODO what does this do??
## optional subsampling of the data to see what performance looks like
ss_ind <- 1:nrow(gr)
#ss_ind <- sample(ss_ind, 100)


## now fit a model using REML
b_reml <- gam(resp ~ s(x, y, k=60), data=gr[ss_ind, ], method="REML")
summary(b_reml)
gam.check(b_reml)

pred_reml <- predict(b_reml, newdata=gr)


## now setup the neighbourhoods for NCV
nei <- list()
nei$k <- c()
nei$m <- c()
mlength <- 0

# math is hard, so setup a matrix with indices in them :)
# lol this code is horrible!
ind_mat <- t(apply(matrix((n_grid^2):1, n_grid, n_grid, byrow=TRUE),
                   1, \(x) x[length(x):1]))

#TODO this with distances, just figure out the resolution (pixels per meter, or whatever) and convert distance to pixels
for(i in 1:n_grid){ # indexing y axis
  for(j in 1:n_grid){ # indexing x axis
    # pmin/pmax here to deal with index bounds
    # unique deals with possible duplicates due to pmax/pmin
    these_inds <- unique(as.vector(
      ind_mat[pmin(n_grid, pmax(1, (i-2):(i+2))),
              pmin(n_grid, pmax(1, (j-2):(j+2)))]))
    nei$m <- c(nei$m, mlength + length(these_inds))
    nei$k <- c(nei$k, these_inds)
    
    # nei$m are positions, so keep track of where we are
    mlength <- mlength + length(these_inds)
  }
}

# I don't see details of the prediction set
# let's say it's the the same as the leave-out data?
nei$i <- nei$k
nei$mi <- nei$m

# plot to check I didn't mess this up!
#TODO check if neighborhoods are overlapping
pl <- list()
# for(pli in sample(1:nrow(gr), 9)){
for(pli in c(10:18)) {
  pl[[paste0(pli)]] <- ggplot(gr, aes(x=x, y=y, fill=resp)) +
    geom_raster() +
    geom_point(aes(x=x, y=y), size=2,
               data=gr[nei$k[(nei$m[pli-1]+1):nei$m[pli]], ]) +
    scale_fill_viridis_c() +
    coord_equal() +
    theme_minimal()
}

patchwork::wrap_plots(pl, ncol=3, nrow=3) + plot_layout(guides = "collect", axes = "collect")

# (phew! that took a while to work out)


# now fit the NCV model
# could also set nei=NULL here to see the LOO performance
b_ncv <- gam(resp ~ s(x, y, k=60), data=gr[ss_ind, ], method="NCV", nei=nei)

summary(b_ncv)
gam.check(b_ncv)

pred_ncv <- predict(b_ncv, newdata=gr)


# plot some results

# predictions
par(mfrow=c(3,2))

# difference between response and predictions
diff_reml_m <- matrix(gr$resp-pred_reml, nrow=n_grid, ncol=n_grid,byrow=TRUE)
diff_ncv_m <- matrix(gr$resp-pred_ncv, nrow=n_grid, ncol=n_grid, byrow=TRUE)

# get a common scale for the "error" column
zlim <- range(c(diff_ncv_m, diff_reml_m, err2))


# first plot truth
t_mat <- matrix(gr$z, nrow=n_grid, ncol=n_grid, byrow=TRUE)
image(t_mat, main="truth", col=viridisLite::viridis(250), asp=1)
contour(t_mat, add=TRUE)

# error term
image(err2, main="correlated errors (8 neighbours)", zlim=zlim,
      col=viridisLite::viridis(250), asp=1)

# REML prediction
pred_reml_m <- matrix(pred_reml, nrow=n_grid, ncol=n_grid, byrow=TRUE)
image(pred_reml_m, main="REML", col=viridisLite::viridis(250), asp=1)
contour(pred_reml_m, add=TRUE)



# plot difference for REML
image(diff_reml_m, main="response - REML prediction", zlim=zlim,
      col=viridisLite::viridis(250), asp=1)

# NCV prediction
pred_ncv_m <- matrix(pred_ncv, nrow=n_grid, ncol=n_grid, byrow=TRUE)
image(pred_ncv_m, main="NCV", col=viridisLite::viridis(250), asp=1)
contour(pred_ncv_m, add=TRUE)


# plot difference between response and NCV prediction
image(diff_ncv_m, main="response - NCV prediction", zlim=zlim,
      col=viridisLite::viridis(250), asp=1)

