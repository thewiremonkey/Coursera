
#make up some data
x <- 10; y <- 10; g <- 5

set.seed(1969)
dat <- matrix(rnorm((x + y) * g), ncol = x + y)

# apply() the t.test function to each row fo the matrix and extract
#just the p value

results <- apply(dat, 1, function(dat) {
        t.test(x = dat[1:x], y = dat[(x + 1):(x + y)])$p.value})

# if you want, you can bind p-values back into the matrix
matrixt<-data.table(cbind(dat, pvals = results))
View(matrixt)

