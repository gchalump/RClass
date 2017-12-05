library(ggplot2)

data(iris)

# select 1:100 for only Setosa an Versicolor
irissubdf <- iris[1:100, c(1, 3, 5)]
names(irissubdf) <- c("sepal", "petal", "species")
head(irissubdf)

ggplot(irissubdf, aes(x = sepal, y = petal)) +
  geom_point(aes(colour=species, shape=species), size = 3) +
  xlab("sepal length") +
  ylab("petal length") +
  ggtitle("Species vs sepal and petal lengths")

#label only 0 and 1 output
irissubdf[, 4] <- 1
irissubdf[irissubdf[, 3] == "setosa", 4] <- 0

#useonly lengths of sepal and petal as input (x)
x <- irissubdf[, c(1, 2)]
#use our 1/0 label as output(y)
y <- irissubdf[, 4]

#example functions
simplePerceptron(x,y,init_w = c(-2,1,2),eta = 0.5 ,max_epoch = 10 ,accepted_error = 0.01)
simplePerceptron(x,y,init_w = c(-2,1,2),eta = 0.5 ,max_epoch = 10 ,accepted_error = 0.005)
