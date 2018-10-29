#test de como mostrar ROC
# roc version 4 otro ejemplo usando ROCR libreria 
library(ROCR)
data(ROCR.hiv)
x   <- prediction(ROCR.hiv$hiv.nn$predictions, ROCR.hiv$hiv.nn$labels)
ROC <- performance(x, "tpr", "fpr")
plot(ROC, col = as.list(1:10))

# annotated text on ggplot2
library(ggplot2)

p <- ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  facet_grid(. ~ cyl) +
  theme(panel.spacing = unit(1, "lines"))
p

dat_text <- data.frame(
  label = c("4 cylinders", "6 cylinders", "8 cylinders"),
  cyl   = c(4, 6, 8)
)
p + geom_text(
  data    = dat_text,
  mapping = aes(x = -Inf, y = -Inf, label = label),
  hjust   = -0.1,
  vjust   = -1
)
#' modificando la posicion del texto 
dat_text <- data.frame(
  label = c("4 cylinders", "6 cylinders", "8 cylinders"),
  cyl   = c(4, 6, 8),
  x     = c(20, 27.5, 25),
  y     = c(4, 4, 4.5)
)

p + geom_text(
  data    = dat_text,
  mapping = aes(x = x, y = y, label = label)
)

#Notes:
#  You can use -Inf and Inf to position text at the edges of a panel.
# You can use hjust and vjust to adjust the text justification.
# The text label data frame dat_text should have a column that works with your facet_grid() or facet_wrap().

mtcars[, c("cyl", "am", "gear")] <- lapply(mtcars[, c("cyl", "am", "gear")], as.factor)

p <- ggplot(mtcars, aes(mpg, wt, group = cyl)) + 
  geom_line(aes(color=cyl)) +
  geom_point(aes(shape=cyl)) + 
  facet_grid(gear ~ am) +
  theme_bw()                                                                      
p 

#long cut way to find number of facets
len <- length(levels(mtcars$gear)) *  length(levels(mtcars$am))

vars <- data.frame(expand.grid(levels(mtcars$gear), levels(mtcars$am)))
colnames(vars) <- c("gear", "am")
dat <- data.frame(x = rep(15, len), y = rep(5, len), vars, labs=LETTERS[1:len])

p + geom_text(aes(x, y, label=labs, group=NULL),data=dat)