library("ggplot2")
library("data.table")

#setwd:
setwd("/Users/seanmhendryx/Research/context/sorted_feature_families/")

#read in file, removing parentheses:
txt = gsub("[()]", "", readLines("Sorted_All_Scores_All_Scores_Sparsenss_numFeatures.csv"))
df = read.csv(text = txt, comment.char = "#")

DT = as.data.table(df)

# GET EQUATION AND R-SQUARED AS STRING
# SOURCE: http://goo.gl/K4yh
lm_eqn <- function(x,y){
    m <- lm(y ~ x);
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)~"="~r, 
         list(a = format(coef(m)[1], digits = 2), 
              b = format(coef(m)[2], digits = 2), 
             r = format((summary(m)$r.squared ^ .5), digits = 3)))
    as.character(as.expression(eq));                 
}

lm_eqnr2 <- function(df){
    m <- lm(y ~ x, df);
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
         list(a = format(coef(m)[1], digits = 2), 
              b = format(coef(m)[2], digits = 2), 
             r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));                 
}

# Example usage:
#library(ggplot2)
#df <- data.frame(x = c(1:100))
#df$y <- 2 + 3 * df$x + rnorm(100, sd = 40)
#p <- ggplot(data = df, aes(x = x, y = y)) +
#            geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
#            geom_point()
#p1 <- p + geom_text(x = 25, y = 300, label = lm_eqn(df), parse = TRUE)

p = ggplot(data = DT, mapping = aes(x = sparseness_mean, y = f1)) + geom_point() + theme_bw() + geom_smooth(method = "lm")
p = p + labs(x = "Feature Matrix Density", y = "F1 Score") + ggtitle("Feature Family Subset Classification Performance")
p = p + theme(plot.title = element_text(hjust = 0.5))
m = lm(DT$f1 ~ DT$sparseness_mean)
r = format(summary(m)$r.squared ^ .5, digits = 3)
text = paste0("r = ", r)
p = p + annotate("text",x = .05, y = .275, label = text)
p


p2 = ggplot(data = DT, mapping = aes(x = recall, y = f1)) + geom_point() + theme_bw() + geom_smooth(method = "lm")
p2 = p2 + labs(x = "Recall", y = "F1 Score") + ggtitle("Feature Family Subset Classification Performance")
p2 = p2 + theme(plot.title = element_text(hjust = 0.5))
m = lm(DT$f1 ~ DT$recall)
r = format(summary(m)$r.squared ^ .5, digits = 3)
text = paste0("r = ", r)
p2 = p2 + annotate("text",x = .3, y = .275, label = text)
p2


p3 = ggplot(data = DT, mapping = aes(x = precision, y = f1)) + geom_point() + theme_bw() + geom_smooth(method = "lm")
p3 = p3 + labs(x = "Precision", y = "F1 Score") + ggtitle("Feature Family Subset Classification Performance")
p3 = p3 + theme(plot.title = element_text(hjust = 0.5))
m = lm(DT$f1 ~ DT$precision)
r = format(summary(m)$r.squared ^ .5, digits = 3)
text = paste0("r = ", r)
p3 = p3 + annotate("text",x = .125, y = .275, label = text)
p3

p4 = ggplot(data = DT, mapping = aes(x = numFeatures_mean, y = f1)) + geom_point() + theme_bw() + geom_smooth(method = "lm")
p4 = p4 + labs(x = "Mean Number of Features", y = "F1 Score") + ggtitle("Feature Family Subset Classification Performance")
p4 = p4 + theme(plot.title = element_text(hjust = 0.5))
m = lm(DT$f1 ~ DT$numFeatures_mean)
r = format(summary(m)$r.squared ^ .5, digits = 3)
text = paste0("r = ", r)
p4 = p4 + annotate("text",x = .125, y = .275, label = text)
p4

p5 = ggplot(data = DT, mapping = aes(x = precision, y = recall)) + geom_point() + theme_bw() + geom_smooth(method = "lm")
p5 = p5 + labs(x = "Precision", y = "Recall") + ggtitle("Feature Family Subset Classification Performance")
p5 = p5 + theme(plot.title = element_text(hjust = 0.5))
m = lm(DT$recall ~ DT$precision)
r = format(summary(m)$r.squared ^ .5, digits = 3)
text = paste0("r = ", r)
p5 = p5 + annotate("text",x = .125, y = .275, label = text)
p5
#Simpson's paradox
