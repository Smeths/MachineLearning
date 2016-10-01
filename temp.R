# Cleaning data
library(ggplot2)
library(grid)
library(gridExtra)

destfile <- "training.csv"
training <- read.csv(destfile)
blanks <- as.vector(apply(training,2,function(x) sum(x=="")==19216))
blanknas <- ifelse(is.na(blanks),TRUE,blanks)
blanknas[160] <- TRUE
training <- training[,blanknas]
training <- training[!training$kurtosis_roll_belt == "",]
type <- split(names(training),sapply(training,function(x) paste(class(x), collaspe=" ")))
training <- training[,c(type$integer,type$numeric,"classe")]
belt <- training[,c(grep("belt",names(training)),68)]
forearm <- training[,c(grep("forearm",names(training)),68)]
arm <- training[,c(grep("_arm",names(training)),68)]
dumbbell <- training[,c(grep("dumbbell",names(training)),68)]

destfile <- "testing.csv"
testing <- read.csv(destfile)


# Plotting belt data
plots <- list()
b1 <- ggplot(data=belt,aes(max_picth_belt,colour=classe)) + geom_density() + theme(axis.title.y=element_blank(),
                                                                                   axis.text.y=element_blank(),
                                                                                   axis.ticks.y=element_blank(),
                                                                                   axis.ticks.x=element_blank(),
                                                                                   legend.position='none')
for(nm in names(belt[,1:16])) {
     plots[[nm]] <- ggplot(belt, aes_string(x=nm,colour="classe")) + geom_density()+ theme(axis.title.y=element_blank(),
                                                                                           axis.text.y=element_blank(),
                                                                                           axis.ticks.y=element_blank(),
                                                                                           axis.ticks.x=element_blank(),
                                                                                           legend.position='none')
}

grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]],
             plots[[5]], plots[[6]], plots[[7]], plots[[8]],
             plots[[9]], plots[[10]], plots[[11]], plots[[12]],
             plots[[13]], plots[[14]], plots[[15]], plots[[16]],ncol=4)

bt <- textGrob("Belt Group")
at <- textGrob("Arm Group")
lay <- rbind(c(1,1,1,1,1),c(2,3,4,5,6),c(7,7,7,7,7),c(8,9,10,11,12))
grid.arrange(bt,b1,b2,b3,b4,b10,at,a1,a5,a6,a10,a11,ncol=6,heights=c(1,5,1,5),layout_matrix=lay)