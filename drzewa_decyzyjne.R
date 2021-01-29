install.packages(rpart)
library(rpart)
df <- read.table("file:///h:/Windows7/Desktop/DATA MINING/SAheart.data", sep = ",", header=T,row.names = 1)


tree<-rpart(as.factor(chd)~.,data=df,cp=0.01,minsplit=5)
print(summary(tree))
plot(tree)
text(tree, use.n=T)

#c mediana zamiast sredniej
x0<- apply(df[,-c(5,10)],2,median)
table(df$famhist)
obs<- data.frame(famhist="Absent", t(as.matrix(x0)))
unname(predict(tree, newdata = obs, type = "class"))

plotcp(tree)
tree$cptable

tree.2 <- prune.rpart(tree, cp=0.04) #wybieramy taka alfe
plot(tree.2)
text(tree.2, use.n = T)

tree.info <- rpart(as.factor(chd)~.,data=df,cp=0.01,minsplit=5, parms=list(split="information"))
plot(tree.info)
text(tree.info, use.n = T)
#information cos powoiazane z entropia 


df <- read.table("file:///h:/Windows7/Desktop/DATA MINING/fitness.txt", header=T)

tree <- rpart(Oxygen~.,data=df,cp=0.01,
              minsplit=2)
plot(tree)
text(tree, use.n=T)
 
#la ktorego bigacza porob tlenu najwuekszy? dla teg oco  czas biegu >9 i puls <160 


obs<- data.frame(t(apply(df[,-3],2,median)))
predict(tree, newdata = obs)

tree$cptable
plotcp(tree)

tree.2p <- rpart(Oxygen~RunTime+Age, data = df, cp=0.02, minsplit=2)

n<- 100 #gestosc siatki 
age.x <- seq(35,60, length.out = 100)
RunTime.y <- seq(8,15, length.out = 100)

newdata.siatka <- expand.grid(Age= age.x, 
                              RunTime=RunTime.y)
newdata.siatka$Oxygen <- predict(tree.2p,
                          newdata = newdata.siatka)
persp(age.x, RunTime.y,
      matrix(newdata.siatka$Oxygen,n),
      theta=100, phi=30, col="lightblue",
      expand = 0.7)
























