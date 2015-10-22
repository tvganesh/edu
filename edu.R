library(rgeos)
library(maptools)
library(ggplot2)
library(dplyr)
library(stringr)
#a <- read.csv("education.csv")
#colnames(a) <- gsub("Educational.level...","",colnames(a))
a <- read.csv("india.csv")
#write.csv(b,file="india.csv")
b <- filter(a,Area.Name=="INDIA" & Total..Rural..Urban=="Total")
# Create a vector
#v <- c(7,23:46)
#c <- b[,v]

# Subset columns with persons
males <- select(b,matches("Males",ignore.case=FALSE))
females <- select(c,matches("Females",ignore.case=FALSE))
persons <- select(c,matches("Persons",ignore.case=FALSE))

j <- males/persons*100
k <- females/persons*100

age <- b[,8]
males <- cbind(age,males)

l <- dim(males)

for(i in 2:l[1]) {
    for(j in 2:l[2]) {
        print(i)
        print(j)
         males[i,j] <- males[i,j]/males[1,j]*100
    }
}
v <- c(1,7:15)
m1 <- males[2:23,v]
m2 <- melt(m1,id.vars="age")
ggplot(m2,aes(x=age,y=value,fill=variable)) + geom_bar(stat = "identity")

