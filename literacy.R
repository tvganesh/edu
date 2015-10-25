library(rgeos)
library(maptools)
library(ggplot2)
library(dplyr)
library(stringr)
library(reshape2)

#a <- read.csv("india.csv")

a <- read.csv("education.csv")
colnames(a) <- gsub("Educational.level...","",colnames(a))
b <- filter(a,Area.Name=="INDIA" & Total..Rural..Urban=="Rural")


# Subset columns with persons
males <- select(b,matches("Males",ignore.case=FALSE))
females <- select(b,matches("Females",ignore.case=FALSE))
persons <- select(b,matches("Persons",ignore.case=FALSE))



l <- dim(males)
for(i in 2:l[1]) {
    for(j in 2:l[2]) {
     
   
        #print(i)
        #print(j)
        males[i,j] <- males[i,j]/males[j,1]*100
    }
}

age <- b[,7]
males <- cbind(age,males)


v <- c(1,7:15)
m1 <- males[2:23,v]

# Needed to add a '0' so that the numeric and lexicographic ordering is fine
m1$age = as.character(m1$age)
m1$age[2] ="05"
m1$age[3] ="06"
m1$age[4] ="07"
m1$age[5] ="08"
m1$age[6] ="09"

m2 <- melt(m1,id.vars="age")
a <- factor(age,levels=age)
ggplot(m2,aes(x=age,y=value,fill=variable)) +     geom_bar(stat = "identity")

