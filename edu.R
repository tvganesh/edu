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


b <- filter(a,Area.Name=="INDIA" & Total..Rural..Urban=="Total")
# Select colums from 8 - 21
c <- b[,8:21]
# Set names
names(c) <-c("Age","Persons","Males","Females","PersonsEdu","MalesEdu",
             "FemalesEdu","IlliteratePersons","IlliterateMales",
             "IlliterateFemales","LiteratePersons","LiterateMales","LiterateFemales")

# Calculate percentages as a sum of total population
d <- c[,3:14]/c[,2]*100

# Add the age column
d <- cbind(c[1],d)

#Use a color palette
pal <- colorRampPalette(c("blue","red"))
colors=pal(22)

# Drop the 1st row
d <- d[2:length(rownames(d)),]

persons <- barplot(d$PersonsEdu,names.arg=d$Age,col="white",border=NA)

with(data=d,lines(persons,PersonsEdu,col="black"))
with(data=d,lines(persons,MalesEdu,col="blue"))
with(data=d,lines(persons,FemalesEdu,col="red"))

persons <- barplot(d$PersonsEdu,names.arg=d$Age,col="white",border=NA)
males <- barplot(d$MalesEdu,names.arg=d$Age,col=colors)

females <- barplot(d$FemalesEdu,names.arg=d$Age,col=colors)
#d <-c[2:23,c(1,5)]
barplot(d$PersonsEdu)

barplot(d$Population.attending.educational.institutions...Persons,names.arg=d$Age.group)

# Store the mid point os the plots
m <-barplot(d$Population.attending.educational.institutions...Persons,names.arg=d$Age.group)
#draw a line 
lines(m,d$Population.attending.educational.institutions...Persons)


barplot(d$Population.attending.educational.institutions...Persons,names.arg=d$Age.group,col=colors)

