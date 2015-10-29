library(rgeos)
library(maptools)
library(ggplot2)
library(dplyr)
library(stringr)
library(reshape2)

#a <- read.csv("india.csv")

educationalLevels <- function() {
    a <- read.csv("education.csv")
    colnames(a) <- gsub("Educational.level...","",colnames(a))
    
    
    a$Area.Name <-gsub("State - ","",a$Area.Name)
    a$Area.Name <- gsub("\\d+","",a$Area.Name)
    
    # Remove trailing spaces
    a$Area.Name <- gsub("[[:space:]]*$","",a$Area.Name)
    
    b <- filter(a,Area.Name=="INDIA" & Total..Rural..Urban=="Rural")
    b <- filter(a,Area.Name=="KERALA" & Total..Rural..Urban=="Rural")
    
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
}

#######################################################################################
#
#
#
##########################################################################################

bar <- function() {
    a <- read.csv("education.csv")
    colnames(a) <- gsub("Educational.level...","",colnames(a))
    
    
    a$Area.Name <-gsub("State - ","",a$Area.Name)
    a$Area.Name <- gsub("\\d+","",a$Area.Name)
    
    # Remove trailing spaces
    a$Area.Name <- gsub("[[:space:]]*$","",a$Area.Name)
    b <- filter(a,Area.Name=="INDIA" & Total..Rural..Urban=="Total")
    # Select colums from 8 - 21
    c <- b[,7:19]
    # Set names
    names(c) <-c("Age","Persons","Males","Females","PersonsEdu","MalesEdu",
                 "FemalesEdu","IlliteratePersons","IlliterateMales",
                 "IlliterateFemales","LiteratePersons","LiterateMales","LiterateFemales")
    
    males <- select(c,matches("Males",ignore.case=FALSE))
    females <- select(c,matches("Females",ignore.case=FALSE))
    persons <- select(c,matches("Persons",ignore.case=FALSE))
    
    
    # Calculate males percent as percent of total males
    IndiaMalesPercent <- males[,2:4]/males[,1]*100
    # Calculate females percent as percent of total females
    IndiaFemalesPercent <- females[,2:4]/females[,1]*100
    # Calculate persons percent as percent of total persons
    IndiaPersonsPercent <- persons[,2:4]/persons[,1]*100
    
    # Add the age column
    IndiaMalesPercent <- cbind(c[1],IndiaMalesPercent)
    IndiaFemalesPercent <- cbind(c[1],IndiaFemalesPercent)
    IndiaPersonsPercent <- cbind(c[1],IndiaPersonsPercent)
    
    # Drop the 1st row
    IndiaMalesPercent <- IndiaMalesPercent[2:length(rownames(IndiaMalesPercent)),]
    IndiaFemalesPercent <- IndiaFemalesPercent[2:length(rownames(IndiaFemalesPercent)),]
    IndiaPersonsPercent <- IndiaPersonsPercent[2:length(rownames(IndiaPersonsPercent)),]
    
    
    
    
    #Use a color palette
    pal <- colorRampPalette(c("blue","red"))
    colors=pal(22)
    
    barplot(IndiaMalesPercent$MalesEdu,names.arg=IndiaMalesPercent$Age,col=colors)
    barplot(IndiaFemalesPercent$FemalesEdu,names.arg=IndiaFemalesPercent$Age,col=colors)
    barplot(IndiaPersonsPercent$PersonsEdu,names.arg=IndiaPersonsPercent$Age,col=colors)
    
   
    
    indiaPersons <- barplot(IndiaPersonsPercent$PersonsEdu,names.arg=IndiaPersonsPercent$Age,
                       col="white",border=NA)
    
    with(data=IndiaPersonsPercent,lines(indiaPersons,PersonsEdu,col="black"))
    with(data=IndiaMalesPercent,lines(indiaPersons,MalesEdu,col="blue"))
    with(data=IndiaFemalesPercent,lines(indiaPersons,FemalesEdu,col="red"))
    
    ### State 
    b <- filter(a,Area.Name=="KERALA" & Total..Rural..Urban=="Total")
    
    # Select colums from 8 - 21
    c <- b[,7:19]
    # Set names
    names(c) <-c("Age","Persons","Males","Females","PersonsEdu","MalesEdu",
                 "FemalesEdu","IlliteratePersons","IlliterateMales",
                 "IlliterateFemales","LiteratePersons","LiterateMales","LiterateFemales")
    
    males <- select(c,matches("Males",ignore.case=FALSE))
    females <- select(c,matches("Females",ignore.case=FALSE))
    persons <- select(c,matches("Persons",ignore.case=FALSE))
    
    
    # Calculate males percent as percent of total males
    malesPercent <- males[,2:4]/males[,1]*100
    # Calculate females percent as percent of total females
    femalesPercent <- females[,2:4]/females[,1]*100
    # Calculate persons percent as percent of total persons
    personsPercent <- persons[,2:4]/persons[,1]*100
    
    # Add the age column
    malesPercent <- cbind(c[1],malesPercent)
    femalesPercent <- cbind(c[1],femalesPercent)
    personsPercent <- cbind(c[1],personsPercent)
    
    # Drop the 1st row
    malesPercent <- malesPercent[2:length(rownames(malesPercent)),]
    femalesPercent <- femalesPercent[2:length(rownames(femalesPercent)),]
    personsPercent <- personsPercent[2:length(rownames(personsPercent)),]
    
    
    
    
    #Use a color palette
    pal <- colorRampPalette(c("yellow","blue"))
    colors=pal(22)
    
    barplot(malesPercent$MalesEdu,names.arg=malesPercent$Age,col=colors)
    with(data=IndiaMalesPercent,lines(indiaPersons,MalesEdu,col="black",lty=3,lwd=4))
    
    barplot(femalesPercent$FemalesEdu,names.arg=femalesPercent$Age,col=colors)
    with(data=IndiaFemalesPercent,lines(indiaPersons,FemalesEdu,col="black",lty=4,lwd=4))
    
    
    barplot(personsPercent$PersonsEdu,names.arg=personsPercent$Age,col=colors)
    with(data=IndiaPersonsPercent,lines(indiaPersons,PersonsEdu,col="black",lty=4,lwd=4))
    
    
    persons <- barplot(personsPercent$PersonsEdu,names.arg=personsPercent$Age,
                       col="white",border=NA)
    
    with(data=personsPercent,lines(persons,PersonsEdu,col="black"))
    with(data=malesPercent,lines(persons,MalesEdu,col="blue"))
    with(data=femalesPercent,lines(persons,FemalesEdu,col="red"))
    
    
    
}

