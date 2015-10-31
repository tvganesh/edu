library(rgeos)
library(maptools)
library(ggplot2)
library(dplyr)
library(stringr)
library(reshape2)
library(RColorBrewer)


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



districtEdu <- function(state){

    ind_dist <- readShapeSpatial("./IND_adm/IND_adm2.shp")
    district_df = ind_dist@data
    state_dist_df = data.frame(district_df[grep(state,district_df$NAME_1),])
    polygon_list = list()
    for (istr in rownames(state_dist_df)){
        i = as.numeric(istr) + 1
        tmp = ind_dist@polygons[i]
        polygon_list = c(polygon_list,tmp)
    }
    
    
    # construct a new shape file with the  districts
    dist_spatial = SpatialPolygons(polygon_list,1:length(polygon_list))
    dist_spatial_frame = SpatialPolygonsDataFrame(dist_spatial,data=state_dist_df)
    shpFile <- paste( state,".shp",sep="")
    districtDir <- paste("./district/",shpFile,sep="")
    writeSpatialShape(dist_spatial_frame,districtDir)
    dist_df = readShapePoly(districtDir)
    
    plot(dist_df)
    
    dist <- fortify(dist_df, region = "NAME_2")
    
    csvFile <- paste(state,".csv",sep="")
    csvDir <- paste("./data/",csvFile,sep="")
    stateData <- read.csv(csvDir)
    a <- filter(stateData,Age.group=="All ages")
    b <- filter(a,grepl("District",Area.Name))
    c <- filter(b,Total..Rural..Urban=="Total")
    c$Area.Name <-gsub("District - ","",c$Area.Name)
    c$Area.Name <- gsub("\\d+","",c$Area.Name)
    c$Area.Name <- gsub(" |\\*","",c$Area.Name)
  
    print("Here")
    df <- NULL
    df <- c[,5:13]
    names(df) <-c("Area.Name","Total..Rural..Urban", "Age.group", "Persons","Males","Females",
                 "PersonsEdu","MalesEdu", "FemalesEdu")
    
    df$PersonsEdu <- df$PersonsEdu/df$Persons * 100
    df$MalesEdu <- df$MalesEdu/df$Males * 100
    df$FemalesEdu <- df$FemalesEdu/df$Females * 100
    m= max(df$PersonsEdu)
    n = min(df$PersonsEdu)
    mid = (m+n)/2
    
    length(intersect(df$Area.Name,unique(dist$id)))
    setdiff(df$Area.Name,unique(dist$id))
    setdiff(unique(dist$id),df$Area.Name)
    
    if(state == "Tamil Nadu"){
        df[df$Area.Name=="TheNilgiris",]$Area.Name = "Nilgiris"
        df[df$Area.Name=="Viluppuram",]$Area.Name = "Villupuram"
        df[df$Area.Name=="Tiruchirappalli",]$Area.Name = "Tiruchchirappalli"
        df[df$Area.Name=="Thoothukkudi",]$Area.Name = "Thoothukudi"
        df[df$Area.Name=="Tirunelveli",]$Area.Name = "Tirunelveli Kattabo"
    } else if(state == "Kerala"){
        df[df$Area.Name=="Pathanamthitta",]$Area.Name = "Pattanamtitta"
    } else if(state == "Andhra Pradesh"){
        df[df$Area.Name=="Visakhapatnam",]$Area.Name = "Vishakhapatnam"
        df[df$Area.Name=="EastGodavari",]$Area.Name = "East Godavari"
        df[df$Area.Name=="WestGodavari",]$Area.Name = "West Godavari"
    } else if(state == "Arunachal Pradesh"){
        df[df$Area.Name=="WestKameng",]$Area.Name = "West Kameng"
        df[df$Area.Name=="EastKameng",]$Area.Name = "East Kameng"
        df[df$Area.Name=="PapumPare",]$Area.Name = "Papum Pare"
        df[df$Area.Name=="LowerSubansiri",]$Area.Name = "Lower Subansiri"
        df[df$Area.Name=="UpperSubansiri",]$Area.Name = "Upper Subansiri"
        df[df$Area.Name=="WestSiang",]$Area.Name = "West Siang"
        df[df$Area.Name=="EastSiang",]$Area.Name = "East Siang"
        df[df$Area.Name=="UpperSiang",]$Area.Name = "Upper Siang"
        df[df$Area.Name=="DibangValley",]$Area.Name = "DibangValley"
    
    } else if(state == "Assam"){
        df[df$Area.Name=="Dhubri",]$Area.Name = "Dhuburi"
        df[df$Area.Name=="KarbiAnglong",]$Area.Name = "Karbi Anglong"
        df[df$Area.Name=="NorthCacharHills",]$Area.Name = "North Cachar Hills"
    } else if(state == "Bihar") {
        df[df$Area.Name=="PashchimChamparan",]$Area.Name = "Pashchim Champaran"
        df[df$Area.Name=="PurbaChamparan",]$Area.Name = "Purba Champaran"
        df[df$Area.Name=="Kaimur(Bhabua)",]$Area.Name = "Bhabua"
    } else if(state == "Gujarat") {
        df[df$Area.Name=="BanasKantha",]$Area.Name = "Banas Kantha"
        df[df$Area.Name=="SabarKantha",]$Area.Name = "Sabar Kantha"
        df[df$Area.Name=="PanchMahals",]$Area.Name = "Panch Mahals"
        df[df$Area.Name=="Dohad",]$Area.Name = "Dahod"
        df[df$Area.Name=="TheDangs",]$Area.Name = "The Dangs"
    } else if(state == "Chhattisgarh") {
        df[df$Area.Name=="Rajnandgaon",]$Area.Name = "Raj Nandgaon"
    } else if(state == "Haryana") {
        df[df$Area.Name=="Yamunanagar",]$Area.Name = "Yamuna Nagar"
        df[df$Area.Name=="Sonipat",]$Area.Name = "Sonepat"
    } else if(state == "Himachal Pradesh") {
        df[df$Area.Name=="Lahul&Spiti",]$Area.Name = "Lahul and Spiti"
        
    }else if(state == "Jharkhand") {
        df[df$Area.Name=="Kodarma",]$Area.Name = "Koderma"
        df[df$Area.Name=="Pakaur",]$Area.Name = "Pakur"
        df[df$Area.Name=="PashchimiSinghbhum",]$Area.Name = "Pashchim Singhbhum"
        df[df$Area.Name=="PurbiSinghbhum",]$Area.Name = "Purba Singhbhum"
        
    } else if(state == "Karnataka") {
        df[df$Area.Name=="UttaraKannada",]$Area.Name = "Uttar Kannand"
        df[df$Area.Name=="Bangalore",]$Area.Name = "Bangalore Urban"
        df[df$Area.Name=="BangaloreRural",]$Area.Name = "Bangalore Rural"
        df[df$Area.Name=="DakshinaKannada",]$Area.Name = "Dakshin Kannad"
        df[df$Area.Name=="Chamarajanagar",]$Area.Name = "Chamrajnagar"
        
    } else if(state == "Madhya Pradesh") {
        df[df$Area.Name=="WestNimar",]$Area.Name = "West Nimar"
        df[df$Area.Name=="EastNimar",]$Area.Name = "East Nimar"
        df[df$Area.Name=="Narsimhapur",]$Area.Name = "Narsinghpur"
    } else if(state == "Mahrashtra") {
        df[df$Area.Name=="Mumbai(Suburban)",]$Area.Name = "Greater Bombay"
        df[df$Area.Name=="Mumbai",]$Area.Name = "Greater Bombay"
        df[df$Area.Name=="Ahmadnagar",]$Area.Name = "Ahmednagar"
        df[df$Area.Name=="Gadchiroli",]$Area.Name = "Garhchiroli"
    } else if(state == "Orissa") {
        df[df$Area.Name=="Bargarh",]$Area.Name = "Baragarh"
        df[df$Area.Name=="Debagarh",]$Area.Name = "Deogarh"
        df[df$Area.Name=="Kendujhar",]$Area.Name = "Keonjhar"
        df[df$Area.Name=="Jagatsinghapur",]$Area.Name = "Jagatsinghpur"
        df[df$Area.Name=="Jajapur",]$Area.Name = "Jajpur"
        df[df$Area.Name=="Anugul",]$Area.Name = "Angul"
        df[df$Area.Name=="Baudh",]$Area.Name = "Boudh"
        df[df$Area.Name=="Sonapur",]$Area.Name = "Sonepur"
        df[df$Area.Name=="Balangir",]$Area.Name = "Bolangir"
        df[df$Area.Name=="Nabarangapur",]$Area.Name = "Nabarangpur"
    } else if(state == "Punjab") {
        df[df$Area.Name=="Nawanshahr",]$Area.Name = "Nawan Shehar"
        df[df$Area.Name=="FatehgarhSahib",]$Area.Name = "Fatehgarh Sahib"
    } else if(state == "Punjab") {
        df[df$Area.Name=="SawaiMadhopur",]$Area.Name = "Sawai Madhopur"
    } else if(state == "Uttar Pradesh") {
        df[df$Area.Name=="JyotibaPhuleNagar",]$Area.Name = "Jyotiba Phule Nagar"
        df[df$Area.Name=="GautamBuddhaNagar",]$Area.Name = "Gautam Buddha Nagar"
        df[df$Area.Name=="Budaun",]$Area.Name = "Badaun"
        df[df$Area.Name=="Kheri",]$Area.Name = "Lakhimpur Kheri"
        df[df$Area.Name=="RaeBareli",]$Area.Name = "Rae Bareli"
        df[df$Area.Name=="KanpurDehat",]$Area.Name = "Kanpur Dehat"
        df[df$Area.Name=="KanpurNagar",]$Area.Name = "Kanpur"
        df[df$Area.Name=="Barabanki",]$Area.Name = "Bara Banki"
        df[df$Area.Name=="AmbedkarNagar",]$Area.Name = "Ambedkar Nagar"
        df[df$Area.Name=="Shrawasti",]$Area.Name = "Shravasti"
        df[df$Area.Name=="Siddharthnagar",]$Area.Name = "Siddharth Nagar"
        df[df$Area.Name=="SantKabirNagar",]$Area.Name = "Sant Kabir Nagar"
        df[df$Area.Name=="SantRavidasNagar",]$Area.Name = "Sant Ravi Das Nagar"
        
    } else if(state == "Uttaranchal") {
        df[df$Area.Name=="Rudraprayag",]$Area.Name = "Rudra Prayag"
        df[df$Area.Name=="TehriGarhwal",]$Area.Name = "Tehri Garhwal"
        df[df$Area.Name=="Dehradun",]$Area.Name = "Dehra Dun"
        df[df$Area.Name=="Garhwal",]$Area.Name = "Pauri Garhwal"
        df[df$Area.Name=="Nainital",]$Area.Name = "Naini Tal" 
        df[df$Area.Name=="UdhamSinghNagar",]$Area.Name = "Udham Singh Nagar"
        df[df$Area.Name=="Hardwar",]$Area.Name = "Haridwar"
    } else if(state == "West Bengal") {
        df[df$Area.Name=="KochBihar",]$Area.Name = "Kochbihar"
        df[df$Area.Name=="UttarDinajpur",]$Area.Name = "Uttar Dinajpu"
        df[df$Area.Name=="DakshinDinajpur",]$Area.Name = "Dakshin Dinajpur"
        df[df$Area.Name=="NorthTwentyFourParganas",]$Area.Name = "North 24 Parganas"
        df[df$Area.Name=="SouthTwentyFourParganas",]$Area.Name = "South 24 Parganas"
        df[df$Area.Name=="Medinipur",]$Area.Name = "West Midnapore"
        df[df$Area.Name=="Medinipur",]$Area.Name = "East Midnapore"
    }
    
        
        # Select the districts with lowest literacy
    m <- head(arrange(df,PersonsEdu),5)
    lowestLiteracy <- paste(m$Area.Name,"(",round(m$PersonsEdu,1),")",sep="")
    
    # Get the min/max latitude and longitude for plotting districts with lowest literacy
    # This is obtained from the fortified data frame 
    minLat= min(dist$lat)
    maxLat =max(dist$lat)
    minLong = min(dist$long)
    maxLong = max(dist$long)
    x = minLong+0.5
    y= minLat + 1.0
    # Create a data frame to primt the top 5 ofenders
    labels <- data.frame(
        xc = c(x,x,x,x,x), 
        yc = c(y,y-.2,y-0.4,y-0.6,y-.8), 
        label = as.vector(lowestLiteracy) 
        
    )
   
    
    print("Here1")
    atitle=paste("Literacy in the state of ", state)
    print(dim(df))
    print(df$PersonsEdu)
    ggplot() + geom_map(data = df, aes(map_id = Area.Name, fill = PersonsEdu),  
                        ,map = dist,color="black",size=0.25) + 
        expand_limits(x = dist$long, y = dist$lat) +  
        scale_fill_distiller(name="Percent", palette = "YlGn")+
        labs(title=atitle)+
    
        #geom_text(aes(label="Bottom 5 districts(literacy)",x+1,y+0.2),colour="blue")+
        geom_text(data = labels, aes(x = xc, y = yc, label = label))+
        #geom_text(aes(label="Data source:https://data.gov.in",maxLong-1,minLat+0.1)) +
        xlab("Longitude") + ylab("Latitude")
    
    
    
    
}

literacyIndia <- function()
{
    #a <- read.csv("india.csv")
    #write.csv(b,file="india.csv")
    #b <- filter(a,Area.Name=="INDIA" & Total..Rural..Urban=="Total") 
    b <- read.csv("education.csv")
    c <- filter(b,Age.group=="All ages" & Total..Rural..Urban=="Total")
    
    males <- select(c,matches("Males",ignore.case=FALSE))
    females <- select(c,matches("Females",ignore.case=FALSE))
    persons <- select(c,matches("Persons",ignore.case=FALSE))
    
    
    # Calculate males percent as percent of total males
    malesPercent <- males[,2:14]/males[,1]*100
    names(malesPercent) <- c("AttendingEdu","Illiterate","Literate","LiterateNoEdu)",
                             "BelowPrimary","Primary","Middle","MatricSecondary","HigherSecIntmdtPU",
                             "NonTechnicalDiploma","TechnicalDiploma","GraduateAndAbove","Unclassified")
    # Calculate females percent as percent of total females
    femalesPercent <- females[,2:14]/females[,1]*100
    names(femalesPercent) <- c("AttendingEdu","Illiterate","Literate","LiterateNoEdu)",
                               "BelowPrimary","Primary","Middle","MatricSecondary","HigherSecIntmdtPU",
                               "NonTechnicalDiploma","TechnicalDiploma","GraduateAndAbove","Unclassified")
    # Calculate persons percent as percent of total persons
    personsPercent <- persons[,2:14]/persons[,1]*100
    names(personsPercent) <- c("AttendingEdu","Illiterate","Literate","LiterateNoEdu)",
                               "BelowPrimary","Primary","Middle","MatricSecondary","HigherSecIntmdtPU",
                               "NonTechnicalDiploma","TechnicalDiploma","GraduateAndAbove","Unclassified")
    
    # Add the age column
    malesPercent <- cbind(c[5],malesPercent)
    femalesPercent <- cbind(c[5],femalesPercent)
    personsPercent <- cbind(c[5],personsPercent)
    
    
    
   
    
   
    
    
    # Remove the row corresponding to India
    malesPercent <- malesPercent[2:length(rownames(malesPercent)),]
    malesPercent$Area.Name <-gsub("State - ","",malesPercent$Area.Name)
    malesPercent$Area.Name <- gsub("\\d+","",malesPercent$Area.Name)
    
    # Remove trailing spaces
    malesPercent$Area.Name <- gsub("[[:space:]]*$","",malesPercent$Area.Name)
    
    ind <- readShapeSpatial("./India_SHP/INDIA.shp")
    plot(ind)
    
    ind <- fortify(ind, region = "ST_NAME")
    
    
    # Set the names as in the map
    malesPercent[malesPercent$Area.Name=="JAMMU & KASHMIR",]$Area.Name = "Jammu And Kashmir"
    
    malesPercent[malesPercent$Area.Name=="HIMACHAL PRADESH",]$Area.Name = "Himachal Pradesh"
    malesPercent[malesPercent$Area.Name=="PUNJAB",]$Area.Name = "Punjab"
    malesPercent[malesPercent$Area.Name=='UTTARANCHAL',]$Area.Name = "Uttarakhand"
    malesPercent[malesPercent$Area.Name=="CHANDIGARH",]$Area.Name = "CHANDIGARH"
    
    malesPercent[malesPercent$Area.Name=="HARYANA",]$Area.Name = "Haryana"
    
    malesPercent[malesPercent$Area.Name=="DELHI",]$Area.Name = "Nct Of Delhi"
    malesPercent[malesPercent$Area.Name=="RAJASTHAN",]$Area.Name = "Rajasthan"
    malesPercent[malesPercent$Area.Name=="UTTAR PRADESH",]$Area.Name = "Uttar Pradesh"
    malesPercent[malesPercent$Area.Name=="BIHAR",]$Area.Name = "Bihar"
    malesPercent[malesPercent$Area.Name=="SIKKIM",]$Area.Name = "Sikkim"
    
    malesPercent[malesPercent$Area.Name=="ARUNACHAL PRADESH",]$Area.Name = "Arunachal Pradesh"
    malesPercent[malesPercent$Area.Name=="NAGALAND",]$Area.Name = "Nagaland"
    malesPercent[malesPercent$Area.Name=="MANIPUR",]$Area.Name = "Manipur"
    malesPercent[malesPercent$Area.Name=="MIZORAM",]$Area.Name = "Mizoram"
    
    malesPercent[malesPercent$Area.Name=="TRIPURA",]$Area.Name = "Tripura"
    malesPercent[malesPercent$Area.Name=="MEGHALAYA",]$Area.Name = "Meghalaya"
    malesPercent[malesPercent$Area.Name=="ASSAM",]$Area.Name = "Assam"
    malesPercent[malesPercent$Area.Name=="WEST BENGAL",]$Area.Name = "West Bengal"
    malesPercent[malesPercent$Area.Name=="JHARKHAND",]$Area.Name = "Jharkhand"
    
    malesPercent[malesPercent$Area.Name=="ORISSA",]$Area.Name = "Orissa"
    malesPercent[malesPercent$Area.Name=="CHHATTISGARH",]$Area.Name = "Chhattisgarh"
    malesPercent[malesPercent$Area.Name=="MADHYA PRADESH",]$Area.Name = "Madhya Pradesh"
    
    malesPercent[malesPercent$Area.Name=="GUJARAT",]$Area.Name = "Gujarat"
    malesPercent[malesPercent$Area.Name=="DAMAN & DIU",]$Area.Name = "DAMAN AND DIU"
    malesPercent[malesPercent$Area.Name=="DADRA & NAGAR HAVELI",]$Area.Name = "DADRA AND NAGAR HAVELI"
    
    malesPercent[malesPercent$Area.Name=="MAHARASHTRA",]$Area.Name = "Maharashtra"
    malesPercent[malesPercent$Area.Name=="ANDHRA PRADESH",]$Area.Name = "Andhra Pradesh"
    malesPercent[malesPercent$Area.Name=="KARNATAKA",]$Area.Name = "Karnataka"
    malesPercent[malesPercent$Area.Name=="GOA",]$Area.Name = "Goa"
    
    malesPercent[malesPercent$Area.Name=="LAKSHADWEEP",]$Area.Name = "LAKSHADWEEP"
    malesPercent[malesPercent$Area.Name=="KERALA",]$Area.Name = "Kerala"
    malesPercent[malesPercent$Area.Name=="TAMIL NADU",]$Area.Name = "Tamil Nadu"
    malesPercent[malesPercent$Area.Name=="PONDICHERRY",]$Area.Name = "Pondicherry"
    malesPercent[malesPercent$Area.Name=="ANDAMAN & NICOBAR ISLANDS",]$Area.Name = "ANDAMAN AND NICOBAR ISLANDS"
    
    i= max(malesPercent$AttendingEdu)
    j = min(malesPercent$AttendingEdu)
    mid = (i+j)/2
    
    ggplot() + geom_map(data = malesPercent, aes(map_id = Area.Name, fill = AttendingEdu),
                        map = ind,,color="black",size=0.25) + 
        expand_limits(x = ind$long, y = ind$lat) + 
        scale_fill_distiller(name="Percent", palette = "YlGn")
        #scale_fill_gradient2(low = "grey",                                                                           
                             #mid = "blue", midpoint = mid, high = "red", limits = c(j, i)) 
    
    
    
    ggplot() + geom_map(data = df, aes(map_id = Area.Name, fill = PersonsEdu),  
                        ,map = dist,color="black",size=0.25) + 
        expand_limits(x = dist$long, y = dist$lat) +  
        scale_fill_distiller(name="Percent", palette = "YlGn")+
    
    
    
    
    
    
    
    
    
    
    
}

