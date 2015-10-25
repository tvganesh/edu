library(rgeos)
library(maptools)
library(ggplot2)
library(dplyr)
library(stringr)
library(reshape2)

#b <- read.csv("education.csv")
#colnames(a) <- gsub("Educational.level...","",colnames(a))
a <- read.csv("india.csv")
#write.csv(b,file="india.csv")
b <- filter(a,Area.Name=="INDIA" & Total..Rural..Urban=="Total")
# Create a vector
#v <- c(7,23:46)
#c <- b[,v]

# Subset columns with persons
males <- select(b,matches("Males",ignore.case=FALSE))
females <- select(b,matches("Females",ignore.case=FALSE))
persons <- select(b,matches("Persons",ignore.case=FALSE))


age <- b[,7]
males <- cbind(age,males)

l <- dim(males)

for(i in 2:l[1]) {
    for(j in 2:l[2]) {
        #print(i)
        #print(j)
         males[i,j] <- males[i,j]/males[1,j]*100
    }
}
v <- c(1,7:15)
m1 <- males[2:23,v]
m2 <- melt(m1,id.vars="age")
ggplot(m2,aes(x=age,y=value,fill=variable)) + geom_bar(stat = "identity")

#######################################################################
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

########################################################
ind_dist <- readShapeSpatial("./IND_adm/IND_adm2.shp")
#ind <- fortify(ind_dist, region = "NAME_1")

###################*******##################################################
### The snippet below is taken 
####from http://www.cse.iitb.ac.in/~srirampc/blog/2012/09/21/visualizing_census_data.html
district_df = ind_dist@data
tn_dist_df = data.frame(district_df[grep("Tamil",district_df$NAME_1),])
polygon_list = list()
for (istr in rownames(tn_dist_df)){
    i = as.numeric(istr) + 1
    tmp = ind_dist@polygons[i]
    polygon_list = c(polygon_list,tmp)
}


# construct a new shape file with only TN's districts
dist_spatial = SpatialPolygons(polygon_list,1:30)
dist_spatial_frame = SpatialPolygonsDataFrame(dist_spatial,data=tn_dist_df)
writeSpatialShape(dist_spatial_frame,"tn_dist_state.shp")
dist_df = readShapePoly("tn_dist_state.shp")
#plot(dist_df,add=TRUE) # should print a map of districts in TN
plot(dist_df)
###################*******##################################################
dist <- fortify(dist_df, region = "NAME_2")

tn <- read.csv("tn1.csv")
a <- filter(tn,Age.group=="All ages")
b <- filter(a,grepl("District",Area.Name))
c <- filter(b,Total..Rural..Urban=="Total")
c$Area.Name <-gsub("District - ","",c$Area.Name)
c$Area.Name <- gsub("\\d+","",c$Area.Name)
c$Area.Name <- gsub(" |\\*","",c$Area.Name)
#ind <- fortify(ind, region = "ST_NAME")


m= max(tot)
n = min(tot)
mid = (m+n)/2

length(intersect(c$Area.Name,unique(dist$id)))
setdiff(c$Area.Name,unique(dist$id))
setdiff(unique(dist$id),c$Area.Name)
c[c$Area.Name=="TheNilgiris",]$Area.Name = "Nilgiris"
c[c$Area.Name=="Viluppuram",]$Area.Name = "Villupuram"
c[c$Area.Name=="Tiruchirappalli",]$Area.Name = "Tiruchchirappalli"
c[c$Area.Name=="Thoothukkudi",]$Area.Name = "Thoothukudi"
c[c$Area.Name=="Tirunelveli",]$Area.Name = "Tirunelveli Kattabo"


ggplot() + geom_map(data = c, aes(map_id = Area.Name, fill = tot), 
                    map = dist) + expand_limits(x = dist$long, y = dist$lat) + 
    scale_fill_gradient2(low = "grey",                                                                           
                         mid = "light blue", midpoint = mid, high = "blue", limits = c(n, m))


##################################################################################
c <- filter(b,Age.group=="All ages" & Total..Rural..Urban=="Total")

l <- c[,5:21]
#Calculate percentages

#m <- l[,7:15]/l[,4]*100
m <- l[,7:15]
m[,1] <- l[,7]/l[,4] *100
m[,2] <- l[,8]/l[,5] *100
m[,3] <- l[,9]/l[,6] *100

# Illiterate
m[,4] <- l[,10]/l[,4] *100
m[,5] <- l[,11]/l[,5] *100
m[,6] <- l[,12]/l[,6] *100

#Literate
m[,7] <- l[,13]/l[,4] *100
m[,8] <- l[,14]/l[,5] *100
m[,9] <- l[,15]/l[,6] *100


#Bind the State column
m <- cbind(l$Area.Name,m)

# Fix column names
names(m) <-c("Area.Name","PersonsEdu","MalesEdu",
             "FemalesEdu","IlliteratePersons","IlliterateMales",
             "IlliterateFemales","LiteratePersons","LiterateMales","LiterateFemales")


# Remove the row corresponding to India
m <- m[2:length(rownames(m)),]
m$Area.Name <-gsub("State - ","",m$Area.Name)
m$Area.Name <- gsub("\\d+","",m$Area.Name)

# Remove trailing spaces
m$Area.Name <- gsub("[[:space:]]*$","",m$Area.Name)

ind <- readShapeSpatial("./India_SHP/INDIA.shp")
#plot(ind)

ind <- fortify(ind, region = "ST_NAME")


# Set the names as in the map
m[m$Area.Name=="JAMMU & KASHMIR",]$Area.Name = "Jammu And Kashmir"

m[m$Area.Name=="HIMACHAL PRADESH",]$Area.Name = "Himachal Pradesh"
m[m$Area.Name=="PUNJAB",]$Area.Name = "Punjab"
m[m$Area.Name=='UTTARANCHAL',]$Area.Name = "Uttarakhand"
m[m$Area.Name=="CHANDIGARH",]$Area.Name = "CHANDIGARH"

m[m$Area.Name=="HARYANA",]$Area.Name = "Haryana"

m[m$Area.Name=="DELHI",]$Area.Name = "Nct Of Delhi"
m[m$Area.Name=="RAJASTHAN",]$Area.Name = "Rajasthan"
m[m$Area.Name=="UTTAR PRADESH",]$Area.Name = "Uttar Pradesh"
m[m$Area.Name=="BIHAR",]$Area.Name = "Bihar"
m[m$Area.Name=="SIKKIM",]$Area.Name = "Sikkim"

m[m$Area.Name=="ARUNACHAL PRADESH",]$Area.Name = "Arunachal Pradesh"
m[m$Area.Name=="NAGALAND",]$Area.Name = "Nagaland"
m[m$Area.Name=="MANIPUR",]$Area.Name = "Manipur"
m[m$Area.Name=="MIZORAM",]$Area.Name = "Mizoram"

m[m$Area.Name=="TRIPURA",]$Area.Name = "Tripura"
m[m$Area.Name=="MEGHALAYA",]$Area.Name = "Meghalaya"
m[m$Area.Name=="ASSAM",]$Area.Name = "Assam"
m[m$Area.Name=="WEST BENGAL",]$Area.Name = "West Bengal"
m[m$Area.Name=="JHARKHAND",]$Area.Name = "Jharkhand"

m[m$Area.Name=="ORISSA",]$Area.Name = "Orissa"
m[m$Area.Name=="CHHATTISGARH",]$Area.Name = "Chhattisgarh"
m[m$Area.Name=="MADHYA PRADESH",]$Area.Name = "Madhya Pradesh"

m[m$Area.Name=="GUJARAT",]$Area.Name = "Gujarat"
m[m$Area.Name=="DAMAN & DIU",]$Area.Name = "DAMAN AND DIU"
m[m$Area.Name=="DADRA & NAGAR HAVELI",]$Area.Name = "DADRA AND NAGAR HAVELI"

m[m$Area.Name=="MAHARASHTRA",]$Area.Name = "Maharashtra"
m[m$Area.Name=="ANDHRA PRADESH",]$Area.Name = "Andhra Pradesh"
m[m$Area.Name=="KARNATAKA",]$Area.Name = "Karnataka"
m[m$Area.Name=="GOA",]$Area.Name = "Goa"

m[m$Area.Name=="LAKSHADWEEP",]$Area.Name = "LAKSHADWEEP"
m[m$Area.Name=="KERALA",]$Area.Name = "Kerala"
m[m$Area.Name=="TAMIL NADU",]$Area.Name = "Tamil Nadu"
m[m$Area.Name=="PONDICHERRY",]$Area.Name = "Pondicherry"
m[m$Area.Name=="ANDAMAN & NICOBAR ISLANDS",]$Area.Name = "ANDAMAN AND NICOBAR ISLANDS"

i= max(m$PersonsEdu)
j = min(m$PersonsEdu)
mid = (i+j)/2

ggplot() + geom_map(data = m, aes(map_id = Area.Name, fill = PersonsEdu ), 
                    map = ind) + expand_limits(x = ind$long, y = ind$lat) + 
    scale_fill_gradient2(low = "grey",                                                                           
                         mid = "blue", midpoint = mid, high = "red", limits = c(j, i))

setdiff(m$Area.Name,unique(ind$id))na

d <- filter(b,Age.group=="All ages" & Total..Rural..Urban=="Urban")
e <- filter(b,Age.group=="All ages" & Total..Rural..Urban=="Rural")


