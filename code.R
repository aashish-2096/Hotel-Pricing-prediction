# Analysis of Hotel Room Pricing In The Indian Market
# NAME: Aashish Kumar
# EMAIL: aashish2096@gmail.com
# COLLEGE / COMPANY: NIT Raipur


> Cities42 <- read_csv("~/R/Cities42.csv")

> str(hotel.df)
> library("psych", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
> describe(hotel.df)

> boxplot(RoomRent~IsWeekend, data=hotel.df,horizontal=TRUE,main="Rent Vs IsWeekend",xlab="Rent",ylab="1 for weekend 0 otherwise")

> scatterplot(RoomRent~Airport, data=hotel.df,horizontal=TRUE,main="Rent Vs Distace from Airport")

> boxplot(RoomRent~IsTouristDestination, data=hotel.df,horizontal=TRUE,main="Rent Vs IsWeekend",ylab="1 for weekend 0 otherwise")
> table(hotel.df$IsTouristDestination)

0    1 
4007 9225 

> plot(RoomRent~StarRating, data=hotel.df,horizontal=TRUE,main="Rent Vs Rating",ylab="Rent " ,xlab="Rating")
> boxplot(RoomRent~HasSwimmingPool, data=hotel.df,horizontal=TRUE,main="Rent Vs Has Swimming pool",ylab="1 for Yes 0 No",xlab="Rent")
> scatterplot(RoomRent~HotelCapacity, data=hotel.df,horizontal=TRUE,main="Rent Vs Hotel Capacity",xlab="Capacity",spread=FALSE)
> hist(hotel.df$HotelCapacity,main="Hotel capacity variation",xlab="Capacity",ylab="Frequency",col="grey")

> cor(hotel.df$CityRank,hotel.df$RoomRent)
[1] 0.09398553
> cor(hotel.df$CityRank,hotel.df$IsNewYearEve)
[1] -0.0006326444
> cor(hotel.df$CityRank,hotel.df$Airport)
[1] 0.505912
> cor(hotel.df$CityRank,hotel.df$StarRating)
[1] -0.133381
> cor(hotel.df$CityRank,hotel.df$RoomRent)
[1] 0.09398553

> cor(hotel.df$RoomRent,hotel.df$IsMetroCity)
[1] -0.06683977
> cor(hotel.df$RoomRent,hotel.df$IsTouristDestination)
[1] 0.122503
> cor(hotel.df$RoomRent,hotel.df$IsWeekend)
[1] 0.004580134
> cor(hotel.df$RoomRent,hotel.df$HasSwimmingPool)
[1] 0.3116577
> cor(hotel.df$RoomRent,hotel.df$FreeBreakfast)
[1] -0.01000637
> cor(hotel.df$RoomRent,hotel.df$FreeWifi)
[1] 0.003627002
> cor(hotel.df$RoomRent,hotel.df$Population)
[1] -0.08872806
> cor(hotel.df$RoomRent,hotel.df$HotelCapacity)
[1] 0.1578733



> bwplot(hotel.df$RoomRent,main="Rent Variation",xlab="rent")

> bwplot(hotel.df$Airport,main="Distance from Airport",xlab="Rent")

> hist(hotel.df$IsTouristDestination,main="Tourist destination")

> barplot(table(hotel.df$HasSwimmingPool),main="Hotels with pool",xlab = "1 for yes 0  for No")


> scatterplot(RoomRent~HasSwimmingPool, data=hotel.df,horizontal=TRUE,main="Rent Vs HasSwimmingpool")
> scatterplot(RoomRent~IsTouristDestination, data=hotel.df,horizontal=TRUE,main="Rent Vs Is tourist destination")


>  finalfactor1 <- c("RoomRent", "IsTouristDestination","HasSwimmingPool","HotelCapacity")
> corrgram(hotel.df[,finalfactor1], order=TRUE,
           +          main="Corrgram with factors of importance",
           +          lower.panel=panel.pts, upper.panel=panel.pie,
           +          diag.panel=panel.minmax, text.panel=panel.txt)



#Varianvce covariance matrix xreation using vcov
> k<- lm(RoomRent ~IsTouristDestination+HasSwimmingPool+HotelCapacity,data=hotel.df)
> vcov(k)

##hypothesis
#The rent of hotel depends on the factors as  TouristDestination, SwimmingPool, HotelCapacity , metrocity, Isweekend,Starrating , freewifi, cityrank,airport distance,
Isnewyear.
> mode <-RoomRent~ IsTouristDestination+ HasSwimmingPool+ HotelCapacity +IsMetroCity+ IsWeekend+IsNewYearEve+ Airport+CityRank+FreeWifi+FreeBreakfast
hyp2 <- lm(mode,data=hotel.df)
> summary(hyp1)
> install.packages("leaps")
> obs <- regsubsets(mode, data = hotel.df, nbest=1)
> plot(obs,scale="adjr2")

#hypothesis2 
#The rent of hotel depends on the factors as  TouristDestination, SwimmingPool, HotelCapacity , metrocity, Isweekend,Starrating , freewifi, cityrank,airport distance,
> mode2 <- RoomRent~ IsTouristDestination+ HasSwimmingPool+ HotelCapacity+IsMetroCit+IsNewYearEve+ Airport+CityRank+FreeWifi
> hyp2 <- lm(mode2,data=hotel.df)
> summary(hyp2)

#correlation matrix 
>  finalfactor1 <- c("RoomRent", "IsTouristDestination","HasSwimmingPool","HotelCapacity")
> matrix<-rcorr(as.matrix(hotel.df[,finalfactor1]))
> matrix