####Tabatha Almonte###
####12/16/22##########
####Final Project#####
###setwd("~/DSF22")###

## Open csv. 
finaldata<- read.csv("dataabortion.csv")
#view data
View(finaldata)

nrow(finaldata)
#There is 912 obs.

##Created subsets to group specific information such as location and year.
##In this case I choose to look at all states which is labeled as "US" and 
#create a subset before 2001 and another one with same location but after 2001.

usbefore2001 <- subset(finaldata, (year  < 2001) &
                               (state == "US"))

usafter2001 <- subset(finaldata, (year  >= 2001) &
                              (state == "US"))

#used range function to know the years range from before and after 21st Century

rangbef<- range(usbefore2001$year, na.rm=T)
rangaf<- range(usafter2001$year, na.rm=T)

#Next, I looked at a specific column after storing both subsets and created a new value.
#In this case, I used the total abortion rate among all ages.

jjjj <- usbefore2001$abortionratetotal
kkkk <- usafter2001$abortionratetotal

########
#Created scatterplot for abortion rates before 21st Century in the US by year    
    
labelnumbf<-usbefore2001$year #label for x-lim 

 plot(x=1:length(jjjj),
       y= jjjj,
       main = "Abortion rates before 21st Century in the US",
       xlim =c(0,28),
       ylim = c(10,40), 
       xlab = "Years",
       ylab = "Rates",
       las=1,
       type ="b",
       xaxt="n")
axis(1, at = 1:28, 
     labels=(labelnumbf), las=2)
#######
#Created scatterplot for abortion rates after 21st Century in the US by year

labelnumaf<-usafter2001$year #label for x-lim 

plot(x=1:length(kkkk),
     y= kkkk,
     main = "21st Century abortion rates in the US",
     xlim =c(0,17),
     ylim = c(10,25), 
     xlab = "Years",
     ylab = "Rates",
     las=1,
     type ="b",
     xaxt="n")
axis(1, at = 1:17, 
     labels=(labelnumaf), las=2)
#######
#Saved means as an object to compare it
  
tmeanbefore<-mean(jjjj, na.rm=T)
tmeanafter<-mean(kkkk, na.rm=T)


#group both objects to create a barplot and compare means of before
valuesbar <- c(tmeanbefore,tmeanafter)


barplot(valuesbar,
        ylim = c(0, 30),
        xlab = "21st century means",
        names= c("Before","After"),
        col = "brown4", # color of bars
        main = "Abortion Comparison Rates in the US", 
        cex.main = 1.1, # size of plot title
        cex.names = .9, # size of name labels
        ylab = "Rate", # yaxis label
        cex.lab = 1.0)# size of yaxis label

#find out difference of means with the before-and-after design
mean(usbefore2001$abortionratetotal) - mean(usafter2001$abortionratetotal)
