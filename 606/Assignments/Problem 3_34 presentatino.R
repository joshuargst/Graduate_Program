



Airlineevent<-data.frame("Bag_Count"= c(0,1,2),
                        "Baggage_Fee" = c(0,25,35), 
                        "Percent_Passengers" = c(0.54,0.34,0.12))


Airlineevent

Revenue_per_Pas <- sum(Airlineevent$Baggage_Fee*Airlineevent$Percent_Passengers)

print(Revenue_per_Pas)


Revenue_Variance<-0.54*(0- Revenue_per_Pas)^2+0.34*(8.5- Revenue_per_Pas)^2+.12*(4.2- Revenue_per_Pas)^2

Revenue_Stdev<-sqrt(Revenue_Variance)

print(Revenue_Stdev)

num_Pas<-120
estimated_revenue <- num_Pas*Revenue_per_Pas
print(estimated_revenue)

estimated_devation<- sqrt(Revenue_Variance*num_Pas)
print(estimated_devation)

