---
title: "606_Presentation"
output:
  pdf_document: default
  html_document: default
---



## R Markdown

An airline charges the following baggage fees: $25 for the first bag and $35 for the
second. Suppose 54% of passengers have no checked luggage, 34% have one piece of checked luggage and
12% have two pieces. We suppose a negligible portion of people check more than two bags.

(a) Build a probability model, compute the average revenue per passenger, and compute the corresponding
standard deviation.




```r
Airlineevent<-data.frame("Bag_Count"= c(0,1,2),
                        "Baggage_Fee" = c(0,25,35), 
                        "Percent_Passengers" = c(0.54,0.34,0.12))

Airlineevent
```

```
##   Bag_Count Baggage_Fee Percent_Passengers
## 1         0           0               0.54
## 2         1          25               0.34
## 3         2          35               0.12
```





```r
Revenue_per_Pas <- sum(Airlineevent$Baggage_Fee*Airlineevent$Percent_Passengers)

print(Revenue_per_Pas)
```

```
## [1] 12.7
```







```r
Revenue_Variance<-sum(Airlineevent$Percent_Passengers*(Airlineevent$Baggage_Fee- Revenue_per_Pas)^2)

Revenue_Stdev<-sqrt(Revenue_Variance)

print(Revenue_Stdev)
```

```
## [1] 14.07871
```



(b) About how much revenue should the airline expect for a 
ight of 120 passengers? With what standard deviation? Note any assumptions you make and if you think they are justifed.




```r
num_Pas<-120
estimated_revenue <- num_Pas*Revenue_per_Pas
print(estimated_revenue)
```

```
## [1] 1524
```

```r
estimated_devation<- sqrt(Revenue_Variance*num_Pas)
print(estimated_devation)
```

```
## [1] 154.2245
```




