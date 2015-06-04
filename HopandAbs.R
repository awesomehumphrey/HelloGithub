channels = c("ABC", "ZTV", "MTV", "HDTV")   #remove quote if given glue factor
films = c("horror", "romance", "drama", "comedy", "biopics", "action")
carAdverts = c(ford = 0.7, nissan = 0.4, bmw = 0.29)#adverts with glue factors
lenfilm = c(forty = 0.45, sixty = 0.37, ninety = 0.29, twohours = 0.21, high = 0.13)

#Teenager: Probability of hopping based on no of breaks and no of adverts
teenprob = 0.38 #Probability of teen hopping independent of other factors
no = c(0,1,2,3,4,5,6,7,8,9)
brkprob = c(0.05,0.08,0.12,0.15,0.21,0.34,0.55,0.7,0.9,0.95)
advprob = c(0.03,0.04,0.11,0.13,0.15,0.23,0.3,0.34,0.39,0.55)
teen = data.frame(no,brkprob,advprob)

counthop = 0
countabs = 0
newch = "Null"

#begin computation
fch = sample(channels,1,replace = TRUE) #initialising of first channel at random

for (x in 1:5){
fm = sample(films,1,replace = TRUE)  #choosing films at random
adv = sample(carAdverts,1,replace = TRUE) #choosing car adverts at random
fmlen = sample(lenfilm,1,replace = TRUE)#choosing film length at random
i = sample(1:10,1,replace = TRUE)#choosing break no at random
j = sample(1:10,1,replace = TRUE)#choosing advert no at random


hop = ((fmlen * teen$brkprob[i] * teen$advprob[j])/adv) + teenprob
      if (hop > 0.4){ #0.5 is the threshold for hopping.
      counthop = counthop + 1
      newch = sample(channels,1,replace = TRUE) #hop to a new channel
        if (newch != fch){
            countabs = countabs + 1
            fch = newch
        }  
          
      }

print (x)
#print (teen$brkprob[i]) 
#print (teen$advprob[j])
print (hop)
#print (fm)
#print (len)
print (fch)
print (newch)
#print (adv)
#
}
print (counthop)
print (countabs)
