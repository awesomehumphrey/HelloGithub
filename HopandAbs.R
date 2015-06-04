channels = c("ABC", "ZTV", "MTV", "HDTV")   #remove quote if given glue factor
films = c("horror", "romance", "drama", "comedy", "biopics", "action")
carAdverts = c(ford = 0.7, nissan = 0.4, bmw = 0.29)#adverts with glue factors
lenfilm = c(forty = 0.45, sixty = 0.37, ninety = 0.29, twohours = 0.21, high = 0.13)

#Teenager: Probability of hopping based on no of breaks and no of adverts
teenprob = 0.38 #Probability of teen hopping independent of other factors
no = c(0,1,2,3,4,5,6,7,8,9)
brkprob = c(0.05,0.08,0.12,0.15,0.21,0.34,0.55,0.7,0.9,0.95)
advprob = c(0.08,0.09,0.16,0.19,0.20,0.28,0.35,0.39,0.44,0.60)
teen = data.frame(no,brkprob,advprob)

startpoint = 1:1000
counthop = 0
countabs = 0
ansHop[startpoint] = "Null"
ansAbs[startpoint] = "Null"
newch[startpoint] = "Null"
fch[startpoint]

#begin computation
fch [1] = sample(channels,1,replace = TRUE) #initialising of first channel at random

for (x in startpoint){
fm[x] = sample(films,1,replace = TRUE)  #choosing films at random
adv[x] = sample(carAdverts,1,replace = TRUE) #choosing car adverts at random
fmlen[x] = sample(lenfilm,1,replace = TRUE)#choosing film length at random
i = sample(1:10,1,replace = TRUE)#choosing break no at random
j = sample(1:10,1,replace = TRUE)#choosing advert no at random


hop[x] = ((fmlen * teen$brkprob[i] * teen$advprob[j])/adv) + teenprob
      if (hop[x] > 0.5){ #0.5 is the threshold for hopping.
      counthop = counthop + 1
      newch[x] = sample(channels,1,replace = TRUE) #hop to a new channel
        if (newch[x] != fch[x]){
            countabs = countabs + 1
            fch[x] = newch[x]
        }  
          
      }

#print (x)
#print (teen$brkprob[i]) 
#print (teen$advprob[j])
#print (hop)
#print (fm)
#print (len)
#print (fch)
#print (newch)
#print (adv)
#
}
print (counthop)
print (countabs)
baa = data.frame(hop,fm,adv,fmlen,fch)
#write.csv(baa,"C:/Users/Humphrey/Desktop/test.csv", col.names = FALSE)
