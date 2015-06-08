channels = c("ABC", "ZTV", "MTV", "HDTV")   #remove quote if given glue factor
movie=c(horror=0.57, romance=0.29, drama=0.7, comedy=0.62, biopics=0.32, action=0.4)
carAdverts = c(ford = 0.09, nissan = 0.15, bmw = 0.19, Maz = 0.22)#adverts with glue factors
lenfilm = c(forty = 0.35, sixty = 0.29, ninety = 0.22, twohours = 0.18)

teenprob = 0.32 #Probability of teen hopping independent of other factors
brkprob = c(0.39,0.39,0.55,0.55,0.55,0.55,0.55,0.55,0.61,0.61)
advprob = c(0.35,0.39,0.39,0.39,0.44,0.44,0.44,0.44,0.44,0.49)

startpoint = 1:1000
counthop = 0
countabs = 0
newch[startpoint] = "Null"
fch[startpoint]

#begin computation #####work on channels sampling with sabina's technique
fch [1] = sample(channels,1,replace = TRUE) #initialising of first channel at random


  nbrk = sample(brkprob,1000,replace = TRUE)#choosing break no at random
  nadvert = sample(advprob,1000,replace = TRUE)#choosing advert no at random
  nmovie = sample(movie,1000,replace = TRUE)#choosing movies no at random
  gluef = sample(carAdverts,1000,replace = TRUE)#choosing gluefactor  at random
  len = sample(lenfilm,1000,replace = TRUE)#choosing advert no at random

hopa = ((len*nbrk*nadvert)/(teenprob*nmovie))-gluef
  
for (x in startpoint){  
  hop[x] = abs(((len[x]*nbrk[x]*nadvert[x])/(teenprob*nmovie[x]))-gluef[x])
  if (hop[x] >= 0.5){ #0.5 is the threshold for hopping.
    counthop = counthop + 1
    newch[x] = sample(channels,1,replace = TRUE) #hop to a new channel
    if (newch[x] != fch[x]){
      countabs = countabs + 1
      fch[x] = newch[x] 
    }
  }
  
}
#subset(data.frame(hop, len, nbrk, nadvert, nmovie, gluef), hop>=1)
print (counthop)
print (countabs)
#((lenfilm[1]*brkprob[10]*advprob[10])/(teenprob*movie[3]))-carAdverts[4]
