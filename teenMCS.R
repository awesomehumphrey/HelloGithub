channels = c("ABC", "ZTV", "MTV", "HDTV")   #remove quote if given glue factor
movie=c(horror=0.22, romance=0.25, drama=0.31, comedy=0.35, biopics=0.23, action=0.26)
carAdverts = c(0.03,0.06,0.09,0.12,0.15,0.19,0.20,0.22)#adverts with glue factors
toyAdverts = c(0.02,0.04,0.05,0.07,0.10,0.11,0.13,0.14)
cosmeticsAdverts = c(0.025,0.065,0.077,0.163,0.181,0.193,0.222,0.245)
foodAdverts = c(0.01,0.075,0.08, 0.099,0.16,0.18,0.21,0.23,0.24,0.25)
lenfilm = c(0.31,0.31,0.27,0.27,0.27,0.22,0.22,0.22,0.18,0.18)

tolerance = 0.44 #Probability of teen hopping independent of other factors
brkprob = c(0.39,0.39,0.49,0.49,0.49,0.49,0.49,0.49,0.55,0.55)
advprob = c(0.35,0.39,0.39,0.39,0.44,0.44,0.44,0.44,0.44,0.48)

totalhop = rep(0,100)
totalabs = rep(0,100)
for (y in 1:100){

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
  gluef = sample(cosmeticsAdverts,1000,replace = TRUE)#choosing gluefactor  at random
  len = sample(lenfilm,1000,replace = TRUE)#choosing advert no at random

  #hopa = ((len*nbrk*nadvert)/(0.38*0.28))-0
  
  for (x in startpoint){  
    hop[x] = abs(((len[x]*nbrk[x]*nadvert[x])/(tolerance*nmovie[x]))-gluef[x])# the absolute function is to ensure all values are positive
    if (hop[x] >= 0.5){ #0.5 is the threshold for hopping.
      counthop = counthop + 1
      newch[x] = sample(channels,1,replace = TRUE) #hop to a new channel
      if (newch[x] != fch[x]){
        countabs = countabs + 1
        fch[x] = newch[x] 
    }
  }
  
}
  totalhop[y] = counthop
  totalabs[y] = countabs
#subset(data.frame(hop, len, nbrk, nadvert, nmovie, gluef), hop>=1)
#print (counthop)
#print (countabs)
#((lenfilm[1]*brkprob[10]*advprob[10])/(teenprob*movie[3]))-carAdverts[4]
}#end of outer for loop