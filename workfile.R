channels = c("ABC", "ZTV", "JTV", "HDTV")   #Television channels
movie=c(0.22, 0.31, 0.35, 0.23, 0.26)
carAdverts = c(0.03,0.06,0.09,0.12,0.15,0.19,0.20,0.22)#adverts with glue factors
furnitureAdverts = c(0.02,0.04,0.05,0.07,0.10,0.11,0.13,0.14)
cosmeticsAdverts = c(0.025,0.065,0.077,0.163,0.181,0.193,0.222,0.245)
foodAdverts = c(0.01,0.075,0.08, 0.099,0.16,0.18,0.21,0.23,0.24,0.25)
adverts = c(carAdverts,foodAdverts,cosmeticsAdverts,furnitureAdverts)
lenfilm = c(0.31,0.31,0.27,0.27,0.27,0.22,0.22,0.22,0.18,0.18)

staying = c(0.25,0.25,0.27,0.27,0.27,0.31,0.31,0.34,0.34,0.34,0.37,0.37,0.37,0.40,0.40,0.42,0.42,0.43,0.43,0.44) #Probability of the different classes hopping independent of other factors
brkprob = c(0.39,0.39,0.49,0.49,0.49,0.49,0.49,0.49,0.55,0.55)
advprob = c(0.35,0.39,0.39,0.39,0.44,0.44,0.44,0.44,0.44,0.48)

#runs = 100
#totalhop = rep(0,runs)
#totalabs = rep(0,runs)
#for (y in 1:runs){
  
  start = 1000
  counthop = 0
  countabs = 0
  newch[1:start] = "Null"
  
  #begin computation #
  fch = sample(channels,start,replace = TRUE) #initialising channels at random
  stay = sample(staying,start,replace = TRUE)#choosing class of viewers at random
  nmovie = sample(movie,start,replace = TRUE)#choosing movie genre at random
  len = sample(lenfilm,start,replace = TRUE)#choosing film length at random
  nbrk = sample(brkprob,start,replace = TRUE)#choosing number of breaks at random
  nadvert = sample(advprob,start,replace = TRUE)#choosing number of adverts at random
  gluef = sample(adverts,start,replace = TRUE)#choosing gluefactor  at random
  
  #hopa = ((len*nbrk*nadvert)/(0.38*0.28))-0
  
  for (x in 1:start){  
    hop[x] = ((len[x]*nbrk[x]*nadvert[x])/(stay[x]*nmovie[x]))-gluef[x]# the absolute function is to ensure all values are positive
    if (hop[x] >= 0.5){ #0.5 is the threshold for hopping.
      counthop = counthop + 1
      newch[x] = sample(channels,1,replace = TRUE) #hop to a new channel
      if (newch[x] != fch[x]){
        countabs = countabs + 1
        fch[x] = newch[x] 
      }
    }
    
  }
  #totalhop[y] = counthop
 # totalabs[y] = countabs
  #subset(data.frame(hop, len, nbrk, nadvert, nmovie, gluef, tol), hop>=0.5)
  #print (counthop)
  #print (countabs)
  ##write.csv(hop,countabs,"C:/Users/Humphrey/Desktop/test.csv", col.names = FALSE)
#}#end of outer for loop
