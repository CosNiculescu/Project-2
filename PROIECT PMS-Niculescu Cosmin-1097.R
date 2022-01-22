      #Proiect PMS

#Ex. 1:
#Spatiul starilor este:

#S = {"M4", "M5", "M6", "M7"}

#Ex. 2:
#Matricea de trecere P:

P<- matrix(data = c(0.9156, 0.0714, 0.0108, 0.0022, 0.9429, 0.0571, 0, 0, 0.8, 0, 0, 0.2, 1, 0, 0, 0), nrow = 4, ncol = 4, byrow = TRUE)
P
stari<- c("M4", "M5", "M6", "M7")  
stari
rownames(P)<-stari
colnames(P)<-stari
P

#Ex. 3:
#Reprezentarea matricei de trecere printr-un graf orientat(am facut pe caiet graful)

#Ex. 12:
#Calculam distributia stationara a lantului

stationary <- function(mat) {
  x = eigen(t(mat))$vectors[,1]
  as.double(x/sum(x))
}

stationary(P)
#Rotunjim la 3 zecimale
round(stationary(P), digits = 3)

#Verificam daca aceasta este distributia stationara

stationary(P)%*%P
round(stationary(P)%*%P, digits = 3)

round(stationary(P), digits = 3)==round(stationary(P)%*%P, digits = 3)
distr_stationara<-stationary(P)
distr_stationara
names(distr_stationara)<-stari
distr_stationara
round(distr_stationara, digits = 3)

medie_timp_cutremur_M7<-1/round(distr_stationara["M7"], digits = 3)
medie_timp_cutremur_M7

#Numarul mediu de perioade in care lantul revine in starea M7 este 250.
#O perioada este egala cu nr. total al cutremurelor impartit la numarul de ani.
nr_cutremure <- 505
nr_ani<-2016-1970+1
nr_ani
o_perioada<-nr_cutremure/nr_ani
o_perioada

#Numarul de ani in care se va produce urmatorul cutremur de magnitudine M7.
medie_ani_cutremur_M7<-medie_timp_cutremur_M7/o_perioada
medie_ani_cutremur_M7

#Exercitiul 14
#Simulare lant Markov


#Exercitiul 15



stari<-c("M4", "M5", "M6", "M7")
init<-c(1,0,0,0)
init
names(init)<-stari
init

P<- matrix(data = c(0.9156, 0.0714, 0.0108, 0.0022, 0.9429, 0.0571, 0, 0, 0.8, 0, 0, 0.2, 1, 0, 0, 0), nrow = 4, ncol = 4, byrow = TRUE)
P
rownames(P) <- stari # adaugam starile ca etichete ale liniilor matricei P
colnames(P) <- stari # adaugam starile ca etichete ale liniilor matricei P
P
markov <- function(init,matrice,n,labels) {
  if (missing(labels)) labels <- 1:length(init)
  simlist <- numeric(n+1)
  states <- 1:length(init)
  simlist[1] <- sample(states,
                       1,
                       prob=init)
  for (i in 2:(n+1))
  { simlist[i] <- sample(states,
                         1,
                         prob=matrice[simlist[i-1],]) }
  labels[simlist] 
}

#generarea traiectoriei pentru 100 de pasi/tranzitii
traiectorie <- markov(init,P,100,stari)
traiectorie
traiectorie[101]
length(traiectorie)
#interpretare: starea in care se afla lantul peste 100 zile daca pleaca din starea initiala generata de cod

#generare distrib de frecvente absolute
distr_frecv_abs <- table(traiectorie)
distr_frecv_abs

#generare distrib de frecvente relative
distr_frecv_rel <- distr_frecv_abs/length(traiectorie)
distr_frecv_rel


#10000 de simulari ale lantului Markov
stari<-c("M4", "M5", "M6", "M7")
init<-c(1,0,0,0)
init
names(init)<-stari
init

P<- matrix(data = c(0.9156, 0.0714, 0.0108, 0.0022, 0.9429, 0.0571, 0, 0, 0.8, 0, 0, 0.2, 1, 0, 0, 0), nrow = 4, ncol = 4, byrow = TRUE)
P
rownames(P) <- stari # adaugam starile ca etichete ale liniilor matricei P
colnames(P) <- stari # adaugam starile ca etichete ale liniilor matricei P
P

markov(init,P,10,stari)


sim_total <- replicate(10000,markov(init,P,10,stari))
sim_total


distr_frecv_abs <- table(sim_total)
distr_frecv_abs

distr_frecv_rel <- distr_frecv_abs/length(sim_total)
distr_frecv_rel
  
  
dpois(0,2,FALSE)
ppois(0,2,FALSE)

ppois(65,60)
ppois(65,60,TRUE)
dpois(18,20)
dpois(52,50)
dpois(18,20)*dpois(52,50)
ppois(3, 4.6)
dpois(18,20)
dpois(52,50)
dpois(18,20)*dpois(52,50)
ppois(200,200)
ppois(6,7.5)
1-ppois(6,7.5)
dpois(2,1.5)
dpois(4,4.5)
dpois(2,1.5)*dpois(4,4.5)
dpois(6,6)
dpois(2,1.5)*dpois(4,4.5)/dpois(6,6)
dpois(0,2.5)
dpois(4,5)
dpois(6,5)
dpois(4,5)*dpois(6,5)
dpois(6,5)
dpois(19,20)
dpois(6,5)*dpois(19,20)
ppois(10,13.5)
1-ppois(10,13.5)
dpois(16,18)
dpois(4,9)
dpois(16,18)*dpois(4,9)
dpois(2,2.25)
dpois(8,9)
dpois(6,6.75)

matrixpower <- function(matrice,k) {
  if (k == 0) return (diag(dim(matrice)[1])) 
  if (k == 1) return(matrice)
  if (k > 1) return( matrice %*% matrixpower(matrice, k-1))
}
P8<-matrixpower(P,8)




