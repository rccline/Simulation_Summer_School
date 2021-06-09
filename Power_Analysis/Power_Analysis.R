###############################################################
##
##
## Simulation Summer School Workshop #2 
## Oliver (Oli) Clark  
## Youtube:  https://www.youtube.com/watch?v=UG4ObG1OBaE  
## Simulation Based Power Analysis
##
##
##############################################################

my_variable <- rnorm(10, mean=0, sd = 1)

mean(my_variable)
sd(my_variable)


mv1 <- rnorm(15, mean=0, sd = 1)
mv2 <- rnorm(15, mean=0.5, sd = 1)
t.test(mv1, mv2)

## This loop will count from 1 to 1000
for(thisRun in 1:1000){
  print(thisRun)
}

## Turn your t.test into a variable----
for(thisRun in 1:1000){
  mv1 <- rnorm(15, mean=0, sd = 1)
  mv2 <- rnorm(15, mean=0.5, sd = 1)
  t.test(mv1, mv2)
}


mrT <- t.test(mv1, mv2)
for(thisRun in 1:1000){
  mv1 <- rnorm(15, mean=0, sd = 1)
  mv2 <- rnorm(15, mean=0.5, sd = 1)
  t.test(mv1, mv2)
} 



for(thisRun in 1:10000){
  mv1 <- rnorm(15, mean=0, sd = 1)
  mv2 <- rnorm(15, mean=0.5, sd = 1)
  mrT <- t.test(mv1, mv2)
  print(mrT$p.value)
  t.test(mv1, mv2)
} 

mrT <- t.test(mv1, mv2)
## Using a Matrix, we run the t.test and store the p-values in a matrix---- 
## Run 100 iterations

Keanu <- matrix(nrow=100)
Keanu

for(thisRun in 1:100){
  mv1 <- rnorm(15, mean=0, sd = 1)
  mv2 <- rnorm(15, mean=0.5, sd = 1)
  mrT <- t.test(mv1, mv2)
  Keanu[thisRun] <- mrT$p.value ## This will store the p values into the matrix
  print(mrT$p.value)
 
}

mean(Keanu)   

### How many of the p-values are significant?----

which(Keanu < 0.05)
length(which(Keanu < 0.05))  ## this will give us power  25:100 is 25% power
### We might change the sample size.  
## We can change the k to 1000 and change the number of loops


#############################################################
### Now we will use 1000 rows in mmatrix.  
k <- 1000
Keanu <- matrix(nrow=k)
Keanu

for(thisRun in 1:1000){    ## iterations = rows
  mv1 <- rnorm(15, mean=0, sd = 1)
  mv2 <- rnorm(15, mean=0.5, sd = 1)
  mrT <- t.test(mv1, mv2)
  Keanu[thisRun] <- mrT$p.value ## This will store the p values into the matrix
  print(mrT$p.value)
  }  

# which(Keanu < 0.05)
length(which(Keanu < 0.05))


#############################################################
################### Change Sample Size of rnorm from 15 to 80 ##########---- 
################### This will increase power to 88 ######
k <- 1000
Keanu <- matrix(nrow=k)
Keanu

for(thisRun in 1:1000){    ## iterations = rows
  mv1 <- rnorm(80, mean=0, sd = 1)
  mv2 <- rnorm(80, mean=0.5, sd = 1)
  mrT <- t.test(mv1, mv2)
  Keanu[thisRun] <- mrT$p.value ## This will store the p values into the matrix
  print(mrT$p.value)
}  

# which(Keanu < 0.05)
length(which(Keanu < 0.05))
#############################################################

## HISTOGRAM 
## Distribution of p values

hist(Keanu)

## Truncate larte p-values
hist(Keanu[Keanu < 0.2], breaks=50)  
## Add breaks, we see the p-values stacked toward Zero.
## Under Ha with a sufficient sample size, p-values are likely
## to be under p 0.05.  

#############################################################  
#### Iv our mean difference is Zero, what do you think this hist would look like: ####
## Simonsohn, U. Nelson, L.D., & Simmons. J. P. (2014). P-curve: 
## a key to the file-drawer.  Journal of experimental psychology:
## General, 143(2)  
## Simonsohn, U., Nelson, L., & Simmons, J. (2014). P-curve: A Key to The File Drawer. Journal of Experimental Psychology: General, 534–547. https://doi.org/10.1037/a0033242

## Uniform distribution - each value is equally likely  
## Change k to 10,000 

k <- 1000
Keanu <- matrix(nrow=k)
Keanu

for(thisRun in 1:k){    ## iterations = rows
  mv1 <- rnorm(80, mean=0, sd = 1)
  mv2 <- rnorm(80, mean=0, sd = 1)
  mrT <- t.test(mv1, mv2)
  Keanu[thisRun] <- mrT$p.value ## This will store the p values into the matrix
  print(mrT$p.value)
}  

# which(Keanu < 0.05)
length(which(Keanu < 0.05))

hist(Keanu)
hist(Keanu[Keanu < 0.2])
hist(Keanu[Keanu < 0.2, breaks = 50])

## Plot a Power Curve---- 
## run  nested loops
# Create a matrix Sample-Sizes and Power 



Morpheus <- matrix(nrow = 7, ncol = 2)
sizes <- c(10,20,30,40,50,60,70)  

for(thisSize in 1:nrow(Morpheus)){
k <- 1000
Keanu <- matrix(nrow = k)

for(thisRun in 1:k){
  mv1 <- rnorm(sizes[thisSize], mean=100, sd = 15)
  mv2 <- rnorm(sizes[thisSize], mean=115, sd = 15)
  mrT <- t.test(mv1, mv2)
  Keanu[thisRun] <- mrT$p.value
}

Morpheus[thisSize, 1] <- sizes[thisSize]
Morpheus[thisSize, 2] <- sum(Keanu < 0.05)

}

plot(Morpheus) # This is our power curve

# The no of people we have to give the drug to to increase the IQ by sd 15
hist(Keanu[Keanu < 0.2], breaks=50) 

# We have a huge effect size.  


