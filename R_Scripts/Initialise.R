library(tibble)
library(dplyr)
library(tidyr)



###########################################
#               PARAMETERS                #
###########################################

patches <- 1                        # Number of patches
num_per_patch <- c(50,30,20)        # Initial number of individuals per patch
num_loci <- 4                       # Number of loci
max_age <- 30                       # max age for starting mosquito
dispersal_rate <- 0.01              # Base dispersal rate (adjust this if necessary)
daily_survival <- c(egg = 0.8, 
                    larva = 0.8, 
                    pupa = 0.8, 
                    adult = 0.8)    #daily survival prob
daily_transition <- c(egg = 0.8,
                      larva = 0.8,
                      pupa = 0.8)   #daily transition prob
offspring_day <- 5                  #Number of offspring per day per female mosquitoes 
carry_k <- 10000                    #Carrying capacity
mate_prob <- 0.65                   #Probability of mating
sim_days <- 75                      #Number of simulation in days
bloodmeal_prob <- 0.65              #Probability that a female find a blood meal





###########################################
#  INITIALISE POPULATION WITH ATTRIBUTES  #
###########################################


# Function to create a chromosome

ini_pop <- function(patches, num_per_patch, max_age, num_loci) {
  patches_pop <- list()
  
  for (i in 1:patches) {
    patches_pop[[i]] <- tibble(
      age = sample(0:max_age, num_per_patch[i], replace = TRUE), # Age of each individual in the patch
      sex = rbinom(num_per_patch[i], 1, 0.5), # Female == 1, random sex
      stage = case_when(
        age <= 3 ~ "egg",
        age <= 10 ~ "larva",
        age <= 12 ~ "pupa",
        TRUE ~ "adult"
      ),
      alive = TRUE
    )
  }
  
  return(patches_pop)
}



Pop <- ini_pop(patches, num_per_patch, max_age)

write.csv(Pop, file = "Pop.csv")


