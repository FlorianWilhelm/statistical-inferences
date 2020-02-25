#Run the lines below to install and load the TOSTER package
#Install TOSTER package if needed
if(!require(TOSTER)){install.packages('TOSTER')}
#Load TOSTER package
library(TOSTER)

#Question 2 and 3 (replace the zeroes with correct values)
# You can type ?TOSTtwo for help with the TOSTtwo function
TOSTtwo(m1 = 4.88, m2 = 5.01, sd1 = 1.35, sd2 = 1.21, n1 = 180, n2 = 190, low_eqbound_d = -0.2, high_eqbound_d = 0.2)

#Question 4 (replace the zeroes with correct values)
# You can type ?powerTOSTtwo for help with the powerTOSTtwo function
powerTOSTtwo(alpha = 0.1, N = 15, statistical_power = 0.95)

#Question 5 and 6 (replace the zeroes with correct values)
# You can type ?powerTOSTtwo for help with the powerTOSTtwo function
powerTOSTtwo(alpha = 0.1, statistical_power = 0.95, low_eqbound_d = -0.25, high_eqbound_d = 0.25)

#Question 7 (replace the zeroes with correct values)
# You can type ?TOSTmeta for help with the TOSTmeta function

TOSTmeta(alpha=0.01, ES=0.04, se=0.002, low_eqbound_d=-0.1, high_eqbound_d=0.1)

#Question 8 (replace the zeroes with correct values)
# You can type ?powerTOSTr for help with the powerTOSTr function
powerTOSTr(alpha=0.05, N=71, statistical_power=0.5)

#Question 9 (replace the zeroes with correct values)
# You can type ?TOSTr for help with the TOSTr function
TOSTr(alpha=0.05, n=71, r = -0.12, low_eqbound_r=-0.2, high_eqbound_r=0.2)





#Š Daniel Lakens, 2018. 
# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. https://creativecommons.org/licenses/by-nc-sa/4.0/