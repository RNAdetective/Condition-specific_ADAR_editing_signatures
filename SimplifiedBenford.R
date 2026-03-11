#Load packages
library(benford.analysis)


benlaw <- function(d){log10(1+(1/d))}
#Below gives the expected benford dist freq of numbers#
benlaw(1)
benlaw(2)
benlaw(3)
benlaw(4)
benlaw(5)
benlaw(6)
benlaw(7)
benlaw(8)
benlaw(9)
#end of expected benford dist freqs#


#Inputs are based on the format of matrix in "OverallbenfordDistributionsInput" file with option for either avg editing rate of sites or all individual editing rates of sites
#On first digit for All# Input is the editing rate of each site in each sample for each condition

DF_All <- benford(DF$DF_All,
                        sign = 'positive',
                        discrete = F,
                        number.of.digits = 1)
plot(DF_All)

DF_All

#Save results of condition specific res:export results of res into 1 csv (all results)

sink('DF_All.csv')  
print(DF_All)  
sink()

#On first digit for Avg# Input is the avg editing rate of a site of samples in a condition

DF_Avg <- benford(DF$DF_Avg,
                      sign = 'positive',
                      discrete = F,
                      number.of.digits = 1)
plot(DF_Avg)

DF_Avg

#Save results of condition specific res:export results of res into 1 csv (all results)

sink('DF_Avg.csv')  
print(DF_Avg)  
sink()