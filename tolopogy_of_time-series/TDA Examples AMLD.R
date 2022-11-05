#load urban population and malaria data
urban_pop<-read.csv("urban_pop.csv")
malaria<-read.csv("malaria_indicators_gab.csv")

#examine urban_pop data
head(urban_pop)
summary(urban_pop)
dim(urban_pop)

#examine malaria data
head(malaria)
summary(malaria)
dim(malaria)

#load TDAstats package
library(TDAstats)

#create analysis of urban_pop data, with 4-year periods and 3-year overlap
ph_test<-rep(NA,61)
for (i in 4:61){
	#create datasets for 4 year periods with 3 year overlap in consecutive
	#time periods
	data1<-urban_pop[(i-3):i,2:5]
	data2<-urban_pop[(i-2):(i+1),2:5]
	
	#create distance matrices
	t1_dist<-dist(data1,diag=T,upper=T)
	t2_dist<-dist(data2,diag=T,upper=T)
	
	#create persistence diagram with Vietoris-Rips sequence
	h_t1<-calculate_homology(t1_dist, dim = 1, format = "distmat")
	h_t2<-calculate_homology(t2_dist, dim = 1, format = "distmat")

	#find p-value of permutation test for 0th homology
	ph_test[i]<-permutation_test(h_t1,h_t2,iterations=25)[[1]]$pvalue
}

#add tda testing resutls to dataset 
#(minus the first 3 years where prior year's data doesn't exist and last year)
urban_pop_results<-urban_pop[-c(1:3,62),]
urban_pop_results$change_indicator<-ph_test[4:61]


#create analysis of malaria data, with 4-year periods and 3-year overlap
ph_test<-rep(NA,19)
for (i in 4:19){
	#create datasets for 4 year periods with 2 year overlap in consecutive
	#time periods
	data1<-malaria[(i-3):i,2]
	data2<-malaria[(i-1):(i+2),2]
	
	#create distance matrices
	t1_dist<-dist(data1,diag=T,upper=T)
	t2_dist<-dist(data2,diag=T,upper=T)
	
	#create persistence diagram with Vietoris-Rips sequence
	h_t1<-calculate_homology(t1_dist, dim = 1, format = "distmat")
	h_t2<-calculate_homology(t2_dist, dim = 1, format = "distmat")

	#find p-value of permutation test for 0th homology
	ph_test[i]<-permutation_test(h_t1,h_t2,iterations=25)[[1]]$pvalue
}

#add tda testing resutls to dataset 
#(minus the first 3 years where prior year's data doesn't exist and last year)
malaria_results<-malaria[-c(1:3,20:21),]
malaria_results$change_indicator<-ph_test[4:19]

