library(ecoreg)


covid_data<-data.frame(read.csv('E:/Work/covid/covid_data20210202_race3.csv'))

county_num <- length(covid_data[,1])


N <- covid_data[,23]
y_confirmed <- covid_data[,24]
y_death <- covid_data[,25]


agenum <- 8
sexnum <- 2
racenum <- 3
cross <- covid_data[,34:(33+agenum*sexnum*racenum)]/covid_data[,33]
age <- matrix(rep(0, agenum*county_num), ncol=agenum)
sex <- matrix(rep(0, sexnum*county_num), ncol=sexnum)
race <- matrix(rep(0, racenum*county_num), ncol=racenum)

for (i in 1:agenum){
	age[,i] <- rowSums(cross[,i+agenum*c(0:(sexnum*racenum-1))])
}
age <- age/rowSums(age)
for (i in 1:sexnum){
	sex[,i] <- rowSums(cross[,c(matrix(1:(agenum*sexnum*racenum),ncol=agenum*sexnum,byrow=T)[,agenum*i-agenum+1:agenum])])
}
sex <- sex/rowSums(sex)
for (i in 1:racenum){
	race[,i] <- rowSums(cross[,c(1:(agenum*sexnum))+agenum*sexnum*i-agenum*sexnum])
}
race <- race/rowSums(race)





# main
ecoregression.main <- eco(cbind(y_death, N) ~ mean_pm25 + mean_no2 + mean_ozone
					+ factor(q_popdensity)
					+ scale(poverty, scale=F) + scale(log(median_house_value), scale=F)
               		   	+ scale(log(median_household_income), scale=F) + scale(owner_occupied, scale=F)
					+ scale(education, scale=F)
		   		   	+ scale(beds/population, scale=F) 
               		   	+ scale(obese, scale=F) + scale(smoke, scale=F)
               		   	+ scale(mean_summer_temp, scale=F) + scale(mean_winter_temp, scale=F)
					+ scale(mean_summer_rm, scale=F) + scale(mean_winter_rm, scale=F)
					, data=covid_data
				  	, categorical = list(age, sex, race)
					, cross=cross)
ecoregression.main



# -cross
ecoregression.cross <- eco(cbind(y_death, N) ~ mean_pm25 + mean_no2 + mean_ozone
					+ factor(q_popdensity)
					+ scale(poverty, scale=F) + scale(log(median_house_value), scale=F)
               		   	+ scale(log(median_household_income), scale=F) + scale(owner_occupied, scale=F)
					+ scale(education, scale=F)
		   		   	+ scale(beds/population, scale=F) 
               		   	+ scale(obese, scale=F) + scale(smoke, scale=F)
               		   	+ scale(mean_summer_temp, scale=F) + scale(mean_winter_temp, scale=F)
					+ scale(mean_summer_rm, scale=F) + scale(mean_winter_rm, scale=F)
					, data=covid_data
				  	, categorical = list(age, sex, race))
ecoregression.cross



# -category
ecoregression.category <- eco(cbind(y_death, N) ~ mean_pm25 + mean_no2 + mean_ozone
					+ factor(q_popdensity)
					+ scale(poverty, scale=F) + scale(log(median_house_value), scale=F)
               		   	+ scale(log(median_household_income), scale=F) + scale(owner_occupied, scale=F)
					+ scale(education, scale=F)
		   		   	+ scale(beds/population, scale=F) 
               		   	+ scale(obese, scale=F) + scale(smoke, scale=F)
               		   	+ scale(mean_summer_temp, scale=F) + scale(mean_winter_temp, scale=F)
					+ scale(mean_summer_rm, scale=F) + scale(mean_winter_rm, scale=F)
					, data=covid_data)
ecoregression.category



# -popdensity
ecoregression.density <- eco(cbind(y_death, N) ~ mean_pm25 + mean_no2 + mean_ozone
					#+ factor(q_popdensity)
					+ scale(poverty, scale=F) + scale(log(median_house_value), scale=F)
               		   	+ scale(log(median_household_income), scale=F) + scale(owner_occupied, scale=F)
					+ scale(education, scale=F)
		   		   	+ scale(beds/population, scale=F) 
               		   	+ scale(obese, scale=F) + scale(smoke, scale=F)
               		   	+ scale(mean_summer_temp, scale=F) + scale(mean_winter_temp, scale=F)
					+ scale(mean_summer_rm, scale=F) + scale(mean_winter_rm, scale=F)
					, data=covid_data, categorical = list(age, sex, race)
					, cross=cross)
ecoregression.density




# -beds
ecoregression.beds <- eco(cbind(y_death, N) ~ mean_pm25 + mean_no2 + mean_ozone
					+ factor(q_popdensity)
					+ scale(poverty, scale=F) + scale(log(median_house_value), scale=F)
               		   	+ scale(log(median_household_income), scale=F) + scale(owner_occupied, scale=F)
					+ scale(education, scale=F)
		   		   	#+ scale(beds/population, scale=F) 
               		   	+ scale(obese, scale=F) + scale(smoke, scale=F)
               		   	+ scale(mean_summer_temp, scale=F) + scale(mean_winter_temp, scale=F)
					+ scale(mean_summer_rm, scale=F) + scale(mean_winter_rm, scale=F)
					, data=covid_data
				  	, categorical = list(age, sex, race)
					, cross=cross)
ecoregression.beds



# - smoking + bmi
ecoregression.brfss <- eco(cbind(y_death, N) ~ mean_pm25 + mean_no2 + mean_ozone
					+ factor(q_popdensity)
					+ scale(poverty, scale=F) + scale(log(median_house_value), scale=F)
               		   	+ scale(log(median_household_income), scale=F) + scale(owner_occupied, scale=F)
					+ scale(education, scale=F)
		   		   	+ scale(beds/population, scale=F) 
               		   	#+ scale(obese, scale=F) + scale(smoke, scale=F)
               		   	+ scale(mean_summer_temp, scale=F) + scale(mean_winter_temp, scale=F)
					+ scale(mean_summer_rm, scale=F) + scale(mean_winter_rm, scale=F)
					, data=covid_data
				  	, categorical = list(age, sex, race)
					, cross=cross)
ecoregression.brfss


# - temp + humidity
ecoregression.weather <- eco(cbind(y_death, N) ~ mean_pm25 + mean_no2 + mean_ozone
					+ factor(q_popdensity)
					+ scale(poverty, scale=F) + scale(log(median_house_value), scale=F)
               		   	+ scale(log(median_household_income), scale=F) + scale(owner_occupied, scale=F)
					+ scale(education, scale=F)
		   		   	+ scale(beds/population, scale=F) 
               		   	+ scale(obese, scale=F) + scale(smoke, scale=F)
               		   	#+ scale(mean_summer_temp, scale=F) + scale(mean_winter_temp, scale=F) + scale(mean_summer_rm, scale=F) + scale(mean_winter_rm, scale=F)
					, data=covid_data
				  	, categorical = list(age, sex, race)
					, cross=cross)
ecoregression.weather



# exclude NY Metro
covid_data.nyc <- subset(covid_data, !(fips %in% c("09001", "42089", "36111","09009","36059","36103","34013",
                                                   "34019", "34027", "34037","34039","42103","34023","34025","34029",
                                                   "34035", "34003", "34017", "34031","36005","36047","36061",
                                                   "36079", "36081", "36085",  "36087",  "36119",  "36027", 
                                                   "36071", "09005", "34021")))
N.nyc <- covid_data.nyc[,23]
y_confirmed.nyc <- covid_data.nyc[,24]
y_death.nyc <- covid_data.nyc[,25]
cross.nyc <- covid_data.nyc[,34:(33+agenum*sexnum*racenum)]/covid_data.nyc[,33]
ecoregression.nyc <- eco(cbind(y_death.nyc, N.nyc) ~ mean_pm25 + mean_no2 + mean_ozone
					+ factor(q_popdensity)
					+ scale(poverty, scale=F) + scale(log(median_house_value), scale=F)
               		   	+ scale(log(median_household_income), scale=F) + scale(owner_occupied, scale=F)
					+ scale(education, scale=F)
		   		   	+ scale(beds/population, scale=F) 
               		   	+ scale(obese, scale=F) + scale(smoke, scale=F)
               		   	+ scale(mean_summer_temp, scale=F) + scale(mean_winter_temp, scale=F) + scale(mean_summer_rm, scale=F) + scale(mean_winter_rm, scale=F)
					, data=covid_data.nyc
				  	, categorical = list(age, sex, race)
					, cross=cross.nyc)
ecoregression.nyc



# exclude < 50 confirmed
covid_data.large <- subset(covid_data, confirmed >= 50)
N.large <- covid_data.large[,23]
y_confirmed.large <- covid_data.large[,24]
y_death.large <- covid_data.large[,25]
cross.large <- covid_data.large[,34:(33+agenum*sexnum*racenum)]/covid_data.large[,33]
ecoregression.large <- eco(cbind(y_death.large, N.large) ~ mean_pm25 + mean_no2 + mean_ozone
					+ factor(q_popdensity)
					+ scale(poverty, scale=F) + scale(log(median_house_value), scale=F)
               		   	+ scale(log(median_household_income), scale=F) + scale(owner_occupied, scale=F)
					+ scale(education, scale=F)
		   		   	+ scale(beds/population, scale=F) 
               		   	+ scale(obese, scale=F) + scale(smoke, scale=F)
               		   	+ scale(mean_summer_temp, scale=F) + scale(mean_winter_temp, scale=F) + scale(mean_summer_rm, scale=F) + scale(mean_winter_rm, scale=F)
					, data=covid_data.large
				  	, categorical = list(age, sex, race)
					, cross=cross.large)
ecoregression.large




# main analysis with category exposure
covid_data$q_pm = 1
quantile_pm = quantile(covid_data$mean_pm25,c(0.25,0.5,0.75))
covid_data$q_pm[covid_data$mean_pm25<=quantile_pm[1]] = 1
covid_data$q_pm[covid_data$mean_pm25>quantile_pm[1] &
                                         covid_data$mean_pm25<=quantile_pm[2]] = 2
covid_data$q_pm[covid_data$mean_pm25>quantile_pm[2] &
                                         covid_data$mean_pm25<=quantile_pm[3]] = 3
#covid_data$q_pm[covid_data$mean_pm25>quantile_pm[3] &
#                                         covid_data$mean_pm25<=quantile_pm[4]] = 4
covid_data$q_pm[covid_data$mean_pm25>quantile_pm[3]] = 4
covid_data$q_no2 = 1
quantile_no2 = quantile(covid_data$mean_no2,c(0.25,0.5,0.75))
covid_data$q_no2[covid_data$mean_no2<=quantile_no2[1]] = 1
covid_data$q_no2[covid_data$mean_no2>quantile_no2[1] &
                                         covid_data$mean_no2<=quantile_no2[2]] = 2
covid_data$q_no2[covid_data$mean_no2>quantile_no2[2] &
                                         covid_data$mean_no2<=quantile_no2[3]] = 3
#covid_data$q_no2[covid_data$mean_no2>quantile_no2[3] &
#                                         covid_data$mean_no2<=quantile_no2[4]] = 4
covid_data$q_no2[covid_data$mean_no2>quantile_no2[3]] = 4
covid_data$q_ozone = 1
quantile_ozone = quantile(covid_data$mean_ozone,c(0.25,0.5,0.75))
covid_data$q_ozone[covid_data$mean_ozone<=quantile_ozone[1]] = 1
covid_data$q_ozone[covid_data$mean_ozone>quantile_ozone[1] &
                                         covid_data$mean_ozone<=quantile_ozone[2]] = 2
covid_data$q_ozone[covid_data$mean_ozone>quantile_ozone[2] &
                                         covid_data$mean_ozone<=quantile_ozone[3]] = 3
#covid_data$q_ozone[covid_data$mean_ozone>quantile_ozone[3] &
#                                         covid_data$mean_ozone<=quantile_ozone[4]] = 4
covid_data$q_ozone[covid_data$mean_ozone>quantile_ozone[3]] = 4

ecoregression.cateex <- eco(cbind(y_death, N) ~ factor(q_pm) + factor(q_no2) + factor(q_ozone)
					+ factor(q_popdensity)
					+ scale(poverty, scale=F) + scale(log(median_house_value), scale=F)
               		   	+ scale(log(median_household_income), scale=F) + scale(owner_occupied, scale=F)
					+ scale(education, scale=F)
		   		   	+ scale(beds/population, scale=F) 
               		   	+ scale(obese, scale=F) + scale(smoke, scale=F)
               		   	+ scale(mean_summer_temp, scale=F) + scale(mean_winter_temp, scale=F) + scale(mean_summer_rm, scale=F) + scale(mean_winter_rm, scale=F)
					, data=covid_data
				  	, categorical = list(age, sex, race)
					, cross=cross)
ecoregression.cateex

                                                                           

# rural
covid_data.rural <- subset(covid_data, X2013.code > 5)
N.rural <- covid_data.rural[,23]
y_confirmed.rural <- covid_data.rural[,24]
y_death.rural <- covid_data.rural[,25]
cross.rural <- covid_data.rural[,34:(33+agenum*sexnum*racenum)]/covid_data.rural[,33]
ecoregression.rural <- eco(cbind(y_death.rural, N.rural) ~ mean_pm25 + mean_no2 + mean_ozone
					+ factor(q_popdensity)
					+ scale(poverty, scale=F) + scale(log(median_house_value), scale=F)
               		   	+ scale(log(median_household_income), scale=F) + scale(owner_occupied, scale=F)
					+ scale(education, scale=F)
		   		   	+ scale(beds/population, scale=F) 
               		   	+ scale(obese, scale=F) + scale(smoke, scale=F)
               		   	+ scale(mean_summer_temp, scale=F) + scale(mean_winter_temp, scale=F) + scale(mean_summer_rm, scale=F) + scale(mean_winter_rm, scale=F)
					, data=covid_data.rural
				  	, categorical = list(age, sex, race)
					, cross=cross.rural)
ecoregression.rural     



# urban
covid_data.urban <- subset(covid_data, X2013.code <= 4)
N.urban <- covid_data.urban[,23]
y_confirmed.urban <- covid_data.urban[,24]
y_death.urban <- covid_data.urban[,25]
cross.urban <- covid_data.urban[,34:(33+agenum*sexnum*racenum)]/covid_data.urban[,33]
ecoregression.urban <- eco(cbind(y_death.urban, N.urban) ~ mean_pm25 + mean_no2 + mean_ozone
					+ factor(q_popdensity)
					+ scale(poverty, scale=F) + scale(log(median_house_value), scale=F)
               		   	+ scale(log(median_household_income), scale=F) + scale(owner_occupied, scale=F)
					+ scale(education, scale=F)
		   		   	+ scale(beds/population, scale=F) 
               		   	+ scale(obese, scale=F) + scale(smoke, scale=F)
               		   	+ scale(mean_summer_temp, scale=F) + scale(mean_winter_temp, scale=F) + scale(mean_summer_rm, scale=F) + scale(mean_winter_rm, scale=F)
					, data=covid_data.urban
				  	, categorical = list(age, sex, race)
					, cross=cross.urban)
ecoregression.urban






                                                                                                   
