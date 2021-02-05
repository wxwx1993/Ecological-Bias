library(ecoreg)


covid_data<-data.frame(read.csv('E:/Work/covid/covid_data20210202.csv'))

county_num <- length(covid_data[,1])


N <- covid_data[,23]
y_confirmed <- covid_data[,24]
y_death <- covid_data[,25]
cross <- covid_data[,34:65]/covid_data[,33]




age <- matrix(rep(0, 8*county_num), ncol=8)
sex <- matrix(rep(0, 2*county_num), ncol=2)
race <- matrix(rep(0, 2*county_num), ncol=2)
for (i in 1:8){
	age[,i] <- rowSums(cross[,i+8*c(0:3)])
}
age <- age/rowSums(age)
for (i in 1:2){
	sex[,i] <- rowSums(cross[,c(matrix(1:32,ncol=16,byrow=T)[,8*i-8+1:8])])
}
sex <- sex/rowSums(sex)
for (i in 1:2){
	race[,i] <- rowSums(cross[,c(1:16)+16*i-16])
}
race <- race/rowSums(race)


agg.eco <- eco(cbind(y_death, N) ~ mean_pm25 + mean_no2 + mean_ozone
		   + factor(q_popdensity)
               + scale(poverty) + scale(log(median_house_value))
               + scale(log(median_household_income)) + scale(owner_occupied)
		   + scale(beds/population) 
               + scale(obese) + scale(smoke)
               + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
		   + factor(StateDummy), data=covid_data)

# Main
ecoregression.main <- eco(cbind(y_death, N) ~ mean_pm25 + mean_no2 + mean_ozone, data=covid_data,
				  categorical = list(age, sex, race), cross=cross)
ecoregression.main


# -cross
ecoregression.cross <- eco(cbind(y_death, N) ~ mean_pm25 + mean_no2 + mean_ozone, data=covid_data,
				  categorical = list(age, sex, race))
ecoregression.cross


# main
ecoregression.main <- eco(cbind(y_death, N) ~ mean_pm25 + mean_no2 + mean_ozone
					+ scale(poverty) + scale(log(median_house_value))
               		   	+ scale(log(median_household_income)) + scale(owner_occupied)
		   		   	+ scale(beds/population) 
               		   	+ scale(obese) + scale(smoke)
               		   	+ scale(mean_summer_temp) + scale(mean_winter_temp)
					+ scale(mean_summer_rm) + scale(mean_winter_rm), data=covid_data,
				  	categorical = list(age, sex, race), cross=cross)
ecoregression.main


# -cross
ecoregression.cross <- eco(cbind(y_death, N) ~ mean_pm25 + mean_no2 + mean_ozone
					+ scale(poverty) + scale(log(median_house_value))
               		   	+ scale(log(median_household_income)) + scale(owner_occupied)
		   		   	+ scale(beds/population) 
               		   	+ scale(obese) + scale(smoke)
               		   	+ scale(mean_summer_temp) + scale(mean_winter_temp)
					+ scale(mean_summer_rm) + scale(mean_winter_rm), data=covid_data,
				  	categorical = list(age, sex, race))
ecoregression.cross



# -category
ecoregression.category <- eco(cbind(y_death, N) ~ mean_pm25 + mean_no2 + mean_ozone
					+ scale(poverty) + scale(log(median_house_value))
               		   	+ scale(log(median_household_income)) + scale(owner_occupied)
		   		   	+ scale(beds/population) 
               		   	+ scale(obese) + scale(smoke)
               		   	+ scale(mean_summer_temp) + scale(mean_winter_temp)
					+ scale(mean_summer_rm) + scale(mean_winter_rm), data=covid_data)
ecoregression.category



# -beds
ecoregression.beds <- eco(cbind(y_death, N) ~ mean_pm25 + mean_no2 + mean_ozone
					+ scale(poverty) + scale(log(median_house_value))
               		   	+ scale(log(median_household_income)) + scale(owner_occupied)
		   		   	#+ scale(beds/population) 
               		   	+ scale(obese) + scale(smoke)
               		   	+ scale(mean_summer_temp) + scale(mean_winter_temp)
					+ scale(mean_summer_rm) + scale(mean_winter_rm), data=covid_data,
				  	categorical = list(age, sex, race), cross=cross)
ecoregression.beds



# - smoking + bmi
ecoregression.brfss <- eco(cbind(y_death, N) ~ mean_pm25 + mean_no2 + mean_ozone
					+ scale(poverty) + scale(log(median_house_value))
               		   	+ scale(log(median_household_income)) + scale(owner_occupied)
		   		   	+ scale(beds/population) 
               		   	#+ scale(obese) + scale(smoke)
               		   	+ scale(mean_summer_temp) + scale(mean_winter_temp)
					+ scale(mean_summer_rm) + scale(mean_winter_rm), data=covid_data,
				  	categorical = list(age, sex, race), cross=cross)
ecoregression.brfss


# - temp + humidity
ecoregression.weather <- eco(cbind(y_death, N) ~ mean_pm25 + mean_no2 + mean_ozone
					+ scale(poverty) + scale(log(median_house_value))
               		   	+ scale(log(median_household_income)) + scale(owner_occupied)
		   		   	+ scale(beds/population) 
               		   	+ scale(obese) + scale(smoke)
               		   	#+ scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
					, data=covid_data,
				  	categorical = list(age, sex, race), cross=cross)
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
cross.nyc <- covid_data.nyc[,34:65]/covid_data.nyc[,33]
ecoregression.nyc <- eco(cbind(y_death.nyc, N.nyc) ~ mean_pm25 + mean_no2 + mean_ozone
					+ scale(poverty) + scale(log(median_house_value))
               		   	+ scale(log(median_household_income)) + scale(owner_occupied)
		   		   	+ scale(beds/population) 
               		   	+ scale(obese) + scale(smoke)
               		   	+ scale(mean_summer_temp) + scale(mean_winter_temp)
					+ scale(mean_summer_rm) + scale(mean_winter_rm), data=covid_data.nyc,
				  	categorical = list(age, sex, race), cross=cross.nyc)
ecoregression.nyc



# exclude <100 confirmed
covid_data.large <- subset(covid_data, confirmed >= 100)
N.large <- covid_data.large[,23]
y_confirmed.large <- covid_data.large[,24]
y_death.large <- covid_data.large[,25]
cross.large <- covid_data.large[,34:65]/covid_data.large[,33]
ecoregression.large <- eco(cbind(y_death.large, N.large) ~ mean_pm25 + mean_no2 + mean_ozone
					+ scale(poverty) + scale(log(median_house_value))
               		   	+ scale(log(median_household_income)) + scale(owner_occupied)
		   		   	+ scale(beds/population) 
               		   	+ scale(obese) + scale(smoke)
               		   	+ scale(mean_summer_temp) + scale(mean_winter_temp)
					+ scale(mean_summer_rm) + scale(mean_winter_rm), data=covid_data.large,
				  	categorical = list(age, sex, race), cross=cross.large)
ecoregression.large


                                                                                                       "36079","36081",  "36085",  "36087",  "36119",  "36027", 

# rural
covid_data.rural <- subset(covid_data, X2013.code > 4)
N.rural <- covid_data.rural[,23]
y_confirmed.rural <- covid_data.rural[,24]
y_death.rural <- covid_data.rural[,25]
cross.rural <- covid_data.rural[,34:65]/covid_data.rural[,33]
ecoregression.rural <- eco(cbind(y_death.rural, N.rural) ~ mean_pm25 + mean_no2 + mean_ozone
					+ scale(poverty) + scale(log(median_house_value))
               		   	+ scale(log(median_household_income)) + scale(owner_occupied)
		   		   	+ scale(beds/population) 
               		   	+ scale(obese) + scale(smoke)
               		   	+ scale(mean_summer_temp) + scale(mean_winter_temp)
					+ scale(mean_summer_rm) + scale(mean_winter_rm), data=covid_data.rural,
				  	categorical = list(age, sex, race), cross=cross.rural)
ecoregression.rural     



# urban
covid_data.urban <- subset(covid_data, X2013.code <= 4)
N.urban <- covid_data.urban[,23]
y_confirmed.urban <- covid_data.urban[,24]
y_death.urban <- covid_data.urban[,25]
cross.urban <- covid_data.urban[,34:65]/covid_data.urban[,33]
ecoregression.urban <- eco(cbind(y_death.urban, N.urban) ~ mean_pm25 + mean_no2 + mean_ozone
					+ scale(poverty) + scale(log(median_house_value))
               		   	+ scale(log(median_household_income)) + scale(owner_occupied)
		   		   	+ scale(beds/population) 
               		   	+ scale(obese) + scale(smoke)
               		   	+ scale(mean_summer_temp) + scale(mean_winter_temp)
					+ scale(mean_summer_rm) + scale(mean_winter_rm), data=covid_data.urban,
				  	categorical = list(age, sex, race), cross=cross.urban)
ecoregression.urban




















                                                                                                            "36071",  "09005",  "34021")
