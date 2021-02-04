library(ecoreg)


covid_data<-data.frame(read.csv('E:/Work/covid/covid_data20210202.csv'))

county_num <- length(covid_data[,1])


N <- covid_data[,23]
y_confirmed <- covid_data[,24]
y_death <- covid_data[,25]
cross <- covid_data[,34:159]/rowSums(covid_data[,34:159])
age <- matrix(rep(0, 9*county_num), ncol=9)
sex <- matrix(rep(0, 2*county_num), ncol=2)
race <- matrix(rep(0, 7*county_num), ncol=7)
for (i in 1:9){
	age[,i] <- rowSums(cross[,i+9*c(0:13)])
}
age <- age/rowSums(age)
for (i in 1:2){
	sex[,i] <- rowSums(cross[,c(matrix(1:126,ncol=18,byrow=T)[,9*i-9+1:9])])
}
sex <- sex/rowSums(sex)
for (i in 1:7){
	race[,i] <- rowSums(cross[,c(1:18)+18*i-18])
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
ecoregression.main <- eco(cbind(y_death, N) ~ mean_pm25 + mean_no2 + mean_ozone
		   		  + factor(q_popdensity)
               		  + scale(poverty) + scale(log(median_house_value))
               		  + scale(log(median_household_income)) + scale(owner_occupied)
		   		  + scale(beds/population) 
               		  + scale(obese) + scale(smoke)
               		  + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
		   		  + factor(StateDummy), data=covid_data,
				  categorical = list(age, sex, race), cross=cross)


# -cross
ecoregression.cross <- eco(cbind(y_death, N) ~ mean_pm25 + mean_no2 + mean_ozone
		   		   + factor(q_popdensity)
               		   + scale(poverty) + scale(log(median_house_value))
               		   + scale(log(median_household_income)) + scale(owner_occupied)
		   		   + scale(beds/population) 
               		   + scale(obese) + scale(smoke)
               		   + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
		   		   + factor(StateDummy), data=covid_data,
				   categorical = list(age, sex, race))

# -beds
ecoregression.beds <- eco(cbind(y_death, N) ~ mean_pm25 + mean_no2 + mean_ozone
		   		   + factor(q_popdensity)
               		   + scale(poverty) + scale(log(median_house_value))
               		   + scale(log(median_household_income)) + scale(owner_occupied)
		   		   #+ scale(beds/population) 
               		   + scale(obese) + scale(smoke)
               		   + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
		   		   + factor(StateDummy), data=covid_data,
				   categorical = list(age, sex, race), cross=cross)

# - smoking + bmi
ecoregression.brfss <- eco(cbind(y_death, N) ~ mean_pm25 + mean_no2 + mean_ozone
		   		   + factor(q_popdensity)
               		   + scale(poverty) + scale(log(median_house_value))
               		   + scale(log(median_household_income)) + scale(owner_occupied)
		   		   + scale(beds/population) 
               		   #+ scale(obese) + scale(smoke)
               		   + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
		   		   + factor(StateDummy), data=covid_data,
				   categorical = list(age, sex, race), cross=cross)

# - temp + humidity
ecoregression.weather <- eco(cbind(y_death, N) ~ mean_pm25 + mean_no2 + mean_ozone
		   		   	+ factor(q_popdensity)
               		   	+ scale(poverty) + scale(log(median_house_value))
               		   	+ scale(log(median_household_income)) + scale(owner_occupied)
		   		   	+ scale(beds/population) 
               		   	+ scale(obese) + scale(smoke)
               		   	#+ scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
		   		   	+ factor(StateDummy), data=covid_data,
				   	categorical = list(age, sex, race), cross=cross)


# exclude NY Metro
ecoregression.nyc <- eco(cbind(y_death, N) ~ mean_pm25 + mean_no2 + mean_ozone
		   		   + factor(q_popdensity)
               		   + scale(poverty) + scale(log(median_house_value))
               		   + scale(log(median_household_income)) + scale(owner_occupied)
		   		   + scale(beds/population)
               		   + scale(obese) + scale(smoke)
               		   + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
		   		   + factor(StateDummy), data=subset(covid_data, !(fips %in% c("09001", "42089", "36111","09009","36059","36103","34013",
                                                                                       "34019", "34027", "34037","34039","42103","34023","34025","34029",
                                                                                       "34035", "34003", "34017", "34031","36005","36047","36061",
                                                                                       "36079", "36081", "36085",  "36087",  "36119",  "36027", 
                                                                                       "36071", "09005", "34021"))),
				   categorical = list(age, sex, race), cross=cross)



# exclude <100 confirmed
ecoregression.large <- eco(cbind(y_death, N) ~ mean_pm25 + mean_no2 + mean_ozone
		   		   	+ factor(q_popdensity)
               		   	+ scale(poverty) + scale(log(median_house_value))
               		   	+ scale(log(median_household_income)) + scale(owner_occupied)
		   		   	+ scale(beds/population) 
               		   	+ scale(obese) + scale(smoke)
               		   	#+ scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
		   		   	+ factor(StateDummy), data=subset(covid_data, confirmed >= 100),
				   	categorical = list(age, sex, race), cross=cross)                                                                                        "34035", "34003", "34017", "34031","36005","36047","36061",
                                                                                                            "36079","36081",  "36085",  "36087",  "36119",  "36027", 

# rural
ecoregression.large <- eco(cbind(y_death, N) ~ mean_pm25 + mean_no2 + mean_ozone
		   		   	+ factor(q_popdensity)
               		   	+ scale(poverty) + scale(log(median_house_value))
               		   	+ scale(log(median_household_income)) + scale(owner_occupied)
		   		   	+ scale(beds/population) 
               		   	+ scale(obese) + scale(smoke)
               		   	#+ scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
		   		   	+ factor(StateDummy), data=subset(covid_data, X2013.code > 4),
				   	categorical = list(age, sex, race), cross=cross)                                                                                        "34035", "34003", "34017", "34031","36005","36047","36061",
              

# urban
ecoregression.large <- eco(cbind(y_death, N) ~ mean_pm25 + mean_no2 + mean_ozone
		   		   	+ factor(q_popdensity)
               		   	+ scale(poverty) + scale(log(median_house_value))
               		   	+ scale(log(median_household_income)) + scale(owner_occupied)
		   		   	+ scale(beds/population) 
               		   	+ scale(obese) + scale(smoke)
               		   	#+ scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
		   		   	+ factor(StateDummy), data=subset(covid_data, X2013.code <= 4),
				   	categorical = list(age, sex, race), cross=cross)                                                                                        "34035", "34003", "34017", "34031","36005","36047","36061",
       




















                                                                                                            "36071",  "09005",  "34021")
