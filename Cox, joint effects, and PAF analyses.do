///Time-varying Cox analysis
stset follow_death, failure(death_HES == 1)
local covariates3 sex new_ethnic i.qualification duration_wear i.smoke alcohol_unit i.diet_score sleep_score1 diabetes_duration BMI wasit self_hypertension i.health_self i.long_illness_injury i.illness_injury_2years
stcox i.LPA_self_4 `covariates3', tvc(age_accel i.season_wear history_cancer_CVD)

stset follow_death, failure(death_HES == 1)
local covariates3 sex new_ethnic i.qualification duration_wear i.smoke alcohol_unit i.diet_score sleep_score1 diabetes_duration BMI wasit self_hypertension i.health_self i.long_illness_injury i.illness_injury_2years
stcox i.MPA_self_4 `covariates3', tvc(age_accel i.season_wear history_cancer_CVD)

stset follow_death, failure(death_HES == 1)
local covariates3 sex new_ethnic i.qualification duration_wear i.smoke alcohol_unit i.diet_score sleep_score1 diabetes_duration BMI wasit self_hypertension i.health_self i.long_illness_injury i.illness_injury_2years
stcox i.VPA_self_4 `covariates3', tvc(age_accel i.season_wear history_cancer_CVD)

stset follow_death, failure(death_HES == 1)
local covariates3 sex new_ethnic i.qualification duration_wear i.smoke alcohol_unit i.diet_score sleep_score1 diabetes_duration BMI wasit self_hypertension i.health_self i.long_illness_injury i.illness_injury_2years i.season_wear 
stcox i.MVPA_self_4 `covariates3', tvc(age_accel history_cancer_CVD)

stset follow_death, failure(death_cancer == 1)
local covariates3 sex new_ethnic i.qualification duration_wear i.smoke alcohol_unit i.diet_score sleep_score1 diabetes_duration BMI wasit self_hypertension i.health_self i.long_illness_injury i.illness_injury_2years 
stcox i.LPA_self_4 `covariates3', tvc(age_accel history_cancer_CVD i.season_wear)

stset follow_death, failure(death_cancer == 1)
local covariates3 sex new_ethnic i.qualification duration_wear i.smoke alcohol_unit i.diet_score sleep_score1 diabetes_duration BMI wasit self_hypertension i.health_self i.long_illness_injury i.illness_injury_2years 
stcox i.MPA_self_4 `covariates3', tvc(age_accel history_cancer_CVD i.season_wear)

stset follow_death, failure(death_cancer == 1)
local covariates3 sex new_ethnic i.qualification duration_wear i.smoke alcohol_unit i.diet_score sleep_score1 diabetes_duration BMI wasit self_hypertension i.health_self i.long_illness_injury i.illness_injury_2years 
stcox i.VPA_self_4 `covariates3', tvc(age_accel history_cancer_CVD i.season_wear)

stset follow_death, failure(death_cancer == 1)
local covariates3 sex new_ethnic i.qualification duration_wear i.smoke alcohol_unit i.diet_score sleep_score1 diabetes_duration BMI wasit self_hypertension i.health_self i.long_illness_injury i.illness_injury_2years 
stcox i.MVPA_self_4 `covariates3', tvc(age_accel history_cancer_CVD i.season_wear)

stset follow_death, failure(death_CVD == 1)
local covariates3 sex new_ethnic i.qualification duration_wear i.smoke alcohol_unit i.diet_score sleep_score1 diabetes_duration BMI wasit self_hypertension i.health_self i.long_illness_injury i.illness_injury_2years i.season_wear
stcox i.LPA_self_4 `covariates3', tvc(age_accel history_cancer_CVD)

stset follow_death, failure(death_CVD == 1)
local covariates3 sex new_ethnic i.qualification duration_wear i.smoke alcohol_unit i.diet_score sleep_score1 diabetes_duration BMI wasit self_hypertension i.health_self i.long_illness_injury i.illness_injury_2years i.season_wear
stcox i.MPA_self_4 `covariates3', tvc(age_accel history_cancer_CVD)

stset follow_death, failure(death_CVD == 1)
local covariates3 sex new_ethnic i.qualification duration_wear i.smoke alcohol_unit i.diet_score sleep_score1 diabetes_duration BMI wasit self_hypertension i.health_self i.long_illness_injury i.illness_injury_2years i.season_wear
stcox i.VPA_self_4 `covariates3', tvc(age_accel history_cancer_CVD)

stset follow_death, failure(death_CVD == 1)
local covariates3 sex new_ethnic i.qualification duration_wear i.smoke alcohol_unit i.diet_score sleep_score1 diabetes_duration BMI wasit self_hypertension i.health_self i.long_illness_injury i.illness_injury_2years i.season_wear
stcox i.MVPA_self_4 `covariates3', tvc(age_accel history_cancer_CVD)

///Analyses for joint effects of different intensities of PA
local covariates3 age_accel sex new_ethnic i.qualification i.season_wear duration_wear i.smoke alcohol_unit i.diet_score sleep_score1 diabetes_duration BMI wasit history_cancer_CVD self_hypertension i.health_self i.long_illness_injury i.illness_injury_2years
foreach v of varlist death_HES death_cancer death_CVD {
stset follow_death , failure(`v' == 1)
stcox i.LPA_self_4#i.MPA_self_4 `covariates3'
est store x1
esttab x1 using `v'_joint.rtf , eform ci(2)  wide nostar b(%9.2f) keep(*.LPA_self_4#*.MPA_self_4) append 

stcox i.LPA_self_4#i.VPA_self_4 `covariates3'
est store x2
esttab x2 using `v'_joint.rtf , eform ci(2)  wide nostar b(%9.2f) keep(*.LPA_self_4#*.VPA_self_4) append 

stcox i.MPA_self_4#i.VPA_self_4 `covariates3'
est store x3
esttab x3 using `v'_joint.rtf , eform ci(2)  wide nostar b(%9.2f) keep(*.MPA_self_4#*.VPA_self_4) append
}

///PAF analysis
local covariates3 age_accel sex new_ethnic i.qualification i.season_wear duration_wear i.smoke alcohol_unit i.diet_score sleep_score1 diabetes_duration BMI wasit history_cancer_CVD self_hypertension i.health_self i.long_illness_injury i.illness_injury_2years

stset follow_death , failure(death_HES == 1)
foreach v of varlist a_LPA_self_4 b_LPA_self_4 c_LPA_self_4 {
stcox a_LPA_self_4 b_LPA_self_4 c_LPA_self_4 `covariates3'
asdoc punaf, eform at(`v'=0), save(PAF_all_cause) append 
}

foreach v of varlist a_MPA_self_4 b_MPA_self_4 c_MPA_self_4 {
stcox a_MPA_self_4 b_MPA_self_4 c_MPA_self_4 `covariates3'
asdoc punaf, eform at(`v'=0), save(PAF_all_cause) append 
}

foreach v of varlist a_VPA_self_4 b_VPA_self_4 c_VPA_self_4 {
stcox a_VPA_self_4 b_VPA_self_4 c_VPA_self_4 `covariates3'
asdoc punaf, eform at(`v'=0), save(PAF_all_cause) append 
}

foreach v of varlist a_MVPA_self_4 b_MVPA_self_4 c_MVPA_self_4 {
stcox a_MVPA_self_4 b_MVPA_self_4 c_MVPA_self_4 `covariates3'
asdoc punaf, eform at(`v'=0), save(PAF_all_cause) append 
}
