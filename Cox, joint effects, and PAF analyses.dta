///Primary Cox analysis for the associations between intensity-specific PA and mortality, mdoel 1-- 2 -- 3
local nn "death_HES death_cancer death_CVD"
local mm "TPA_self_4 LPA_self_4 MPA_self_4 VPA_self_4 MVPA_self_4 TPA_4 LPA_4 MPA_4 VPA_4 MVPA_4 TPA_5 LPA_5 MPA_5 VPA_5 MVPA_5"
local covariates1 age_accel sex new_ethnic i.qualification i.season_wear duration_wear 
local covariates2 age_accel sex new_ethnic i.qualification i.season_wear duration_wear i.smoke alcohol_unit i.diet_score sleep_score1
local covariates3 age_accel sex new_ethnic i.qualification i.season_wear duration_wear i.smoke alcohol_unit i.diet_score sleep_score1 diabetes_duration BMI wasit history_cancer_CVD self_hypertension i.health_self i.long_illness_injury i.illness_injury_2years
foreach v of varlist `nn'{
       foreach w of varlist `mm'{
	   stset follow_death, failure(`v'==1)
	   stcox i.`w' `covariates1'  //model1
	   est store `w'_1
	   asdoc estat phtest, detail, save(PH_`v'_model1) append
	   esttab `w'_1 using primary_model1.rtf, title("`v'") eform ci(2) wide nostar b(%9.2f) keep(*.`w') append
	   
	   stcox i.`w' `covariates2'  //model2
	   est store `w'_2
	   asdoc estat phtest, detail, save(PH_`v'_model2) append
	   esttab `w'_2 using primary_model2.rtf, title("`v'") eform ci(2) wide nostar b(%9.2f) keep(*.`w') append
	   
	   stcox i.`w' `covariates3'  //model3
	   est store `w'_3
	   asdoc estat phtest, detail, save(PH_`v'_model3) append
	   esttab `w'_3 using primary_model3.rtf, title("`v'") eform ci(2) wide nostar b(%9.2f) keep(*.`w') append
	   }
	   }

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
foreach v of varlist LPA_self_4 MPA_self_4 VPA_self_4 MVPA_self_4 {
gen a_`v' = 0
replace a_`v' = 1 if `v' == 1
gen b_`v' = 0
replace b_`v' = 1 if `v' == 2
gen c_`v' = 0
replace c_`v' = 1 if `v' == 3
gen d_`v' = 0
replace d_`v' = 1 if `v' == 4
}

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
