cd "D:\Work\pwords"  
import delimited "patent_reg_data.csv", clear varnames(1) 

* EDA Plot
collapse (mean) mean_hype = frac_hype_words ///     
         (mean) grant_rate = decision ///         
         (first) section = cpc_section ///          
         (count) n_patents = appeal_decision ///          
         , by(cpc_class)
export delimited using "D:\Work\pwords\plot_patent_EDA.csv", replace

encode team_gender, generate(team_gender_n)
encode attorney_gender, generate(attorney_gender_n)
encode examiner_gender, generate(examiner_gender_n)
encode cpc_class, generate(cpc_class_n)
encode cpc_section, generate(cpc_section_n)
encode applicant_entity, generate(applicant_entity_n)
encode business_entity_status, generate(business_entity_status_n)


gen num_words_log = log(num_words+1)
gen num_prior_apps_log = log(num_prior_apps+1)
gen num_prior_patents_log = log(num_prior_patents+1)
gen attorney_num_prior_apps_log = log(attorney_num_prior_apps+1)
gen examiner_num_prior_apps_log = log(examiner_num_prior_apps+1)

gen attorney_num_prior_patents_log = log(attorney_num_prior_patents+1)
egen num_words_5cate = cut(num_words), group(5)
egen examiner_num_prior_apps_5cate = cut(examiner_num_prior_apps), group(5)

egen rank_novelty = rank(novelty_tail_10)
gen novelty_min_qnorm = (rank_novelty) / _N

* Model 1: Base Model
eststo mod1_patent: logit decision ///
    frac_hype_words  ///
    ib2.team_gender_n ib2.attorney_gender_n ib2.examiner_gender_n ///
    i.year i.cpc_class_n i.applicant_entity_n i.business_entity_status_n  ///
	, iter(10)  

* Model 2: + Linguistic Variables
eststo mod2_patent: logit decision ///
    frac_hype_words i.examiner_num_prior_apps_5cate  ///
    flesch_reading_ease concreteness novelty_min_qnorm num_cpc_code ///
    i.num_words_5cate ///
    ib2.team_gender_n ib2.attorney_gender_n ib2.examiner_gender_n ///
    i.year i.cpc_class_n i.applicant_entity_n i.business_entity_status_n  ///
	, iter(10)  
* Model 3: + Human-related Variables
eststo mod3_patent: logit decision ///
    frac_hype_words ///
    flesch_reading_ease concreteness novelty_min_qnorm num_cpc_code num_inventors ///
    i.num_words_5cate i.examiner_num_prior_apps_5cate ///
    num_prior_apps_log num_prior_patents_log attorney_num_prior_apps_log ///
    ib2.team_gender_n ib2.attorney_gender_n ib2.examiner_gender_n ///
    i.year i.cpc_class_n i.applicant_entity_n i.business_entity_status_n  ///
	, iter(10)  
* margins patent
estimates restore mod3_patent
quietly margins, at(frac_hype_words=(0(0.0005)0.005) year=2019) vsquish post saving("margins_patent.dta", replace)


***CPC Section interaction
eststo logit_cpc: logit decision ///
    c.frac_hype_words##i.cpc_section_n ///
    flesch_reading_ease concreteness novelty_min_qnorm ///
    num_cpc_code num_inventors ///
    i.num_words_5cate   i.examiner_num_prior_apps_5cate  ///
    num_prior_apps_log num_prior_patents_log attorney_num_prior_apps_log ///
    ib2.team_gender_n ib2.attorney_gender_n ib2.examiner_gender_n ///
    i.year i.applicant_entity_n i.business_entity_status_n ///
	, iter(10) 
	
estimates restore logit_cpc 
margins i.cpc_section_n, at(frac_hype_words=(0(0.0005)0.005))  vsquish post saving("margins_patent_cpc.dta", replace)
marginsplot	


***examiner_num_prior_apps_5cate interaction
eststo logit_mod_interact_prior: logit decision ///
    c.frac_hype_words##i.examiner_num_prior_apps_5cate ///
    flesch_reading_ease concreteness novelty_min_qnorm ///
    num_cpc_code num_inventors ///
    i.num_words_5cate  ///
    num_prior_apps_log num_prior_patents_log  attorney_num_prior_apps_log ///
    ib2.team_gender_n ib2.attorney_gender_n ib2.examiner_gender_n ///
    i.year i.cpc_class_n i.applicant_entity_n i.business_entity_status_n ///
	, iter(10) 

est restore logit_mod_interact_prior	
quietly margins, i.examiner_num_prior_apps_5cate at(frac_hype_words=(0(0.0005)0.005) year=2019) 
vsquish post saving("margins_patent_prior.dta", replace)


	
***gender interaction
eststo mod_interact_gender: logit decision ///
    c.frac_hype_words##ib2.examiner_gender_n ///
    flesch_reading_ease concreteness novelty_min_qnorm ///
    num_cpc_code num_inventors ///
    i.num_words_5cate i.examiner_num_prior_apps_5cate ///
    num_prior_apps_log num_prior_patents_log attorney_num_prior_apps_log ///
    ib2.team_gender_n ib2.attorney_gender_n ///
    i.year i.cpc_class_n i.applicant_entity_n i.business_entity_status_n ///
	, iter(10)  
	
estimates restore mod_interact_gender	
margins ib2.examiner_gender_n, ///
    at(c.frac_hype_words=(0(0.0005)0.005)) ///
    predict(pr) vsquish post saving("patent_gender.dta", replace) 



*** DV = transfer

* Model 1: Base Model
eststo mod_transfer1: logit application_transfered ///
    frac_hype_words  ///
    ib2.team_gender_n ib2.attorney_gender_n ///
    i.year i.cpc_class_n i.applicant_entity_n i.business_entity_status_n , iter(10) 

* Model 2: + Linguistic Variables
eststo mod_transfer2: logit application_transfered ///
    frac_hype_words i.cpc_section_n ///
    flesch_reading_ease concreteness novelty_min_qnorm num_cpc_code ///
    i.num_words_5cate ///
    ib2.team_gender_n ib2.attorney_gender_n ///
    i.year i.cpc_class_n i.applicant_entity_n i.business_entity_status_n, iter(10) 
* Model 3: + Human-related Variables
eststo mod_transfer3: logit application_transfered ///
    frac_hype_words i.cpc_section_n ///
    flesch_reading_ease concreteness novelty_min_qnorm num_cpc_code num_inventors ///
    i.num_words_5cate ///
    num_prior_apps_log num_prior_patents_log attorney_num_prior_apps_log ///
    ib2.team_gender_n ib2.attorney_gender_n ///
    i.year i.cpc_class_n i.applicant_entity_n i.business_entity_status_n, iter(10) 

* margins transfer
estimates restore mod_transfer3
quietly margins, at(frac_hype_words=(0(0.0005)0.005)) vsquish post saving("margins_transfer.dta", replace)

*** cpc interaction
eststo cpc_transfer: logit application_transfered ///
    c.frac_hype_words##i.cpc_section_n ///
    flesch_reading_ease concreteness novelty_min_qnorm num_cpc_code num_inventors ///
    i.num_words_5cate ///
    num_prior_apps_log num_prior_patents_log attorney_num_prior_apps_log ///
    ib2.team_gender_n ib2.attorney_gender_n ///
    i.year i.applicant_entity_n i.business_entity_status_n ///
    , iter(10) 
	
estimates restore cpc_transfer
margins i.cpc_section_n, at(frac_hype_words=(0(0.0005)0.005) year=2023) vsquish post saving("transfered_cpc.dta", replace)


* DV = novelty score percentile
* Model 1: Base Model
eststo novelty_1: regress novelty_min_qnorm ///
    frac_hype_words ///
    ib2.team_gender_n ib2.attorney_gender_n ///
    i.year i.cpc_class_n i.applicant_entity_n i.business_entity_status_n , iter(10) 

* Model 2: + Linguistic Variables
eststo novelty_2: regress novelty_min_qnorm ///
    frac_hype_words ///
    flesch_reading_ease concreteness num_cpc_code ///
    i.num_words_5cate ///
    ib2.team_gender_n ib2.attorney_gender_n ///
    i.year i.cpc_class_n i.applicant_entity_n i.business_entity_status_n, iter(10) 
	
* Model 3: + Human-related Variables
eststo novelty_3: regress novelty_min_qnorm ///
    frac_hype_words ///
	flesch_reading_ease concreteness ///
    num_cpc_code num_inventors num_prior_apps_log ///
    num_prior_patents_log attorney_num_prior_apps_log ///
    i.num_words_5cate ib2.team_gender_n ib2.attorney_gender_n ///
    i.year i.cpc_class_n i.applicant_entity_n i.business_entity_status_n , iter(10) 
		
* Margins Novelty
estimates restore novelty_3
margins, at(frac_hype_words=(0(0.0005)0.005)) atmeans vsquish post saving("margins_novelty_min.dta", replace)

/*	
eststo novelty_min_model: regress novelty_min_qnorm ///
    c.frac_hype_words##i.cpc_section_n flesch_reading_ease concreteness ///
    num_cpc_code num_inventors num_prior_apps_log ///
    num_prior_patents_log attorney_num_prior_apps_log ///
    i.num_words_5cate ib2.team_gender_n ib2.attorney_gender_n ///
    i.year i.applicant_entity_n i.business_entity_status_n 
margins i.cpc_section_n, at(frac_hype_words = (0(0.0005)0.005)) atmeans vsquish post saving("novelty_cpc2.dta", replace)

	
eststo novelty_median_model: regress novelty_median ///
    frac_hype_words flesch_reading_ease concreteness ///
    num_cpc_code num_inventors num_prior_apps_log ///
    num_prior_patents_log attorney_num_prior_apps_log ///
    i.num_words_5cate ib2.team_gender_n ib2.attorney_gender_n ///
    i.year i.cpc_class_n i.applicant_entity_n i.business_entity_status_n 
estimates restore novelty_median_model
margins, at(frac_hype_words=(0(0.0005)0.005)) atmeans vsquish post saving("margins_novelty_median.dta", replace)
*/