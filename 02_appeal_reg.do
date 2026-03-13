cd "D:\Work\pwords"  
import delimited "appeal_reg_data.csv", clear varnames(1) 
* EDA Plot
collapse (mean) mean_hype = frac_hype_words ///      
         (mean) grant_rate = appeal_decision ///           
         (first) section = cpc_section ///          
         (count) n_patents = appeal_decision ///           
         , by(cpc_class)
export delimited using "D:\Work\pwords\plot_appeal_EDA.csv", replace

encode team_gender, generate(team_gender_n)
encode attorney_gender, generate(attorney_gender_n)
encode cpc_class, generate(cpc_class_n)
encode cpc_section, generate(cpc_section_n)
encode applicant_entity, generate(applicant_entity_n)
encode business_entity_status, generate(business_entity_status_n)

gen num_words_log = log(num_words+1)
gen num_prior_apps_log = log(num_prior_apps+1)
gen num_prior_patents_log = log(num_prior_patents+1)
gen attorney_num_prior_apps_log = log(attorney_num_prior_apps+1)
gen attorney_num_prior_patents_log = log(attorney_num_prior_patents+1)
egen num_words_5cate = cut(num_words), group(5)

egen rank_novelty = rank(novelty_tail_10)
gen novelty_min_qnorm = (rank_novelty) / _N


* Model 1: Base Model
eststo mod_appeal1: logit appeal_decision ///
    frac_hype_words ///
    ib2.team_gender_n ib2.attorney_gender_n ///
    i.year i.cpc_class_n i.applicant_entity_n i.business_entity_status_n ///
    , iter(50) vce(robust) // 

* Model 2: + Linguistic & Content Variables
eststo mod_appeal2: logit appeal_decision ///
    frac_hype_words ///
    flesch_reading_ease concreteness novelty_min_qnorm num_cpc_code ///
    i.num_words_5cate ///
    ib2.team_gender_n ib2.attorney_gender_n ///
    i.year i.cpc_class_n i.applicant_entity_n i.business_entity_status_n ///
    , iter(50) vce(robust)

* Model 3: + Human-related Variables (Full Model)
eststo mod_appeal3: logit appeal_decision ///
    frac_hype_words ///
    flesch_reading_ease concreteness novelty_min_qnorm num_cpc_code num_inventors ///
    i.num_words_5cate ///
    num_prior_apps_log num_prior_patents_log ///
    attorney_num_prior_apps_log  /// 
    ib2.team_gender_n ib2.attorney_gender_n ///
    i.year i.cpc_class_n i.applicant_entity_n i.business_entity_status_n ///
    , iter(50) vce(robust)
* margins 		
estimates restore mod_appeal3
margins, at(frac_hype_words=(0(0.0005)0.005)) atmeans vsquish post saving("margins_appeal.dta", replace)


/*
eststo logit_appeal: logit appeal_decision ///
    c.frac_hype_words##i.cpc_section_n ///
    flesch_reading_ease concreteness novelty_percentile num_cpc_code num_inventors ///
    i.num_words_5cate  ///
    num_prior_apps_log num_prior_patents_log attorney_num_prior_apps_log ///
    ib2.team_gender_n ib2.attorney_gender_n  ///
    i.year i.applicant_entity_n i.business_entity_status_n ///
    , iter(10)  
	
margins i.cpc_section_n, at(frac_hype_words=(0(0.0005)0.005))  vsquish post saving("appeal_cpc.dta", replace)
*/