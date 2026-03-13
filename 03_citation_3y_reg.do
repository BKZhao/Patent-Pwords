cd "D:\Work\pwords"  
import delimited "citation_3_reg_data.csv", clear varnames(1) 

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
gen attorney_num_prior_patents_log = log(attorney_num_prior_patents+1)
egen num_words_5cate = cut(num_words), group(5)


egen rank_citations = rank(year_citations_3)
egen rank_novelty = rank(novelty_tail_10)

gen year_citations_3_qnorm = (rank_citations) / _N
gen novelty_min_qnorm = (rank_novelty) / _N

* EDA Plot
collapse (mean) mean_hype = frac_hype_words ///   
         (mean) grant_rate = year_citations_3_qnorm ///            
         (first) section = cpc_section ///         
         (count) n_patents = year_citations_3_qnorm ///          
         , by(cpc_class)

export delimited using "D:\Work\pwords\plot_citations_EDA.csv", replace


* Model 1: Base Model
eststo citation_1: regress year_citations_3_qnorm ///
    frac_hype_words ///
    ib2.team_gender_n ib2.attorney_gender_n ///
    i.year i.cpc_class_n i.applicant_entity_n i.business_entity_status_n ///
     , vce(robust)

* Model 2: + Linguistic & Content Variables
eststo citation_2: regress year_citations_3_qnorm ///
    frac_hype_words ///
    flesch_reading_ease concreteness novelty_min_qnorm num_cpc_code ///
    i.num_words_5cate ///
    ib2.team_gender_n ib2.attorney_gender_n ///
    i.year i.cpc_class_n i.applicant_entity_n i.business_entity_status_n ///
     , vce(robust)

* Model 3: + Human-related Variables (Full Model)
 eststo citation_3: regress year_citations_3_qnorm ///
     frac_hype_words ///
     flesch_reading_ease concreteness novelty_min_qnorm num_cpc_code num_inventors ///
     i.num_words_5cate ///
     num_prior_apps_log num_prior_patents_log attorney_num_prior_apps_log ///
     ib2.team_gender_n ib2.attorney_gender_n ///
     i.year i.cpc_class_n i.applicant_entity_n i.business_entity_status_n ///
     , vce(robust)
* margins 	
estimates restore citation_3	
margins, at(frac_hype_words=(0(0.0005)0.005)) atmeans vsquish post saving("margins_citation_3.dta", replace)
	 
	 
/*	 
*=========================================
* Percentile Ranks by years
*=========================================
bysort year: egen rank_citations_year = rank(year_citations_3)
bysort year: gen count_year_cite = _N
gen citations_3_qnorm = (rank_citations_year) / count_year_cite 

eststo reg_citation_3: regress citations_3_qnorm ///
     frac_hype_words ///
     flesch_reading_ease concreteness novelty_min_qnorm num_cpc_code num_inventors ///
     i.num_words_5cate ///
     num_prior_apps_log num_prior_patents_log attorney_num_prior_apps_log ///
     ib2.team_gender_n ib2.attorney_gender_n ///
     i.year i.cpc_class_n i.applicant_entity_n i.business_entity_status_n ///
     , vce(robust)

egen citations_3 = rank(year_citations_3)
replace citations_3 = citations_3 / _N
*/

/*
eststo nbr_citation_3: nbreg citations_3  ///
    c.frac_hype_words##i.cpc_section_n ///
    flesch_reading_ease concreteness novelty_min num_cpc_code num_inventors ///
    i.num_words_5cate  ///
    num_prior_apps_log num_prior_patents_log attorney_num_prior_apps_log ///
    ib2.team_gender_n ib2.attorney_gender_n  ///
    i.year i.applicant_entity_n i.business_entity_status_n 
	
estimates restore nbr_citation_3
margins i.cpc_section_n, at(frac_hype_words = (0(0.0005)0.005)) atmeans vsquish post saving("citation_cpc.dta", replace)
margins, at(frac_hype_words=(0(0.0005)0.005)) atmeans vsquish post saving("citation_cpc.dta", replace)
*/
