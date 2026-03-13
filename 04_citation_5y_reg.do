cd "D:\Work\pwords"  
import delimited "citation_5_reg_data.csv", clear varnames(1) 

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


egen rank_citations = rank(year_citations_5)
egen rank_novelty = rank(novelty_tail_10)

gen year_citations_5_qnorm = (rank_citations) / _N
gen novelty_min_qnorm = (rank_novelty) / _N


* Model 1: Base Model
eststo citation_1: regress year_citations_5_qnorm ///
    frac_hype_words ///
    ib2.team_gender_n ib2.attorney_gender_n ///
    i.year i.cpc_class_n i.applicant_entity_n i.business_entity_status_n ///
     , vce(robust)

* Model 2: + Linguistic & Content Variables
eststo citation_2: regress year_citations_5_qnorm ///
    frac_hype_words ///
    flesch_reading_ease concreteness novelty_min_qnorm num_cpc_code ///
    i.num_words_5cate ///
    ib2.team_gender_n ib2.attorney_gender_n ///
    i.year i.cpc_class_n i.applicant_entity_n i.business_entity_status_n ///
     , vce(robust)

* Model 3: + Human-related Variables (Full Model)
 eststo citation_3: regress year_citations_5_qnorm ///
     frac_hype_words ///
     flesch_reading_ease concreteness novelty_min_qnorm num_cpc_code num_inventors ///
     i.num_words_5cate ///
     num_prior_apps_log num_prior_patents_log attorney_num_prior_apps_log ///
     ib2.team_gender_n ib2.attorney_gender_n ///
     i.year i.cpc_class_n i.applicant_entity_n i.business_entity_status_n ///
     , vce(robust)
estimates restore citation_3		
margins, at(frac_hype_words=(0(0.0005)0.005)) atmeans vsquish post saving("margins_citation_5.dta", replace)