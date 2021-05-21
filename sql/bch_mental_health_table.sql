-- table including only primary diagnosis, inpatients from 11 to 17 y/o
CREATE TABLE mental_health_primary
AS SELECT obs.patient_num, obs.concept_cd, obs.start_date, pat.birth_date, floor(months_between(obs.start_date, pat.birth_date) / 12) as age, 
         peds_codes.mental_health_disorder_group,  peds_codes.description, obs.modifier_cd, vis.inout_cd
FROM observation_fact obs
INNER JOIN patient_dimension pat ON obs.patient_num=pat.patient_num
INNER JOIN visit_dimension vis ON obs.encounter_num= vis.encounter_num
INNER JOIN PEDS_ICD_CODES peds_codes ON obs.concept_cd=peds_codes.ICD10_CODE
WHERE floor(months_between(obs.start_date, pat.birth_date) / 12) > 10 
      AND floor(months_between(obs.start_date, pat.birth_date) / 12) < 18 
      AND lower(vis.inout_cd) = 'inpatient' 
      AND obs.modifier_cd = 'ICD:P';

-- table including only primary and secondary diagnosis, inpatients from 11 to 17 y/o
CREATE TABLE mental_health_icdPandS
AS SELECT obs.patient_num, obs.concept_cd, obs.start_date, pat.birth_date, floor(months_between(obs.start_date, pat.birth_date) / 12) as age, 
         peds_codes.mental_health_disorder_group,  peds_codes.description, obs.modifier_cd, vis.inout_cd
FROM observation_fact obs
INNER JOIN patient_dimension pat ON obs.patient_num=pat.patient_num
INNER JOIN visit_dimension vis ON obs.encounter_num= vis.encounter_num
INNER JOIN PEDS_ICD_CODES peds_codes ON obs.concept_cd=peds_codes.ICD10_CODE
WHERE floor(months_between(obs.start_date, pat.birth_date) / 12) > 6
      AND floor(months_between(obs.start_date, pat.birth_date) / 12) < 18 
      AND lower(vis.inout_cd) IN ('inpatient', 'outpatient')
      AND obs.modifier_cd in ('ICD:P', 'ICD:S');