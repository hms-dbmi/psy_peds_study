CREATE TABLE ase_mental_health
AS SELECT obs.patient_num, obs.concept_cd, obs.start_date, pat.birth_date, floor(months_between(obs.start_date, pat.birth_date) / 12) as age, 
         peds_codes.mental_health_disorder_group,  peds_codes.description
FROM observation_fact obs
INNER JOIN patient_dimension pat ON obs.patient_num=pat.patient_num
INNER JOIN ASE_PEDS_ICD_CODES peds_codes ON obs.concept_cd=peds_codes.ICD10_CODE
WHERE     obs.start_date > '21-04-2020'
      AND floor(months_between(obs.start_date, pat.birth_date) / 12) > 10 
      AND floor(months_between(obs.start_date, pat.birth_date) / 12) < 18;
SELECT COUNT(*) as nrows 
FROM ase_mental_health
;


SELECT COUNT(*) as nrows 
FROM ase_mental_health
;