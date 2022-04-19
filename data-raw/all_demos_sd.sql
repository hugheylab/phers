-- date: 04/18/2022

SELECT ages.person_id, sex, dob, first_date, last_date
FROM (SELECT person_id, min(entry_date) AS first_date, max(entry_date) AS last_date FROM
(SELECT PERSON_SOURCE_VALUE AS person_id, date(event_date) AS entry_date, concept_code AS icd, vocabulary_ID AS flag from DENNY_OMOP_SD..V_all_events e
JOIN DENNY_OMOP_SD..person using (PERSON_ID)
JOIN DENNY_OMOP_SD..concept c4 on c4.concept_ID = source_concept_ID
WHERE vocabulary_ID = 'ICD9CM' OR vocabulary_ID = 'ICD10CM'
GROUP BY PERSON_SOURCE_VALUE, event_date, concept_code, vocabulary_ID) AS icds GROUP BY person_id) AS ages
JOIN (SELECT PERSON_SOURCE_VALUE AS person_id, gender_source_value AS sex, date(birth_datetime) AS dob FROM DENNY_OMOP_SD..person) AS demos
ON ages.person_id = demos.person_id;
