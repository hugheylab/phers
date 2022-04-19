-- date: 04/18/2022

SELECT PERSON_SOURCE_VALUE AS person_id, DATE(event_date) AS Entry_date, concept_code AS ICD, vocabulary_ID AS flag FROM DENNY_OMOP_SD..V_all_events e
JOIN DENNY_OMOP_SD..person using (PERSON_ID)
JOIN DENNY_OMOP_SD..concept c4 ON c4.concept_ID = source_concept_ID
WHERE vocabulary_ID = 'ICD9CM' OR vocabulary_ID = 'ICD10CM'
GROUP BY PERSON_SOURCE_VALUE, event_date, concept_code, vocabulary_ID;
