#EPICO España
#Load Hmisc library
#install.packages("Rtools")
#devtools::install_github("3wen/legendMap")
library(Hmisc)
library(tidyverse)
library(clean)
library(writexl)
library(lubridate)
library(magrittr)
library(readxl)
#librerias para mapas
library(giscoR)
library(dplyr)
library(sf)
library(viridis)
library(maps)
library(ggrepel)
library(maps)
library(maptools)
library(ggplot2)
library(grid)
#library(Rtools)
####ESPAÑA

#Cargar base de datos
rm(list=ls())
data <- read.csv("~/EPICO/Datos/España/EPICOCOVID19_DATA_2022-09-22_1222.csv")

#Arreglar base de datos
#-------------------------------------------------------------------------------
#Setting Labels
label(data$record)="Identification"
label(data$redcap_data_access_group)="Data Access Group"
label(data$hospital)="Hospital"
label(data$health_worker_name)="Who fills the paper data notebook (name)"
label(data$center_id_complete)="Complete?"
label(data$patient_birth_date)="Patients date of birth"
label(data$admission_date)="Date of admission"
label(data$age_months)="Age in months"
label(data$sex)="Sex at Birth:"
label(data$last_15)="Has the child been previously admitted to hospital in the last 15 days?"
label(data$demographics_complete)="Complete?"
label(data$symptoms_epi_travel)="A history of travel to an area with documented cases of novel coronavirus infection"
label(data$symptoms_epi_physical)="Close contact with a confirmed or probable case of nCoV infection"
label(data$symptoms_epi_healthfac)="Presence in a healthcare facility where nCoV infections have been managed"
label(data$symptoms_epi_animal)="Direct contact with animals in countries where the nCoV is known to be circulating in animal populations or where human infections have occurred as a result of presumed zoonotic transmission"
label(data$symptoms_epi_comm)="Area with community transmission"
label(data$contact___1)="Close contact (choice=Health care associated exposure, including providing direct care for novel coronavirus patients, e.g. health care worker, working with health care workers infected with novel coronavirus, visiting patients or staying in the same close environment of a novel coronavirus patient, or direct exposure to body fluids or specimens including aerosols)"
label(data$contact___2)="Close contact (choice=Working together in close proximity or sharing the same classroom environment with a novel coronavirus patient)"
label(data$contact___3)="Close contact (choice=Traveling together with novel coronavirus patient in any kind of conveyance)"
label(data$contact___4)="Close contact (choice=Living in the same household as a novel coronavirus patient)"
label(data$index_case)="Who is the index case (father, mother, uncle ...)"
label(data$inclusion_criteria_complete)="Complete?"
label(data$fever_start_date)="Fever onset date"
label(data$fever_days)="Fever days (from start to admission, including start day)"
label(data$fever_days_total)="Fever days (from start to end, including start day)"
label(data$fever_3)="Fever days >= 3"
label(data$anamnesis_complete)="Complete?"
label(data$comorb)="Comorbidities"
label(data$chroniccard_mhyn)="Chronic cardiac disease, including congenital heart disease (not hypertesnion)"
label(data$hypertension_mhyn)="Hypertension"
label(data$chronicpul_mhyn)="Chronic pulmonary disease (not asthma)"
label(data$asthma_mhyn)="Asthma (or recurrent wheezing)"
label(data$tb_mhyn)="Tuberculosis"
label(data$resp_inf_mhyn)="History of respiratory infection in the previous 4 weeks prior to current illness?"
label(data$renal_mhyn)="Chronic kidney disease"
label(data$modliver_mhyn)="Moderate or severe liver disease"
label(data$mildliv_mhyn)="Mild Liver disease"
label(data$chronicneu_mhyn)="Chronic neurological disorder"
label(data$malignantneo_mhyn)="Malignant neoplasm"
label(data$chronhaemo_mhyn)="Chronic hematologic disease"
label(data$aidshiv_mhyn)="AIDS/HIV"
label(data$obesity_mhyn)="Obesity (as defined by clinical staff)"
label(data$diabetiscomp_mhyn)="Diabetes with complications"
label(data$diabetes_mhyn)="Diabetes without complications"
label(data$inflammatory_mhyr)="Inflammatory disorder"
label(data$rheumatology_mhyr)="Rheumatologic disorder"
label(data$kawa_mhyn)="Past history of Kawasaki disease"
label(data$kawa_fam_mhyn)="Family history of Kawasaki disease"
label(data$dementia_mhyn)="Dementia"
label(data$malnutrition_mhyn)="Malnutrition"
label(data$other_mhyn)="Other relevant risk factors"
label(data$inf_reum_type)="Please specify inflammatory or rheumatological disorder"
label(data$smoking_mhyn)="Smoking"
label(data$otherrisktext)=" Specify Other relevant risk factor"
label(data$immunosupressors)="On immunosuppressive medication?"
label(data$comorbidities_complete)="Complete?"
label(data$influenza_full_regimen)="Influenza vaccine (current year)"
label(data$apsc_vcageind)="Vaccinations appropriate for age/country?"
label(data$cov_vac)="COVID-19 vaccine"
label(data$cov_vac_nd)="Number of COVID-19 vaccine doses"
label(data$cov_vac_date)="Date of last dose of COVID-19 vaccine"
label(data$vaccines_complete)="Complete?"
label(data$weight)="Weight (grams)"
label(data$weight_percentile)="Weight percentile"
label(data$size)="Size (cm)"
label(data$size_percentile)="Size percentile"
label(data$anthropometry_complete)="Complete?"
label(data$temp_vsorres)="Temperature"
label(data$hr_vsorres)="Heart Rate"
label(data$rr_vsorres)="Respiratory Rate"
label(data$sysbp_vsorres)="Systolic blood pressure"
label(data$admission_diabp_vsorres)="Diastolic blood pressure"
label(data$oxy_vsorres)="Oxygen saturation"
label(data$muac)="Mid-upper arm circumference"
label(data$dehydr)="Dehydratation"
label(data$fever_ceoccur_v2)="History of fever"
label(data$cough_ceoccur_v2)="Cough"
label(data$sorethroat_ceoccur_v2)="Sore throat"
label(data$runnynose_ceoccur_v2)="Runny nose (Rhinorrhoea)"
label(data$wheeze_ceoccur_v2)="Wheezing"
label(data$chestpain_ceoccur_v2)="Chest pain"
label(data$myalgia_ceoccur_v2)="Muscle aches (Myalgia)"
label(data$jointpain_ceoccur_v2)="Joint pain (Arthralgia)"
label(data$fatigue_ceoccur_v2)="Fatigue / Malaise"
label(data$shortbreath_ceoccur_v2)="Shortness of breath or work of breathing (Dyspnea)"
label(data$lowerchest_ceoccur_v2)="Lower chest wall indrawing"
label(data$headache_ceoccur_v2)="Headache"
label(data$confusion_ceoccur_v2)="Altered consciousness / confusion"
label(data$seizures_cecoccur_v2)="Seizures"
label(data$abdopain_ceoccur_v2)="Abdominal pain"
label(data$vomit_ceoccur_v2)="Vomiting / Nausea"
label(data$diarrhoea_ceoccur_v2)="Diarrhoea"
label(data$conjunct_ceoccur_v2)="Conjunctivitis"
label(data$oral_inf)="Oral mucosal inflammation signs"
label(data$pale_skin)="Pale/mottled skin"
label(data$rash_ceoccur_v2)="Skin rash"
label(data$skinulcers_ceoccur_v2)="Skin ulcers"
label(data$peri_inf)="Peripheral cutaneous inflammation signs (hands or feet)"
label(data$lymp_ceoccur_v2)="Lymphadenopathy"
label(data$bleed_ceoccur_v2)="Bleeding (Haemorrhage)"
label(data$cold_hands)="Cold hands/feet"
label(data$cap_refill)="Capillary refill time > 2 seconds ?"
label(data$sw_joints)="Swollen joints"
label(data$st_neck)="Stiff neck"
label(data$hypo_flop)="Hypotonia / floppiness"
label(data$paralysis)="Paralysis"
label(data$shock)="Shock signs"
label(data$sens_alt)="Alteration in taste or smell"
label(data$admission_signs_and_symptoms_complete)="Complete?"
label(data$hb)="Hemoglobin"
label(data$leukocytes)="Leukocytes"
label(data$neutrophils)="Neutrophils"
label(data$lymphocytes)="Lymphocytes"
label(data$platelets)="Platelets"
label(data$aptt_aptr)="Activated Partial Thromboplastin Time / Ratio (APTT / APTR)"
label(data$pt)="Prothrombin Time (PT)"
label(data$inr)="International Normalized Ratio (INR)"
label(data$fibrino)="Fibrinogen"
label(data$c_reactive_protein)="C-reactive protein"
label(data$procalcitonin)="Procalcitonin"
label(data$cpk)="CPK"
label(data$alt_gpt)="Alanine aminotransferase-GPT"
label(data$ast_got)="Aspartate aminotransferase-GOT"
label(data$urea)="Urea"
label(data$creatinin)="Creatinine"
label(data$ldh)="Lactate dehydrogenase"
label(data$albumin)="Albumin"
label(data$sodium)="Serum sodium"
label(data$d_dimer)="Dimer-D"
label(data$il_6)="IL-6"
label(data$ferritin)="Ferritin"
label(data$trig)="Triglycerides"
label(data$bilirubin)="Total bilirubin"
label(data$lactate)="Lactate"
label(data$ph)="pH"
label(data$po2)="pO2"
label(data$pco2)="pCO2"
label(data$blood_tests_complete)="Complete?"
label(data$pcr_cov2_1)="SARS-CoV-2 PCR at diagnosis"
label(data$pcr_cov2_variant)="SARS-CoV-2 variant"
label(data$ct_cov2_1)="Cycle threshold (Ct) of SARS-CoV-2 PCR at diagnosis"
label(data$pcr_cov2_1_date)="Date of SARS-CoV-2 PCR at diagnosis"
label(data$pcr_cov2_2)="SARS-CoV-2 PCR at visit 2"
label(data$ct_cov2_2)="Cycle threshold (Ct) of SARS-CoV-2 PCR at visit 2"
label(data$pcr_cov2_2_date)="Date of SARS-CoV-2 PCR at visit 2"
label(data$pcr_cov2_3)="SARS-CoV-2 PCR at visit 3"
label(data$ct_cov2_3)="Cycle threshold (Ct) of SARS-CoV-2 PCR at visit 3"
label(data$pcr_cov2_3_date)="Date of SARS-CoV-2 PCR at visit 3"
label(data$pcr_cov2_4)="SARS-CoV-2 PCR at visit 4"
label(data$ct_cov2_4)="Cycle threshold (Ct) of SARS-CoV-2 PCR at visit 4"
label(data$pcr_cov2_4_date)="Date of SARS-CoV-2 PCR at visit 4"
label(data$rapid_ag_test_covid___1)="Rapid Antigenic Test (choice=Nasopharyngeal smear)"
label(data$rapid_ag_test_covid___2)="Rapid Antigenic Test (choice=Saliva)"
label(data$rapid_ag_test_covid_1)="Nasopharyngeal smear result"
label(data$rapid_ag_test_covid_2)="Saliva result"
label(data$virus_obtained_pcr_16)="PCR obtained for 16 viruses in respiratory secretion"
label(data$pcr_types_virus_sr___1)="Positive result in respiratory secretion PCR (choice=VRS)"
label(data$pcr_types_virus_sr___2)="Positive result in respiratory secretion PCR (choice=hMPV)"
label(data$pcr_types_virus_sr___3)="Positive result in respiratory secretion PCR (choice=PIV 1)"
label(data$pcr_types_virus_sr___4)="Positive result in respiratory secretion PCR (choice=PIV 2)"
label(data$pcr_types_virus_sr___5)="Positive result in respiratory secretion PCR (choice=PIV 3)"
label(data$pcr_types_virus_sr___6)="Positive result in respiratory secretion PCR (choice=PIV 4)"
label(data$pcr_types_virus_sr___7)="Positive result in respiratory secretion PCR (choice=Influenza A)"
label(data$pcr_types_virus_sr___8)="Positive result in respiratory secretion PCR (choice=Influenza B)"
label(data$pcr_types_virus_sr___9)="Positive result in respiratory secretion PCR (choice=hBoV)"
label(data$pcr_types_virus_sr___10)="Positive result in respiratory secretion PCR (choice=ADV)"
label(data$pcr_types_virus_sr___11)="Positive result in respiratory secretion PCR (choice=EV-hRV)"
label(data$pcr_types_virus_sr___12)="Positive result in respiratory secretion PCR (choice=Rhinovirus)"
label(data$pcr_types_virus_sr___13)="Positive result in respiratory secretion PCR (choice=CoV 229E)"
label(data$pcr_types_virus_sr___14)="Positive result in respiratory secretion PCR (choice=CoV OC43)"
label(data$pcr_types_virus_sr___15)="Positive result in respiratory secretion PCR (choice=CoV NL63)"
label(data$pcr_types_virus_sr___16)="Positive result in respiratory secretion PCR (choice=CoV HKU12)"
label(data$pcr_atipical_sr)="Obtained PCR sample for atypical respiratory secretion?"
label(data$pcr_mn_sr)="PCR Mycoplasma pneumoniae in respiratory secretion"
label(data$pcr_cn_sr)="PCR Chlamydophila pneumoniae in respiratory secretion"
label(data$pcr_hib_sr)="Haemophilus influenzae PCR in respiratory secretion"
label(data$pcr_leg_sr)="PCR Legionella in respiratory secretion"
label(data$pcr_bordetella_sr)="PCR Bordetella in respiratory secretion"
label(data$anti_influenza)="Antigen Influenza"
label(data$anti_rsv)="Antigen RSV"
label(data$res_samples)="Respiratory samples stored?"
label(data$respiratory_secretion_microbiology_complete)="Complete?"
label(data$first_sr_sc2)="Acute phase serology SARS-CoV-2 performed"
label(data$first_sr_sc2_type)="Acute phase serology test"
label(data$first_sr_sc2_type_ot)="Please, specify other test (1)"
label(data$first_sr_sc2_date)="Date of acute phase serology SARS-CoV-2"
label(data$igm_sc2_1)="Acute phase serology IgM SARS-CoV-2"
label(data$igg_sc2_1)="Acute phase serology IgG SARS-CoV-2"
label(data$second_sr_sc2)="Follow-up serology SARS-CoV-2 performed"
label(data$second_sr_sc2_type)="Follow-up serology test"
label(data$second_sr_sc2_type_ot)="Please, specify other test (2)"
label(data$second_sr_sc2_date)="Date of follow-up serology SARS-CoV-2"
label(data$igm_sc2_2)="Follow-up serology IgM SARS-CoV-2"
label(data$igg_sc2_2)="Follow-up serology IgG SARS-CoV-2"
label(data$sr_sc2_days)="Days from acute phase and follow-up serology for SARS-COV-2 (including the day when acute phase serology was done)"
label(data$blood_culture)="Blood culture for significant bacteria"
label(data$probable_bacterial_etiology_hemoculture)="Most likely bacterial aetiology in blood culture"
label(data$probable_text)="Please, specify (1)"
label(data$other_bacteria_hemoculture)="Other bacteria in blood culture"
label(data$probable_text_2)="Please, specify (2)"
label(data$first_sr_atipical)="First atypical serology performed"
label(data$date_ser_1)="Date of first atypical bacteria serology"
label(data$serology1_igm_mn)="Serology 1 IgM Mycoplasma pneumoniae"
label(data$serology1_igg_mn)="Serology 1 IgG Mycoplasma pneumoniae"
label(data$title1_igg_mn)="Title 1 IgG Mycoplasma pneumoniae"
label(data$serology1_igm_cn)="Serology 1 IgM Chlamydophila pneumoniae"
label(data$serology1_igg_cn)="Serology 1 IgG Chlamydophila pneumoniae"
label(data$title1_igg_cn)="Title 1 IgG Chlamydophila pneumoniae"
label(data$second_sr_atipical)="Second atypical serology performed"
label(data$date_ser_2)="Date of second atypical bacteria serology"
label(data$serology2_igm_mn)="Serology 2 IgM Mycoplasma pneumoniae"
label(data$serology2_igg_mn)="Serology 2 IgG Mycoplasma pneumoniae"
label(data$title2_igg_mn)="Title 2 IgG Mycoplasma pneumoniae"
label(data$serology2_igm_cn)="Serology 2 IgM Chlamydophila pneumoniae"
label(data$serology2_igg_cn)="Serology 2 IgG Chlamydophila pneumoniae"
label(data$title_igg_mn)="Title 2 IgG Chlamydophila pneumoniae"
label(data$seroconversion_mn)="Seroconversion"
label(data$blood_samples)="Whole blood samples stored?"
label(data$plasma_samples)="Plasma samples stored?"
label(data$blood_microbiology_complete)="Complete?"
label(data$codetection)="Codetection"
label(data$codetecion_type)="Codetection type"
label(data$codetecion_complete)="Complete?"
label(data$thoracentesis_done)="Thoracentesis performed"
label(data$ph_thoracentesis)="pH thoracentesis"
label(data$ag_pneumo_pf)="Pneumococcal antigen in pleural fluid"
label(data$pcr_pneumo_pf)="Pleural fluid pneumococcal PCR"
label(data$culture_pf)="Pleural fluid culture"
label(data$cultured_bacteria_pf)="Bacteria grown in pleural fluid"
label(data$other_cultured_bacteria_pf)="Other bacteria in pleural fluid culture"
label(data$pleural_fluid_microbiology_complete)="Complete?"
label(data$fever_end_date)="End date of fever"
label(data$oxygen_therapy)="Oxygen therapy"
label(data$oxigen_start_date)="Oxygen therapy start date"
label(data$oxigen_end_date)="End date oxygen therapy"
label(data$oxygen_therapy_time)="Days of oxygen therapy (including start day)"
label(data$hft)="High flow therapy"
label(data$hft_start_date)="High flow start date"
label(data$hft_end_date)="High flow end date"
label(data$hft_time)="Days of high flow therapy (including start day)"
label(data$cpap)="CPAP"
label(data$cpap_start_date)="CPAP start date"
label(data$cpap_end_date)="CPAP end date"
label(data$cpap_time)="Days of CPAP (including start day)"
label(data$picu_admission)="Admission to PICU"
label(data$picu_admission_date)="PICU admission date"
label(data$picu_discharge_date)="PICU discharge date"
label(data$picu_time)="Days of PICU admission (including admission day)"
label(data$mechanical_ventilation)="Mechanical ventilation"
label(data$nimv)="Noninvasive mechanical ventilation"
label(data$nimv_start_date)="Noninvasive mechanical ventilation start date"
label(data$nimv_end_date)="Noninvasive mechanical ventilation end date"
label(data$nimv_time)="Days of noninvasive mechanical ventilation (including start day)"
label(data$imv)="Invasive mechanical ventilation"
label(data$imv_start_date)="Invasive mechanical ventilation start date"
label(data$imv_end_date)="Invasive mechanical ventilation end date"
label(data$imv_time)="Days of invasive mechanical ventilation (including start day)"
label(data$discharge_date)="Hospital discharge date"
label(data$hosp_time)="Days of admission (including admission day)"
label(data$evolution_complete)="Complete?"
label(data$complications)="Complications (including new-onset signs or diagnosis after admission, such as Kawasaki-like / Inflammatory Syndrome symptoms)"
label(data$pleural_effusion)="Pleural effusion"
label(data$effusion_type)="Type of pleural effusion"
label(data$corticosteroids)="Systemic corticosteroids"
label(data$pleural_drainage)="Pleural drainage"
label(data$drainage_type)="Drain type"
label(data$pneumatocele_abscess)="Necrotizing pneumonia / abscess / pneumatocele"
label(data$pneumothorax)="Pneumothorax"
label(data$cardio_comp)="Cardiological complications"
label(data$cardio_comp_myoc)="Myocarditis / myocardial dysfunction"
label(data$cardio_comp_peri)="Pericarditis"
label(data$cardio_comp_valve)="Valve dysfunction"
label(data$cardio_comp_arry)="Arrhythmia"
label(data$cardio_comp_cor)="Coronary abnormalities"
label(data$cardio_comp_ane)="Aneurysms"
label(data$cardio_comp_ane_date)="If Aneurysms, please specify date"
label(data$cardio_comp_bnp)="NT-proBNP"
label(data$cardio_comp_trop)="Troponin I"
label(data$cardio_comp_other)="Other cardiologic complications"
label(data$sepsis)="Sepsis"
label(data$renal_failure)="Renal failure"
label(data$kawa)="Kawasaki-like / Inflammatory Syndrome symptoms"
label(data$other_complications)="Other complications"
label(data$complications_complete)="Complete?"
label(data$x_ray_1)="X-ray 1 image"
label(data$x_ray_1_date)="Date of X-ray 1"
label(data$xray_result_1)="Interpretation of first radiological image"
label(data$x_ray_1_reported_by___1)="Responsible for first radiological image interpretation (choice=Clinician)"
label(data$x_ray_1_reported_by___2)="Responsible for first radiological image interpretation (choice=Radiology report)"
label(data$x_ray_1_reported_by___3)="Responsible for first radiological image interpretation (choice=Researcher)"
label(data$x_ray_1_result_ext)="First radiological image read by external researcher not involved with the patient"
label(data$x_ray_2)="X-ray 2 image"
label(data$x_ray_2_date)="Date of X-ray 2"
label(data$xray_result_2)="Interpretation of second radiological image"
label(data$x_ray_2_reported_by___1)="Responsible for second radiological image interpretation (choice=Clinician)"
label(data$x_ray_2_reported_by___2)="Responsible for second radiological image interpretation (choice=Radiology report)"
label(data$x_ray_2_reported_by___3)="Responsible for second radiological image interpretation (choice=Researcher)"
label(data$x_ray_2_result_ext)="Second radiological image read by external researcher not involved with the patient"
label(data$x_ray_3)="X-ray 3 image"
label(data$x_ray_3_date)="Date of X-ray 3"
label(data$xray_result_3)="Interpretation of third radiological image"
label(data$x_ray_3_reported_by___1)="Responsible for third radiological image interpretation (choice=Clinician)"
label(data$x_ray_3_reported_by___2)="Responsible for third radiological image interpretation (choice=Radiology report)"
label(data$x_ray_3_reported_by___3)="Responsible for third radiological image interpretation (choice=Researcher)"
label(data$x_ray_3_result_ext)="Third radiological image read by external researcher not involved with the patient"
label(data$lung_ct)="Lung CT scan image"
label(data$lung_ct_date)="Date of Lung CT scan image"
label(data$cardiac_img)="Cardiac imaging performed?"
label(data$card_img_results)="Please, specify name of technique and findings"
label(data$radiology_complete)="Complete?"
label(data$oro_fluids)="Oral/orogastric fluids?"
label(data$iv_fluids)="Intravenous fluids?"
label(data$antiviral_cmyn)="Antiviral agent"
label(data$antiviral_cmtrt___1)="Specify antiviral agent (choice=Ribavirin)"
label(data$antiviral_cmtrt___2)="Specify antiviral agent (choice=Lopinavir/Ritonavir)"
label(data$antiviral_cmtrt___3)="Specify antiviral agent (choice=Interferon-alpha)"
label(data$antiviral_cmtrt___4)="Specify antiviral agent (choice=Interferon-beta)"
label(data$antiviral_cmtrt___5)="Specify antiviral agent (choice=Neuraminidase inhibitors)"
label(data$antiviral_cmtrt___6)="Specify antiviral agent (choice=Remdesivir)"
label(data$antiviral_cmtrt___7)="Specify antiviral agent (choice=Chloroquine)"
label(data$antiviral_cmtrt___8)="Specify antiviral agent (choice=Hydroxychloroquine)"
label(data$antiviral_cmtrt___10)="Specify antiviral agent (choice=Anakinra)"
label(data$antiviral_cmtrt___11)="Specify antiviral agent (choice=Ivermectin)"
label(data$antiviral_cmtrt___9)="Specify antiviral agent (choice=Other)"
label(data$antiviral_cmtype)="Specify other antiviral type"
label(data$antiviral_days)="Days with antiviral"
label(data$antibiotic_cmyn)="Antibiotic"
label(data$iv_ab)="Intravenous antibiotic"
label(data$type_iv_ab___0)="Type of intravenous antibiotic (choice=Ampicillin)"
label(data$type_iv_ab___1)="Type of intravenous antibiotic (choice=Cefotaxime)"
label(data$type_iv_ab___2)="Type of intravenous antibiotic (choice=Cefuroxime)"
label(data$type_iv_ab___3)="Type of intravenous antibiotic (choice=Vancomycin)"
label(data$type_iv_ab___4)="Type of intravenous antibiotic (choice=Meropenem)"
label(data$type_iv_ab___5)="Type of intravenous antibiotic (choice=Clindamycin)"
label(data$type_iv_ab___6)="Type of intravenous antibiotic (choice=Cefazolin)"
label(data$type_iv_ab___9)="Type of intravenous antibiotic (choice=Ceftriaxone)"
label(data$type_iv_ab___8)="Type of intravenous antibiotic (choice=Clavulanic acid)"
label(data$type_iv_ab___7)="Type of intravenous antibiotic (choice=Other (specify))"
label(data$other_iv_ab)="Other intravenous antibiotic"
label(data$iv_ab_start_date)="Intravenous antibiotic start date"
label(data$iv_ab_end_date)="Intravenous antibiotic end date"
label(data$iv_ab_days)="Days of intravenous antibiotic (including start date)"
label(data$o_ab)="Intrahospital oral antibiotic "
label(data$type_o_ab___1)="Oral antibiotic type (choice=Amoxicillin)"
label(data$type_o_ab___2)="Oral antibiotic type (choice=Amoxicillin-Clavulanic)"
label(data$type_o_ab___3)="Oral antibiotic type (choice=Cefuroxime)"
label(data$type_o_ab___4)="Oral antibiotic type (choice=Azithromycin)"
label(data$type_o_ab___5)="Oral antibiotic type (choice=Erythromycin)"
label(data$type_o_ab___6)="Oral antibiotic type (choice=Clindamycin)"
label(data$type_o_ab___7)="Oral antibiotic type (choice=Cefadroxil)"
label(data$type_o_ab___8)="Oral antibiotic type (choice=Other (specify))"
label(data$other_o_ab)="Other oral antibiotic"
label(data$o_ab_start_date)="Oral antibiotic start date"
label(data$o_ab_end_date)="Oral antibiotic end date"
label(data$o_ab_days)="Days of oral antibiotic (including start date)"
label(data$azithromycin_or_erythromycin)="Oral Azithromycin or Erythromycin"
label(data$azithromycin_or_erythromycin_2d)="Oral Azithromycin or Erythromycin (during the first 2 days, 36h)"
label(data$corticost_cmyn)="Corticosteroids"
label(data$costicost_date)="Specify corticosteroid start date"
label(data$corticost_cmtrt)="Specify corticosteroid type"
label(data$corticost_cmdose)="Specify corticosteroid dose"
label(data$corticost_cmroute)="Specify corticosteroid route"
label(data$anti_inflammatory___1)="Anti-inflammatory treatment (choice=Ibuprofen)"
label(data$anti_inflammatory___2)="Anti-inflammatory treatment (choice=Tocilizumab)"
label(data$anti_inflammatory_date)="Specify Tocilizumab start date"
label(data$other_anti_inflammatory)="Other non-Steroidal Anti-Inflammatory Drugs (NSAIDs)"
label(data$other_anti_inflammatory_type)="Please, specify name of other NSAIDs"
label(data$iv_imglob)="Intravenous immune globulin"
label(data$iv_imglob_date)="Specify intravenous immune globulin start date"
label(data$iv_imglob_dose)="Daily intravenous immune globulin dose"
label(data$iv_imglob_days)="Days of intravenous immune globulin treatment"
label(data$imod)="Immunomodulators"
label(data$imod_date)="Specify immunomodulators start date"
label(data$imod_type)="Please, specify name of immunomodulator"
label(data$sys_anticoag)="Systemic anticoagulation"
label(data$sys_anticoag_type)="Please, specify name of systemic anticoagulation agent"
label(data$anti_mal)="Antimalarial agent"
label(data$anti_mal_type)="Please, specify name of antimalarial agent"
label(data$antifung_cmyn)="Antifungal agent"
label(data$antifung_type)="Please, specify name of antifungal agent"
label(data$experimental)="Experimental agent"
label(data$experimental_type)="Please, specify name of experimental agent"
label(data$other_sc2_treatments)="Other medications directed to cure COVID-19"
label(data$other_sc2_treatments_text)="Please, specify other COVID-19 treatments"
label(data$vasopressors)="Inotropes/vasopressors"
label(data$vasopressors_type___1)="Please, specify name of inotropes/vasopressors (choice=Adrenalin)"
label(data$vasopressors_type___2)="Please, specify name of inotropes/vasopressors (choice=Noradrenaline)"
label(data$vasopressors_type___3)="Please, specify name of inotropes/vasopressors (choice=Dopamine)"
label(data$vasopressors_type___4)="Please, specify name of inotropes/vasopressors (choice=Dobutamine)"
label(data$vasopressors_type___5)="Please, specify name of inotropes/vasopressors (choice=Milrinone)"
label(data$vasopressors_type___6)="Please, specify name of inotropes/vasopressors (choice=Other (please, specify in comments))"
label(data$ecmo)="Extracorporeal (ECMO) support"
label(data$blood_trans)="Blood transfusion"
label(data$medication_complete)="Complete?"
label(data$dsterm)="Patient outcome"
label(data$dsterm_other_text)="Please, specify which facility has been transferred to"
label(data$excluded_reason)="Reason for exclusion"
label(data$excluded_reason_ot)="Please specify reason for exclusion"
label(data$final_dx_global_concept)="Final diagnosis, ICD-10-CM concept/s"
label(data$final_dx_global_code)="Final diagnosis, ICD-10-CM code/s"
label(data$final_dx_1)="Primary diagnosis related to COVID-19"
label(data$related_1)="Is the primary diagnosis related to COVID-19?"
label(data$final_dx_2)="Secondary diagnosis: comorbidity which conditioned this admission"
label(data$related_2)="Is the secondary diagnosis related to COVID-19?"
label(data$final_dx_3)="Other diagnosis (3)"
label(data$final_dx_4)="Other diagnosis (4)"
label(data$final_dx_5)="Other diagnosis (5)"
label(data$final_dx_6)="Other diagnosis (6)"
label(data$related_3)="Is the other diagnosis related to COVID-19?"
label(data$miscvac)="Potential MIS-C after vaccination"
label(data$miscvac_doses)="Number of COVID-19 vaccine doses received before MIS-C"
label(data$miscvac_date)="Date of the last COVID-19 vaccine dose before MIS-C"
label(data$miscvac_iggas)="SARS-CoV-2 IgG anti-S"
label(data$miscvac_iggan)="SARS-CoV-2 IgG anti-N"
label(data$outcome_complete)="Complete?"
label(data$fvc_6m)="Forced vital capacity (FVC)"
label(data$fev1_6m)="Forced expiratory volume (FEV1)"
label(data$esp_ave_flux_6m)="Average expiratory flow"
label(data$spirometry_complete)="Complete?"
label(data$fever_ws)="Fever without source at emergency ward admission"
label(data$lumbar_punc)="Lumbar puncture performed"
label(data$csf_bacteria)="CSF bacterial culture"
label(data$csf_baceria_type)="Bacteria isolated in CSF"
label(data$csf_vhs)="CSF VHS PCR "
label(data$csf_enterovirus)="CSF Enterovirus PCR "
label(data$csf_sarscov2)="CSF SARSCoV2 PCR "
label(data$csf_leuco)="Leucocytes in CSF"
label(data$csf_neutro)="Percentage of Neutrophils CSF"
label(data$csf_lympho)="Percentage of Lymphocytes in CSF"
label(data$protein_csf)="Protein in CSF"
label(data$glucose_csf)="Glucose in CSF"
label(data$urine_collection)="Urine collection method"
label(data$urine_dipstik)="Urine dipstick"
label(data$urine_culture)="Urine culture"
label(data$bacteria_urine)="Bacteria isolated in urine "
label(data$final_diagnosis)="Final diagnosis after fever without source episode, ICD-10-CM concept"
label(data$final_diagnosis_icd10)="Final diagnosis after fever without source episode, ICD-10-CM code"
label(data$comments_fws)="Comments"
label(data$fever_without_source_study_complete)="Complete?"
label(data$qc_stage)="Current stage of quality control"
label(data$qc_date)="Date of quality control stage change"
label(data$first_analysis)="First Analysis"
label(data$quality_control_complete)="Complete?"
#Setting Units


#Setting Factors(will create new variable for factors)
data$redcap_data_access_group = factor(data$redcap_data_access_group,levels=c("althaia_xarxa_assi","complejo_asistenci","complejo_asistencib","complejo_hospitala","complejo_hospitalab","complejo_hospitalac","complejo_hospitalad","consorcio_hosp_gen","hnsp_talavera","hospital_lvaro_cun","hospital_aranjuez","hospital_central_d","hospital_clnico_un","hospital_clnico_unb","hospital_clnico_unc","hospital_clinico_u","hospital_de_denia","hospital_de_getafe","hospital_de_la_san","hospital_de_manaco","hospital_de_merida","hospital_de_sagunt","hospital_de_txagor","hospital_del_mar_d","hospital_general_d","hospital_general_db","hospital_general_dc","hospital_general_u","hospital_general_ub","hospital_general_uc","hospital_hm_montep","hospital_infanta_c","hospital_infantil","hospital_infantilb","hospital_juan_ramo","hospital_la_fe","hospital_la_morale","hospital_llus_alca","hospital_mare_de_d","hospital_miquel_se","hospital_puerta_de","hospital_regional","hospital_san_pedro","hospital_sant_joan","hospitalsantjoande","hospital_teresa_he","hospital_universit","hospital_universitb","hospital_universitc","hospital_universitd","hospital_universite","hospital_universitf","hospital_universitg","hospital_universith","hospital_universiti","hospital_universitj","hospital_universitk","hospital_universitl","hospital_universitm","hospital_universitn","hospital_universito","hospital_universitp","hospital_universitq","hospital_universitr","hospital_universits","hospital_universitt","hospital_universitu","hospital_universitv","hospital_universitw","hospital_universitx","hospital_university","hospital_universitz","hospital_universitaa","hospital_universitab","hospital_universitac","hospital_universitad","hospital_universitae","hospital_universitaf","hospital_universitag","hospital_universitah","hospital_universitai","hospital_universitaj","hospital_universitak","hospital_universital","hospital_universitam","hospital_universitan","hospital_universitao","hospital_vall_dheb","hospital_virgen_de","hospital_virgen_deb","hospital_vithas_ar","hospitales_madrid","institut_dinvestig","instituto_hispalen","pius_hospital_de_v","red_de_medicos_cen"))
data$hospital = factor(data$hospital,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96"))
data$center_id_complete = factor(data$center_id_complete,levels=c("0","1","2"))
data$sex = factor(data$sex,levels=c("1","2","3"))
data$last_15 = factor(data$last_15,levels=c("1","2","3"))
data$demographics_complete = factor(data$demographics_complete,levels=c("0","1","2"))
data$symptoms_epi_travel = factor(data$symptoms_epi_travel,levels=c("1","2","3"))
data$symptoms_epi_physical = factor(data$symptoms_epi_physical,levels=c("1","2","3"))
data$symptoms_epi_healthfac = factor(data$symptoms_epi_healthfac,levels=c("1","2","3"))
data$symptoms_epi_animal = factor(data$symptoms_epi_animal,levels=c("1","2","3"))
data$symptoms_epi_comm = factor(data$symptoms_epi_comm,levels=c("1","2","3"))
data$contact___1 = factor(data$contact___1,levels=c("0","1"))
data$contact___2 = factor(data$contact___2,levels=c("0","1"))
data$contact___3 = factor(data$contact___3,levels=c("0","1"))
data$contact___4 = factor(data$contact___4,levels=c("0","1"))
data$inclusion_criteria_complete = factor(data$inclusion_criteria_complete,levels=c("0","1","2"))
data$anamnesis_complete = factor(data$anamnesis_complete,levels=c("0","1","2"))
data$comorb = factor(data$comorb,levels=c("1","0"))
data$chroniccard_mhyn = factor(data$chroniccard_mhyn,levels=c("1","2","3"))
data$hypertension_mhyn = factor(data$hypertension_mhyn,levels=c("1","2","3"))
data$chronicpul_mhyn = factor(data$chronicpul_mhyn,levels=c("1","2","3"))
data$asthma_mhyn = factor(data$asthma_mhyn,levels=c("1","2","3"))
data$tb_mhyn = factor(data$tb_mhyn,levels=c("1","2","3"))
data$resp_inf_mhyn = factor(data$resp_inf_mhyn,levels=c("1","2","3"))
data$renal_mhyn = factor(data$renal_mhyn,levels=c("1","2","3"))
data$modliver_mhyn = factor(data$modliver_mhyn,levels=c("1","2","3"))
data$mildliv_mhyn = factor(data$mildliv_mhyn,levels=c("1","2","3"))
data$chronicneu_mhyn = factor(data$chronicneu_mhyn,levels=c("1","2","3"))
data$malignantneo_mhyn = factor(data$malignantneo_mhyn,levels=c("1","2","3"))
data$chronhaemo_mhyn = factor(data$chronhaemo_mhyn,levels=c("1","2","3"))
data$aidshiv_mhyn = factor(data$aidshiv_mhyn,levels=c("1","2","3"))
data$obesity_mhyn = factor(data$obesity_mhyn,levels=c("1","2","3"))
data$diabetiscomp_mhyn = factor(data$diabetiscomp_mhyn,levels=c("1","2","3"))
data$diabetes_mhyn = factor(data$diabetes_mhyn,levels=c("1","2","3"))
data$inflammatory_mhyr = factor(data$inflammatory_mhyr,levels=c("1","2","3"))
data$rheumatology_mhyr = factor(data$rheumatology_mhyr,levels=c("1","2","3"))
data$kawa_mhyn = factor(data$kawa_mhyn,levels=c("1","2","3"))
data$kawa_fam_mhyn = factor(data$kawa_fam_mhyn,levels=c("1","2","3"))
data$dementia_mhyn = factor(data$dementia_mhyn,levels=c("1","2","3"))
data$malnutrition_mhyn = factor(data$malnutrition_mhyn,levels=c("1","2","3"))
data$other_mhyn = factor(data$other_mhyn,levels=c("1","2","3"))
data$smoking_mhyn = factor(data$smoking_mhyn,levels=c("1","2","3"))
data$immunosupressors = factor(data$immunosupressors,levels=c("1","0"))
data$comorbidities_complete = factor(data$comorbidities_complete,levels=c("0","1","2"))
data$influenza_full_regimen = factor(data$influenza_full_regimen,levels=c("1","0"))
data$apsc_vcageind = factor(data$apsc_vcageind,levels=c("1","2","3"))
data$cov_vac = factor(data$cov_vac,levels=c("1","0"))
data$vaccines_complete = factor(data$vaccines_complete,levels=c("0","1","2"))
data$anthropometry_complete = factor(data$anthropometry_complete,levels=c("0","1","2"))
data$dehydr = factor(data$dehydr,levels=c("1","2","3"))
data$fever_ceoccur_v2 = factor(data$fever_ceoccur_v2,levels=c("1","2","3"))
data$cough_ceoccur_v2 = factor(data$cough_ceoccur_v2,levels=c("1","2","3"))
data$sorethroat_ceoccur_v2 = factor(data$sorethroat_ceoccur_v2,levels=c("1","2","3"))
data$runnynose_ceoccur_v2 = factor(data$runnynose_ceoccur_v2,levels=c("1","2","3"))
data$wheeze_ceoccur_v2 = factor(data$wheeze_ceoccur_v2,levels=c("1","2","3"))
data$chestpain_ceoccur_v2 = factor(data$chestpain_ceoccur_v2,levels=c("1","2","3"))
data$myalgia_ceoccur_v2 = factor(data$myalgia_ceoccur_v2,levels=c("1","2","3"))
data$jointpain_ceoccur_v2 = factor(data$jointpain_ceoccur_v2,levels=c("1","2","3"))
data$fatigue_ceoccur_v2 = factor(data$fatigue_ceoccur_v2,levels=c("1","2","3"))
data$shortbreath_ceoccur_v2 = factor(data$shortbreath_ceoccur_v2,levels=c("1","2","3"))
data$lowerchest_ceoccur_v2 = factor(data$lowerchest_ceoccur_v2,levels=c("1","2","3"))
data$headache_ceoccur_v2 = factor(data$headache_ceoccur_v2,levels=c("1","2","3"))
data$confusion_ceoccur_v2 = factor(data$confusion_ceoccur_v2,levels=c("1","2","3"))
data$seizures_cecoccur_v2 = factor(data$seizures_cecoccur_v2,levels=c("1","2","3"))
data$abdopain_ceoccur_v2 = factor(data$abdopain_ceoccur_v2,levels=c("1","2","3"))
data$vomit_ceoccur_v2 = factor(data$vomit_ceoccur_v2,levels=c("1","2","3"))
data$diarrhoea_ceoccur_v2 = factor(data$diarrhoea_ceoccur_v2,levels=c("1","2","3"))
data$conjunct_ceoccur_v2 = factor(data$conjunct_ceoccur_v2,levels=c("1","2","3"))
data$oral_inf = factor(data$oral_inf,levels=c("1","2","3"))
data$pale_skin = factor(data$pale_skin,levels=c("1","2","3"))
data$rash_ceoccur_v2 = factor(data$rash_ceoccur_v2,levels=c("1","2","3"))
data$skinulcers_ceoccur_v2 = factor(data$skinulcers_ceoccur_v2,levels=c("1","2","3"))
data$peri_inf = factor(data$peri_inf,levels=c("1","2","3"))
data$lymp_ceoccur_v2 = factor(data$lymp_ceoccur_v2,levels=c("1","2","3"))
data$bleed_ceoccur_v2 = factor(data$bleed_ceoccur_v2,levels=c("1","2","3"))
data$cold_hands = factor(data$cold_hands,levels=c("1","2","3"))
data$cap_refill = factor(data$cap_refill,levels=c("1","2","3"))
data$sw_joints = factor(data$sw_joints,levels=c("1","2","3"))
data$st_neck = factor(data$st_neck,levels=c("1","2","3"))
data$hypo_flop = factor(data$hypo_flop,levels=c("1","2","3"))
data$paralysis = factor(data$paralysis,levels=c("1","2","3"))
data$shock = factor(data$shock,levels=c("1","2","3"))
data$sens_alt = factor(data$sens_alt,levels=c("1","2","3"))
data$admission_signs_and_symptoms_complete = factor(data$admission_signs_and_symptoms_complete,levels=c("0","1","2"))
data$blood_tests_complete = factor(data$blood_tests_complete,levels=c("0","1","2"))
data$pcr_cov2_1 = factor(data$pcr_cov2_1,levels=c("1","2","3","4"))
data$pcr_cov2_variant = factor(data$pcr_cov2_variant,levels=c("0","1","2","4","3"))
data$pcr_cov2_2 = factor(data$pcr_cov2_2,levels=c("1","2","3","4"))
data$pcr_cov2_3 = factor(data$pcr_cov2_3,levels=c("1","2","3","4"))
data$pcr_cov2_4 = factor(data$pcr_cov2_4,levels=c("1","2","3","4"))
data$rapid_ag_test_covid___1 = factor(data$rapid_ag_test_covid___1,levels=c("0","1"))
data$rapid_ag_test_covid___2 = factor(data$rapid_ag_test_covid___2,levels=c("0","1"))
data$rapid_ag_test_covid_1 = factor(data$rapid_ag_test_covid_1,levels=c("1","2"))
data$rapid_ag_test_covid_2 = factor(data$rapid_ag_test_covid_2,levels=c("1","2"))
data$virus_obtained_pcr_16 = factor(data$virus_obtained_pcr_16,levels=c("1","0"))
data$pcr_types_virus_sr___1 = factor(data$pcr_types_virus_sr___1,levels=c("0","1"))
data$pcr_types_virus_sr___2 = factor(data$pcr_types_virus_sr___2,levels=c("0","1"))
data$pcr_types_virus_sr___3 = factor(data$pcr_types_virus_sr___3,levels=c("0","1"))
data$pcr_types_virus_sr___4 = factor(data$pcr_types_virus_sr___4,levels=c("0","1"))
data$pcr_types_virus_sr___5 = factor(data$pcr_types_virus_sr___5,levels=c("0","1"))
data$pcr_types_virus_sr___6 = factor(data$pcr_types_virus_sr___6,levels=c("0","1"))
data$pcr_types_virus_sr___7 = factor(data$pcr_types_virus_sr___7,levels=c("0","1"))
data$pcr_types_virus_sr___8 = factor(data$pcr_types_virus_sr___8,levels=c("0","1"))
data$pcr_types_virus_sr___9 = factor(data$pcr_types_virus_sr___9,levels=c("0","1"))
data$pcr_types_virus_sr___10 = factor(data$pcr_types_virus_sr___10,levels=c("0","1"))
data$pcr_types_virus_sr___11 = factor(data$pcr_types_virus_sr___11,levels=c("0","1"))
data$pcr_types_virus_sr___12 = factor(data$pcr_types_virus_sr___12,levels=c("0","1"))
data$pcr_types_virus_sr___13 = factor(data$pcr_types_virus_sr___13,levels=c("0","1"))
data$pcr_types_virus_sr___14 = factor(data$pcr_types_virus_sr___14,levels=c("0","1"))
data$pcr_types_virus_sr___15 = factor(data$pcr_types_virus_sr___15,levels=c("0","1"))
data$pcr_types_virus_sr___16 = factor(data$pcr_types_virus_sr___16,levels=c("0","1"))
data$pcr_atipical_sr = factor(data$pcr_atipical_sr,levels=c("1","0"))
data$pcr_mn_sr = factor(data$pcr_mn_sr,levels=c("1","2","3","4"))
data$pcr_cn_sr = factor(data$pcr_cn_sr,levels=c("1","2","3","4"))
data$pcr_hib_sr = factor(data$pcr_hib_sr,levels=c("1","2","3","4"))
data$pcr_leg_sr = factor(data$pcr_leg_sr,levels=c("1","2","3","4"))
data$pcr_bordetella_sr = factor(data$pcr_bordetella_sr,levels=c("1","2","3","4"))
data$anti_influenza = factor(data$anti_influenza,levels=c("1","2","3"))
data$anti_rsv = factor(data$anti_rsv,levels=c("1","2","3"))
data$res_samples = factor(data$res_samples,levels=c("1","0"))
data$respiratory_secretion_microbiology_complete = factor(data$respiratory_secretion_microbiology_complete,levels=c("0","1","2"))
data$first_sr_sc2 = factor(data$first_sr_sc2,levels=c("1","0","2","3"))
data$first_sr_sc2_type = factor(data$first_sr_sc2_type,levels=c("1","2","3","4"))
data$igm_sc2_1 = factor(data$igm_sc2_1,levels=c("1","2","3","4"))
data$igg_sc2_1 = factor(data$igg_sc2_1,levels=c("1","2","3","4"))
data$second_sr_sc2 = factor(data$second_sr_sc2,levels=c("1","0","2","3"))
data$second_sr_sc2_type = factor(data$second_sr_sc2_type,levels=c("1","2","3","4"))
data$igm_sc2_2 = factor(data$igm_sc2_2,levels=c("1","2","3","4"))
data$igg_sc2_2 = factor(data$igg_sc2_2,levels=c("1","2","3","4"))
data$blood_culture = factor(data$blood_culture,levels=c("1","0"))
data$probable_bacterial_etiology_hemoculture = factor(data$probable_bacterial_etiology_hemoculture,levels=c("1","2","3","4"))
data$other_bacteria_hemoculture = factor(data$other_bacteria_hemoculture,levels=c("1","0"))
data$first_sr_atipical = factor(data$first_sr_atipical,levels=c("1","0","2","3"))
data$serology1_igm_mn = factor(data$serology1_igm_mn,levels=c("1","2","3","4"))
data$serology1_igg_mn = factor(data$serology1_igg_mn,levels=c("1","2","3","4"))
data$serology1_igm_cn = factor(data$serology1_igm_cn,levels=c("1","2","3","4"))
data$serology1_igg_cn = factor(data$serology1_igg_cn,levels=c("1","2","3","4"))
data$second_sr_atipical = factor(data$second_sr_atipical,levels=c("1","0","2","3"))
data$serology2_igm_mn = factor(data$serology2_igm_mn,levels=c("1","2","3","4"))
data$serology2_igg_mn = factor(data$serology2_igg_mn,levels=c("1","2","3","4"))
data$serology2_igm_cn = factor(data$serology2_igm_cn,levels=c("1","2","3","4"))
data$serology2_igg_cn = factor(data$serology2_igg_cn,levels=c("1","2","3","4"))
data$seroconversion_mn = factor(data$seroconversion_mn,levels=c("1","2","3","4"))
data$blood_samples = factor(data$blood_samples,levels=c("1","0"))
data$plasma_samples = factor(data$plasma_samples,levels=c("1","0"))
data$blood_microbiology_complete = factor(data$blood_microbiology_complete,levels=c("0","1","2"))
data$codetection = factor(data$codetection,levels=c("1","2","3","4"))
data$codetecion_type = factor(data$codetecion_type,levels=c("1","2"))
data$codetecion_complete = factor(data$codetecion_complete,levels=c("0","1","2"))
data$thoracentesis_done = factor(data$thoracentesis_done,levels=c("1","0"))
data$ag_pneumo_pf = factor(data$ag_pneumo_pf,levels=c("1","2","3"))
data$pcr_pneumo_pf = factor(data$pcr_pneumo_pf,levels=c("1","2","3"))
data$culture_pf = factor(data$culture_pf,levels=c("1","2"))
data$cultured_bacteria_pf = factor(data$cultured_bacteria_pf,levels=c("1","2","3","4","5"))
data$pleural_fluid_microbiology_complete = factor(data$pleural_fluid_microbiology_complete,levels=c("0","1","2"))
data$oxygen_therapy = factor(data$oxygen_therapy,levels=c("1","0"))
data$hft = factor(data$hft,levels=c("1","0"))
data$cpap = factor(data$cpap,levels=c("1","0"))
data$picu_admission = factor(data$picu_admission,levels=c("1","0"))
data$mechanical_ventilation = factor(data$mechanical_ventilation,levels=c("1","0"))
data$nimv = factor(data$nimv,levels=c("1","0"))
data$imv = factor(data$imv,levels=c("1","0"))
data$evolution_complete = factor(data$evolution_complete,levels=c("0","1","2"))
data$complications = factor(data$complications,levels=c("1","0"))
data$pleural_effusion = factor(data$pleural_effusion,levels=c("1","0"))
data$effusion_type = factor(data$effusion_type,levels=c("1","2"))
data$corticosteroids = factor(data$corticosteroids,levels=c("1","0"))
data$pleural_drainage = factor(data$pleural_drainage,levels=c("1","0"))
data$drainage_type = factor(data$drainage_type,levels=c("1","2","3"))
data$pneumatocele_abscess = factor(data$pneumatocele_abscess,levels=c("1","0"))
data$pneumothorax = factor(data$pneumothorax,levels=c("1","0"))
data$cardio_comp = factor(data$cardio_comp,levels=c("1","0"))
data$cardio_comp_myoc = factor(data$cardio_comp_myoc,levels=c("1","0"))
data$cardio_comp_peri = factor(data$cardio_comp_peri,levels=c("1","0"))
data$cardio_comp_valve = factor(data$cardio_comp_valve,levels=c("1","0"))
data$cardio_comp_arry = factor(data$cardio_comp_arry,levels=c("1","0"))
data$cardio_comp_cor = factor(data$cardio_comp_cor,levels=c("1","0"))
data$cardio_comp_ane = factor(data$cardio_comp_ane,levels=c("1","0"))
data$sepsis = factor(data$sepsis,levels=c("1","0"))
data$renal_failure = factor(data$renal_failure,levels=c("1","0"))
data$kawa = factor(data$kawa,levels=c("1","0"))
data$complications_complete = factor(data$complications_complete,levels=c("0","1","2"))
data$xray_result_1 = factor(data$xray_result_1,levels=c("1","2","3"))
data$x_ray_1_reported_by___1 = factor(data$x_ray_1_reported_by___1,levels=c("0","1"))
data$x_ray_1_reported_by___2 = factor(data$x_ray_1_reported_by___2,levels=c("0","1"))
data$x_ray_1_reported_by___3 = factor(data$x_ray_1_reported_by___3,levels=c("0","1"))
data$x_ray_1_result_ext = factor(data$x_ray_1_result_ext,levels=c("1","2","3"))
data$xray_result_2 = factor(data$xray_result_2,levels=c("1","2","3"))
data$x_ray_2_reported_by___1 = factor(data$x_ray_2_reported_by___1,levels=c("0","1"))
data$x_ray_2_reported_by___2 = factor(data$x_ray_2_reported_by___2,levels=c("0","1"))
data$x_ray_2_reported_by___3 = factor(data$x_ray_2_reported_by___3,levels=c("0","1"))
data$x_ray_2_result_ext = factor(data$x_ray_2_result_ext,levels=c("1","2","3"))
data$xray_result_3 = factor(data$xray_result_3,levels=c("1","2","3"))
data$x_ray_3_reported_by___1 = factor(data$x_ray_3_reported_by___1,levels=c("0","1"))
data$x_ray_3_reported_by___2 = factor(data$x_ray_3_reported_by___2,levels=c("0","1"))
data$x_ray_3_reported_by___3 = factor(data$x_ray_3_reported_by___3,levels=c("0","1"))
data$x_ray_3_result_ext = factor(data$x_ray_3_result_ext,levels=c("1","2","3"))
data$cardiac_img = factor(data$cardiac_img,levels=c("1","2","3"))
data$radiology_complete = factor(data$radiology_complete,levels=c("0","1","2"))
data$oro_fluids = factor(data$oro_fluids,levels=c("1","2","3"))
data$iv_fluids = factor(data$iv_fluids,levels=c("1","2","3"))
data$antiviral_cmyn = factor(data$antiviral_cmyn,levels=c("1","2","3"))
data$antiviral_cmtrt___1 = factor(data$antiviral_cmtrt___1,levels=c("0","1"))
data$antiviral_cmtrt___2 = factor(data$antiviral_cmtrt___2,levels=c("0","1"))
data$antiviral_cmtrt___3 = factor(data$antiviral_cmtrt___3,levels=c("0","1"))
data$antiviral_cmtrt___4 = factor(data$antiviral_cmtrt___4,levels=c("0","1"))
data$antiviral_cmtrt___5 = factor(data$antiviral_cmtrt___5,levels=c("0","1"))
data$antiviral_cmtrt___6 = factor(data$antiviral_cmtrt___6,levels=c("0","1"))
data$antiviral_cmtrt___7 = factor(data$antiviral_cmtrt___7,levels=c("0","1"))
data$antiviral_cmtrt___8 = factor(data$antiviral_cmtrt___8,levels=c("0","1"))
data$antiviral_cmtrt___10 = factor(data$antiviral_cmtrt___10,levels=c("0","1"))
data$antiviral_cmtrt___11 = factor(data$antiviral_cmtrt___11,levels=c("0","1"))
data$antiviral_cmtrt___9 = factor(data$antiviral_cmtrt___9,levels=c("0","1"))
data$antibiotic_cmyn = factor(data$antibiotic_cmyn,levels=c("1","2","3"))
data$iv_ab = factor(data$iv_ab,levels=c("1","2","3"))
data$type_iv_ab___0 = factor(data$type_iv_ab___0,levels=c("0","1"))
data$type_iv_ab___1 = factor(data$type_iv_ab___1,levels=c("0","1"))
data$type_iv_ab___2 = factor(data$type_iv_ab___2,levels=c("0","1"))
data$type_iv_ab___3 = factor(data$type_iv_ab___3,levels=c("0","1"))
data$type_iv_ab___4 = factor(data$type_iv_ab___4,levels=c("0","1"))
data$type_iv_ab___5 = factor(data$type_iv_ab___5,levels=c("0","1"))
data$type_iv_ab___6 = factor(data$type_iv_ab___6,levels=c("0","1"))
data$type_iv_ab___9 = factor(data$type_iv_ab___9,levels=c("0","1"))
data$type_iv_ab___8 = factor(data$type_iv_ab___8,levels=c("0","1"))
data$type_iv_ab___7 = factor(data$type_iv_ab___7,levels=c("0","1"))
data$o_ab = factor(data$o_ab,levels=c("1","0"))
data$type_o_ab___1 = factor(data$type_o_ab___1,levels=c("0","1"))
data$type_o_ab___2 = factor(data$type_o_ab___2,levels=c("0","1"))
data$type_o_ab___3 = factor(data$type_o_ab___3,levels=c("0","1"))
data$type_o_ab___4 = factor(data$type_o_ab___4,levels=c("0","1"))
data$type_o_ab___5 = factor(data$type_o_ab___5,levels=c("0","1"))
data$type_o_ab___6 = factor(data$type_o_ab___6,levels=c("0","1"))
data$type_o_ab___7 = factor(data$type_o_ab___7,levels=c("0","1"))
data$type_o_ab___8 = factor(data$type_o_ab___8,levels=c("0","1"))
data$corticost_cmyn = factor(data$corticost_cmyn,levels=c("1","2","3"))
data$corticost_cmroute = factor(data$corticost_cmroute,levels=c("1","2","3"))
data$anti_inflammatory___1 = factor(data$anti_inflammatory___1,levels=c("0","1"))
data$anti_inflammatory___2 = factor(data$anti_inflammatory___2,levels=c("0","1"))
data$other_anti_inflammatory = factor(data$other_anti_inflammatory,levels=c("1","2","3"))
data$iv_imglob = factor(data$iv_imglob,levels=c("1","2","3"))
data$imod = factor(data$imod,levels=c("1","2","3"))
data$sys_anticoag = factor(data$sys_anticoag,levels=c("1","2","3"))
data$anti_mal = factor(data$anti_mal,levels=c("1","2","3"))
data$antifung_cmyn = factor(data$antifung_cmyn,levels=c("1","2","3"))
data$experimental = factor(data$experimental,levels=c("1","2","3"))
data$other_sc2_treatments = factor(data$other_sc2_treatments,levels=c("1","2","3"))
data$vasopressors = factor(data$vasopressors,levels=c("1","2","3"))
data$vasopressors_type___1 = factor(data$vasopressors_type___1,levels=c("0","1"))
data$vasopressors_type___2 = factor(data$vasopressors_type___2,levels=c("0","1"))
data$vasopressors_type___3 = factor(data$vasopressors_type___3,levels=c("0","1"))
data$vasopressors_type___4 = factor(data$vasopressors_type___4,levels=c("0","1"))
data$vasopressors_type___5 = factor(data$vasopressors_type___5,levels=c("0","1"))
data$vasopressors_type___6 = factor(data$vasopressors_type___6,levels=c("0","1"))
data$ecmo = factor(data$ecmo,levels=c("1","2","3"))
data$blood_trans = factor(data$blood_trans,levels=c("1","2","3"))
data$medication_complete = factor(data$medication_complete,levels=c("0","1","2"))
data$dsterm = factor(data$dsterm,levels=c("1","2","3","4","5","8","6","7"))
data$excluded_reason = factor(data$excluded_reason,levels=c("1","2","3"))
data$final_dx_1 = factor(data$final_dx_1,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51"))
data$related_1 = factor(data$related_1,levels=c("1","3"))
data$final_dx_2 = factor(data$final_dx_2,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51"))
data$related_2 = factor(data$related_2,levels=c("1","2","3"))
data$final_dx_3 = factor(data$final_dx_3,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51"))
data$final_dx_4 = factor(data$final_dx_4,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51"))
data$final_dx_5 = factor(data$final_dx_5,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51"))
data$final_dx_6 = factor(data$final_dx_6,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51"))
data$related_3 = factor(data$related_3,levels=c("1","2","3"))
data$miscvac = factor(data$miscvac,levels=c("1","0","9"))
data$miscvac_doses = factor(data$miscvac_doses,levels=c("1","2","3","4","9"))
data$miscvac_iggas = factor(data$miscvac_iggas,levels=c("0","1","9"))
data$miscvac_iggan = factor(data$miscvac_iggan,levels=c("0","1","9"))
data$outcome_complete = factor(data$outcome_complete,levels=c("0","1","2"))
data$spirometry_complete = factor(data$spirometry_complete,levels=c("0","1","2"))
data$fever_ws = factor(data$fever_ws,levels=c("1","0"))
data$lumbar_punc = factor(data$lumbar_punc,levels=c("1","0"))
data$csf_bacteria = factor(data$csf_bacteria,levels=c("1","0"))
data$csf_vhs = factor(data$csf_vhs,levels=c("1","0","2"))
data$csf_enterovirus = factor(data$csf_enterovirus,levels=c("1","0","2"))
data$csf_sarscov2 = factor(data$csf_sarscov2,levels=c("1","0","2"))
data$urine_collection = factor(data$urine_collection,levels=c("1","2","3","4","5"))
data$urine_dipstik = factor(data$urine_dipstik,levels=c("1","2","3","4","5"))
data$urine_culture = factor(data$urine_culture,levels=c("1","0","2"))
data$fever_without_source_study_complete = factor(data$fever_without_source_study_complete,levels=c("0","1","2"))
data$qc_stage = factor(data$qc_stage,levels=c("0","1","2","3","4"))
data$first_analysis = factor(data$first_analysis,levels=c("1","0"))
data$quality_control_complete = factor(data$quality_control_complete,levels=c("0","1","2"))

levels(data$redcap_data_access_group)=c("Althaia, Xarxa Assistencial Universitària de Manresa","Complejo Asistencial Universitario de Leon (CAULE)","Complejo Asistencial Universitario de Palencia (CAUPA)","Complejo Hospitalario de Badajoz","Complejo Hospitalario de Navarra (CHN)","Complejo Hospitalario de Toledo (CHT)","Complejo Hospitalario Universitario de Albacete (CHUA)","Consorcio Hosp. General U. de Valencia","HNSP Talavera","Hospital Álvaro Cunqueiro de Vigo","Hospital Aranjuez","Hospital Central de la Defensa Gomez Ulla (HCD)","Hospital Clínico Universitario de Valencia (HCUV)","Hospital Clínico Universitario Lozano Blesa","Hospital Clínico Universitario Valladolid","Hospital Clinico Universitario Virgen de la Arrixaca (HCUVA)","Hospital de Denia","Hospital de Getafe (HUG)","Hospital de la Santa Creu i Sant Pau","Hospital de Manacor","Hospital de Merida","Hospital de Sagunto","Hospital de Txagorritxu","Hospital del Mar de Barcelona","Hospital General de Granollers","Hospital General de Segovia","Hospital General de Villalba (VIL)","Hospital General Universitari de Castello (HGUCS)","Hospital General Universitario de Alicante","Hospital General Universitario Gregorio Marañon (HGM)","Hospital HM Monteprincipe","Hospital Infanta Cristina (HIC)","Hospital Infantil de Zaragoza","Hospital Infantil Universitario Niño Jesus (HNJ)","Hospital Juan Ramon Jimenez de Huelva","Hospital la Fe","Hospital la Moraleja","Hospital Lluís Alcanyis","Hospital Mare de Déu dels Lliris","Hospital Miquel Servet de Zaragoza","Hospital Puerta del Mar (Cádiz)","Hospital Regional Universitario de Malaga (MAL)","Hospital San Pedro (HSP)","Hospital Sant Joan dAlacant","Hospital Sant Joan de Déu (SJD)","Hospital Teresa Herrera A Coruña","Hospital Universitaio Ramon y Cajal (RYC)","Hospital Universitari Arnau de Vilanova (HUAV)","Hospital Universitari Sant Joan de Reus (HUSJR)","Hospital Universitari Son Espases (HUSE)","Hospital Universitario 12 de Octubre (HDO)","Hospital Universitario Central de Asturias (HUCA)","Hospital Universitario Clinico San Carlos (CLI)","Hospital Universitario de Basurto (HUB)","Hospital Universitario de Burgos","Hospital Universitario de Caceres","Hospital Universitario de Canarias (HUC)","Hospital Universitario de Cruces","Hospital Universitario de Fuenlabrada (HUF)","Hospital Universitario de Mostoles (HUM)","Hospital Universitario de Salamanca (HUS)","Hospital Universitario de Torrejon (HUT)","Hospital Universitario del Sureste (SUR)","Hospital Universitario del Tajo (HUDT)","Hospital Universitario Donostia","Hospital Universitario Fundacion Alcorcon (HUFA)","Hospital Universitario Fundacion Jimenez Diaz (FJD)","Hospital Universitario Infanta Elena (HUIE)","Hospital Universitario Infanta Leonor (HUIL)","Hospital Universitario Infanta Sofia (HUIS)","Hospital Universitario La Paz (HLP)","Hospital Universitario Marqués de Valdecilla (HUMV)","Hospital Universitario Materno Infantil de las Palmas","Hospital Universitario Principe de Asturias (HUPA)","Hospital Universitario Puerta de Hierro Majadahonda (PDH)","Hospital Universitario Quirónsalud Madrid (HUQM)","Hospital Universitario Reina Sofia (HURS)","Hospital Universitario Rey Juan Carlos (HURJC)","Hospital Universitario Río Hortega (HURH)","Hospital Universitario San Cecilio Granada","Hospital Universitario Sanitas La Zarzuela","Hospital Universitario Severo Ochoa (HUSO)","Hospital Universitario Son Llatzer de Palma de Mallorca (HSLL)","Hospital Universitario Torrecardenas","Hospital Universitario Virgen de las Nieves (HUVN)","Hospital Universitario Virgen del Rocio (ROC)","Hospital Universitario Virgen Macarena","Hospital Vall d´Hebron","Hospital Virgen de la Luz (CU-HVL)","Hospital Virgen de la Salud","Hospital Vithas Aravaca","Hospitales Madrid","Institut dInvestigacio en Ciencies de la Salut Germans Trias i Pujol (IGTP)","Instituto Hispalense de Pediatria (IHP)","PIUS Hospital de Valls","Red de Medicos Centinela de Navarra")
levels(data$hospital)=c("Hospital Universitario 12 de Octubre (HDO)","Hospital Universitario Infanta Sofía (HUIS)","Hospital Universitario La Paz (HLP)","Hospital Universitario Clínico San Carlos (CLI)","Hospital Universitario Fundación Jiménez Díaz (FJD)","Hospital Universitario Ramón y Cajal (RYC)","Hospital Universitario Virgen del Rocío (ROC)","Hospital Regional Universitario de Málaga (MAL)","Hospital General de Villalba (VIL)","Hospital Universitario del Sureste (SUR)","Hospital Infantil Universitario Niño Jesús (HNJ)","Hospital Universitario Puerta de Hierro Majadahonda (PDH)","Hospital General Universitario Gregorio Marañón (HGM)","Hospital de Getafe (HUG)","Hospital Infanta Cristina (HIC)","Hospital Universitario Príncipe de Asturias (HUPA)","Hospital Universitario Fundacion Alcorcón (HUFA)","Hospital Universitario del Tajo (HUDT)","Hospital Universitario de Fuenlabrada (HUF)","Hospital Universitario Infanta Elena (HUIE)","Hospital Universitario Infanta Leonor (HUIL)","Hospital Universitario Severo Ochoa (HUSO)","Hospital Universitario de Torrejón (HUT)","Hospital Universitario de Móstoles (HUM)","Hospital Universitario Rey Juan Carlos (HURJC)","Hospital Central de la Defensa Gómez Ulla (HCD)","Hospital Universitari Son Espases (HUSE)","Hospital Universitario Reina Sofía (HURS)","Hospital Clínico Universitario Virgen de la Arrixaca (HCUVA)","Institut dInvestigació en Ciències de la Salut Germans Trias i Pujol (IGTP)","Instituto Hispalense de Pediatría (IHP)","Hospital San Pedro (HSP)","Hospital Universitario Quirónsalud Madrid (HUQM)","Hospital Universitari i Politècnic La Fe (HUPLF)","Hospital Universitario La Moraleja (HULM)","HM Hospitales Madrid","Vithas Hospital Nisa Pardo de Aravaca","Hospital Universitario Puerta del Mar (HUPM)","Hospital Universitari Vall dHebron (HUVH)","Consorci Hospital General Universitari de València (CHGUV)","Complejo Hospitalario de Navarra (CHN)","Hospital Universitari Sant Joan de Reus (HUSJR)","Complejo Hospitalario Universitario de Albacete (CHUA)","Hospital Clínico Universitario de Valencia (HCUV)","Hospital Universitario Virgen de las Nieves (HUVN)","Hospital Universitario Sanitas La Zarzuela","Hospital Sant Joan de Deu (SJD)","Hospital de Mérida","Complejo Hospitalario de Toledo (CHT)","Althaia, Xarxa Assistencial Universitària de Manresa","Hospital Universitario de Cruces","Hospital Universitario Río Hortega","Hospital Clínico Universitario Lozano Blesa","Hospital Universitario Central de Asturias (HUCA)","Hospital General Universitario de Alicante","Hospital Universitari Arnau de Vilanova (HUAV)","Hospital Universitario Torrecárdenas","Hospital Virgen de la Luz (CU-HVL)","Hospital Universitario de Salamanca (HUS)","Complejo Asistencial Universitario de Palencia (CAUPA)","Red de Médicos Centinela de Navarra","Hospital Universitario Materno Infantil de las Palmas","Hospital Universitario de Basurto (HUB)","Hospital General Universitari de Castelló (HGUCS)","Complejo Asistencial Universitario de León (CAULE)","PIUS Hospital de Valls","Hospital Mare de Déu dels Lliris","Hospital Universitario de Canarias (HUC)","Hospital Universitario Marqués de Valdecilla (HUMV)","Hospital Sant Joan dAlacant","Hospital Infantil de Zaragoza","Hospital Lluís Alcanyis","Hospital Uniersitario Son Llatzer de Palma de Mallorca (HSLL)","Hospital de Manacor","Hospital Universitario Donostia","Hospital Universitario de Cáceres","Hospital Virgen de la Salud","Hospital General Granollers","Hospital de Txagorritxu","Hospital HM Montepríncipe","Hospital Universitario de Burgos","Hospital de Sagunto","Hospital Universitario San Cecilio Granada","Hospital Universitario Virgen Macarena","Hospital Miquel Servet de Zaragoza","Hospital General de Segovia","Hospital Juan Ramon Jimenez de Huelva","Hospital de la Santa Creu i Sant Pau","Complejo Hospitalario de Badajoz","Hospital Teresa Herrera A Coruña","Hospital del Mar de Barcelona","Hospital de Denia","Hospital de Talavera (HNSP)","Hospital Clínico Universitario Valladolid","Hospital Álvaro Cunqueiro de Vigo")
levels(data$center_id_complete)=c("Incomplete","Unverified","Complete")
levels(data$sex)=c("Male","Female","Not specified")
levels(data$last_15)=c("Yes","No","Unknown")
levels(data$demographics_complete)=c("Incomplete","Unverified","Complete")
levels(data$symptoms_epi_travel)=c("Yes","No","Unknown")
levels(data$symptoms_epi_physical)=c("Yes","No","Unknown")
levels(data$symptoms_epi_healthfac)=c("Yes","No","Unknown")
levels(data$symptoms_epi_animal)=c("Yes","No","Unknown")
levels(data$symptoms_epi_comm)=c("Yes","No","Unknown")
levels(data$contact___1)=c("Unchecked","Checked")
levels(data$contact___2)=c("Unchecked","Checked")
levels(data$contact___3)=c("Unchecked","Checked")
levels(data$contact___4)=c("Unchecked","Checked")
levels(data$inclusion_criteria_complete)=c("Incomplete","Unverified","Complete")
levels(data$anamnesis_complete)=c("Incomplete","Unverified","Complete")
levels(data$comorb)=c("Yes","No")
levels(data$chroniccard_mhyn)=c("Yes","No","Unknown")
levels(data$hypertension_mhyn)=c("Yes","No","Unknown")
levels(data$chronicpul_mhyn)=c("Yes","No","Unknown")
levels(data$asthma_mhyn)=c("Yes","No","Unknown")
levels(data$tb_mhyn)=c("Yes","No","Unknown")
levels(data$resp_inf_mhyn)=c("Yes","No","Unknown")
levels(data$renal_mhyn)=c("Yes","No","Unknown")
levels(data$modliver_mhyn)=c("Yes","No","Unknown")
levels(data$mildliv_mhyn)=c("Yes","No","Unknown")
levels(data$chronicneu_mhyn)=c("Yes","No","Unknown")
levels(data$malignantneo_mhyn)=c("Yes","No","Unknown")
levels(data$chronhaemo_mhyn)=c("Yes","No","Unknown")
levels(data$aidshiv_mhyn)=c("Yes","No","Unknown")
levels(data$obesity_mhyn)=c("Yes","No","Unknown")
levels(data$diabetiscomp_mhyn)=c("Yes","No","Unknown")
levels(data$diabetes_mhyn)=c("Yes","No","Unknown")
levels(data$inflammatory_mhyr)=c("Yes","No","Unknown")
levels(data$rheumatology_mhyr)=c("Yes","No","Unknown")
levels(data$kawa_mhyn)=c("Yes","No","Unknown")
levels(data$kawa_fam_mhyn)=c("Yes","No","Unknown")
levels(data$dementia_mhyn)=c("Yes","No","Unknown")
levels(data$malnutrition_mhyn)=c("Yes","No","Unknown")
levels(data$other_mhyn)=c("Yes","No","Unknown")
levels(data$smoking_mhyn)=c("Yes","Never Smoker","Former Smoker")
levels(data$immunosupressors)=c("Yes","No")
levels(data$comorbidities_complete)=c("Incomplete","Unverified","Complete")
levels(data$influenza_full_regimen)=c("Yes","No")
levels(data$apsc_vcageind)=c("Yes","No","Unknown")
levels(data$cov_vac)=c("Yes","No")
levels(data$vaccines_complete)=c("Incomplete","Unverified","Complete")
levels(data$anthropometry_complete)=c("Incomplete","Unverified","Complete")
levels(data$dehydr)=c("Severe","Some","None")
levels(data$fever_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$cough_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$sorethroat_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$runnynose_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$wheeze_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$chestpain_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$myalgia_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$jointpain_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$fatigue_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$shortbreath_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$lowerchest_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$headache_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$confusion_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$seizures_cecoccur_v2)=c("Yes","No","Unknown")
levels(data$abdopain_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$vomit_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$diarrhoea_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$conjunct_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$oral_inf)=c("Yes","No","Unknown")
levels(data$pale_skin)=c("Yes","No","Unknown")
levels(data$rash_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$skinulcers_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$peri_inf)=c("Yes","No","Unknown")
levels(data$lymp_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$bleed_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$cold_hands)=c("Yes","No","Unknown")
levels(data$cap_refill)=c("Yes","No","Unknown")
levels(data$sw_joints)=c("Yes","No","Unknown")
levels(data$st_neck)=c("Yes","No","Unknown")
levels(data$hypo_flop)=c("Yes","No","Unknown")
levels(data$paralysis)=c("Yes","No","Unknown")
levels(data$shock)=c("Yes","No","Unknown")
levels(data$sens_alt)=c("Yes","No","Unknown")
levels(data$admission_signs_and_symptoms_complete)=c("Incomplete","Unverified","Complete")
levels(data$blood_tests_complete)=c("Incomplete","Unverified","Complete")
levels(data$pcr_cov2_1)=c("Positive","Negative","Pending","Unavailable")
levels(data$pcr_cov2_variant)=c("Alfa","Beta","Gamma","Delta","Omicron")
levels(data$pcr_cov2_2)=c("Positive","Negative","Pending","Unavailable")
levels(data$pcr_cov2_3)=c("Positive","Negative","Pending","Unavailable")
levels(data$pcr_cov2_4)=c("Positive","Negative","Pending","Unavailable")
levels(data$rapid_ag_test_covid___1)=c("Unchecked","Checked")
levels(data$rapid_ag_test_covid___2)=c("Unchecked","Checked")
levels(data$rapid_ag_test_covid_1)=c("Positive","Negative")
levels(data$rapid_ag_test_covid_2)=c("Positive","Negative")
levels(data$virus_obtained_pcr_16)=c("Yes","No")
levels(data$pcr_types_virus_sr___1)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___2)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___3)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___4)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___5)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___6)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___7)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___8)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___9)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___10)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___11)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___12)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___13)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___14)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___15)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___16)=c("Unchecked","Checked")
levels(data$pcr_atipical_sr)=c("Yes","No")
levels(data$pcr_mn_sr)=c("Positive","Negative","Pending","Unavailable")
levels(data$pcr_cn_sr)=c("Positive","Negative","Pending","Unavailable")
levels(data$pcr_hib_sr)=c("Positive","Negative","Pending","Unavailable")
levels(data$pcr_leg_sr)=c("Positive","Negative","Pending","Unavailable")
levels(data$pcr_bordetella_sr)=c("Positive","Negative","Pending","Unavailable")
levels(data$anti_influenza)=c("Positive","Negative","Not performed")
levels(data$anti_rsv)=c("Positive","Negative","Not performed")
levels(data$res_samples)=c("Yes","No")
levels(data$respiratory_secretion_microbiology_complete)=c("Incomplete","Unverified","Complete")
levels(data$first_sr_sc2)=c("Yes","No","Sample collection pending","Results pending")
levels(data$first_sr_sc2_type)=c("Rapid test","ELISA","Neutralization test","Other")
levels(data$igm_sc2_1)=c("Positive","Negative","Pending","Unavailable")
levels(data$igg_sc2_1)=c("Positive","Negative","Pending","Unavailable")
levels(data$second_sr_sc2)=c("Yes","No","Sample collection pending","Results pending")
levels(data$second_sr_sc2_type)=c("Rapid test","ELISA","Neutralization test","Other")
levels(data$igm_sc2_2)=c("Positive","Negative","Pending","Unavailable")
levels(data$igg_sc2_2)=c("Positive","Negative","Pending","Unavailable")
levels(data$blood_culture)=c("Yes","No")
levels(data$probable_bacterial_etiology_hemoculture)=c("S. pneumoniae","S. aureus","S. pyogenes","Other (specify)")
levels(data$other_bacteria_hemoculture)=c("Yes","No")
levels(data$first_sr_atipical)=c("Yes","No","Sample collection pending","Results pending")
levels(data$serology1_igm_mn)=c("Positive","Negative","Pending","Unavailable")
levels(data$serology1_igg_mn)=c("Positive","Negative","Pending","Unavailable")
levels(data$serology1_igm_cn)=c("Positive","Negative","Pending","Unavailable")
levels(data$serology1_igg_cn)=c("Positive","Negative","Pending","Unavailable")
levels(data$second_sr_atipical)=c("Yes","No","Sample collection pending","Results pending")
levels(data$serology2_igm_mn)=c("Positive","Negative","Pending","Unavailable")
levels(data$serology2_igg_mn)=c("Positive","Negative","Pending","Unavailable")
levels(data$serology2_igm_cn)=c("Positive","Negative","Pending","Unavailable")
levels(data$serology2_igg_cn)=c("Positive","Negative","Pending","Unavailable")
levels(data$seroconversion_mn)=c("Yes","No","Pending","Unavailable")
levels(data$blood_samples)=c("Yes","No")
levels(data$plasma_samples)=c("Yes","No")
levels(data$blood_microbiology_complete)=c("Incomplete","Unverified","Complete")
levels(data$codetection)=c("Yes","No","Pending","Unknown due to lack of microbiological data")
levels(data$codetecion_type)=c("Virus-Virus","Virus-Bacteria")
levels(data$codetecion_complete)=c("Incomplete","Unverified","Complete")
levels(data$thoracentesis_done)=c("Yes","No")
levels(data$ag_pneumo_pf)=c("Positive","Negative","Not performed")
levels(data$pcr_pneumo_pf)=c("Positive","Negative","Not performed")
levels(data$culture_pf)=c("Positive","Negative")
levels(data$cultured_bacteria_pf)=c("Pneumococcus","S. pyogenes","S. aureus","Haemophilus influenzae","Other")
levels(data$pleural_fluid_microbiology_complete)=c("Incomplete","Unverified","Complete")
levels(data$oxygen_therapy)=c("Yes","No")
levels(data$hft)=c("Yes","No")
levels(data$cpap)=c("Yes","No")
levels(data$picu_admission)=c("Yes","No")
levels(data$mechanical_ventilation)=c("Yes","No")
levels(data$nimv)=c("Yes","No")
levels(data$imv)=c("Yes","No")
levels(data$evolution_complete)=c("Incomplete","Unverified","Complete")
levels(data$complications)=c("Yes","No")
levels(data$pleural_effusion)=c("Yes","No")
levels(data$effusion_type)=c("Not complicated","Complicated (pH< 7 and/or partitions)")
levels(data$corticosteroids)=c("Yes","No")
levels(data$pleural_drainage)=c("Yes","No")
levels(data$drainage_type)=c("Pleural tube","VATS","Evacuating thoracentesis only")
levels(data$pneumatocele_abscess)=c("Yes","No")
levels(data$pneumothorax)=c("Yes","No")
levels(data$cardio_comp)=c("Yes","No")
levels(data$cardio_comp_myoc)=c("Yes","No")
levels(data$cardio_comp_peri)=c("Yes","No")
levels(data$cardio_comp_valve)=c("Yes","No")
levels(data$cardio_comp_arry)=c("Yes","No")
levels(data$cardio_comp_cor)=c("Yes","No")
levels(data$cardio_comp_ane)=c("Yes","No")
levels(data$sepsis)=c("Yes","No")
levels(data$renal_failure)=c("Yes","No")
levels(data$kawa)=c("Yes","No")
levels(data$complications_complete)=c("Incomplete","Unverified","Complete")
levels(data$xray_result_1)=c("Condensation","Other infiltrates","Normal")
levels(data$x_ray_1_reported_by___1)=c("Unchecked","Checked")
levels(data$x_ray_1_reported_by___2)=c("Unchecked","Checked")
levels(data$x_ray_1_reported_by___3)=c("Unchecked","Checked")
levels(data$x_ray_1_result_ext)=c("Condensation","Other infiltrates","Normal")
levels(data$xray_result_2)=c("Condensation","Other infiltrates","Normal")
levels(data$x_ray_2_reported_by___1)=c("Unchecked","Checked")
levels(data$x_ray_2_reported_by___2)=c("Unchecked","Checked")
levels(data$x_ray_2_reported_by___3)=c("Unchecked","Checked")
levels(data$x_ray_2_result_ext)=c("Condensation","Other infiltrates","Normal")
levels(data$xray_result_3)=c("Condensation","Other infiltrates","Normal")
levels(data$x_ray_3_reported_by___1)=c("Unchecked","Checked")
levels(data$x_ray_3_reported_by___2)=c("Unchecked","Checked")
levels(data$x_ray_3_reported_by___3)=c("Unchecked","Checked")
levels(data$x_ray_3_result_ext)=c("Condensation","Other infiltrates","Normal")
levels(data$cardiac_img)=c("Yes","No","Unknown")
levels(data$radiology_complete)=c("Incomplete","Unverified","Complete")
levels(data$oro_fluids)=c("Yes","No","Unknown")
levels(data$iv_fluids)=c("Yes","No","Unknown")
levels(data$antiviral_cmyn)=c("Yes","No","Unknown")
levels(data$antiviral_cmtrt___1)=c("Unchecked","Checked")
levels(data$antiviral_cmtrt___2)=c("Unchecked","Checked")
levels(data$antiviral_cmtrt___3)=c("Unchecked","Checked")
levels(data$antiviral_cmtrt___4)=c("Unchecked","Checked")
levels(data$antiviral_cmtrt___5)=c("Unchecked","Checked")
levels(data$antiviral_cmtrt___6)=c("Unchecked","Checked")
levels(data$antiviral_cmtrt___7)=c("Unchecked","Checked")
levels(data$antiviral_cmtrt___8)=c("Unchecked","Checked")
levels(data$antiviral_cmtrt___10)=c("Unchecked","Checked")
levels(data$antiviral_cmtrt___11)=c("Unchecked","Checked")
levels(data$antiviral_cmtrt___9)=c("Unchecked","Checked")
levels(data$antibiotic_cmyn)=c("Yes","No","Unknown")
levels(data$iv_ab)=c("Yes","No","Data unavailable")
levels(data$type_iv_ab___0)=c("Unchecked","Checked")
levels(data$type_iv_ab___1)=c("Unchecked","Checked")
levels(data$type_iv_ab___2)=c("Unchecked","Checked")
levels(data$type_iv_ab___3)=c("Unchecked","Checked")
levels(data$type_iv_ab___4)=c("Unchecked","Checked")
levels(data$type_iv_ab___5)=c("Unchecked","Checked")
levels(data$type_iv_ab___6)=c("Unchecked","Checked")
levels(data$type_iv_ab___9)=c("Unchecked","Checked")
levels(data$type_iv_ab___8)=c("Unchecked","Checked")
levels(data$type_iv_ab___7)=c("Unchecked","Checked")
levels(data$o_ab)=c("Yes","No")
levels(data$type_o_ab___1)=c("Unchecked","Checked")
levels(data$type_o_ab___2)=c("Unchecked","Checked")
levels(data$type_o_ab___3)=c("Unchecked","Checked")
levels(data$type_o_ab___4)=c("Unchecked","Checked")
levels(data$type_o_ab___5)=c("Unchecked","Checked")
levels(data$type_o_ab___6)=c("Unchecked","Checked")
levels(data$type_o_ab___7)=c("Unchecked","Checked")
levels(data$type_o_ab___8)=c("Unchecked","Checked")
levels(data$corticost_cmyn)=c("Yes","No","Unknown")
levels(data$corticost_cmroute)=c("Oral","Intravenous","Inhaled")
levels(data$anti_inflammatory___1)=c("Unchecked","Checked")
levels(data$anti_inflammatory___2)=c("Unchecked","Checked")
levels(data$other_anti_inflammatory)=c("Yes","No","Unknown")
levels(data$iv_imglob)=c("Yes","No","Unknown")
levels(data$imod)=c("Yes","No","Unknown")
levels(data$sys_anticoag)=c("Yes","No","Unknown")
levels(data$anti_mal)=c("Yes","No","Unknown")
levels(data$antifung_cmyn)=c("Yes","No","Unknown")
levels(data$experimental)=c("Yes","No","Unknown")
levels(data$other_sc2_treatments)=c("Yes","No","Unknown")
levels(data$vasopressors)=c("Yes","No","Unknown")
levels(data$vasopressors_type___1)=c("Unchecked","Checked")
levels(data$vasopressors_type___2)=c("Unchecked","Checked")
levels(data$vasopressors_type___3)=c("Unchecked","Checked")
levels(data$vasopressors_type___4)=c("Unchecked","Checked")
levels(data$vasopressors_type___5)=c("Unchecked","Checked")
levels(data$vasopressors_type___6)=c("Unchecked","Checked")
levels(data$ecmo)=c("Yes","No","Unknown")
levels(data$blood_trans)=c("Yes","No","Unknown")
levels(data$medication_complete)=c("Incomplete","Unverified","Complete")
levels(data$dsterm)=c("Discharged alive (not hospitalized)","Discharged alive after hospitalization","Transfer to other facility","Death","Palliative discharge","Currently hospitalized","Unknown","Excluded")
levels(data$excluded_reason)=c("Not fulfilled inclusion criteria","Withdrawal of consent","Other")
levels(data$final_dx_1)=c("Abdominal pain","Abscess","Acquired Hemolytic Anaemia","Adenitis","Appendicitis / Peritonitis","Asthma flare","Asymptomatic","Asymptomatic infection","Bacteriemia","Bronchiolitis","Bronchitis","Dehydration","Diabetic debut","Fever without a source","Flu-like syndrome","Gastroenteritis","Gastroenteritis (other)","Hematemesis","Hepatitis","HFH","Hypertension","Inflammatory syndrome","Jaundice","Kawasaki","Kidney stone","Liver failure","Mastoiditis","Neoplasm","Nephritis syndrome","Pneumonia","Psychiatric admission","Pyelonephritis","Salmonellosis","Seizures","Sepsis","Skin/mucosae problems","Social admission","Thrombosis","Trauma","URTI","UTI","Whooping cough","Immunosuppression","Chronic disease","Admission for observation due to unspecified risk","Neonate","Shock","Transconjunctival Sutureless Vitrectomy (TSV)","Laryngitis","Kidney (Renal) failure","Pleural effusion")
levels(data$related_1)=c("Related","Unclear")
levels(data$final_dx_2)=c("Abdominal pain","Abscess","Acquired Hemolytic Anaemia","Adenitis","Appendicitis / Peritonitis","Asthma flare","Asymptomatic","Asymptomatic infection","Bacteriemia","Bronchiolitis","Bronchitis","Dehydration","Diabetic debut","Fever without a source","Flu-like syndrome","Gastroenteritis","Gastroenteritis (other)","Hematemesis","Hepatitis","HFH","Hypertension","Inflammatory syndrome","Jaundice","Kawasaki","Kidney stone","Liver failure","Mastoiditis","Neoplasm","Nephritis syndrome","Pneumonia","Psychiatric admission","Pyelonephritis","Salmonellosis","Seizures","Sepsis","Skin/mucosae problems","Social admission","Thrombosis","Trauma","URTI","UTI","Whooping cough","Immunosuppression","Chronic disease","Admission for observation due to unspecified risk","Neonate","Shock","Transconjunctival Sutureless Vitrectomy (TSV)","Laryngitis","Kidney (Renal) failure","Pleural effusion")
levels(data$related_2)=c("Related","Not Related","Unclear")
levels(data$final_dx_3)=c("Abdominal pain","Abscess","Acquired Hemolytic Anaemia","Adenitis","Appendicitis / Peritonitis","Asthma flare","Asymptomatic","Asymptomatic infection","Bacteriemia","Bronchiolitis","Bronchitis","Dehydration","Diabetic debut","Fever without a source","Flu-like syndrome","Gastroenteritis","Gastroenteritis (other)","Hematemesis","Hepatitis","HFH","Hypertension","Inflammatory syndrome","Jaundice","Kawasaki","Kidney stone","Liver failure","Mastoiditis","Neoplasm","Nephritis syndrome","Pneumonia","Psychiatric admission","Pyelonephritis","Salmonellosis","Seizures","Sepsis","Skin/mucosae problems","Social admission","Thrombosis","Trauma","URTI","UTI","Whooping cough","Immunosuppression","Chronic disease","Admission for observation due to unspecified risk","Neonate","Shock","Transconjunctival Sutureless Vitrectomy (TSV)","Laryngitis","Kidney (Renal) failure","Pleural effusion")
levels(data$final_dx_4)=c("Abdominal pain","Abscess","Acquired Hemolytic Anaemia","Adenitis","Appendicitis / Peritonitis","Asthma flare","Asymptomatic","Asymptomatic infection","Bacteriemia","Bronchiolitis","Bronchitis","Dehydration","Diabetic debut","Fever without a source","Flu-like syndrome","Gastroenteritis","Gastroenteritis (other)","Hematemesis","Hepatitis","HFH","Hypertension","Inflammatory syndrome","Jaundice","Kawasaki","Kidney stone","Liver failure","Mastoiditis","Neoplasm","Nephritis syndrome","Pneumonia","Psychiatric admission","Pyelonephritis","Salmonellosis","Seizures","Sepsis","Skin/mucosae problems","Social admission","Thrombosis","Trauma","URTI","UTI","Whooping cough","Immunosuppression","Chronic disease","Admission for observation due to unspecified risk","Neonate","Shock","Transconjunctival Sutureless Vitrectomy (TSV)","Laryngitis","Kidney (Renal) failure","Pleural effusion")
levels(data$final_dx_5)=c("Abdominal pain","Abscess","Acquired Hemolytic Anaemia","Adenitis","Appendicitis / Peritonitis","Asthma flare","Asymptomatic","Asymptomatic infection","Bacteriemia","Bronchiolitis","Bronchitis","Dehydration","Diabetic debut","Fever without a source","Flu-like syndrome","Gastroenteritis","Gastroenteritis (other)","Hematemesis","Hepatitis","HFH","Hypertension","Inflammatory syndrome","Jaundice","Kawasaki","Kidney stone","Liver failure","Mastoiditis","Neoplasm","Nephritis syndrome","Pneumonia","Psychiatric admission","Pyelonephritis","Salmonellosis","Seizures","Sepsis","Skin/mucosae problems","Social admission","Thrombosis","Trauma","URTI","UTI","Whooping cough","Immunosuppression","Chronic disease","Admission for observation due to unspecified risk","Neonate","Shock","Transconjunctival Sutureless Vitrectomy (TSV)","Laryngitis","Kidney (Renal) failure","Pleural effusion")
levels(data$final_dx_6)=c("Abdominal pain","Abscess","Acquired Hemolytic Anaemia","Adenitis","Appendicitis / Peritonitis","Asthma flare","Asymptomatic","Asymptomatic infection","Bacteriemia","Bronchiolitis","Bronchitis","Dehydration","Diabetic debut","Fever without a source","Flu-like syndrome","Gastroenteritis","Gastroenteritis (other)","Hematemesis","Hepatitis","HFH","Hypertension","Inflammatory syndrome","Jaundice","Kawasaki","Kidney stone","Liver failure","Mastoiditis","Neoplasm","Nephritis syndrome","Pneumonia","Psychiatric admission","Pyelonephritis","Salmonellosis","Seizures","Sepsis","Skin/mucosae problems","Social admission","Thrombosis","Trauma","URTI","UTI","Whooping cough","Immunosuppression","Chronic disease","Admission for observation due to unspecified risk","Neonate","Shock","Transconjunctival Sutureless Vitrectomy (TSV)","Laryngitis","Kidney (Renal) failure","Pleural effusion")
levels(data$related_3)=c("Related","Not Related","Unclear")
levels(data$miscvac)=c("Yes","No","Unknown SARS-CoV-2 vaccination status")
levels(data$miscvac_doses)=c("1","2","3","4","Unknown")
levels(data$miscvac_iggas)=c("Negative","Positive","Not performed")
levels(data$miscvac_iggan)=c("Negative","Positive","Not performed")
levels(data$outcome_complete)=c("Incomplete","Unverified","Complete")
levels(data$spirometry_complete)=c("Incomplete","Unverified","Complete")
levels(data$fever_ws)=c("Yes","No")
levels(data$lumbar_punc)=c("Yes","No")
levels(data$csf_bacteria)=c("Positive","Negative")
levels(data$csf_vhs)=c("Positive","Negative","Not performed")
levels(data$csf_enterovirus)=c("Positive","Negative","Not performed")
levels(data$csf_sarscov2)=c("Positive","Negative","Not performed")
levels(data$urine_collection)=c("Catheter","Tapping / spontaneus","Perineal bag","Puncture","Urine collection not performed")
levels(data$urine_dipstik)=c("Leucocytes","Nitrites","Leucocytes + Nitrites","Normal","Not performed")
levels(data$urine_culture)=c("Positive","Negative","Not performed")
levels(data$fever_without_source_study_complete)=c("Incomplete","Unverified","Complete")
levels(data$qc_stage)=c("Not started","Clinical quality control performed, waiting for answers to clinical queries","Clinical queries resolved, pending data quality control","Data quality control performed, waiting for answers to data queries","Quality control completed")
levels(data$first_analysis)=c("Yes","No")
levels(data$quality_control_complete)=c("Incomplete","Unverified","Complete")

#-------------------------------------------------------------------------------

#Hospitales con pacientes hospitalizados:
#-------------------------------------------------------------------------------
freq(data$hospital,nmax = getOption("max.print.freq", default = 64))
freq(data$dsterm)
data_hospitalizados <- data %>% filter(dsterm!="Discharged alive (not hospitalized)" | dsterm!="Excluded")
data$admission_date %<>% as.Date()
data$discharge_date %<>% as.Date()
hospitalizados <- data %>% filter(is.na(discharge_date)==F)

#hospitales <- hospitalizados %>% group_by(hospital) %>% summarise(n=n())
#write_xlsx(hospitales,"C:/Users/gfrie/OneDrive/Documents/EPICO/Resultados/Dataframes/HospitalesHospitalizados_Espania.xlsx")
#-------------------------------------------------------------------------------


####COLOMBIA

#Cargar base de datos
data <- read.csv("~/EPICO/Datos/Colombia/EPICOCOLOMBIA_DATA_2022-09-22_1131.csv")

#Arreglar base de datos
#-------------------------------------------------------------------------------
#Setting Labels

label(data$record)="Identification"
label(data$redcap_data_access_group)="Data Access Group"
label(data$hospital)="Hospital"
label(data$health_worker_name)="Who fills the paper data notebook (name)"
label(data$center_id_complete)="Complete?"
label(data$patient_birth_date)="Patients date of birth"
label(data$admission_date)="Date of admission"
label(data$age_months)="Age in months"
label(data$sex)="Sex at Birth:"
label(data$last_15)="Has the child been previously admitted to hospital in the last 15 days?"
label(data$demographics_complete)="Complete?"
label(data$symptoms_epi_travel)="A history of travel to an area with documented cases of novel coronavirus infection"
label(data$symptoms_epi_physical)="Close contact with a confirmed or probable case of nCoV infection"
label(data$symptoms_epi_healthfac)="Presence in a healthcare facility where nCoV infections have been managed"
label(data$symptoms_epi_animal)="Direct contact with animals in countries where the nCoV is known to be circulating in animal populations or where human infections have occurred as a result of presumed zoonotic transmission"
label(data$symptoms_epi_comm)="Area with community transmission"
label(data$contact___1)="Close contact (choice=Health care associated exposure, including providing direct care for novel coronavirus patients, e.g. health care worker, working with health care workers infected with novel coronavirus, visiting patients or staying in the same close environment of a novel coronavirus patient, or direct exposure to body fluids or specimens including aerosols)"
label(data$contact___2)="Close contact (choice=Working together in close proximity or sharing the same classroom environment with a novel coronavirus patient)"
label(data$contact___3)="Close contact (choice=Traveling together with novel coronavirus patient in any kind of conveyance)"
label(data$contact___4)="Close contact (choice=Living in the same household as a novel coronavirus patient)"
label(data$index_case)="Who is the index case (father, mother, uncle ...)"
label(data$inclusion_criteria_complete)="Complete?"
label(data$fever_start_date)="Fever onset date"
label(data$fever_days)="Fever days (from start to admission, including start day)"
label(data$fever_days_total)="Fever days (from start to end, including start day)"
label(data$fever_3)="Fever days >= 3"
label(data$anamnesis_complete)="Complete?"
label(data$comorb)="Comorbidities"
label(data$chroniccard_mhyn)="Chronic cardiac disease, including congenital heart disease (not hypertesnion)"
label(data$hypertension_mhyn)="Hypertension"
label(data$chronicpul_mhyn)="Chronic pulmonary disease (not asthma)"
label(data$asthma_mhyn)="Asthma (or recurrent wheezing)"
label(data$tb_mhyn)="Tuberculosis"
label(data$resp_inf_mhyn)="History of respiratory infection in the previous 4 weeks prior to current illness?"
label(data$renal_mhyn)="Chronic kidney disease"
label(data$modliver_mhyn)="Moderate or severe liver disease"
label(data$mildliv_mhyn)="Mild Liver disease"
label(data$chronicneu_mhyn)="Chronic neurological disorder"
label(data$malignantneo_mhyn)="Malignant neoplasm"
label(data$chronhaemo_mhyn)="Chronic hematologic disease"
label(data$aidshiv_mhyn)="AIDS/HIV"
label(data$obesity_mhyn)="Obesity (as defined by clinical staff)"
label(data$diabetiscomp_mhyn)="Diabetes with complications"
label(data$diabetes_mhyn)="Diabetes without complications"
label(data$inflammatory_mhyr)="Inflammatory disorder"
label(data$rheumatology_mhyr)="Rheumatologic disorder"
label(data$kawa_mhyn)="Past history of Kawasaki disease"
label(data$kawa_fam_mhyn)="Family history of Kawasaki disease"
label(data$dementia_mhyn)="Dementia"
label(data$malnutrition_mhyn)="Malnutrition"
label(data$other_mhyn)="Other relevant risk factors"
label(data$inf_reum_type)="Please specify inflammatory or rheumatological disorder"
label(data$smoking_mhyn)="Smoking"
label(data$otherrisktext)="Other  relevant risk factor; Specify"
label(data$immunosupressors)="On immunosuppressive medication?"
label(data$comorbidities_complete)="Complete?"
label(data$influenza_full_regimen)="Influenza vaccine (current year)"
label(data$apsc_vcageind)="Vaccinations appropriate for age/country?"
label(data$vaccines_complete)="Complete?"
label(data$weight)="Weight (grams)"
label(data$weight_percentile)="Weight percentile"
label(data$size)="Size (cm)"
label(data$size_percentile)="Size percentile"
label(data$anthropometry_complete)="Complete?"
label(data$temp_vsorres)="Temperature"
label(data$hr_vsorres)="Heart Rate"
label(data$rr_vsorres)="Respiratory Rate"
label(data$sysbp_vsorres)="Systolic blood pressure"
label(data$admission_diabp_vsorres)="Diastolic blood pressure"
label(data$oxy_vsorres)="Oxygen saturation"
label(data$muac)="Mid-upper arm circumference"
label(data$dehydr)="Dehydratation"
label(data$fever_ceoccur_v2)="History of fever"
label(data$cough_ceoccur_v2)="Cough"
label(data$sorethroat_ceoccur_v2)="Sore throat"
label(data$runnynose_ceoccur_v2)="Runny nose (Rhinorrhoea)"
label(data$wheeze_ceoccur_v2)="Wheezing"
label(data$chestpain_ceoccur_v2)="Chest pain"
label(data$myalgia_ceoccur_v2)="Muscle aches (Myalgia)"
label(data$jointpain_ceoccur_v2)="Joint pain (Arthralgia)"
label(data$fatigue_ceoccur_v2)="Fatigue / Malaise"
label(data$shortbreath_ceoccur_v2)="Shortness of breath or work of breathing (Dyspnea)"
label(data$lowerchest_ceoccur_v2)="Lower chest wall indrawing"
label(data$headache_ceoccur_v2)="Headache"
label(data$confusion_ceoccur_v2)="Altered consciousness / confusion"
label(data$seizures_cecoccur_v2)="Seizures"
label(data$abdopain_ceoccur_v2)="Abdominal pain"
label(data$vomit_ceoccur_v2)="Vomiting / Nausea"
label(data$diarrhoea_ceoccur_v2)="Diarrhoea"
label(data$conjunct_ceoccur_v2)="Conjunctivitis"
label(data$oral_inf)="Oral mucosal inflammation signs"
label(data$pale_skin)="Pale/mottled skin"
label(data$rash_ceoccur_v2)="Skin rash"
label(data$skinulcers_ceoccur_v2)="Skin ulcers"
label(data$peri_inf)="Peripheral cutaneous inflammation signs (hands or feet)"
label(data$lymp_ceoccur_v2)="Lymphadenopathy"
label(data$bleed_ceoccur_v2)="Bleeding (Haemorrhage)"
label(data$cold_hands)="Cold hands/feet"
label(data$cap_refill)="Capillary refill time > 2 seconds ?"
label(data$sw_joints)="Swollen joints"
label(data$st_neck)="Stiff neck"
label(data$hypo_flop)="Hypotonia / floppiness"
label(data$paralysis)="Paralysis"
label(data$shock)="Shock signs"
label(data$sens_alt)="Alteration in taste or smell"
label(data$admission_signs_and_symptoms_complete)="Complete?"
label(data$hb)="Hemoglobin"
label(data$leukocytes)="Leukocytes"
label(data$neutrophils)="Neutrophils"
label(data$lymphocytes)="Lymphocytes"
label(data$platelets)="Platelets"
label(data$aptt_aptr)="Activated Partial Thromboplastin Time / Ratio (APTT / APTR)"
label(data$pt)="Prothrombin Time (PT)"
label(data$inr)="International Normalized Ratio (INR)"
label(data$fibrino)="Fibrinogen"
label(data$c_reactive_protein)="C-reactive protein"
label(data$procalcitonin)="Procalcitonin"
label(data$cpk)="CPK"
label(data$alt_gpt)="Alanine aminotransferase-GPT"
label(data$ast_got)="Aspartate aminotransferase-GOT"
label(data$urea)="Urea"
label(data$creatinin)="Creatinine"
label(data$ldh)="Lactate dehydrogenase"
label(data$albumin)="Albumin"
label(data$sodium)="Serum sodium"
label(data$d_dimer)="Dimer-D"
label(data$il_6)="IL-6"
label(data$ferritin)="Ferritin"
label(data$trig)="Triglycerides"
label(data$bilirubin)="Total bilirubin"
label(data$lactate)="Lactate"
label(data$ph)="pH"
label(data$po2)="pO2"
label(data$pco2)="pCO2"
label(data$blood_tests_complete)="Complete?"
label(data$pcr_cov2_1)="SARS-CoV-2 PCR at diagnosis"
label(data$ct_cov2_1)="Cycle threshold (Ct) of SARS-CoV-2 PCR at diagnosis"
label(data$pcr_cov2_1_date)="Date of SARS-CoV-2 PCR at diagnosis"
label(data$pcr_cov2_2)="SARS-CoV-2 PCR at visit 2"
label(data$ct_cov2_2)="Cycle threshold (Ct) of SARS-CoV-2 PCR at visit 2"
label(data$pcr_cov2_2_date)="Date of SARS-CoV-2 PCR at visit 2"
label(data$pcr_cov2_3)="SARS-CoV-2 PCR at visit 3"
label(data$ct_cov2_3)="Cycle threshold (Ct) of SARS-CoV-2 PCR at visit 3"
label(data$pcr_cov2_3_date)="Date of SARS-CoV-2 PCR at visit 3"
label(data$pcr_cov2_4)="SARS-CoV-2 PCR at visit 4"
label(data$ct_cov2_4)="Cycle threshold (Ct) of SARS-CoV-2 PCR at visit 4"
label(data$pcr_cov2_4_date)="Date of SARS-CoV-2 PCR at visit 4"
label(data$rapid_ag_test_covid___1)="Rapid Antigenic Test (choice=Nasopharyngeal smear)"
label(data$rapid_ag_test_covid___2)="Rapid Antigenic Test (choice=Saliva)"
label(data$rapid_ag_test_covid_1)="Nasopharyngeal smear result"
label(data$rapid_ag_test_covid_2)="Saliva result"
label(data$virus_obtained_pcr_16)="PCR obtained for 16 viruses in respiratory secretion"
label(data$pcr_types_virus_sr___1)="Positive result in respiratory secretion PCR (choice=VRS)"
label(data$pcr_types_virus_sr___2)="Positive result in respiratory secretion PCR (choice=hMPV)"
label(data$pcr_types_virus_sr___3)="Positive result in respiratory secretion PCR (choice=PIV 1)"
label(data$pcr_types_virus_sr___4)="Positive result in respiratory secretion PCR (choice=PIV 2)"
label(data$pcr_types_virus_sr___5)="Positive result in respiratory secretion PCR (choice=PIV 3)"
label(data$pcr_types_virus_sr___6)="Positive result in respiratory secretion PCR (choice=PIV 4)"
label(data$pcr_types_virus_sr___7)="Positive result in respiratory secretion PCR (choice=Influenza A)"
label(data$pcr_types_virus_sr___8)="Positive result in respiratory secretion PCR (choice=Influenza B)"
label(data$pcr_types_virus_sr___9)="Positive result in respiratory secretion PCR (choice=hBoV)"
label(data$pcr_types_virus_sr___10)="Positive result in respiratory secretion PCR (choice=ADV)"
label(data$pcr_types_virus_sr___11)="Positive result in respiratory secretion PCR (choice=EV-hRV)"
label(data$pcr_types_virus_sr___12)="Positive result in respiratory secretion PCR (choice=Rhinovirus)"
label(data$pcr_types_virus_sr___13)="Positive result in respiratory secretion PCR (choice=CoV 229E)"
label(data$pcr_types_virus_sr___14)="Positive result in respiratory secretion PCR (choice=CoV OC43)"
label(data$pcr_types_virus_sr___15)="Positive result in respiratory secretion PCR (choice=CoV NL63)"
label(data$pcr_types_virus_sr___16)="Positive result in respiratory secretion PCR (choice=CoV HKU12)"
label(data$pcr_atipical_sr)="Obtained PCR sample for atypical respiratory secretion?"
label(data$pcr_mn_sr)="PCR Mycoplasma pneumoniae in respiratory secretion"
label(data$pcr_cn_sr)="PCR Chlamydophila pneumoniae in respiratory secretion"
label(data$pcr_hib_sr)="Haemophilus influenzae PCR in respiratory secretion"
label(data$pcr_leg_sr)="PCR Legionella in respiratory secretion"
label(data$pcr_bordetella_sr)="PCR Bordetella in respiratory secretion"
label(data$anti_influenza)="Antigen Influenza"
label(data$anti_rsv)="Antigen RSV"
label(data$res_samples)="Respiratory samples stored?"
label(data$respiratory_secretion_microbiology_complete)="Complete?"
label(data$first_sr_sc2)="Acute phase serology SARS-CoV-2 performed"
label(data$first_sr_sc2_type)="Acute phase serology test"
label(data$first_sr_sc2_type_ot)="Please, specify other test"
label(data$first_sr_sc2_date)="Date of acute phase serology SARS-CoV-2"
label(data$igm_sc2_1)="Acute phase serology IgM SARS-CoV-2"
label(data$igg_sc2_1)="Acute phase serology IgG SARS-CoV-2"
label(data$second_sr_sc2)="Follow-up serology SARS-CoV-2 performed"
label(data$second_sr_sc2_type)="Follow-up serology test"
label(data$second_sr_sc2_type_ot)="Please, specify other test"
label(data$second_sr_sc2_date)="Date of follow-up serology SARS-CoV-2"
label(data$igm_sc2_2)="Follow-up serology IgM SARS-CoV-2"
label(data$igg_sc2_2)="Follow-up serology IgG SARS-CoV-2"
label(data$sr_sc2_days)="Days from acute phase and follow-up serology for SARS-COV-2 (including the day when acute phase serology was done)"
label(data$blood_culture)="Blood culture for significant bacteria"
label(data$probable_bacterial_etiology_hemoculture)="Most likely bacterial aetiology in blood culture"
label(data$probable_text)="Please, specify"
label(data$other_bacteria_hemoculture)="Other bacteria in blood culture"
label(data$probable_text_2)="Please, specify"
label(data$first_sr_atipical)="First atypical serology performed"
label(data$date_ser_1)="Date of first atypical bacteria serology"
label(data$serology1_igm_mn)="Serology 1 IgM Mycoplasma pneumoniae"
label(data$serology1_igg_mn)="Serology 1 IgG Mycoplasma pneumoniae"
label(data$title1_igg_mn)="Title 1 IgG Mycoplasma pneumoniae"
label(data$serology1_igm_cn)="Serology 1 IgM Chlamydophila pneumoniae"
label(data$serology1_igg_cn)="Serology 1 IgG Chlamydophila pneumoniae"
label(data$title1_igg_cn)="Title 1 IgG Chlamydophila pneumoniae"
label(data$second_sr_atipical)="Second atypical serology performed"
label(data$date_ser_2)="Date of second atypical bacteria serology"
label(data$serology2_igm_mn)="Serology 2 IgM Mycoplasma pneumoniae"
label(data$serology2_igg_mn)="Serology 2 IgG Mycoplasma pneumoniae"
label(data$title2_igg_mn)="Title 2 IgG Mycoplasma pneumoniae"
label(data$serology2_igm_cn)="Serology 2 IgM Chlamydophila pneumoniae"
label(data$serology2_igg_cn)="Serology 2 IgG Chlamydophila pneumoniae"
label(data$title_igg_mn)="Title 2 IgG Chlamydophila pneumoniae"
label(data$seroconversion_mn)="Seroconversion"
label(data$blood_samples)="Whole blood samples stored?"
label(data$plasma_samples)="Plasma samples stored?"
label(data$blood_microbiology_complete)="Complete?"
label(data$codetection)="Codetection"
label(data$codetecion_type)="Codetection type"
label(data$codetecion_complete)="Complete?"
label(data$thoracentesis_done)="Thoracentesis performed"
label(data$ph_thoracentesis)="pH thoracentesis"
label(data$ag_pneumo_pf)="Pneumococcal antigen in pleural fluid"
label(data$pcr_pneumo_pf)="Pleural fluid pneumococcal PCR"
label(data$culture_pf)="Pleural fluid culture"
label(data$cultured_bacteria_pf)="Bacteria grown in pleural fluid"
label(data$other_cultured_bacteria_pf)="Other bacteria in pleural fluid culture"
label(data$pleural_fluid_microbiology_complete)="Complete?"
label(data$fever_end_date)="End date of fever"
label(data$oxygen_therapy)="Oxygen therapy"
label(data$oxigen_start_date)="Oxygen therapy start date"
label(data$oxigen_end_date)="End date oxygen therapy"
label(data$oxygen_therapy_time)="Days of oxygen therapy (including start day)"
label(data$hft)="High flow therapy"
label(data$hft_start_date)="High flow start date"
label(data$hft_end_date)="High flow end date"
label(data$hft_time)="Days of high flow therapy (including start day)"
label(data$cpap)="CPAP"
label(data$cpap_start_date)="CPAP start date"
label(data$cpap_end_date)="CPAP end date"
label(data$cpap_time)="Days of CPAP (including start day)"
label(data$picu_admission)="Admission to PICU"
label(data$picu_admission_date)="PICU admission date"
label(data$picu_discharge_date)="PICU discharge date"
label(data$picu_time)="Days of PICU admission (including admission day)"
label(data$mechanical_ventilation)="Mechanical ventilation"
label(data$nimv)="Noninvasive mechanical ventilation"
label(data$nimv_start_date)="Noninvasive mechanical ventilation start date"
label(data$nimv_end_date)="Noninvasive mechanical ventilation end date"
label(data$nimv_time)="Days of noninvasive mechanical ventilation (including start day)"
label(data$imv)="Invasive mechanical ventilation"
label(data$imv_start_date)="Invasive mechanical ventilation start date"
label(data$imv_end_date)="Invasive mechanical ventilation end date"
label(data$imv_time)="Days of invasive mechanical ventilation (including start day)"
label(data$discharge_date)="Hospital discharge date"
label(data$hosp_time)="Days of admission (including admission day)"
label(data$evolution_complete)="Complete?"
label(data$complications)="Complications (including new-onset signs or diagnosis after admission, such as Kawasaki-like / Inflammatory Syndrome symptoms)"
label(data$pleural_effusion)="Pleural effusion"
label(data$effusion_type)="Type of pleural effusion"
label(data$corticosteroids)="Systemic corticosteroids"
label(data$pleural_drainage)="Pleural drainage"
label(data$drainage_type)="Drain type"
label(data$pneumatocele_abscess)="Necrotizing pneumonia / abscess / pneumatocele"
label(data$pneumothorax)="Pneumothorax"
label(data$cardio_comp)="Cardiological complications"
label(data$cardio_comp_myoc)="Myocarditis / myocardial dysfunction"
label(data$cardio_comp_peri)="Pericarditis"
label(data$cardio_comp_valve)="Valve dysfunction"
label(data$cardio_comp_arry)="Arrhythmia"
label(data$cardio_comp_cor)="Coronary abnormalities"
label(data$cardio_comp_ane)="Aneurysms"
label(data$cardio_comp_bnp)="NT-proBNP"
label(data$cardio_comp_trop)="Troponin I"
label(data$cardio_comp_other)="Other cardiologic complications"
label(data$sepsis)="Sepsis"
label(data$renal_failure)="Renal failure"
label(data$kawa)="Kawasaki-like / Inflammatory Syndrome symptoms"
label(data$other_complications)="Other complications"
label(data$complications_complete)="Complete?"
label(data$x_ray_1)="X-ray 1 image"
label(data$x_ray_1_date)="Date of X-ray 1"
label(data$xray_result_1)="Interpretation of first radiological image"
label(data$x_ray_1_reported_by___1)="Responsible for first radiological image interpretation (choice=Clinician)"
label(data$x_ray_1_reported_by___2)="Responsible for first radiological image interpretation (choice=Radiology report)"
label(data$x_ray_1_reported_by___3)="Responsible for first radiological image interpretation (choice=Researcher)"
label(data$x_ray_1_result_ext)="First radiological image read by external researcher not involved with the patient"
label(data$x_ray_2)="X-ray 2 image"
label(data$x_ray_2_date)="Date of X-ray 2"
label(data$xray_result_2)="Interpretation of second radiological image"
label(data$x_ray_2_reported_by___1)="Responsible for second radiological image interpretation (choice=Clinician)"
label(data$x_ray_2_reported_by___2)="Responsible for second radiological image interpretation (choice=Radiology report)"
label(data$x_ray_2_reported_by___3)="Responsible for second radiological image interpretation (choice=Researcher)"
label(data$x_ray_2_result_ext)="Second radiological image read by external researcher not involved with the patient"
label(data$x_ray_3)="X-ray 3 image"
label(data$x_ray_3_date)="Date of X-ray 3"
label(data$xray_result_3)="Interpretation of third radiological image"
label(data$x_ray_3_reported_by___1)="Responsible for third radiological image interpretation (choice=Clinician)"
label(data$x_ray_3_reported_by___2)="Responsible for third radiological image interpretation (choice=Radiology report)"
label(data$x_ray_3_reported_by___3)="Responsible for third radiological image interpretation (choice=Researcher)"
label(data$x_ray_3_result_ext)="Third radiological image read by external researcher not involved with the patient"
label(data$lung_ct)="Lung CT scan image"
label(data$lung_ct_date)="Date of Lung CT scan image"
label(data$cardiac_img)="Cardiac imaging performed?"
label(data$card_img_results)="Please, specify name of technique and findings"
label(data$radiology_complete)="Complete?"
label(data$oro_fluids)="Oral/orogastric fluids?"
label(data$iv_fluids)="Intravenous fluids?"
label(data$antiviral_cmyn)="Antiviral agent"
label(data$antiviral_cmtrt___1)="Specify antiviral agent (choice=Ribavirin)"
label(data$antiviral_cmtrt___2)="Specify antiviral agent (choice=Lopinavir/Ritonavir)"
label(data$antiviral_cmtrt___3)="Specify antiviral agent (choice=Interferon-alpha)"
label(data$antiviral_cmtrt___4)="Specify antiviral agent (choice=Interferon-beta)"
label(data$antiviral_cmtrt___5)="Specify antiviral agent (choice=Neuraminidase inhibitors)"
label(data$antiviral_cmtrt___6)="Specify antiviral agent (choice=Remdesivir)"
label(data$antiviral_cmtrt___7)="Specify antiviral agent (choice=Chloroquine)"
label(data$antiviral_cmtrt___8)="Specify antiviral agent (choice=Hydroxychloroquine)"
label(data$antiviral_cmtrt___10)="Specify antiviral agent (choice=Anakinra)"
label(data$antiviral_cmtrt___11)="Specify antiviral agent (choice=Ivermectin)"
label(data$antiviral_cmtrt___9)="Specify antiviral agent (choice=Other)"
label(data$antiviral_cmtype)="Specify other antiviral type"
label(data$antiviral_days)="Days with antiviral"
label(data$antibiotic_cmyn)="Antibiotic"
label(data$iv_ab)="Intravenous antibiotic"
label(data$type_iv_ab___0)="Type of intravenous antibiotic (choice=Ampicillin)"
label(data$type_iv_ab___1)="Type of intravenous antibiotic (choice=Cefotaxime)"
label(data$type_iv_ab___2)="Type of intravenous antibiotic (choice=Cefuroxime)"
label(data$type_iv_ab___3)="Type of intravenous antibiotic (choice=Vancomycin)"
label(data$type_iv_ab___4)="Type of intravenous antibiotic (choice=Meropenem)"
label(data$type_iv_ab___5)="Type of intravenous antibiotic (choice=Clindamycin)"
label(data$type_iv_ab___6)="Type of intravenous antibiotic (choice=Cefazolin)"
label(data$type_iv_ab___9)="Type of intravenous antibiotic (choice=Ceftriaxone)"
label(data$type_iv_ab___8)="Type of intravenous antibiotic (choice=Clavulanic acid)"
label(data$type_iv_ab___7)="Type of intravenous antibiotic (choice=Other (specify))"
label(data$other_iv_ab)="Other intravenous antibiotic"
label(data$iv_ab_start_date)="Intravenous antibiotic start date"
label(data$iv_ab_end_date)="Intravenous antibiotic end date"
label(data$iv_ab_days)="Days of intravenous antibiotic (including start date)"
label(data$o_ab)="Intrahospital oral antibiotic "
label(data$type_o_ab___1)="Oral antibiotic type (choice=Amoxicillin)"
label(data$type_o_ab___2)="Oral antibiotic type (choice=Amoxicillin-Clavulanic)"
label(data$type_o_ab___3)="Oral antibiotic type (choice=Cefuroxime)"
label(data$type_o_ab___4)="Oral antibiotic type (choice=Azithromycin)"
label(data$type_o_ab___5)="Oral antibiotic type (choice=Erythromycin)"
label(data$type_o_ab___6)="Oral antibiotic type (choice=Clindamycin)"
label(data$type_o_ab___7)="Oral antibiotic type (choice=Cefadroxil)"
label(data$type_o_ab___8)="Oral antibiotic type (choice=Other (specify))"
label(data$other_o_ab)="Other oral antibiotic"
label(data$o_ab_start_date)="Oral antibiotic start date"
label(data$o_ab_end_date)="Oral antibiotic end date"
label(data$o_ab_days)="Days of oral antibiotic (including start date)"
label(data$azithromycin_or_erythromycin)="Oral Azithromycin or Erythromycin"
label(data$azithromycin_or_erythromycin_2d)="Oral Azithromycin or Erythromycin (during the first 2 days, 36h)"
label(data$corticost_cmyn)="Corticosteroids"
label(data$corticost_cmyn_date)="Specify Corticosteroids start date"
label(data$corticost_cmtrt)="Specify corticosteroid type"
label(data$corticost_cmdose)="Specify corticosteroid dose"
label(data$corticost_cmroute)="Specify corticosteroid route"
label(data$anti_inflammatory___1)="Anti-inflammatory treatment (choice=Ibuprofen)"
label(data$anti_inflammatory___2)="Anti-inflammatory treatment (choice=Tocilizumab)"
label(data$other_anti_inflammatory)="Other non-Steroidal Anti-Inflammatory Drugs (NSAIDs)"
label(data$other_anti_inflammatory_type)="Please, specify name of other NSAIDs"
label(data$iv_imglob)="Intravenous immune globulin"
label(data$iv_imglob_date)="Specify intravenous immune globulin start date"
label(data$iv_imglob_dose)="Daily intravenous immune globulin dose"
label(data$iv_imglob_days)="Days of intravenous immune globulin treatment"
label(data$imod)="Immunomodulators"
label(data$imod_type)="Please, specify name of immunomodulator"
label(data$sys_anticoag)="Systemic anticoagulation"
label(data$sys_anticoag_type)="Please, specify name of systemic anticoagulation agent"
label(data$anti_mal)="Antimalarial agent"
label(data$anti_mal_type)="Please, specify name of antimalarial agent"
label(data$antifung_cmyn)="Antifungal agent"
label(data$antifung_type)="Please, specify name of antifungal agent"
label(data$experimental)="Experimental agent"
label(data$experimental_type)="Please, specify name of experimental agent"
label(data$other_sc2_treatments)="Other medications directed to cure COVID-19"
label(data$other_sc2_treatments_text)="Please, specify other COVID-19 treatments"
label(data$vasopressors)="Inotropes/vasopressors"
label(data$vasopressors_type___1)="Please, specify name of inotropes/vasopressors (choice=Adrenalin)"
label(data$vasopressors_type___2)="Please, specify name of inotropes/vasopressors (choice=Noradrenaline)"
label(data$vasopressors_type___3)="Please, specify name of inotropes/vasopressors (choice=Dopamine)"
label(data$vasopressors_type___4)="Please, specify name of inotropes/vasopressors (choice=Dobutamine)"
label(data$vasopressors_type___5)="Please, specify name of inotropes/vasopressors (choice=Milrinone)"
label(data$vasopressors_type___6)="Please, specify name of inotropes/vasopressors (choice=Other (please, specify in comments))"
label(data$ecmo)="Extracorporeal (ECMO) support"
label(data$blood_trans)="Blood transfusion"
label(data$medication_complete)="Complete?"
label(data$dsterm)="Patient outcome"
label(data$dsterm_other_text)="Please, specify which facility has been transferred to"
label(data$excluded_reason)="Reason for exclusion"
label(data$excluded_reason_ot)="Please specify reason for exclusion"
label(data$final_dx_global_concept)="Final diagnosis, ICD-10-CM concept/s"
label(data$final_dx_global_code)="Final diagnosis, ICD-10-CM code/s"
label(data$final_dx_1)="Primary diagnosis related with COVID-19"
label(data$related_1)="Is the primary diagnosis related to COVID-19?"
label(data$final_dx_2)="Secondary diagnosis: comorbidity which conditioned this admission"
label(data$related_2)="Is the secondary diagnosis related to COVID-19?"
label(data$final_dx_3)="Other diagnosis"
label(data$final_dx_4)="Other diagnosis"
label(data$final_dx_5)="Other diagnosis"
label(data$final_dx_6)="Other diagnosis"
label(data$related_3)="Is the other diagnosis related to COVID-19?"
label(data$outcome_complete)="Complete?"
label(data$fvc_6m)="Forced vital capacity (FVC)"
label(data$fev1_6m)="Forced expiratory volume (FEV1)"
label(data$esp_ave_flux_6m)="Average expiratory flow"
label(data$spirometry_complete)="Complete?"
label(data$fever_ws)="Fever without source at emergency ward admission"
label(data$lumbar_punc)="Lumbar puncture performed"
label(data$csf_bacteria)="CSF bacterial culture"
label(data$csf_baceria_type)="Bacteria isolated in CSF"
label(data$csf_vhs)="CSF VHS PCR "
label(data$csf_enterovirus)="CSF Enterovirus PCR "
label(data$csf_sarscov2)="CSF SARSCoV2 PCR "
label(data$csf_leuco)="Leucocytes in CSF"
label(data$csf_neutro)="Percentage of Neutrophils CSF"
label(data$csf_lympho)="Percentage of Lymphocytes in CSF"
label(data$protein_csf)="Protein in CSF"
label(data$glucose_csf)="Glucose in CSF"
label(data$urine_collection)="Urine collection method"
label(data$urine_dipstik)="Urine dipstick"
label(data$urine_culture)="Urine culture"
label(data$bacteria_urine)="Bacteria isolated in urine "
label(data$final_diagnosis)="Final diagnosis after fever without source episode, ICD-10-CM concept"
label(data$final_diagnosis_icd10)="Final diagnosis after fever without source episode, ICD-10-CM code"
label(data$comments_fws)="Comments"
label(data$fever_without_source_study_complete)="Complete?"
label(data$qc_stage)="Current stage of quality control"
label(data$qc_date)="Date of quality control stage change"
label(data$first_analysis)="First Analysis"
label(data$quality_control_complete)="Complete?"
#Setting Units


#Setting Factors(will create new variable for factors)
data$redcap_data_access_group = factor(data$redcap_data_access_group,levels=c("centro_medico_crec","clnica_colsanitas","clnica_colsanitasb","clnica_colsanitasc","clnica_colsanitasd","clinica_del_rosari","clinica_infantil_c","clinica_los_cobos","clinica_materno_in","clinica_soma","fundacion_san_vice","hospital_de_nario","hospital_militar","hospital_regional","hospital_universia","hospital_universit","instituto_roosevel","ministerio_de_salu","san_jos_centro","san_jos_infantil","universidad_de_los"))
data$hospital = factor(data$hospital,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","15","16","17","18","19","20","21","22"))
data$center_id_complete = factor(data$center_id_complete,levels=c("0","1","2"))
data$sex = factor(data$sex,levels=c("1","2","3"))
data$last_15 = factor(data$last_15,levels=c("1","2","3"))
data$demographics_complete = factor(data$demographics_complete,levels=c("0","1","2"))
data$symptoms_epi_travel = factor(data$symptoms_epi_travel,levels=c("1","2","3"))
data$symptoms_epi_physical = factor(data$symptoms_epi_physical,levels=c("1","2","3"))
data$symptoms_epi_healthfac = factor(data$symptoms_epi_healthfac,levels=c("1","2","3"))
data$symptoms_epi_animal = factor(data$symptoms_epi_animal,levels=c("1","2","3"))
data$symptoms_epi_comm = factor(data$symptoms_epi_comm,levels=c("1","2","3"))
data$contact___1 = factor(data$contact___1,levels=c("0","1"))
data$contact___2 = factor(data$contact___2,levels=c("0","1"))
data$contact___3 = factor(data$contact___3,levels=c("0","1"))
data$contact___4 = factor(data$contact___4,levels=c("0","1"))
data$inclusion_criteria_complete = factor(data$inclusion_criteria_complete,levels=c("0","1","2"))
data$anamnesis_complete = factor(data$anamnesis_complete,levels=c("0","1","2"))
data$comorb = factor(data$comorb,levels=c("1","0"))
data$chroniccard_mhyn = factor(data$chroniccard_mhyn,levels=c("1","2","3"))
data$hypertension_mhyn = factor(data$hypertension_mhyn,levels=c("1","2","3"))
data$chronicpul_mhyn = factor(data$chronicpul_mhyn,levels=c("1","2","3"))
data$asthma_mhyn = factor(data$asthma_mhyn,levels=c("1","2","3"))
data$tb_mhyn = factor(data$tb_mhyn,levels=c("1","2","3"))
data$resp_inf_mhyn = factor(data$resp_inf_mhyn,levels=c("1","2","3"))
data$renal_mhyn = factor(data$renal_mhyn,levels=c("1","2","3"))
data$modliver_mhyn = factor(data$modliver_mhyn,levels=c("1","2","3"))
data$mildliv_mhyn = factor(data$mildliv_mhyn,levels=c("1","2","3"))
data$chronicneu_mhyn = factor(data$chronicneu_mhyn,levels=c("1","2","3"))
data$malignantneo_mhyn = factor(data$malignantneo_mhyn,levels=c("1","2","3"))
data$chronhaemo_mhyn = factor(data$chronhaemo_mhyn,levels=c("1","2","3"))
data$aidshiv_mhyn = factor(data$aidshiv_mhyn,levels=c("1","2","3"))
data$obesity_mhyn = factor(data$obesity_mhyn,levels=c("1","2","3"))
data$diabetiscomp_mhyn = factor(data$diabetiscomp_mhyn,levels=c("1","2","3"))
data$diabetes_mhyn = factor(data$diabetes_mhyn,levels=c("1","2","3"))
data$inflammatory_mhyr = factor(data$inflammatory_mhyr,levels=c("1","2","3"))
data$rheumatology_mhyr = factor(data$rheumatology_mhyr,levels=c("1","2","3"))
data$kawa_mhyn = factor(data$kawa_mhyn,levels=c("1","2","3"))
data$kawa_fam_mhyn = factor(data$kawa_fam_mhyn,levels=c("1","2","3"))
data$dementia_mhyn = factor(data$dementia_mhyn,levels=c("1","2","3"))
data$malnutrition_mhyn = factor(data$malnutrition_mhyn,levels=c("1","2","3"))
data$other_mhyn = factor(data$other_mhyn,levels=c("1","2","3"))
data$smoking_mhyn = factor(data$smoking_mhyn,levels=c("1","2","3"))
data$immunosupressors = factor(data$immunosupressors,levels=c("1","0"))
data$comorbidities_complete = factor(data$comorbidities_complete,levels=c("0","1","2"))
data$influenza_full_regimen = factor(data$influenza_full_regimen,levels=c("1","0"))
data$apsc_vcageind = factor(data$apsc_vcageind,levels=c("1","2","3"))
data$vaccines_complete = factor(data$vaccines_complete,levels=c("0","1","2"))
data$anthropometry_complete = factor(data$anthropometry_complete,levels=c("0","1","2"))
data$dehydr = factor(data$dehydr,levels=c("1","2","3"))
data$fever_ceoccur_v2 = factor(data$fever_ceoccur_v2,levels=c("1","2","3"))
data$cough_ceoccur_v2 = factor(data$cough_ceoccur_v2,levels=c("1","2","3"))
data$sorethroat_ceoccur_v2 = factor(data$sorethroat_ceoccur_v2,levels=c("1","2","3"))
data$runnynose_ceoccur_v2 = factor(data$runnynose_ceoccur_v2,levels=c("1","2","3"))
data$wheeze_ceoccur_v2 = factor(data$wheeze_ceoccur_v2,levels=c("1","2","3"))
data$chestpain_ceoccur_v2 = factor(data$chestpain_ceoccur_v2,levels=c("1","2","3"))
data$myalgia_ceoccur_v2 = factor(data$myalgia_ceoccur_v2,levels=c("1","2","3"))
data$jointpain_ceoccur_v2 = factor(data$jointpain_ceoccur_v2,levels=c("1","2","3"))
data$fatigue_ceoccur_v2 = factor(data$fatigue_ceoccur_v2,levels=c("1","2","3"))
data$shortbreath_ceoccur_v2 = factor(data$shortbreath_ceoccur_v2,levels=c("1","2","3"))
data$lowerchest_ceoccur_v2 = factor(data$lowerchest_ceoccur_v2,levels=c("1","2","3"))
data$headache_ceoccur_v2 = factor(data$headache_ceoccur_v2,levels=c("1","2","3"))
data$confusion_ceoccur_v2 = factor(data$confusion_ceoccur_v2,levels=c("1","2","3"))
data$seizures_cecoccur_v2 = factor(data$seizures_cecoccur_v2,levels=c("1","2","3"))
data$abdopain_ceoccur_v2 = factor(data$abdopain_ceoccur_v2,levels=c("1","2","3"))
data$vomit_ceoccur_v2 = factor(data$vomit_ceoccur_v2,levels=c("1","2","3"))
data$diarrhoea_ceoccur_v2 = factor(data$diarrhoea_ceoccur_v2,levels=c("1","2","3"))
data$conjunct_ceoccur_v2 = factor(data$conjunct_ceoccur_v2,levels=c("1","2","3"))
data$oral_inf = factor(data$oral_inf,levels=c("1","2","3"))
data$pale_skin = factor(data$pale_skin,levels=c("1","2","3"))
data$rash_ceoccur_v2 = factor(data$rash_ceoccur_v2,levels=c("1","2","3"))
data$skinulcers_ceoccur_v2 = factor(data$skinulcers_ceoccur_v2,levels=c("1","2","3"))
data$peri_inf = factor(data$peri_inf,levels=c("1","2","3"))
data$lymp_ceoccur_v2 = factor(data$lymp_ceoccur_v2,levels=c("1","2","3"))
data$bleed_ceoccur_v2 = factor(data$bleed_ceoccur_v2,levels=c("1","2","3"))
data$cold_hands = factor(data$cold_hands,levels=c("1","2","3"))
data$cap_refill = factor(data$cap_refill,levels=c("1","2","3"))
data$sw_joints = factor(data$sw_joints,levels=c("1","2","3"))
data$st_neck = factor(data$st_neck,levels=c("1","2","3"))
data$hypo_flop = factor(data$hypo_flop,levels=c("1","2","3"))
data$paralysis = factor(data$paralysis,levels=c("1","2","3"))
data$shock = factor(data$shock,levels=c("1","2","3"))
data$sens_alt = factor(data$sens_alt,levels=c("1","2","3"))
data$admission_signs_and_symptoms_complete = factor(data$admission_signs_and_symptoms_complete,levels=c("0","1","2"))
data$blood_tests_complete = factor(data$blood_tests_complete,levels=c("0","1","2"))
data$pcr_cov2_1 = factor(data$pcr_cov2_1,levels=c("1","2","3","4"))
data$pcr_cov2_2 = factor(data$pcr_cov2_2,levels=c("1","2","3","4"))
data$pcr_cov2_3 = factor(data$pcr_cov2_3,levels=c("1","2","3","4"))
data$pcr_cov2_4 = factor(data$pcr_cov2_4,levels=c("1","2","3","4"))
data$rapid_ag_test_covid___1 = factor(data$rapid_ag_test_covid___1,levels=c("0","1"))
data$rapid_ag_test_covid___2 = factor(data$rapid_ag_test_covid___2,levels=c("0","1"))
data$rapid_ag_test_covid_1 = factor(data$rapid_ag_test_covid_1,levels=c("1","2"))
data$rapid_ag_test_covid_2 = factor(data$rapid_ag_test_covid_2,levels=c("1","2"))
data$virus_obtained_pcr_16 = factor(data$virus_obtained_pcr_16,levels=c("1","0"))
data$pcr_types_virus_sr___1 = factor(data$pcr_types_virus_sr___1,levels=c("0","1"))
data$pcr_types_virus_sr___2 = factor(data$pcr_types_virus_sr___2,levels=c("0","1"))
data$pcr_types_virus_sr___3 = factor(data$pcr_types_virus_sr___3,levels=c("0","1"))
data$pcr_types_virus_sr___4 = factor(data$pcr_types_virus_sr___4,levels=c("0","1"))
data$pcr_types_virus_sr___5 = factor(data$pcr_types_virus_sr___5,levels=c("0","1"))
data$pcr_types_virus_sr___6 = factor(data$pcr_types_virus_sr___6,levels=c("0","1"))
data$pcr_types_virus_sr___7 = factor(data$pcr_types_virus_sr___7,levels=c("0","1"))
data$pcr_types_virus_sr___8 = factor(data$pcr_types_virus_sr___8,levels=c("0","1"))
data$pcr_types_virus_sr___9 = factor(data$pcr_types_virus_sr___9,levels=c("0","1"))
data$pcr_types_virus_sr___10 = factor(data$pcr_types_virus_sr___10,levels=c("0","1"))
data$pcr_types_virus_sr___11 = factor(data$pcr_types_virus_sr___11,levels=c("0","1"))
data$pcr_types_virus_sr___12 = factor(data$pcr_types_virus_sr___12,levels=c("0","1"))
data$pcr_types_virus_sr___13 = factor(data$pcr_types_virus_sr___13,levels=c("0","1"))
data$pcr_types_virus_sr___14 = factor(data$pcr_types_virus_sr___14,levels=c("0","1"))
data$pcr_types_virus_sr___15 = factor(data$pcr_types_virus_sr___15,levels=c("0","1"))
data$pcr_types_virus_sr___16 = factor(data$pcr_types_virus_sr___16,levels=c("0","1"))
data$pcr_atipical_sr = factor(data$pcr_atipical_sr,levels=c("1","0"))
data$pcr_mn_sr = factor(data$pcr_mn_sr,levels=c("1","2","3","4"))
data$pcr_cn_sr = factor(data$pcr_cn_sr,levels=c("1","2","3","4"))
data$pcr_hib_sr = factor(data$pcr_hib_sr,levels=c("1","2","3","4"))
data$pcr_leg_sr = factor(data$pcr_leg_sr,levels=c("1","2","3","4"))
data$pcr_bordetella_sr = factor(data$pcr_bordetella_sr,levels=c("1","2","3","4"))
data$anti_influenza = factor(data$anti_influenza,levels=c("1","2","3"))
data$anti_rsv = factor(data$anti_rsv,levels=c("1","2","3"))
data$res_samples = factor(data$res_samples,levels=c("1","0"))
data$respiratory_secretion_microbiology_complete = factor(data$respiratory_secretion_microbiology_complete,levels=c("0","1","2"))
data$first_sr_sc2 = factor(data$first_sr_sc2,levels=c("1","0","2","3"))
data$first_sr_sc2_type = factor(data$first_sr_sc2_type,levels=c("1","2","3","4"))
data$igm_sc2_1 = factor(data$igm_sc2_1,levels=c("1","2","3","4"))
data$igg_sc2_1 = factor(data$igg_sc2_1,levels=c("1","2","3","4"))
data$second_sr_sc2 = factor(data$second_sr_sc2,levels=c("1","0","2","3"))
data$second_sr_sc2_type = factor(data$second_sr_sc2_type,levels=c("1","2","3","4"))
data$igm_sc2_2 = factor(data$igm_sc2_2,levels=c("1","2","3","4"))
data$igg_sc2_2 = factor(data$igg_sc2_2,levels=c("1","2","3","4"))
data$blood_culture = factor(data$blood_culture,levels=c("1","0"))
data$probable_bacterial_etiology_hemoculture = factor(data$probable_bacterial_etiology_hemoculture,levels=c("1","2","3","4"))
data$other_bacteria_hemoculture = factor(data$other_bacteria_hemoculture,levels=c("1","0"))
data$first_sr_atipical = factor(data$first_sr_atipical,levels=c("1","0","2","3"))
data$serology1_igm_mn = factor(data$serology1_igm_mn,levels=c("1","2","3","4"))
data$serology1_igg_mn = factor(data$serology1_igg_mn,levels=c("1","2","3","4"))
data$serology1_igm_cn = factor(data$serology1_igm_cn,levels=c("1","2","3","4"))
data$serology1_igg_cn = factor(data$serology1_igg_cn,levels=c("1","2","3","4"))
data$second_sr_atipical = factor(data$second_sr_atipical,levels=c("1","0","2","3"))
data$serology2_igm_mn = factor(data$serology2_igm_mn,levels=c("1","2","3","4"))
data$serology2_igg_mn = factor(data$serology2_igg_mn,levels=c("1","2","3","4"))
data$serology2_igm_cn = factor(data$serology2_igm_cn,levels=c("1","2","3","4"))
data$serology2_igg_cn = factor(data$serology2_igg_cn,levels=c("1","2","3","4"))
data$seroconversion_mn = factor(data$seroconversion_mn,levels=c("1","2","3","4"))
data$blood_samples = factor(data$blood_samples,levels=c("1","0"))
data$plasma_samples = factor(data$plasma_samples,levels=c("1","0"))
data$blood_microbiology_complete = factor(data$blood_microbiology_complete,levels=c("0","1","2"))
data$codetection = factor(data$codetection,levels=c("1","2","3","4"))
data$codetecion_type = factor(data$codetecion_type,levels=c("1","2"))
data$codetecion_complete = factor(data$codetecion_complete,levels=c("0","1","2"))
data$thoracentesis_done = factor(data$thoracentesis_done,levels=c("1","0"))
data$ag_pneumo_pf = factor(data$ag_pneumo_pf,levels=c("1","2","3"))
data$pcr_pneumo_pf = factor(data$pcr_pneumo_pf,levels=c("1","2","3"))
data$culture_pf = factor(data$culture_pf,levels=c("1","2"))
data$cultured_bacteria_pf = factor(data$cultured_bacteria_pf,levels=c("1","2","3","4","5"))
data$pleural_fluid_microbiology_complete = factor(data$pleural_fluid_microbiology_complete,levels=c("0","1","2"))
data$oxygen_therapy = factor(data$oxygen_therapy,levels=c("1","0"))
data$hft = factor(data$hft,levels=c("1","0"))
data$cpap = factor(data$cpap,levels=c("1","0"))
data$picu_admission = factor(data$picu_admission,levels=c("1","0"))
data$mechanical_ventilation = factor(data$mechanical_ventilation,levels=c("1","0"))
data$nimv = factor(data$nimv,levels=c("1","0"))
data$imv = factor(data$imv,levels=c("1","0"))
data$evolution_complete = factor(data$evolution_complete,levels=c("0","1","2"))
data$complications = factor(data$complications,levels=c("1","0"))
data$pleural_effusion = factor(data$pleural_effusion,levels=c("1","0"))
data$effusion_type = factor(data$effusion_type,levels=c("1","2"))
data$corticosteroids = factor(data$corticosteroids,levels=c("1","0"))
data$pleural_drainage = factor(data$pleural_drainage,levels=c("1","0"))
data$drainage_type = factor(data$drainage_type,levels=c("1","2","3"))
data$pneumatocele_abscess = factor(data$pneumatocele_abscess,levels=c("1","0"))
data$pneumothorax = factor(data$pneumothorax,levels=c("1","0"))
data$cardio_comp = factor(data$cardio_comp,levels=c("1","0"))
data$cardio_comp_myoc = factor(data$cardio_comp_myoc,levels=c("1","0"))
data$cardio_comp_peri = factor(data$cardio_comp_peri,levels=c("1","0"))
data$cardio_comp_valve = factor(data$cardio_comp_valve,levels=c("1","0"))
data$cardio_comp_arry = factor(data$cardio_comp_arry,levels=c("1","0"))
data$cardio_comp_cor = factor(data$cardio_comp_cor,levels=c("1","0"))
data$cardio_comp_ane = factor(data$cardio_comp_ane,levels=c("1","0"))
data$sepsis = factor(data$sepsis,levels=c("1","0"))
data$renal_failure = factor(data$renal_failure,levels=c("1","0"))
data$kawa = factor(data$kawa,levels=c("1","0"))
data$complications_complete = factor(data$complications_complete,levels=c("0","1","2"))
data$xray_result_1 = factor(data$xray_result_1,levels=c("1","2","3"))
data$x_ray_1_reported_by___1 = factor(data$x_ray_1_reported_by___1,levels=c("0","1"))
data$x_ray_1_reported_by___2 = factor(data$x_ray_1_reported_by___2,levels=c("0","1"))
data$x_ray_1_reported_by___3 = factor(data$x_ray_1_reported_by___3,levels=c("0","1"))
data$x_ray_1_result_ext = factor(data$x_ray_1_result_ext,levels=c("1","2","3"))
data$xray_result_2 = factor(data$xray_result_2,levels=c("1","2","3"))
data$x_ray_2_reported_by___1 = factor(data$x_ray_2_reported_by___1,levels=c("0","1"))
data$x_ray_2_reported_by___2 = factor(data$x_ray_2_reported_by___2,levels=c("0","1"))
data$x_ray_2_reported_by___3 = factor(data$x_ray_2_reported_by___3,levels=c("0","1"))
data$x_ray_2_result_ext = factor(data$x_ray_2_result_ext,levels=c("1","2","3"))
data$xray_result_3 = factor(data$xray_result_3,levels=c("1","2","3"))
data$x_ray_3_reported_by___1 = factor(data$x_ray_3_reported_by___1,levels=c("0","1"))
data$x_ray_3_reported_by___2 = factor(data$x_ray_3_reported_by___2,levels=c("0","1"))
data$x_ray_3_reported_by___3 = factor(data$x_ray_3_reported_by___3,levels=c("0","1"))
data$x_ray_3_result_ext = factor(data$x_ray_3_result_ext,levels=c("1","2","3"))
data$cardiac_img = factor(data$cardiac_img,levels=c("1","2","3"))
data$radiology_complete = factor(data$radiology_complete,levels=c("0","1","2"))
data$oro_fluids = factor(data$oro_fluids,levels=c("1","2","3"))
data$iv_fluids = factor(data$iv_fluids,levels=c("1","2","3"))
data$antiviral_cmyn = factor(data$antiviral_cmyn,levels=c("1","2","3"))
data$antiviral_cmtrt___1 = factor(data$antiviral_cmtrt___1,levels=c("0","1"))
data$antiviral_cmtrt___2 = factor(data$antiviral_cmtrt___2,levels=c("0","1"))
data$antiviral_cmtrt___3 = factor(data$antiviral_cmtrt___3,levels=c("0","1"))
data$antiviral_cmtrt___4 = factor(data$antiviral_cmtrt___4,levels=c("0","1"))
data$antiviral_cmtrt___5 = factor(data$antiviral_cmtrt___5,levels=c("0","1"))
data$antiviral_cmtrt___6 = factor(data$antiviral_cmtrt___6,levels=c("0","1"))
data$antiviral_cmtrt___7 = factor(data$antiviral_cmtrt___7,levels=c("0","1"))
data$antiviral_cmtrt___8 = factor(data$antiviral_cmtrt___8,levels=c("0","1"))
data$antiviral_cmtrt___10 = factor(data$antiviral_cmtrt___10,levels=c("0","1"))
data$antiviral_cmtrt___11 = factor(data$antiviral_cmtrt___11,levels=c("0","1"))
data$antiviral_cmtrt___9 = factor(data$antiviral_cmtrt___9,levels=c("0","1"))
data$antibiotic_cmyn = factor(data$antibiotic_cmyn,levels=c("1","2","3"))
data$iv_ab = factor(data$iv_ab,levels=c("1","2","3"))
data$type_iv_ab___0 = factor(data$type_iv_ab___0,levels=c("0","1"))
data$type_iv_ab___1 = factor(data$type_iv_ab___1,levels=c("0","1"))
data$type_iv_ab___2 = factor(data$type_iv_ab___2,levels=c("0","1"))
data$type_iv_ab___3 = factor(data$type_iv_ab___3,levels=c("0","1"))
data$type_iv_ab___4 = factor(data$type_iv_ab___4,levels=c("0","1"))
data$type_iv_ab___5 = factor(data$type_iv_ab___5,levels=c("0","1"))
data$type_iv_ab___6 = factor(data$type_iv_ab___6,levels=c("0","1"))
data$type_iv_ab___9 = factor(data$type_iv_ab___9,levels=c("0","1"))
data$type_iv_ab___8 = factor(data$type_iv_ab___8,levels=c("0","1"))
data$type_iv_ab___7 = factor(data$type_iv_ab___7,levels=c("0","1"))
data$o_ab = factor(data$o_ab,levels=c("1","0"))
data$type_o_ab___1 = factor(data$type_o_ab___1,levels=c("0","1"))
data$type_o_ab___2 = factor(data$type_o_ab___2,levels=c("0","1"))
data$type_o_ab___3 = factor(data$type_o_ab___3,levels=c("0","1"))
data$type_o_ab___4 = factor(data$type_o_ab___4,levels=c("0","1"))
data$type_o_ab___5 = factor(data$type_o_ab___5,levels=c("0","1"))
data$type_o_ab___6 = factor(data$type_o_ab___6,levels=c("0","1"))
data$type_o_ab___7 = factor(data$type_o_ab___7,levels=c("0","1"))
data$type_o_ab___8 = factor(data$type_o_ab___8,levels=c("0","1"))
data$corticost_cmyn = factor(data$corticost_cmyn,levels=c("1","2","3"))
data$corticost_cmroute = factor(data$corticost_cmroute,levels=c("1","2","3"))
data$anti_inflammatory___1 = factor(data$anti_inflammatory___1,levels=c("0","1"))
data$anti_inflammatory___2 = factor(data$anti_inflammatory___2,levels=c("0","1"))
data$other_anti_inflammatory = factor(data$other_anti_inflammatory,levels=c("1","2","3"))
data$iv_imglob = factor(data$iv_imglob,levels=c("1","2","3"))
data$imod = factor(data$imod,levels=c("1","2","3"))
data$sys_anticoag = factor(data$sys_anticoag,levels=c("1","2","3"))
data$anti_mal = factor(data$anti_mal,levels=c("1","2","3"))
data$antifung_cmyn = factor(data$antifung_cmyn,levels=c("1","2","3"))
data$experimental = factor(data$experimental,levels=c("1","2","3"))
data$other_sc2_treatments = factor(data$other_sc2_treatments,levels=c("1","2","3"))
data$vasopressors = factor(data$vasopressors,levels=c("1","2","3"))
data$vasopressors_type___1 = factor(data$vasopressors_type___1,levels=c("0","1"))
data$vasopressors_type___2 = factor(data$vasopressors_type___2,levels=c("0","1"))
data$vasopressors_type___3 = factor(data$vasopressors_type___3,levels=c("0","1"))
data$vasopressors_type___4 = factor(data$vasopressors_type___4,levels=c("0","1"))
data$vasopressors_type___5 = factor(data$vasopressors_type___5,levels=c("0","1"))
data$vasopressors_type___6 = factor(data$vasopressors_type___6,levels=c("0","1"))
data$ecmo = factor(data$ecmo,levels=c("1","2","3"))
data$blood_trans = factor(data$blood_trans,levels=c("1","2","3"))
data$medication_complete = factor(data$medication_complete,levels=c("0","1","2"))
data$dsterm = factor(data$dsterm,levels=c("1","2","3","4","5","8","6","7"))
data$excluded_reason = factor(data$excluded_reason,levels=c("1","2","3"))
data$final_dx_1 = factor(data$final_dx_1,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47"))
data$related_1 = factor(data$related_1,levels=c("1","2"))
data$final_dx_2 = factor(data$final_dx_2,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47"))
data$related_2 = factor(data$related_2,levels=c("1","2","3"))
data$final_dx_3 = factor(data$final_dx_3,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47"))
data$final_dx_4 = factor(data$final_dx_4,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47"))
data$final_dx_5 = factor(data$final_dx_5,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47"))
data$final_dx_6 = factor(data$final_dx_6,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47"))
data$related_3 = factor(data$related_3,levels=c("1","2","3"))
data$outcome_complete = factor(data$outcome_complete,levels=c("0","1","2"))
data$spirometry_complete = factor(data$spirometry_complete,levels=c("0","1","2"))
data$fever_ws = factor(data$fever_ws,levels=c("1","0"))
data$lumbar_punc = factor(data$lumbar_punc,levels=c("1","0"))
data$csf_bacteria = factor(data$csf_bacteria,levels=c("1","0"))
data$csf_vhs = factor(data$csf_vhs,levels=c("1","0","2"))
data$csf_enterovirus = factor(data$csf_enterovirus,levels=c("1","0","2"))
data$csf_sarscov2 = factor(data$csf_sarscov2,levels=c("1","0","2"))
data$urine_collection = factor(data$urine_collection,levels=c("1","2","3","4","5"))
data$urine_dipstik = factor(data$urine_dipstik,levels=c("1","2","3","4","5"))
data$urine_culture = factor(data$urine_culture,levels=c("1","0","2"))
data$fever_without_source_study_complete = factor(data$fever_without_source_study_complete,levels=c("0","1","2"))
data$qc_stage = factor(data$qc_stage,levels=c("0","1","2","3","4"))
data$first_analysis = factor(data$first_analysis,levels=c("1","0"))
data$quality_control_complete = factor(data$quality_control_complete,levels=c("0","1","2"))

levels(data$redcap_data_access_group)=c("Centro Medico Crecer (Cartagena)","Clínica Colsanitas S.A. Sede Clínica Pediatrica","Clínica Colsanitas S.A. Sede Clínica Reina Sofía","Clínica Colsanitas S.A. Sede Clínica Universitaria Colombia ","Clínica Colsanitas S.A. Sede Santa María del Lago","Clinica del Rosario","Clinica Infantil Colsubsidio (CIC)","Clinica Los Cobos","Clinica Materno infantil San Luis","Clinica SOMA","Fundacion San Vicente de Paul (Medellin)","Hospital de Nariño","Hospital Militar","Hospital Regional de la Orinoquia","Hospital Universiario Fundacion Santa Fe de Bogota (HUFSFB)","Hospital Universitario Fundacion Valle del Lili","Instituto Roosevelt (IR)","Ministerio de Salud de Colombia","San José Centro","San José Infantil","Universidad de los Andes")
levels(data$hospital)=c("Clínica Infantil Colsubsidio (CIC)","Hospital Universitário Fundación Santa Fé de Bogotá (HUFSFB)","Instituto Roosevelt (IR)","Ministerio de Salud de Colombia","Universidad de los Andes","Hospital de Nariño","San José Centro","San José Infantil","Hospital Militar","Clínica Colsanitas Pediátrica","Clínica Colsanitas S.A. Sede Santa María del Lago","Clinica Colsanitas Reina Sofia","Clinica Colsanitas Universitaria Colombia","Hospital Regional de la Orinoquia","Clinica Los Cobos","Clinica Materno Infantil San Luis","Clinica del Rosario","Clinica SOMA","Hospital Universitario Fundacion Valle del Lili","Fundacion San Vicente de Paul (Medellin)","Centro Medico Crecer (Cartagena)")
levels(data$center_id_complete)=c("Incomplete","Unverified","Complete")
levels(data$sex)=c("Male","Female","Not specified")
levels(data$last_15)=c("Yes","No","Unknown")
levels(data$demographics_complete)=c("Incomplete","Unverified","Complete")
levels(data$symptoms_epi_travel)=c("Yes","No","Unknown")
levels(data$symptoms_epi_physical)=c("Yes","No","Unknown")
levels(data$symptoms_epi_healthfac)=c("Yes","No","Unknown")
levels(data$symptoms_epi_animal)=c("Yes","No","Unknown")
levels(data$symptoms_epi_comm)=c("Yes","No","Unknown")
levels(data$contact___1)=c("Unchecked","Checked")
levels(data$contact___2)=c("Unchecked","Checked")
levels(data$contact___3)=c("Unchecked","Checked")
levels(data$contact___4)=c("Unchecked","Checked")
levels(data$inclusion_criteria_complete)=c("Incomplete","Unverified","Complete")
levels(data$anamnesis_complete)=c("Incomplete","Unverified","Complete")
levels(data$comorb)=c("Yes","No")
levels(data$chroniccard_mhyn)=c("Yes","No","Unknown")
levels(data$hypertension_mhyn)=c("Yes","No","Unknown")
levels(data$chronicpul_mhyn)=c("Yes","No","Unknown")
levels(data$asthma_mhyn)=c("Yes","No","Unknown")
levels(data$tb_mhyn)=c("Yes","No","Unknown")
levels(data$resp_inf_mhyn)=c("Yes","No","Unknown")
levels(data$renal_mhyn)=c("Yes","No","Unknown")
levels(data$modliver_mhyn)=c("Yes","No","Unknown")
levels(data$mildliv_mhyn)=c("Yes","No","Unknown")
levels(data$chronicneu_mhyn)=c("Yes","No","Unknown")
levels(data$malignantneo_mhyn)=c("Yes","No","Unknown")
levels(data$chronhaemo_mhyn)=c("Yes","No","Unknown")
levels(data$aidshiv_mhyn)=c("Yes","No","Unknown")
levels(data$obesity_mhyn)=c("Yes","No","Unknown")
levels(data$diabetiscomp_mhyn)=c("Yes","No","Unknown")
levels(data$diabetes_mhyn)=c("Yes","No","Unknown")
levels(data$inflammatory_mhyr)=c("Yes","No","Unknown")
levels(data$rheumatology_mhyr)=c("Yes","No","Unknown")
levels(data$kawa_mhyn)=c("Yes","No","Unknown")
levels(data$kawa_fam_mhyn)=c("Yes","No","Unknown")
levels(data$dementia_mhyn)=c("Yes","No","Unknown")
levels(data$malnutrition_mhyn)=c("Yes","No","Unknown")
levels(data$other_mhyn)=c("Yes","No","Unknown")
levels(data$smoking_mhyn)=c("Yes","Never Smoker","Former Smoker")
levels(data$immunosupressors)=c("Yes","No")
levels(data$comorbidities_complete)=c("Incomplete","Unverified","Complete")
levels(data$influenza_full_regimen)=c("Yes","No")
levels(data$apsc_vcageind)=c("Yes","No","Unknown")
levels(data$vaccines_complete)=c("Incomplete","Unverified","Complete")
levels(data$anthropometry_complete)=c("Incomplete","Unverified","Complete")
levels(data$dehydr)=c("Severe","Some","None")
levels(data$fever_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$cough_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$sorethroat_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$runnynose_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$wheeze_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$chestpain_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$myalgia_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$jointpain_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$fatigue_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$shortbreath_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$lowerchest_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$headache_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$confusion_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$seizures_cecoccur_v2)=c("Yes","No","Unknown")
levels(data$abdopain_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$vomit_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$diarrhoea_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$conjunct_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$oral_inf)=c("Yes","No","Unknown")
levels(data$pale_skin)=c("Yes","No","Unknown")
levels(data$rash_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$skinulcers_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$peri_inf)=c("Yes","No","Unknown")
levels(data$lymp_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$bleed_ceoccur_v2)=c("Yes","No","Unknown")
levels(data$cold_hands)=c("Yes","No","Unknown")
levels(data$cap_refill)=c("Yes","No","Unknown")
levels(data$sw_joints)=c("Yes","No","Unknown")
levels(data$st_neck)=c("Yes","No","Unknown")
levels(data$hypo_flop)=c("Yes","No","Unknown")
levels(data$paralysis)=c("Yes","No","Unknown")
levels(data$shock)=c("Yes","No","Unknown")
levels(data$sens_alt)=c("Yes","No","Unknown")
levels(data$admission_signs_and_symptoms_complete)=c("Incomplete","Unverified","Complete")
levels(data$blood_tests_complete)=c("Incomplete","Unverified","Complete")
levels(data$pcr_cov2_1)=c("Positive","Negative","Pending","Unavailable")
levels(data$pcr_cov2_2)=c("Positive","Negative","Pending","Unavailable")
levels(data$pcr_cov2_3)=c("Positive","Negative","Pending","Unavailable")
levels(data$pcr_cov2_4)=c("Positive","Negative","Pending","Unavailable")
levels(data$rapid_ag_test_covid___1)=c("Unchecked","Checked")
levels(data$rapid_ag_test_covid___2)=c("Unchecked","Checked")
levels(data$rapid_ag_test_covid_1)=c("Positive","Negative")
levels(data$rapid_ag_test_covid_2)=c("Positive","Negative")
levels(data$virus_obtained_pcr_16)=c("Yes","No")
levels(data$pcr_types_virus_sr___1)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___2)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___3)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___4)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___5)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___6)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___7)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___8)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___9)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___10)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___11)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___12)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___13)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___14)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___15)=c("Unchecked","Checked")
levels(data$pcr_types_virus_sr___16)=c("Unchecked","Checked")
levels(data$pcr_atipical_sr)=c("Yes","No")
levels(data$pcr_mn_sr)=c("Positive","Negative","Pending","Unavailable")
levels(data$pcr_cn_sr)=c("Positive","Negative","Pending","Unavailable")
levels(data$pcr_hib_sr)=c("Positive","Negative","Pending","Unavailable")
levels(data$pcr_leg_sr)=c("Positive","Negative","Pending","Unavailable")
levels(data$pcr_bordetella_sr)=c("Positive","Negative","Pending","Unavailable")
levels(data$anti_influenza)=c("Positive","Negative","Not performed")
levels(data$anti_rsv)=c("Positive","Negative","Not performed")
levels(data$res_samples)=c("Yes","No")
levels(data$respiratory_secretion_microbiology_complete)=c("Incomplete","Unverified","Complete")
levels(data$first_sr_sc2)=c("Yes","No","Sample collection pending","Results pending")
levels(data$first_sr_sc2_type)=c("Rapid test","ELISA","Neutralization test","Other")
levels(data$igm_sc2_1)=c("Positive","Negative","Pending","Unavailable")
levels(data$igg_sc2_1)=c("Positive","Negative","Pending","Unavailable")
levels(data$second_sr_sc2)=c("Yes","No","Sample collection pending","Results pending")
levels(data$second_sr_sc2_type)=c("Rapid test","ELISA","Neutralization test","Other")
levels(data$igm_sc2_2)=c("Positive","Negative","Pending","Unavailable")
levels(data$igg_sc2_2)=c("Positive","Negative","Pending","Unavailable")
levels(data$blood_culture)=c("Yes","No")
levels(data$probable_bacterial_etiology_hemoculture)=c("S. pneumoniae","S. aureus","S. pyogenes","Other (specify)")
levels(data$other_bacteria_hemoculture)=c("Yes","No")
levels(data$first_sr_atipical)=c("Yes","No","Sample collection pending","Results pending")
levels(data$serology1_igm_mn)=c("Positive","Negative","Pending","Unavailable")
levels(data$serology1_igg_mn)=c("Positive","Negative","Pending","Unavailable")
levels(data$serology1_igm_cn)=c("Positive","Negative","Pending","Unavailable")
levels(data$serology1_igg_cn)=c("Positive","Negative","Pending","Unavailable")
levels(data$second_sr_atipical)=c("Yes","No","Sample collection pending","Results pending")
levels(data$serology2_igm_mn)=c("Positive","Negative","Pending","Unavailable")
levels(data$serology2_igg_mn)=c("Positive","Negative","Pending","Unavailable")
levels(data$serology2_igm_cn)=c("Positive","Negative","Pending","Unavailable")
levels(data$serology2_igg_cn)=c("Positive","Negative","Pending","Unavailable")
levels(data$seroconversion_mn)=c("Yes","No","Pending","Unavailable")
levels(data$blood_samples)=c("Yes","No")
levels(data$plasma_samples)=c("Yes","No")
levels(data$blood_microbiology_complete)=c("Incomplete","Unverified","Complete")
levels(data$codetection)=c("Yes","No","Pending","Unknown due to lack of microbiological data")
levels(data$codetecion_type)=c("Virus-Virus","Virus-Bacteria")
levels(data$codetecion_complete)=c("Incomplete","Unverified","Complete")
levels(data$thoracentesis_done)=c("Yes","No")
levels(data$ag_pneumo_pf)=c("Positive","Negative","Not performed")
levels(data$pcr_pneumo_pf)=c("Positive","Negative","Not performed")
levels(data$culture_pf)=c("Positive","Negative")
levels(data$cultured_bacteria_pf)=c("Pneumococcus","S. pyogenes","S. aureus","Haemophilus influenzae","Other")
levels(data$pleural_fluid_microbiology_complete)=c("Incomplete","Unverified","Complete")
levels(data$oxygen_therapy)=c("Yes","No")
levels(data$hft)=c("Yes","No")
levels(data$cpap)=c("Yes","No")
levels(data$picu_admission)=c("Yes","No")
levels(data$mechanical_ventilation)=c("Yes","No")
levels(data$nimv)=c("Yes","No")
levels(data$imv)=c("Yes","No")
levels(data$evolution_complete)=c("Incomplete","Unverified","Complete")
levels(data$complications)=c("Yes","No")
levels(data$pleural_effusion)=c("Yes","No")
levels(data$effusion_type)=c("Not complicated","Complicated (pH< 7 and/or partitions)")
levels(data$corticosteroids)=c("Yes","No")
levels(data$pleural_drainage)=c("Yes","No")
levels(data$drainage_type)=c("Pleural tube","VATS","Evacuating thoracentesis only")
levels(data$pneumatocele_abscess)=c("Yes","No")
levels(data$pneumothorax)=c("Yes","No")
levels(data$cardio_comp)=c("Yes","No")
levels(data$cardio_comp_myoc)=c("Yes","No")
levels(data$cardio_comp_peri)=c("Yes","No")
levels(data$cardio_comp_valve)=c("Yes","No")
levels(data$cardio_comp_arry)=c("Yes","No")
levels(data$cardio_comp_cor)=c("Yes","No")
levels(data$cardio_comp_ane)=c("Yes","No")
levels(data$sepsis)=c("Yes","No")
levels(data$renal_failure)=c("Yes","No")
levels(data$kawa)=c("Yes","No")
levels(data$complications_complete)=c("Incomplete","Unverified","Complete")
levels(data$xray_result_1)=c("Condensation","Other infiltrates","Normal")
levels(data$x_ray_1_reported_by___1)=c("Unchecked","Checked")
levels(data$x_ray_1_reported_by___2)=c("Unchecked","Checked")
levels(data$x_ray_1_reported_by___3)=c("Unchecked","Checked")
levels(data$x_ray_1_result_ext)=c("Condensation","Other infiltrates","Normal")
levels(data$xray_result_2)=c("Condensation","Other infiltrates","Normal")
levels(data$x_ray_2_reported_by___1)=c("Unchecked","Checked")
levels(data$x_ray_2_reported_by___2)=c("Unchecked","Checked")
levels(data$x_ray_2_reported_by___3)=c("Unchecked","Checked")
levels(data$x_ray_2_result_ext)=c("Condensation","Other infiltrates","Normal")
levels(data$xray_result_3)=c("Condensation","Other infiltrates","Normal")
levels(data$x_ray_3_reported_by___1)=c("Unchecked","Checked")
levels(data$x_ray_3_reported_by___2)=c("Unchecked","Checked")
levels(data$x_ray_3_reported_by___3)=c("Unchecked","Checked")
levels(data$x_ray_3_result_ext)=c("Condensation","Other infiltrates","Normal")
levels(data$cardiac_img)=c("Yes","No","Unknown")
levels(data$radiology_complete)=c("Incomplete","Unverified","Complete")
levels(data$oro_fluids)=c("Yes","No","Unknown")
levels(data$iv_fluids)=c("Yes","No","Unknown")
levels(data$antiviral_cmyn)=c("Yes","No","Unknown")
levels(data$antiviral_cmtrt___1)=c("Unchecked","Checked")
levels(data$antiviral_cmtrt___2)=c("Unchecked","Checked")
levels(data$antiviral_cmtrt___3)=c("Unchecked","Checked")
levels(data$antiviral_cmtrt___4)=c("Unchecked","Checked")
levels(data$antiviral_cmtrt___5)=c("Unchecked","Checked")
levels(data$antiviral_cmtrt___6)=c("Unchecked","Checked")
levels(data$antiviral_cmtrt___7)=c("Unchecked","Checked")
levels(data$antiviral_cmtrt___8)=c("Unchecked","Checked")
levels(data$antiviral_cmtrt___10)=c("Unchecked","Checked")
levels(data$antiviral_cmtrt___11)=c("Unchecked","Checked")
levels(data$antiviral_cmtrt___9)=c("Unchecked","Checked")
levels(data$antibiotic_cmyn)=c("Yes","No","Unknown")
levels(data$iv_ab)=c("Yes","No","Data unavailable")
levels(data$type_iv_ab___0)=c("Unchecked","Checked")
levels(data$type_iv_ab___1)=c("Unchecked","Checked")
levels(data$type_iv_ab___2)=c("Unchecked","Checked")
levels(data$type_iv_ab___3)=c("Unchecked","Checked")
levels(data$type_iv_ab___4)=c("Unchecked","Checked")
levels(data$type_iv_ab___5)=c("Unchecked","Checked")
levels(data$type_iv_ab___6)=c("Unchecked","Checked")
levels(data$type_iv_ab___9)=c("Unchecked","Checked")
levels(data$type_iv_ab___8)=c("Unchecked","Checked")
levels(data$type_iv_ab___7)=c("Unchecked","Checked")
levels(data$o_ab)=c("Yes","No")
levels(data$type_o_ab___1)=c("Unchecked","Checked")
levels(data$type_o_ab___2)=c("Unchecked","Checked")
levels(data$type_o_ab___3)=c("Unchecked","Checked")
levels(data$type_o_ab___4)=c("Unchecked","Checked")
levels(data$type_o_ab___5)=c("Unchecked","Checked")
levels(data$type_o_ab___6)=c("Unchecked","Checked")
levels(data$type_o_ab___7)=c("Unchecked","Checked")
levels(data$type_o_ab___8)=c("Unchecked","Checked")
levels(data$corticost_cmyn)=c("Yes","No","Unknown")
levels(data$corticost_cmroute)=c("Oral","Intravenous","Inhaled")
levels(data$anti_inflammatory___1)=c("Unchecked","Checked")
levels(data$anti_inflammatory___2)=c("Unchecked","Checked")
levels(data$other_anti_inflammatory)=c("Yes","No","Unknown")
levels(data$iv_imglob)=c("Yes","No","Unknown")
levels(data$imod)=c("Yes","No","Unknown")
levels(data$sys_anticoag)=c("Yes","No","Unknown")
levels(data$anti_mal)=c("Yes","No","Unknown")
levels(data$antifung_cmyn)=c("Yes","No","Unknown")
levels(data$experimental)=c("Yes","No","Unknown")
levels(data$other_sc2_treatments)=c("Yes","No","Unknown")
levels(data$vasopressors)=c("Yes","No","Unknown")
levels(data$vasopressors_type___1)=c("Unchecked","Checked")
levels(data$vasopressors_type___2)=c("Unchecked","Checked")
levels(data$vasopressors_type___3)=c("Unchecked","Checked")
levels(data$vasopressors_type___4)=c("Unchecked","Checked")
levels(data$vasopressors_type___5)=c("Unchecked","Checked")
levels(data$vasopressors_type___6)=c("Unchecked","Checked")
levels(data$ecmo)=c("Yes","No","Unknown")
levels(data$blood_trans)=c("Yes","No","Unknown")
levels(data$medication_complete)=c("Incomplete","Unverified","Complete")
levels(data$dsterm)=c("Discharged alive (not hospitalized)","Discharged alive after hospitalization","Transfer to other facility","Death","Palliative discharge","Currently hospitalized","Unknown","Excluded")
levels(data$excluded_reason)=c("Not fulfilled inclusion criteria","Withdrawal of consent","Other")
levels(data$final_dx_1)=c("Abdominal pain","Abscess","Acquired Hemolytic Anaemia","Adenitis","Appendicitis / Peritonitis","Asthma flare","Asymptomatic","Asymptomatic infection","Bacteriemia","Bronchiolitis","Bronchitis","Dehydration","Diabetic debut","Fever without a source","Flu-like syndrome","Gastroenteritis","Gastroenteritis (other)","Hematemesis","Hepatitis","HFH","Hypertension","Inflammatory syndrome","Jaundice","Kawasaki","Kidney stone","Liver failure","Mastoiditis","Neoplasm","Nephritis syndrome","Pneumonia","Psychiatric admission","Pyelonephritis","Salmonellosis","Seizures","Sepsis","Skin/mucosae problems","Social admission","Thrombosis","Trauma","URTI","UTI","Whooping cough","Immunosuppression","Chronic disease","Admission for observation due to unspecified risk","Neonate","Shock")
levels(data$related_1)=c("Related","Unclear")
levels(data$final_dx_2)=c("Abdominal pain","Abscess","Acquired Hemolytic Anaemia","Adenitis","Appendicitis / Peritonitis","Asthma flare","Asymptomatic","Asymptomatic infection","Bacteriemia","Bronchiolitis","Bronchitis","Dehydration","Diabetic debut","Fever without a source","Flu-like syndrome","Gastroenteritis","Gastroenteritis (other)","Hematemesis","Hepatitis","HFH","Hypertension","Inflammatory syndrome","Jaundice","Kawasaki","Kidney stone","Liver failure","Mastoiditis","Neoplasm","Nephritis syndrome","Pneumonia","Psychiatric admission","Pyelonephritis","Salmonellosis","Seizures","Sepsis","Skin/mucosae problems","Social admission","Thrombosis","Trauma","URTI","UTI","Whooping cough","Immunosuppression","Chronic disease","Admission for observation due to unspecified risk","Neonate","Shock")
levels(data$related_2)=c("Related","Not Related","Unclear")
levels(data$final_dx_3)=c("Abdominal pain","Abscess","Acquired Hemolytic Anaemia","Adenitis","Appendicitis / Peritonitis","Asthma flare","Asymptomatic","Asymptomatic infection","Bacteriemia","Bronchiolitis","Bronchitis","Dehydration","Diabetic debut","Fever without a source","Flu-like syndrome","Gastroenteritis","Gastroenteritis (other)","Hematemesis","Hepatitis","HFH","Hypertension","Inflammatory syndrome","Jaundice","Kawasaki","Kidney stone","Liver failure","Mastoiditis","Neoplasm","Nephritis syndrome","Pneumonia","Psychiatric admission","Pyelonephritis","Salmonellosis","Seizures","Sepsis","Skin/mucosae problems","Social admission","Thrombosis","Trauma","URTI","UTI","Whooping cough","Immunosuppression","Chronic disease","Admission for observation due to unspecified risk","Neonate","Shock")
levels(data$final_dx_4)=c("Abdominal pain","Abscess","Acquired Hemolytic Anaemia","Adenitis","Appendicitis / Peritonitis","Asthma flare","Asymptomatic","Asymptomatic infection","Bacteriemia","Bronchiolitis","Bronchitis","Dehydration","Diabetic debut","Fever without a source","Flu-like syndrome","Gastroenteritis","Gastroenteritis (other)","Hematemesis","Hepatitis","HFH","Hypertension","Inflammatory syndrome","Jaundice","Kawasaki","Kidney stone","Liver failure","Mastoiditis","Neoplasm","Nephritis syndrome","Pneumonia","Psychiatric admission","Pyelonephritis","Salmonellosis","Seizures","Sepsis","Skin/mucosae problems","Social admission","Thrombosis","Trauma","URTI","UTI","Whooping cough","Immunosuppression","Chronic disease","Admission for observation due to unspecified risk","Neonate","Shock")
levels(data$final_dx_5)=c("Abdominal pain","Abscess","Acquired Hemolytic Anaemia","Adenitis","Appendicitis / Peritonitis","Asthma flare","Asymptomatic","Asymptomatic infection","Bacteriemia","Bronchiolitis","Bronchitis","Dehydration","Diabetic debut","Fever without a source","Flu-like syndrome","Gastroenteritis","Gastroenteritis (other)","Hematemesis","Hepatitis","HFH","Hypertension","Inflammatory syndrome","Jaundice","Kawasaki","Kidney stone","Liver failure","Mastoiditis","Neoplasm","Nephritis syndrome","Pneumonia","Psychiatric admission","Pyelonephritis","Salmonellosis","Seizures","Sepsis","Skin/mucosae problems","Social admission","Thrombosis","Trauma","URTI","UTI","Whooping cough","Immunosuppression","Chronic disease","Admission for observation due to unspecified risk","Neonate","Shock")
levels(data$final_dx_6)=c("Abdominal pain","Abscess","Acquired Hemolytic Anaemia","Adenitis","Appendicitis / Peritonitis","Asthma flare","Asymptomatic","Asymptomatic infection","Bacteriemia","Bronchiolitis","Bronchitis","Dehydration","Diabetic debut","Fever without a source","Flu-like syndrome","Gastroenteritis","Gastroenteritis (other)","Hematemesis","Hepatitis","HFH","Hypertension","Inflammatory syndrome","Jaundice","Kawasaki","Kidney stone","Liver failure","Mastoiditis","Neoplasm","Nephritis syndrome","Pneumonia","Psychiatric admission","Pyelonephritis","Salmonellosis","Seizures","Sepsis","Skin/mucosae problems","Social admission","Thrombosis","Trauma","URTI","UTI","Whooping cough","Immunosuppression","Chronic disease","Admission for observation due to unspecified risk","Neonate","Shock")
levels(data$related_3)=c("Related","Not Related","Unclear")
levels(data$outcome_complete)=c("Incomplete","Unverified","Complete")
levels(data$spirometry_complete)=c("Incomplete","Unverified","Complete")
levels(data$fever_ws)=c("Yes","No")
levels(data$lumbar_punc)=c("Yes","No")
levels(data$csf_bacteria)=c("Positive","Negative")
levels(data$csf_vhs)=c("Positive","Negative","Not performed")
levels(data$csf_enterovirus)=c("Positive","Negative","Not performed")
levels(data$csf_sarscov2)=c("Positive","Negative","Not performed")
levels(data$urine_collection)=c("Catheter","Tapping / spontaneus","Perineal bag","Puncture","Urine collection not performed")
levels(data$urine_dipstik)=c("Leucocytes","Nitrites","Leucocytes + Nitrites","Normal","Not performed")
levels(data$urine_culture)=c("Positive","Negative","Not performed")
levels(data$fever_without_source_study_complete)=c("Incomplete","Unverified","Complete")
levels(data$qc_stage)=c("Not started","Clinical quality control performed, waiting for answers to clinical queries","Clinical queries resolved, pending data quality control","Data quality control performed, waiting for answers to data queries","Quality control completed")
levels(data$first_analysis)=c("Yes","No")
levels(data$quality_control_complete)=c("Incomplete","Unverified","Complete")
#-------------------------------------------------------------------------------


#Hospitales con pacientes hospitalizados:
#-------------------------------------------------------------------------------
data %<>% filter(dsterm!="Excluded")
neonatos <- c("30656-16","30654-2","30653-51","30653-40","30422-158","30426-2","30426-26","30426-27","30426-38","30426-76","30426-82","30426-97","30653-11","30653-14",
              "30653-17","30653-24","30653-27","30653-28","30653-41","30653-64","30653-65","30655-1","30655-5","30655-8","30655-16")

data=data[!data$record %in% neonatos,]
which(is.na(data$age_months))
data<-data[-125,]#No tiene info age

#hospitales <- data %>% group_by(hospital) %>% summarise(n=n())
#write_xlsx(hospitales,"C:/Users/gfrie/OneDrive/Documents/EPICO/Resultados/Dataframes/HospitalesHospitalizados_Colombia.xlsx")

#-------------------------------------------------------------------------------



####BUBBLE MAP COLOMBIA Y ESPAÑA
#-------------------------------------------------------------------------------

#Datos
Datos_Espania <- read_excel("EPICO/Resultados/Dataframes/HospitalesHospitalizados_Espania.xlsx")
Datos_Espania_n <- Datos_Espania %>% group_by(Ciudad, Latitud, Longitud) %>% summarise(n = sum(n))
Datos_Colombia <- read_excel("EPICO/Resultados/Dataframes/HospitalesHospitalizados_Colombia.xlsx")
Datos_Colombia_n <- Datos_Colombia %>% group_by(Ciudad, Latitud, Longitud) %>% summarise(n = sum(n))
str(Datos_Espania_n)
#Spatial Polygons
Espania <- map_data("world") %>% filter(region=="Spain")
Colombia <- map_data("world") %>% filter(region=="Colombia")

#Graficos
ggplot() +
  geom_polygon(data = Espania, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(data=Datos_Espania_n, aes(x=Longitud, y=Latitud, size=n, color=n))+
  geom_text_repel(data=Datos_Espania_n, aes(x=Longitud, y=Latitud, label=Ciudad), size=3)+
  theme_void()+
  scale_color_viridis(trans="log10") +
  theme(legend.position="none") + coord_map() 

#Graficos
p <- ggplot() +
  geom_polygon(data = Colombia, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(data=Datos_Colombia_n, aes(x=Longitud, y=Latitud, size=n, color=n))+
  geom_text_repel(data=Datos_Colombia_n, aes(x=Longitud, y=Latitud, label=Ciudad), size=3)+
  theme_void()+
  scale_color_viridis(trans="log10") +
  theme(legend.position="none") +
  coord_map()
p
p + scale_bar(lon = -130, lat = 26, 
              distance_lon = 500, distance_lat = 100, distance_legend = 200, 
              dist_unit = "km", orientation = FALSE)

#-------------------------------------------------------------------------------




