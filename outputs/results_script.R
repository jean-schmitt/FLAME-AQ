source("model_setup.R")
outputs_path <- "outputs"
# Scenarios -----------------------------------
# Main scenarios - Including different grid mixes
## Using COBRA - Standard scenarios
#do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_no_elec"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "NoEVs_intermediate"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_CA_intermediate"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_low_case"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_high_decarb"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_mid_case_delta"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "no_ldvs"))
#do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "AEO_2021_intermediate"))

#### Scenarios for comparison with Minet et al. and Gai et al.
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_20"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_high_decarb_20"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_25"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_high_decarb_25"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_50"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_high_decarb_50"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_100"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_high_decarb_100"))

#### Scenarios for comparison with Gai et al.
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NG_benef_ev_2023"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_RE_benef_ev_2023"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NG_no_icevs"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_RE_no_icevs"))


#### Comparison with Tong&Azevedo - Run with 2014 emission factors
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_inter_benef_ev_2014_mix"))
#do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "NoEVs_inter_benef_ev_2014_mix"))

#### Additional No LDVs scenarios with other grid mixes
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "no_ldvs_current_mix"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "no_ldvs_low_case"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "no_ldvs_high_decarb"))


## Alternative fleet compositions
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "AEO_reference_2023"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "AEO_no_IRA_intermediate"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "AEO_low_IRA_intermediate"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "AEO_high_IRA_intermediate"))

## Alternative electric mixes
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_EFS_Ref"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_EFS_High"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_AEO_2023_no_ira"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_AEO_2022_reference"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_AEO_2021_reference"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_AEO_2020_reference"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_AEO_2019_reference"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_AEO_low"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_AEO_high"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_AEO_mid_case"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_NREL"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_low_case_NREL"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_high_decarb_NREL"))

# Sensitivity analysis for brake and tire wear emission factors
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_brake_low"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_brake_high"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_tire_low"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_tire_high"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_overall_low"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_overall_high"))

# Calculation of the benefits per EV
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_new_only"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix_new_only"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "NoEVs_intermediate_new_only"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_low_case_new_only"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_high_decarb_new_only"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_AEO_mid_case_new_only"))

do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_old_only"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix_old_only"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "NoEVs_intermediate_old_only"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_low_case_old_only"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_high_decarb_old_only"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_AEO_mid_case_old_only"))

# Delays in ZEVs
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_delay1"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_delay2"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_delay5"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_delay10"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_delay15"))

# Delays in clean electricity
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_elec_delay1"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_elec_delay2"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_elec_delay5"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_elec_delay10"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_elec_delay15"))

## Sensitivity analysis on the background emissions
    ### No evolution of background emissions
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_bkg_none"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix_bkg_none"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "NoEVs_intermediate_bkg_none"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_CA_intermediate_bkg_none"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_low_case_bkg_none"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_high_decarb_bkg_none"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_mid_case_delta_bkg_none"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_AEO_mid_case_bkg_none"))
    ### Plateau after 2028
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_bkg_plateau"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix_bkg_plateau"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "NoEVs_intermediate_bkg_plateau"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_CA_intermediate_bkg_plateau"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_low_case_bkg_plateau"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_high_decarb_bkg_plateau"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_mid_case_delta_bkg_plateau"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_AEO_mid_case_bkg_plateau"))


## Sensitivity analysis on power plants emissions factors
### Converge of the emission factors toward the best in class in 10 years
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_efs_conv10"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix_efs_conv10"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "NoEVs_intermediate_efs_conv10"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_CA_intermediate_efs_conv10"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_low_case_efs_conv10"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_high_decarb_efs_conv10"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_mid_case_delta_efs_conv10"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_AEO_mid_case_efs_conv10"))
### Real evolution between 2017 and 2021
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_efs_decline"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix_efs_decline"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "NoEVs_intermediate_efs_decline"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_CA_intermediate_efs_decline"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_low_case_efs_decline"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_high_decarb_efs_decline"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_mid_case_delta_efs_decline"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_AEO_mid_case_efs_decline"))

## Sensitivity analysis on the stack height allocation
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_stack_class2"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix_stack_class2"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_stack_class3"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix_stack_class3"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_stack_average"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix_stack_average"))

## Sensitivity using AVERT emission factors
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_AVERT"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix_AVERT"))

## Sensitivity using alternative grid mixes
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_NREL"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_CA_intermediate_NREL"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_low_case_NREL"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_high_decarb_NREL"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_AEO_low"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_AEO_high"))

##### Calculation of the benefits per EV - EFS Retirement Constraints Scenarios
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_efs_benef_ev_2022"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_efs_benef_ev_2025"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_efs_benef_ev_2030"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_efs_benef_ev_2035"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_efs_benef_ev_2040"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_efs_benef_ev_2045"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_efs_benef_ev_2050"))

## Using EASIUR
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "NoEVs_NREL_intermediate_EASIUR"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_EASIUR"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix_EASIUR"))

## Using INMAP
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "NoEVs_NREL_intermediate_INMAP"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_INMAP"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix_INMAP"))

# Scenarios with different implementation timelines for fleet electrification policies
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_P1_NREL_intermediate"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_P2_NREL_intermediate"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_P5_NREL_intermediate"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_P10_NREL_intermediate"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_P15_NREL_intermediate"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate"))

# Scenarios with different implementation timelines for cleaner electric grid in the mid-case scenario
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_mid_case_shift1"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_mid_case_shift2"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_mid_case_shift5"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_mid_case_shift10"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_mid_case_shift15"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate"))

# Scenarios with different implementation timelines for cleaner electric grid in the high renewables scenario
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_high_renewables_shift1"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_high_renewables_shift2"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_high_renewables_shift5"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_high_renewables_shift10"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_high_renewables_shift15"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_high_decarb"))

# Sensitivity analysis for brake and tire wear emission factors
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_brake_low"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_brake_high"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_tire_low"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_tire_high"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_overall_low"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_overall_high"))

# Sensitivity analysis for battery production emissions
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_batt_tessum2014_low"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_batt_tessum2014_high"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_batt_greet_total"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_intermediate_batt_greet_assembly"))

# Scenario to compare with Tong&Azevedo paper
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix_car_bev_100"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix_truck_bev_100"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix_car_icev_100"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix_truck_icev_100"))
