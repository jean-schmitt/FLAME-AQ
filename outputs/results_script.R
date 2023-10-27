source("model_setup.R")
outputs_path <- "outputs"
# Scenarios -----------------------------------

# Scenarios for presentation - Full calculation
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "NoCars")) # Updated 01.09

do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "NoEVs_NREL_intermediate"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Default_NREL_intermediate"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_CA_NREL_intermediate"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_low_case"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_high_decarb"))

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
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_tessum2014_low"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_tessum2014_high"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_greet_total"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_greet_total_decrase"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_greet_assembly"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_greet_assembly_decrease"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate"))

#Breakdown and aggregated results in 2030 and 2050
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "NoEVs_NREL_intermediate_aggregated_2030"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "NoEVs_NREL_intermediate_aggregated_2050"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "NoEVs_NREL_intermediate_breakdown_2030"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_breakdown_2030"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "NoEVs_NREL_intermediate_breakdown_2050"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_breakdown_2050"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "EV100_NREL_Intermediate_breakdown_2030"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "EV100_NREL_Intermediate_breakdown_2050"))

# Scenarios for presentation - Aggregated
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "NoEVs_NREL_intermediate_aggregated"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Default_NREL_intermediate_aggregated"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_CA_NREL_intermediate_aggregated"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_aggregated"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix_aggregated"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_low_case_aggregated"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_high_decarb_aggregated"))

# Scenarios for presentation - Pollutants breakdown
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "NoEVs_NREL_intermediate_breakdown"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Default_NREL_intermediate_breakdown"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_CA_NREL_intermediate_breakdown"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_breakdown"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix_breakdown"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_low_case_breakdown"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_high_decarb_breakdown"))


# Scenario to compare with Tong&Azevedo paper
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Elec100_Car100_mid_case_aggregated"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Elec100_Truck100_mid_case_aggregated"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Elec0_Car100_mid_case_aggregated"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Elec0_Truck100_mid_case_aggregated"))

do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Elec100_Car100_current_mix_aggregated"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Elec0_Car100_current_mix_aggregated"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Elec100_Truck100_current_mix_aggregated"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Elec0_Truck100_current_mix_aggregated"))

do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Elec100_Car100_2014_mix_aggregated"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Elec0_Car100_2014_mix_aggregated"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Elec100_Truck100_2014_mix_aggregated"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Elec0_Truck100_2014_mix_aggregated"))

# Pollutants breakdowns
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_breakdown"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Default_NREL_intermediate_breakdown"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix_breakdown"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Default_current_mix_breakdown"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_high_decarb_breakdown"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_low_case_breakdown"))
# Aggregated
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_aggregated"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Default_NREL_intermediate_aggregated"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix_aggregated"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Default_current_mix_aggregated"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_high_decarb_aggregated"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_low_case_aggregated"))
# Full results
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_breakdown"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Default_NREL_intermediate_breakdown"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix_breakdown"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Default_current_mix_breakdown"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_high_decarb_breakdown"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_low_case_breakdown"))

# Validation - Compare with Azevedo paper
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Elec100_Car100_current_mix_aggregated"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Elec100_Truck100_current_mix_aggregated"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Elec0_Car100_current_mix_aggregated"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Elec0_Truck100_current_mix_aggregated"))

# Previous scenarios to run
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_break"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_default"))
#do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Default_NREL_intermediate"))
#do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Default_current_mix"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_high_decarb"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_low_case"))
#
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix_breakdown"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_2035decarb_breakdown"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Default_NREL_intermediate_breakdown"))
#do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Current_fleet"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_aggregated"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_intermediate_breakdown"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Default_NREL_intermediate"))
#do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_current_mix"))
#do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "Default_current_mix"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_CAMBIUM_high_demand"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_high_demand"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_2050_decarb"))
do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= "total_benefits_calculation_f",scen_tbc = "ZEV_all_NREL_2035_decarb"))





# Simulations -------------------------------------------------------------

do.call(write_simulation_f,list(outputs_path=outputs_path,function_tbc = ,scen_tbc = ,sim_tbc = ,sim_type = ))

# Sensitivity analysis ----------------------------------------------------

do.call(write_simulation_f,list(outputs_path=outputs_path,function_tbc = ,scen_tbc = ,sens_tbc = ,get_default_attr_val = ))
