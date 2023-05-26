#' fleet_adj_vkt_f
#' Function: Calculates distance traveled by the U.S. LDV fleet by technology, size, age and year in kilometers adjusted to turnover.
#' @export
fleet_adj_vkt_f <- function(vkt_turnover_adj_mdl = NA, survival_rate_adj_age = NA){
  attribute_f("fleet_adj_vkt_f")
  fleet_vkt_f_res <- do.call(fun_res_f,list(fun_name="fleet_vkt_f"))
  fleet_vint_vkt <- fleet_vkt_f_res[["fleet_vint_vkt"]]
  if (vkt_turnover_adj_mdl=="y" & survival_rate_adj_age!=0){
    #Get total fleet VKT from current simulations
    tot_vkt <- aggregate(formula = Value~Year,data = fleet_vint_vkt,FUN=sum)
    #Simulate fleet with no turnover
    #Update attribute value with no turnover adjustments
    update_attribute_values(list(survival_rate_adj_age=0))
    new_fleet_vkt_f_res <- do.call(fleet_vkt_f,list(use_res_env="n"))
    new_fleet_vint_vkt <- new_fleet_vkt_f_res[["fleet_vint_vkt"]]
    new_tot_vkt <- aggregate(formula = Value~Year,data = new_fleet_vint_vkt,FUN=sum)
    #Calculate adjustment ratio 
    tot_vkt_adj_ratio <- tot_vkt
    tot_vkt_adj_ratio$Value <- sapply(1:nrow(tot_vkt_adj_ratio),function(x)subset(new_tot_vkt,Year==tot_vkt_adj_ratio[x,"Year"])$Value/subset(tot_vkt,Year==tot_vkt_adj_ratio[x,"Year"])$Value)
    #Update attribute value with no turnover adjustments
    update_attribute_values(list(survival_rate_adj_age=survival_rate_adj_age))
    #Update VKT results
    new_fleet_vint_vkt <- fleet_vkt_f_res[["fleet_vint_vkt"]]
    new_fleet_vint_vkt$Value <- sapply(1:nrow(new_fleet_vint_vkt),function(x)fleet_vkt_f_res[["fleet_vint_vkt"]][x,"Value"]*subset(tot_vkt_adj_ratio,Year==new_fleet_vint_vkt[x,"Year"])$Value)
    new_fleet_vkt <- fleet_vkt_f_res[["fleet_vkt"]]
    new_fleet_vkt$Value <- sapply(1:nrow(new_fleet_vkt),function(x)fleet_vkt_f_res[["fleet_vkt"]][x,"Value"]*subset(tot_vkt_adj_ratio,Year==new_fleet_vkt[x,"Year"])$Value)
    new_fleet_vkt_new <- fleet_vkt_f_res[["fleet_vkt_new"]]
    new_fleet_vkt_new$Value <- sapply(1:nrow(new_fleet_vkt_new),function(x)fleet_vkt_f_res[["fleet_vkt_new"]][x,"Value"]*subset(tot_vkt_adj_ratio,Year==new_fleet_vkt_new[x,"Year"])$Value)
    return(list(fleet_vint_vkt=new_fleet_vint_vkt,
                fleet_vkt=new_fleet_vkt,
                fleet_vkt_new=new_fleet_vkt_new))
  } else {
    return(fleet_vkt_f_res)
  }
}
