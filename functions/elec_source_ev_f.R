#' elec_source_ev_f
#' Function: Returns electricity mixes by source for the electricity used in EVs.
#' @importFrom reshape2 acast
#' @export
elec_source_ev_f <- function(aeo_scen=NA,elec_mix_adj_mdl=NA,elec_mix_adj_coef=NA,elec_mix_mdl=NA,first_yr=NA,last_yr=NA, electric_grid_data_source=NA){
  attribute_f("elec_source_ev_f")
  #Calculate national electricity mixes by source
  if (grepl("CAMBIUM", elec_mix_mdl)|grepl("NREL", elec_mix_mdl)|(elec_mix_mdl == "current_mix") | grepl("aeo",tolower(elec_mix_mdl)) | grepl("efs", tolower(elec_mix_mdl))) {
    us_elec_mix_hist  <- get_input_f(input_name = 'us_elec_mix_hist')
    us_elec_mix_proj  <- get_input_f(input_name = 'us_elec_mix_proj')
    last_hist_yr <- max(us_elec_mix_hist$Year)
    region_tbc <- "United States"#switch(strsplit(elec_mix_mdl,"_")[[1]][2],'us'="United States",strsplit(elec_mix_mdl,"_")[[1]][2])
    #Create matrix
    cambium_scenario <- do.call(cambium_el_grid_scenario_f, list(output = "electricity", elec_mix_mdl = elec_mix_mdl))[["grid_mix_national"]]
    ## Add the source "Other"
    if ("Oil"%in%unique(cambium_scenario$Source)) {
      cambium_scenario$Source[which(cambium_scenario$Source == "Oil")] <- "Other"
    }
    if (!"Biomass"%in%unique(cambium_scenario$Source)) {
      temp <- cambium_scenario[which(cambium_scenario$Source == "Coal"),]
      temp$Source <- "Biomass"
      temp$Fraction <- 0
      cambium_scenario <- rbind(cambium_scenario, temp)
    }
    matrix_mix_cambium <- acast(data=cambium_scenario, Source ~ Year , value.var='Fraction',fun.aggregate=sum, margins=FALSE)
    matrix_mix_hist <- acast(data=subset(us_elec_mix_hist, Year>=first_yr & Region=="United States"), Source ~ Year , value.var='Mix',fun.aggregate=sum, margins=FALSE)
    matrix_mix_proj <- acast(data=subset(us_elec_mix_proj,Aeo_case==aeo_scen & Year > last_hist_yr & Year < min(cambium_scenario$Year) & Region==region_tbc), Source ~ Year , value.var='Mix',fun.aggregate=sum, margins=FALSE)
    # Add missing energy sources that are present in cambium
    differences <- setdiff(rownames(matrix_mix_cambium), rownames(matrix_mix_hist)) 
    for (i in differences) {
      matrix_mix_hist <- rbind(matrix_mix_hist, matrix(data = 0, nrow = 1, ncol = dim(matrix_mix_hist)[2]))
      rownames(matrix_mix_hist)[dim(matrix_mix_hist)[1]] <- i
      matrix_mix_proj <- rbind(matrix_mix_proj, matrix(data = 0, nrow = 1, ncol = dim(matrix_mix_proj)[2]))
      rownames(matrix_mix_proj)[dim(matrix_mix_proj)[1]] <- i
    }
    matrix_mix <- cbind(matrix_mix_hist, matrix_mix_proj, matrix_mix_cambium[rownames(matrix_mix_hist),])
  } else if (grepl("aeo",elec_mix_mdl)){
    #Inputs
    us_elec_mix_hist  <- get_input_f(input_name = 'us_elec_mix_hist')
    us_elec_mix_proj  <- get_input_f(input_name = 'us_elec_mix_proj')
    last_hist_yr <- max(us_elec_mix_hist$Year)
    region_tbc <- switch(strsplit(elec_mix_mdl,"_")[[1]][2],'us'="United States",strsplit(elec_mix_mdl,"_")[[1]][2])
    #Create matrix
    matrix_mix_hist <- acast(data=subset(us_elec_mix_hist, Year>=first_yr & Region=="United States"), Source ~ Year , value.var='Mix',fun.aggregate=sum, margins=FALSE)
    matrix_mix_proj <- acast(data=subset(us_elec_mix_proj,Aeo_case==aeo_scen & Year > last_hist_yr & Region==region_tbc), Source ~ Year , value.var='Mix',fun.aggregate=sum, margins=FALSE)
    matrix_mix <- cbind(matrix_mix_hist,matrix_mix_proj[rownames(matrix_mix_hist),])
  #Calculate weighted electricity mixes by regional EV stock and use. DEPRECIATED
  } else if (elec_mix_mdl=="reg_ev"){
    last_hist_yr <- 2018
    matrix_mix <- do.call(fun_res_f,list(fun_name="elec_source_reg_ev_f"))[[1]]
  }
  if (elec_mix_adj_mdl=="linear"){
    adj_matrix_mix <- matrix_mix
    source_name <- "Renewable"
    # lin_reg = lm(formula = Value ~ Year, data = data.frame(Year=as.numeric(colnames(matrix_mix)),Value=colSums(matrix_mix[source_name,,drop=FALSE])))
    # lin_reg$coefficients["Year"]
    ann_grw_rt <- elec_mix_adj_coef
    #Adjust the elec mixes: linear absolute rate
    adj_matrix_mix[source_name,as.character(last_hist_yr:last_yr)] <- (matrix_mix[source_name,as.character(last_hist_yr:last_yr),drop=FALSE] %*% 
                                                                         diag(x=1/colSums(matrix_mix[source_name,as.character(last_hist_yr:last_yr),drop=FALSE]),nrow=(last_yr-last_hist_yr)+1,ncol=(last_yr-last_hist_yr)+1)) %*%
      diag(x=sapply(sum(matrix_mix[source_name,as.character(last_hist_yr)])+ann_grw_rt*0:(last_yr-last_hist_yr),function(x)ifelse(x<=1,x,1)),nrow=(last_yr-last_hist_yr)+1,ncol=(last_yr-last_hist_yr)+1)
    #Adjust the other elec source
    #Assumption: Keep the same ratio
    adj_matrix_mix[!rownames(adj_matrix_mix)%in%source_name,as.character((last_hist_yr+1):last_yr)] <- (matrix_mix[!rownames(matrix_mix)%in%source_name,as.character((last_hist_yr+1):last_yr),drop=FALSE] %*%
                                                                                                          diag(x=1/colSums(matrix_mix[!rownames(matrix_mix)%in%source_name,as.character((last_hist_yr+1):last_yr),drop=FALSE]),nrow=last_yr-last_hist_yr,ncol=last_yr-last_hist_yr)) %*%
      diag(x=round(1-colSums(adj_matrix_mix[source_name,as.character((last_hist_yr+1):last_yr),drop=FALSE]),digits=7),nrow=last_yr-last_hist_yr,ncol=last_yr-last_hist_yr)
    matrix_mix <- adj_matrix_mix
  } else if (elec_mix_adj_mdl=="constant"){
    #Constant electricity mixes from last historical data onward
    matrix_mix[,as.character((last_hist_yr+1):last_yr)] <- sapply((last_hist_yr+1):last_yr,function(year)matrix_mix[,as.character(last_hist_yr)])
  } else if (elec_mix_adj_mdl=="coal"){
    matrix_mix["Coal",] <- 1
    matrix_mix[rownames(matrix_mix)!="Coal",] <- 0
  } else if (elec_mix_adj_mdl=="gas"){
    matrix_mix["Natural Gas",] <- 1
    matrix_mix[rownames(matrix_mix)!="Natural Gas",] <- 0
  } else if (elec_mix_adj_mdl=="renewable"){
    matrix_mix["Renewable",] <- 1
    matrix_mix[rownames(matrix_mix)!="Renewable",] <- 0
  }
  return(list(matrix_mix=matrix_mix))
}
