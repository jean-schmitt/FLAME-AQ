#' survival_rate_f
#' Function: Gives the probability of a vehicle of a given age to survive knowing that it survives until the previous age.
#' @export
#scrap_rate_mdl: 1=empirical data from VISION ; 2=empirical data from TEDB ; 3=Logistic curve from Bandivadekar ; 4=Weibull distribution from Garcia et al.
survival_rate_f<-function(year,size,techno,survival_rate_mdl=NA,survival_rate_adj_age=NA,survival_rate_transition_period=NA,survival_rate_techno_specific=NA,cumulative_rate="n"){
  attribute_f(fun_name="survival_rate_f")
  last_age_tbc <- 30
  conv_techno <- c("ICEV-G","ICEV-D","FFV","HEV","CNG")
  #Create output matrix
  mat_sr <- matrix(NA,nrow=1,ncol=last_age_tbc,dimnames=list("Survival rates",1:last_age_tbc))
  if (survival_rate_mdl%in%c(1,2,3,6) | year<2021){
    #Inputs
    survival_rates  <- get_input_f(input_name = 'survival_rates')
    scrap_rate_src <- ifelse(year<2021,"own",switch(as.character(survival_rate_mdl),"1"="own","2"="VISION","3"="TEDB","6"="melissa"))
    #Extract survival rates data associated with source and create a matrix
    tmp_mat <- acast(data=subset(survival_rates,subset=Source==scrap_rate_src & Size%in%c(size,"all") & Age<=last_age_tbc & Age>0), Data ~ Age , value.var='Value',fun.aggregate=sum, margins=FALSE)
    #Fill the final matrix with the data
    mat_sr[1,colnames(tmp_mat)] <- tmp_mat[1,]
    #If survival rates stop before last_age_tbc, then assumes constant.
    if (any(is.na(mat_sr))){
      na_cols <- colnames(mat_sr)[is.na(mat_sr[1,])]
      mat_sr[1,na_cols] <- mat_sr[,as.character(min(as.numeric(na_cols))-1)]
    }
    #If cumulative_rate is specified, calculate the cumulative product of the matrix and update the matrix
    if (cumulative_rate=="y"){
      mat_sr[1,] <- cumprod(mat_sr[1,])
    }
    }else if(survival_rate_mdl==4){
      #Parameters definition for Bandivadekar model
      lifetime <- switch(size,"Car"=16.9,"Light truck"=15.5)
      beta <- switch(size,"Car"=0.28,"Light truck"=0.22)
      if(cumulative_rate=="y"){
        mat_sr[1,] <- sapply(1:last_age_tbc,function(age)1-1/(1+exp(-beta*(age-lifetime))))
      } else {
        mat_sr[1,] <- sapply(1:last_age_tbc,function(age)(1-1/(1+exp(-beta*(age-lifetime))))/(1-1/(1+exp(-beta*(age-1-lifetime)))))
      }
    }else if(survival_rate_mdl==5){
      #Weibull distribution from Garcia et al.
      lambda <- switch(size,"Car"=11,"Light truck"=11)
      mu <- switch(size,"Car"=35,"Light truck"=35)
      if(cumulative_rate=="y"){
        mat_sr[1,] <- sapply(1:last_age_tbc,function(age)exp(-((age+lambda)/mu)^lambda))
      } else {
        mat_sr[1,] <- sapply(1:last_age_tbc,function(age)exp(-((age+lambda)/mu)^lambda)/(exp(-((age-1+lambda)/mu)^lambda)))
      }
    }
  if(year>2020 & survival_rate_adj_age!=0 & (survival_rate_techno_specific=="n" | survival_rate_techno_specific=="y" & techno%in%conv_techno)){
    if(year<=2020+survival_rate_transition_period){
      age_limit <- last_age_tbc - (last_age_tbc - survival_rate_adj_age)/(survival_rate_transition_period+1)*(year-2020)
    } else {
      age_limit <- survival_rate_adj_age
    }
    mat_sr[1,] <- sapply(1:last_age_tbc,function(age)ifelse(age<=age_limit,mat_sr[1,as.character(age)],0))
  }
  return(mat_sr)
}


