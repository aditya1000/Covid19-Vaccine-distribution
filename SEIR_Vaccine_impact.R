library(dplyr)
library(ggplot2)
##setwd('C:/aditya/Covid19/covid-19-india-data-master/complete.csv')


percent_dis <- read.csv('C:/aditya/Covid19/Vaccine/percent_dis_t.csv')


df1 = read.csv('C:/aditya/Covid19/covid-19-india-data-master/complete.csv')
complete <- read.csv('C:/aditya/Covid19/covid-19-india-data-master/complete.csv')
complete$Latitude <- NULL
complete$Longitude <- NULL
colnames(complete)
colnames(df1) <- colnames(complete)

df1$Name.of.State...UT <- as.character(df1$Name.of.State...UT)
df1$Name.of.State...UT <- gsub("\\#", "", df1$Name.of.State...UT)
df1$Name.of.State...UT <- gsub("\\*", "", df1$Name.of.State...UT)

percent_dis$State <- gsub("\\s", "", percent_dis$State)
##df1 = read.csv('C:/aditya/Covid19/covid-19-india-data-master/complete.csv')
colnames(df1)
df1$Date <- as.Date(df1$Date, "%Y-%m-%d")

# for(State_nam in unique(df1$Name.of.State...UT)){
#   df1$Date[max(which(df1$Date == "2020-04-13" &  
#           df1$Name.of.State...UT == State_nam))] <- "2020-04-14"
#               
# }
# write.csv(df1, 'C:/aditya/Covid19/covid-19-india-data-master/state_level_corr.csv', 
#           row.names = F)
# write.csv(df1_for_Sum, 'C:/aditya/Covid19/covid-19-india-data-master/India_cases_corr.csv', 
#           row.names = F)
# 
library(dplyr) 
df1_for_Sum  <- df1 %>% dplyr::select(-c("Name.of.State...UT"))
df1_for_Sum <- df1_for_Sum %>% 
  group_by(Date) %>%
  summarise_all(sum)


df1_for_Sum$Name.of.State...UT <- "India"  
df1$Latitude <- NULL
df1$Longitude <- NULL
colnames(df1_for_Sum)
colnames(df1)

df1 <- rbind(df1_for_Sum, df1)
# all_contries_Conf  = read.csv('C:/aditya/Covid19/COVI
# all_contries_Conf  = read.csv('C:/aditya/Covid19/COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv')
# all_contries_Conf<- all_contries_Conf[all_contries_Conf$Country.Region =="India",]
# all_contries_Rec = read.csv('C:/aditya/Covid19/COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv')
# all_contries_Death = read.csv('C:/aditya/Covid19/COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv')


df1$Name.of.State...UT <-  gsub("Union Territory of Jammu and Kashmir", "Jammu and Kashmir", df1$Name.of.State...UT)
df1$Name.of.State...UT <-  gsub("Odisha", "Orissa", df1$Name.of.State...UT)
df1$Name.of.State...UT <-  gsub("Union Territory of Chandigarh", "Chandigarh", df1$Name.of.State...UT)
df1$Name.of.State...UT <-  gsub("Union Territory of Ladakh", "Ladakh", df1$Name.of.State...UT)

population_data = read.csv('C:/aditya/Covid19/covid-19-india-data-master/Population_data.csv')
population_data$Population <- as.numeric(gsub("\\,", "", population_data$Population))

sum(population_data$Population, na.rm =NA)
setdiff(unique(df1$Name.of.State...UT), population_data$State)

##scenario_list <- c(200,600,1500,3000,5000,7000)
avg_nat_list <- NULL

##for(i in scenario_list){
temp_R <- NULL
i = 20

temp_fit <- NULL
#int_gamma = 0.28
#int_beta = 0.27
# pdf(
# paste("C:/aditya/Covid19/covid-19-india-data-master/all_state_projections_cumulative_",i,
#         ".pdf"), width = 8 , height = 5)
pdf(paste("C:/aditya/Covid19/covid-19-india-data-master/seir_exp_results/SEIR_For_Vac",i,
        ".pdf"), width = 8 , height = 5)
#par(mfrow =c(1,2))
opt_recovery = F
Pred_time = 60
dose_q = 1000000
lagtime = 14
incubation_period = 5
E = incubation_period
delta = 0.20721
gamma = 1/20
average_days_recover = 20
for(model in colnames(percent_dis)[-1]){
for(State_nam in unique(percent_dis$State)){
p  <- percent_dis[which(percent_dis$State == State_nam) ,
                  which(colnames(percent_dis) == model)]

df <-  df1 %>% filter(Name.of.State...UT == State_nam)

df <-  df %>% filter(Total.Confirmed.cases >= i )
if(nrow(df) >= 3){
#df = df1_for_Sum
df$Date <- as.Date(df$Date, "%Y-%m-%d")

##df$Total_ConfirmedCases <- df$ Total.Confirmed.cases..Indian.National. + df$Total.Confirmed.cases...Foreign.National..


##df = df[-c(1:7),]
# df$Recover[17] = 44
Infected =  df$Total.Confirmed.cases - (df$Cured.Discharged.Migrated + df$Death)
Recovered = df$Cured.Discharged.Migrated 
Exposed  =  c(diff(Infected, E),rep(NA, E))
Exposed[which(Exposed < 0)] <- 0

Vac <- df$S
# Infected = log(Infected,base = 10) #### Natural Log 
# Infected = Infected[c(1:16)]
##Infected <- c(diff(df$Total.Confirmed.cases), NA)

N = population_data$Population[population_data$State == State_nam]  #### Population of India 


#int_gamma <- df$Cured.Discharged.Migrated[nrow(df)]/df$Total.Confirmed.cases[nrow(df)]
#int_beta <- Infected[nrow(df)]/df$Total.Confirmed.cases[nrow(df)]

# int_beta1 <- int_beta - 0.9*(int_beta) 
# int_beta2 <- int_beta + 0.9*(int_beta) 
# 
# int_gamma1 <- int_gamma - 0.9*(int_gamma) 
# int_gamma2 <- int_gamma + 0.9*(int_gamma) 

int_beta1 <- 0.001
int_beta2 <- 1
#gamma <- 1/average_days_recover
int_gamma1 <- 1/32
int_gamma2 <- 1/8
#delta <- (1/incubation_period) + (7.21/1000)
int_delta1 <- 1/15
int_delta2 <- 1/4

death_Rate <- median(df1$Death/df1$Total.Confirmed.cases, na.rm = T)
# R0_lo  <- int_beta1 / int_gamma1
# R0_up  <- int_beta2 / int_gamma2
#N = as.character(N)
#N = as.integer(N)
# N = log(33406061,base = 10)   ## NaturalLog 

Day=1:(length(Infected))

library(deSolve) # using the "ode" function
# Old<- par(mfrow=c(1,2))
# plot(Day, Infected, type ="b")
# plot(Day, Infected, log = "y")
# abline(lm(log10(Infected) ~ Day))
# title("Confirmed Cases 2019-nCoV India", outer = TRUE, line = -2)
#N = 1339200000

seir_model = function (current_timepoint, state_values, parameters)
{
  # create state variables (local variables)
  S = state_values [1]        # susceptibles
  E = state_values [2]        # exposed
  I = state_values [3]        # infectious
  R = state_values [4]        # recovered
  with (
    as.list (parameters),     # variable names within parameters can be used
    {
      # compute derivatives
      dS =  -(beta * S * I)/N
      dE = (beta * S * I)/N - (delta * E)
      dI = (delta * E) - (gamma * I)
      dR = (gamma * I)
      # combine results
      results = c (dS, dE, dI, dR)
      list (results)
    }
  )
}

######strt= time from where we are making prediction##########
strt <- length(Infected)
Vac <- (p*dose_q)/100 

Day <- (strt-lagtime):length(Infected)
init <- c(S = N-Infected[strt-lagtime]- Exposed[strt-lagtime]-Vac - Recovered[strt-lagtime], 
          E = Exposed[strt-lagtime],
          I = Infected[strt-lagtime],
          R = Recovered[strt-lagtime])

Err1 <- function(parameters) {
  #print("Hello")
  names(parameters) <- c("beta")##,"gamma","delta")
  #print(init)
  #out <- ode(y = init, times = Day, func = sis_model, parms = parameters)
  #print(out)
  out <- ode(init, Day, seir_model, parameters)
  #print(out)
  fit <- out[,3]
  #print(parameters["gamma"])
  #sum((Infected - fit)^2+(Removed - out[,4])^2)
  x <- sum((Infected-fit)^2)
  return(x)
}

# 
Opt <- tryCatch(optim(c(0.5, gamma, delta), Err1,
       method = "L-BFGS-B",lower = c(int_beta1, int_gamma1, int_delta1),
       upper = c(int_beta2, int_gamma2, int_delta2)) 
       # optimize with some sensible conditions
       ,error = function(e){Opt = "NA"})

##if(Opt !="NA"){

Opt_par <- setNames(Opt$par, c("beta","gamma","delta"))

####Opt_par <- c(beta, gamma, delta)

print(Opt_par)


t <- (strt-lagtime):(nrow(df) + Pred_time)# time in days

model1 <- ode(y = init, times = t, func = seir_model, parms = Opt_par)
summary(model1)
fit <- data.frame(ode(y = init, times = t, func = seir_model, parms = Opt_par))

col <- 1:2 # colour
fit$Date <- seq(df$Date[strt-lagtime], df$Date[strt -lagtime] + (lagtime+1 + (Pred_time-1)), 1)

df$Total.Confirmed.cases_non_cum  <- c(diff(df$Total.Confirmed.cases) , NA)
df$Total.Confirmed.cases_non_cum[which(df$Total.Confirmed.cases_non_cum < 0)] = 0

df$Cured.Discharged.Migrated_non_cum  <- c(diff(df$Cured.Discharged.Migrated) , NA)
df$Cured.Discharged.Migrated_non_cum[which(df$Cured.Discharged.Migrated_non_cum < 0)] = 0

df$Total_Exposed   <- Exposed
df$Total_Exposed_non_cum   <- c(diff(Exposed), NA) 
df$Total_Exposed_non_cum[which(df$Total_Exposed_non_cum < 0)] = 0

fit$Actual_Infected <- c(df$Total.Confirmed.cases[(strt-lagtime):nrow(df)], rep(NA, (nrow(fit) - length((strt-lagtime):nrow(df)))))
fit$Actual_Recoverd <- c(df$Cured.Discharged.Migrated[(strt-lagtime):nrow(df)], rep(NA, (nrow(fit) - length((strt-lagtime):nrow(df)))))
fit$Actual_Exposed <-  c(df$Total_Exposed[(strt-lagtime):nrow(df)], rep(NA, (nrow(fit) - length((strt-lagtime):nrow(df)))))
fit$Actual_Death <-  c(df$Death[(strt-lagtime):nrow(df)], rep(NA, (nrow(fit) - length((strt-lagtime):nrow(df)))))

fit$Predicted_Death <- (fit$I + fit$R )*death_Rate

fit$Actual_Infected_Non_cum <- c(df$Total.Confirmed.cases_non_cum[(strt-lagtime):nrow(df)], rep(NA, (nrow(fit) - length((strt-lagtime):nrow(df)))))
fit$Actual_Recoverd_Non_cum <- c(df$Cured.Discharged.Migrated_non_cum[(strt-lagtime):nrow(df)], rep(NA, (nrow(fit) - length((strt-lagtime):nrow(df)))))
fit$Actual_Exposed_Non_cum <- c(df$Total_Exposed_non_cum[(strt-lagtime):nrow(df)], rep(NA, (nrow(fit) - length((strt-lagtime):nrow(df)))))

#matplot(fit$time, fit[ , 3:4], type = "l", 
#        xlab = "Day", ylab = "Number of subjects", 
#        lwd = 2, lty = 1, col = col)

#plot(fit[ , 3:4] ~ fit$Date, type = "l", 
#        xlab = "Day", ylab = "Number of subjects", 
#        lwd = 2, lty = 1, col = col)

############ ggplots #
colnames(fit)[5] <- "Predicted_recovered" 
colnames(fit)[4] <- "Predicted_Infected" 
colnames(fit)[3] <- "Predicted_Exposed" 
fit$Predicted_recovered_non_cum  <- c(diff(fit$Predicted_recovered) , NA)
fit$Predicted_recovered_non_cum[which(fit$Predicted_recovered_non_cum < 0)] = 0
fit$Predicted_Infected_non_cum  <- c(diff(fit$Predicted_Infected) , NA)
fit$Predicted_Infected_non_cum[which(fit$Predicted_Infected_non_cum < 0)] = 0
fit$Predicted_Exposed_non_cum  <- c(diff(fit$Predicted_Exposed) , NA)
fit$Predicted_Exposed_non_cum[which(fit$Predicted_Exposed_non_cum < 0)] = 0



fit$time <- NULL

fit_raw <- fit 
fit_raw$State_nam <- State_nam

temp_fit <- rbind(temp_fit, fit_raw)


 fit_sel_non_cum <- fit %>% dplyr::select(Date,Predicted_Infected_non_cum,
                                   Predicted_recovered_non_cum,
                                  Actual_Infected_Non_cum,
                                  Actual_Recoverd_Non_cum,
                                  Actual_Exposed_Non_cum,
                                  Predicted_Exposed_non_cum
                                  
 )
write.csv(fit, 
           paste0("C:/aditya/Covid19/Vaccine/" , 
                  State_nam,"_",model, "_Projections.csv"),row.names = F )
## fit_sel_non_cum  <- fit %>% dplyr::select(Date,Predicted_Infected_non_cum,
#                                    Actual_Infected_Non_cum)

colnames(fit_sel_non_cum) <- gsub("_Non_cum", "", colnames(fit_sel_non_cum), ignore.case = T)
fit_melt <- reshape2::melt(fit_sel_non_cum, id = c("Date"))
R0 <-  Opt_par["beta"] / Opt_par["gamma"]
p = ggplot2::ggplot(data = fit_melt , aes(x = Date, y = value,
                color = variable,group = variable)) + geom_line()
p <- p + theme(axis.text.x=element_text(angle = 45, size = 14, hjust=1),
               axis.title =element_text(size = 14, face = "bold"))
p <- p + labs(title = paste0(State_nam, " Starting From ",df$Date[1],", New Cases, R0= ", round(R0,2)),
              caption = "Data Source: https://www.mohfw.gov.in/,
              IIITD professors Tavpritesh Sethi, Ponnurangam Kumaraguru & Sriram K. along with their teams
              Aditya Nagori,Raghav Awasthi, Chandan Gupta") +
       theme(legend.position="top", text = element_text(size = 12))
print(p + geom_point())

temp_R <- rbind(temp_R, c(i,State_nam, R0, as.character(df$Date[1]),
                Opt_par["beta"] , Opt_par["gamma"], Opt_par["delta"]))
}else{
  temp_R <- rbind(temp_R, c(i, State_nam, "Not enough data", NA, NA, NA, NA))
}
}
}  
dev.off()
temp_R <- data.frame(temp_R)
colnames(temp_R) <- c("starting_cases_no", "State", "R0", "start_date", "beta", "gamma", "delta")


