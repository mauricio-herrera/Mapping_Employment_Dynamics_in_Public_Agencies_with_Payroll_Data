#############
# Figures
#############

setwd('')# set working director with data in it:
# Data can be download from Harvard Dataverse:
# Herrera-Marín M, Brieba D. Data Chilean Gov. 2006 - 2020; (2024)
# https://doi.org/10.7910/DVN/Z2EPOS

#
dir()
pacman::p_load(tidyverse,reshape2,lattice,sjPlot,ggeffects,knitr,lme4,lmerTest,
               report,lsmeans,broom,lubridate,dplyr,pryr,ggplot2,plotly,hrbrthemes,
               viridis,ggsci,ggpubr,ggthemes,gtsummary,gt,data.table,Hmisc,mice,
               nlme,MASS,gridExtra,kableExtra,haven,ggtern,ggtern,ggfortify,
               survMisc,survival,survminer,coxme,MatchIt)
select<-dplyr::select
filter<-dplyr::filter

theme01=theme_ipsum()+theme_bw()+theme(
  legend.position = "none",
  axis.title.x = element_text(color="black", size=16,family = "Arial Black"),
  axis.title.y = element_text(color="black", size=16,family = "Arial Black"),
  axis.text.y = element_text(color="black", size=14,family = "Microsoft Sans Serif"),
  plot.title = element_text(color="black", size=24,family = "Arial Narrow",face='bold'),
  plot.subtitle = element_text(color="black", size=14,family = "Arial Narrow",face='italic'),
  plot.caption = element_text(color="black", size=10,family = "Times")
)

# FIGURE 1
# Average staff turnover (percentage) for a random sample of twenty
#State agencies. The figure shows wide variability in the levels of staff turnover
#between state agencies, from “stable” agencies to others showing very high levels of staff
#turnover. (Source: authors)

service_turnover_data=read.csv("service_turnover_annual_data.csv")
service_names=read.csv("names_services_sample_for_figures.csv")
names(service_turnover_data)
unique(service_turnover_data$service)
service_turnover_data%>%group_by(service)%>%summarise(avg_turnover=round(mean(turnover),1))%>%
  left_join(service_names,by='service')%>%na.omit()%>%
  ggplot(aes(x=reorder(NameService,avg_turnover),y=avg_turnover,fill='Red',color='white'))+
  geom_bar(stat = "identity", show.legend = F, alpha=0.8)+
  geom_text(aes(label=avg_turnover),
            hjust=1.1, vjust=0.35,size=3.5,family = "Arial black",color='white')->p
p+coord_flip()+theme01+
  xlab('Agencies')+ylab('Average Turnover (%)')

# FIGURE 2:
#Difference between the mean staff turnover in the first year of a new
#administration and the base or regular turnover in non-administration
#(regular) years for a sample of twenty agencies. The difference in agency staff
#turnover measured in the first year of a new administration compared to the base staff
#turnover measured in non-administration (regular) years is a proxy for the independence
#of State agencies concerning political cycles. (Source: authors)

service_turnover_data%>%group_by(service,post_election_year)%>%
summarise(avg_turnover=round(mean(turnover),1))%>%
  reshape2::dcast(service ~post_election_year,value.var='avg_turnover')%>%
  rename(avg_regular_turnover="0",avg_post_electoral_turnover='1')%>%
  mutate(Difference_in_Turnover=round(avg_post_electoral_turnover-avg_regular_turnover,1))%>%
  mutate(sign=as.factor(ifelse(Difference_in_Turnover>0,1,0)))%>%
  left_join(service_names,by='service')%>%na.omit()%>%
  ggplot(aes(x=reorder(NameService,Difference_in_Turnover),y=Difference_in_Turnover,fill=sign))+
  scale_fill_brewer(name = "sign")+
  geom_bar(stat = "identity", show.legend = F)+
  geom_text(aes(label=Difference_in_Turnover),
            hjust=-0.0, vjust=0.35,size=4.5,
            family = "Arial Narrow")-> p

p+coord_flip()+theme01+
  xlab('Agencies')+ ylab('Difference in Turnover (%)')

# FIGURE 4:
# An overview of the staff turnover trends. “Spaghetti plots” illustrating
#time trends for each agency by year type—regular years (left panel) and the first year of
#different government administrations (right panel) —for a sample of 79 agencies, with
#overall linear fits depicted by the bold orange line. (Source: authors)


theme02=theme_ipsum()+theme_bw()+theme(
  legend.position = "none",
  axis.title.x = element_text(color="black", size=16,family = "Arial Black"),
  axis.title.y = element_text(color="black", size=16,family = "Arial Black"),
  axis.text.y = element_text(color="black", size=14,family = "Microsoft Sans Serif"),
  axis.text.x = element_text(color="black", size=14,family = "Microsoft Sans Serif"),
  plot.title = element_text(color="black", size=24,family = "Arial Narrow",face='bold'),
  plot.subtitle = element_text(color="black", size=14,family = "Arial Narrow",face='italic'),
  plot.caption = element_text(color="black", size=10,family = "Times")
)
B1 <- c('ine')
service_turnover_data%>% filter(turnover>1)%>% filter(!service%in%B1)%>%
  ggplot(aes(x = year2006, y = log(turnover))) +
  geom_point(size=1.5)+
  geom_line(aes(group =service), color = "dark grey") +
  geom_smooth(aes(group = 1), color = "orange", size = 1,method = 'lm') +
  facet_wrap(~post_election_year,ncol=2)->p

p=p+theme02+
  labs(x = "Years since 2006", y = "log(Turnover)")+
  scale_x_continuous(limits=c(0,13), breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13))
p
# ggplotly(p)

# FIGURE 5:
#The intercepts and slopes for the unconditional growth model. The
#figure presents the intercept values ui (panel a) and slope values vi (panel b) with error
#bars for each agency i, derived from fitting the Unconditional Growth Model. For clarity,
#only twenty services are displayed. (Source: authors)

regressions <- service_turnover_data%>%filter(service%in%service_names$service) %>%rename(Year=year2006)%>%
  group_by(service) %>% do(fit = lm(turnover ~ Year , data=.)) %>%mutate(m=list(tidy(fit)))
s=length(regressions$service)
Q=cbind(service=regressions$service[1],
        regressions$m[[1]][1],
        regressions$m[[1]][2],
        regressions$m[[1]][3],
        regressions$m[[1]][4],
        regressions$m[[1]][5])
for (i in 2:s){
  w=cbind(service=regressions$service[i],
          regressions$m[[i]][1],
          regressions$m[[i]][2],
          regressions$m[[i]][3],
          regressions$m[[i]][4],
          regressions$m[[i]][5])
  Q=rbind(Q,w)
}
Q%>%select(service,term,estimate)%>%group_by(service)%>%
  spread(key = term, value = estimate)%>%
  rename(rate = Year, int = `(Intercept)`)%>%ungroup()->lm_info1
Q%>%select(service, term, std.error)%>%group_by(service)%>%
  spread(key = term, value = std.error) %>%
  rename(se_rate = Year , se_int = `(Intercept)`)%>%ungroup()->lm_info2
Q%>%select(service, term, p.value)%>%group_by(service)%>%
  spread(key = term, value = p.value) %>%
  rename(p.value_rate = Year , p.value_int = `(Intercept)`)%>%
  ungroup()->lm_info3
lm_info <- regressions %>%
  inner_join(lm_info1, by = "service") %>%
  inner_join(lm_info2, by = "service") %>%
  inner_join(lm_info3, by = "service")
w=cbind(service=regressions$service[1],glance(regressions$fit[[1]]))
for (i in 2:s){
  Q=cbind(service=regressions$service[i],glance(regressions$fit[[i]]))
  w=rbind(w,Q)
}
w%>%select(service, r.squared, df.residual,p.value)%>%
  rename(p.value.global= p.value)->lm_info4
lm_info <- regressions %>%
  inner_join(lm_info1, by = "service") %>%
  inner_join(lm_info2, by = "service") %>%
  inner_join(lm_info3, by = "service")%>%
  inner_join(lm_info4, by = "service")%>%
  mutate(tstar = qt(.95, df.residual),
         intlb = int - tstar * se_int,
         intub = int + tstar * se_int,
         ratelb = rate - tstar * se_rate,
         rateub = rate + tstar * se_rate)

lm_info%>%mutate(sign1=as.factor(ifelse(int>=0,'1','0')),
                 sign2=as.factor(ifelse(rate>=0,'1','0')))->lm_info
glimpse(lm_info)
lm_info%>%left_join(service_names,by='service')->lm_info
int.ci <- ggplot(lm_info, aes(y=rate, x=reorder(NameService,int),color=sign2)) +scale_color_aaas()+
  geom_point() + theme01 +
  geom_errorbar(aes(ymin=ratelb, ymax=rateub,color=sign2)) +
  coord_flip() + labs(y="Slope",x="Service",title="(b)")
slope.ci <- ggplot(lm_info, aes(y=int, x=reorder(NameService,int),color=sign1)) +scale_color_aaas()+
  geom_point() + theme01 +
  geom_errorbar(aes(ymin=intlb, ymax=intub,color=sign1)) +
  coord_flip() + labs(y="Intercepts",x="Service",title="(a)")
lon.cis1 <- grid.arrange(slope.ci, int.ci, ncol=2)
grid.arrange(slope.ci,int.ci,ncol=2)


#FIGURE 6:
# Average annual staff turnovel. The figure shows the annual turnover
#distributions for 77 services of the Central Chilean State. The blue line and orange dots
#indicate the evolution of the average annual turnover values. (Source: authors)

data_months=read.csv('data_chilean_gov_2006_2020.csv',header=T,dec='.',sep=',')
data_months %>%
  # 1. Modify the Rank column to handle NA values
  mutate(Rank = ifelse(is.na(Rank) & ContractRegime == "Temporary", "No information", Rank),
         Rank = ifelse(is.na(Rank), "No information", Rank)) %>%
# 2. Generate the summary with the average turnover and counts by gender, type of contract and range
 group_by(Label, Service,staffing,year_month,Label1,churns) %>%
  # Calculate the average turnover
  summarise(avg_turnover = mean(turnover, na.rm = TRUE),
  # Calculate sex counts (male and female)
    male_count = sum(Sex == "Male", na.rm = TRUE),
    female_count = sum(Sex == "Female", na.rm = TRUE),
  # Calculate ContractRegime counts (Temporary, Contractual, Permanent)
    temporary_count = sum(ContractRegime == "Temporary", na.rm = TRUE),
    contractual_count = sum(ContractRegime == "Contractual", na.rm = TRUE),
    permanent_count = sum(ContractRegime == "Permanent", na.rm = TRUE),
  # Calculate Rank counts
    professional_count = sum(Rank == "Professional", na.rm = TRUE),
    assistant_count = sum(Rank == "Assistant", na.rm = TRUE),
    administrative_count = sum(Rank == "Administrative", na.rm = TRUE),
    technician_count = sum(Rank == "Technician", na.rm = TRUE),
    managerial_count = sum(Rank == "Managerial", na.rm = TRUE),
    inspector_count = sum(Rank == "Inspector", na.rm = TRUE),
    gov_authority_count = sum(Rank == "Government_Authority", na.rm = TRUE),
    no_info_count = sum(Rank == "No information", na.rm = TRUE)) %>%
  ungroup()->data_months_summary
service_subset=c('odepa','ine','chilecompra','cne')
ff=function(x){
  return(x-x%%1)
}
data_months_summary%>%mutate(year=ff(year_month))->data_months_summary
data_months_summary%>%group_by(year)%>%summarise(avg_turnover_year=mean(avg_turnover))->dy_year
data_months_summary%>%left_join(dy_year,by='year')->data_months_summary

data_months_summary%>%
  filter(!Service%in%service_subset)%>%
  group_by(Label,year_month,avg_turnover_year,avg_turnover,year)%>%
  summarise(avg_turnover_month=mean(avg_turnover))%>%filter(avg_turnover<=10)%>%
  ggplot()+
  geom_boxplot(aes(x=factor(year),y=avg_turnover_month))+
  geom_line(aes(x=as.factor(year),y=avg_turnover_year,group=1),linewidth=1.0,color='blue')+
  geom_point(aes(x=as.factor(year),y=avg_turnover_year,group=1),size=2.0,color='orange')->p
p=p+ theme02+ xlab('Year')+ylab('Turnover')
p

#############
# MODELS
#############

# MIXED LINEAR MODELS FOR TURNOVER

# (1). MODEL A, UNCONDITIONAL MEANS MODEL (POST-ELECTION YEARS)

y0=service_turnover_data%>%filter(post_election_year==1)%>%filter(!service%in%c('ine'))
y0$service=factor(y0$service)
model.a <- lmer(turnover ~ 1 + (1|service), REML=T, data=y0)
summary(model.a )
#alpha0 = 15.675  = the mean turnover across all agencies and all years
# sigma^2= 44.71 = the variance in within-agency deviations between individual yearly turnover and the agency mean across all years
# sigmaU^2 = 65.62   = the variance in between-agency deviations between agency means and the overall mean across all agencies and all years
# Based on the intraclass correlation coefficient:
65.62 /(65.62 +44.71 )
VCrandom <- VarCorr(model.a)
print(VCrandom, comp = c("Variance", "Std.Dev."))
cat(" Number of Level Two groups = ",
    summary(model.a)$ngrps)
coef(summary(model.a))
# Same calculation for regular years
y1=service_turnover_data%>%filter(post_election_year==0)%>%filter(!service%in%c('ine'))
y1$service=factor(y1$service)
model_regular.a <- lmer(turnover ~ 1 + (1|service), REML=T, data=y1)
summary(model_regular.a )
# Based on the intraclass correlation coefficient:
23.25/(23.25 +46.18 )
tab_model(model.a, p.style = "scientific_stars")

# (2). MODEL B,  UNCONDITIONAL GROWTH MODEL (POST-ELECTION YEARS)
bb=c("sml","onemi","aduana",'minrel','dpp','vialidad',"ciren",'dgop','agci',
     "senadis",'submin',"serviuatacama","infor","ingresa","cne")
y0%>%rename(Year=year2006)->y0
y0%>%filter(!service%in%bb)->y00
model.b <- lmer(turnover~ Year + (Year|service), REML=T, data=y00)
summary(model.b)
# MODEL DESCRIPTION
# alpha0=15.28  Mean turnover in 2006
# beta0=0.18 & Mean yearly change in turnover
# hat_sigma2= 27.97,  Within-agency variance
# hat_sigma2= 0.23 Between-agency variance in rate changes
# hat_rho_uv=-0.38, Correlation between 2006 turnover and rate change

VCrandom <- VarCorr(model.b)
print(VCrandom, comp = c("Variance", "Std.Dev."))
cat(" Number of Level Two groups = ",
    summary(model.b)$ngrps)
coef(summary(model.b))
cat(" AIC = ", AIC(model.b), ";  BIC = ", BIC(model.b))
report(model.b)
tab_model(model.b, p.style = "scientific_stars")

# MODEL VISUALIZATION: RANDOM INTERCEPS AND SLOPES
as.data.frame(ranef(model.b , condVar=T))%>%filter(grp%in%service_names$service)%>%
  mutate(service=grp)%>%
  left_join(service_names,by='service')%>%
  ggplot(aes(y=reorder(NameService,condval),x=condval)) +
    geom_point(color='orange',size=2) + facet_wrap(~term,scales="free_x") +
    geom_errorbarh(aes(xmin=condval -2*condsd,
                       xmax=condval +2*condsd), height=0)->p
p=p+ theme02+ ylab('Agencies')+ xlab('Intercepts and slopes')
p

# (3) MODELING QUADRATIC TIME TREND

y00%>%mutate(Year2=Year^2)->y00
model.b2 <- lmer(turnover~ Year + Year2 + (1|service), REML=T, y00)
summary(model.b2)
VCrandom <- VarCorr(model.b2)
print(VCrandom, comp = c("Variance", "Std.Dev."))
cat(" Number of Level Two groups = ",
    summary(model.b2)$ngrps)
coef(summary(model.b2))
cat(" AIC = ", AIC(model.b2), ";  BIC = ", BIC(model.b2))
report(model.b2)
tab_model(model.b2, p.style = "scientific_stars")

#hat_alpha0 = 13.95, The mean staff turnover for all agencies in 2006.
#hat_beta0 = 1.05, The rate of change in staff turnover for the Year.
# hat_beta1 = -0.078, The rate of change in staff turnover for Year$^2$.
#hat_sigma2 = 32.76, The staff turnover variance within-agency deviations.
#hat_sigma2_u = 65.49,The staff turnover variance between agencies in 2006.


# (4). MODEL C,  CONDITIONAL GROWTH MODEL (POST-ELECTION YEARS)

# (4a) (only) ProfessionalStatus

y00%>%mutate(prop_professional=round(num_professional_employee/staffing,2))->y00
quantile(y00$prop_professional,c(0.25,0.5,0.7,0.75,0.9,1))
(Q2=quantile(y00$prop_professional,c(0.5)))
y00%>%mutate(ProfessionalStatus=as.factor(ifelse(prop_professional>Q2,'1','0')))->y00
table(y00$ProfessionalStatus)

model.c <- lmer(turnover~  Year + ProfessionalStatus+
                  ProfessionalStatus:Year + (Year|service), REML=T, data=y00)
summary(model.c)
report(model.c)
tab_model(model.c, p.style = "scientific_stars")

# (4b) ProfessionalStatus + TemporaryStaff

y00%>%mutate(prop_temporary=round(num_temporary_employee/staffing,3))->y00
quantile(y00$prop_temporary,c(0.25,0.5,0.7,0.75,0.9,1),na.rm=T)
y00$prop_temporary
(Q2_temporary=quantile(y00$prop_temporary,c(0.5),na.rm=T))
y00%>%mutate(TemporaryStaff=as.factor(ifelse(prop_temporary>Q2_temporary,'1','0')))->y00
table(y00$TemporaryStaff)
model.d=lmer(turnover~ ProfessionalStatus +
               TemporaryStaff +
               Year +
               ProfessionalStatus:Year +
               TemporaryStaff:Year +
               (Year|service), REML=T, data=y00)
summary(model.d)
report(model.d)
tab_model(model.d, p.style = "scientific_stars")


# (5) CAUSAL ANALYSIS: PROPENSITY SCORE MATCH MODEL

ym=function(year,month){
  return(year + (month - 1) / 12)
}
# Create the treatment variable (1 = after the change of government, 0 = before)
(start_interval=ym(2015,1))
(end_interval=ym(2018,12))
(change_point=ym(2017,12))
service_subset=c('odepa','ine')
data_months_summary%>%filter(year_month>start_interval & year_month  <=end_interval)->df
df$treatment <- ifelse(df$year_month >= change_point, 1, 0)
table(df$treatment)
df%>%mutate(prop_temporary=(temporary_count/staffing)*100,
            prop_professional=(professional_count/staffing)*100)%>%filter(!Service%in%service_subset)%>%
  mutate( prop_contractual=(contractual_count /staffing)*100,
          prop_male=(male_count/staffing)*100,
          prop_managerial=(managerial_count/staffing)*100,
          prop_administrative=(administrative_count/staffing)*100,
          prop_permanent=(permanent_count/staffing)*100
          ) ->df
# Rescale the variables
df$prop_contractual_scaled <- scale(df$prop_contractual)
df$prop_male_scaled <- scale(df$prop_male)
df$prop_managerial_scaled <- scale(df$prop_managerial)
df$prop_administrative_scaled <- scale(df$prop_administrative)
df$prop_permanent_scaled <- scale(df$prop_permanent)
df$staffing_scaled <- scale(df$staffing)
df$prop_temporary_scaled <- scale(df$prop_temporary)
df$prop_professional_scaled <- scale(df$prop_professional)

# Fit the Propensity Score model with the rescaled variables

ps_model <- glm(treatment ~ staffing_scaled +
                  prop_temporary_scaled +
                  prop_professional_scaled+
                  prop_contractual_scaled+
                  prop_male_scaled +
                  prop_managerial_scaled+
                  prop_administrative_scaled+
                  prop_permanent_scaled,
                data = df, family = binomial)

# Perform matching with the rescaled variables

match <- matchit(treatment ~ staffing_scaled + prop_temporary_scaled + prop_professional_scaled+
                   prop_contractual_scaled+prop_male_scaled +prop_managerial_scaled+
                   prop_administrative_scaled+prop_permanent_scaled,
                 data = df, method = "nearest")

# Get the matched data
matched_data <- match.data(match)
summary(match)
plot(summary(match), var.order = "unmatched")

# Fit the mixed model with the rescaled variables
mixed_model <- lmer(avg_turnover ~ treatment +
                      staffing_scaled +
                      prop_temporary_scaled +
                      prop_professional_scaled+
                      prop_contractual_scaled+
                      prop_male_scaled +
                      prop_managerial_scaled+
                      prop_administrative_scaled+
                      (1 | Service), data = matched_data)
summary(mixed_model)

# INTERPRETATION:

# The linear mixed model fitted with data matched through Propensity Score Matching (PSM)
# reveals significant insights into the factors affecting staff turnover across various public
# services:
# Random Effects: The variance between services (\( \sigma^2_{\text{service}} = 0.1652 \))
# indicates moderate variability in turnover rates across different agencies,
# while the residual variance (\( \sigma^2_{\text{residual}} = 4.9191 \))
# suggests considerable within-service variability in turnover.

# Fixed Effects:

# Intercept: The baseline turnover rate, represented by the intercept
# \( \hat{\alpha}_0 = 1.166 \), is statistically significant (\( p < 2e^{-16} \)),
# indicating the average turnover in the absence of other factors.
# Treatment (Government Transition): The treatment variable, which captures the effect
# of government transitions on turnover, shows a significant positive effect
# (\( \hat{\beta} = 0.590 \), \( p = 3.50e^{-8} \)), suggesting that turnover increases
# significantly after a change in administration.
# Proportion of Temporary Staff: The scaled proportion of temporary staff has a strong
# positive effect on turnover (\( \hat{\beta} = 0.801 \), \( p = 2.43e^{-9} \)), indicating that agencies with higher proportions of temporary contracts experience higher turnover.
# Proportion of Managerial Staff: This also shows a significant positive effect on turnover
# (\( \hat{\beta} = 0.203 \), \( p = 0.014 \)), suggesting that services with more managerial
# staff tend to have slightly higher turnover.
# Non-significant Variables: Other factors such as staffing levels,
# proportion of professional staff, proportion of contractual staff,
# proportion of male staff, and proportion of administrative staff do not show statistically
# significant effects on turnover. This suggests that these factors do not play a
# decisive role in influencing turnover rates under the current model.
# Model Fit: The REML criterion at convergence is 8004, indicating the relative goodness
# of fit. Scaled residuals indicate some variability in model fit, with the highest residual
# at 15.26.
# In conclusion, the results show that government transitions and the proportion of
# temporary staff are the most significant factors influencing turnover,
# while other factors contribute less. The model also captures the variability
# in turnover between different services, highlighting the importance of agency-specific
# characteristics.


# VISUALIZATION MATCH
pacman::p_load(ggplot2,MatchIt,tableone,lme4,
               lmerTest, stargazer,broom.mixed,
               sjPlot,mediation)

# Visualize covariate balance before and after matching

# This generates a graph of standardized mean differences before and after matching,
# useful for seeing how matching improved the balance between the treated and control groups.

plot(summary(match), var.order = "unmatched")

# Create balance table before and after matching
#We use the tableone package to generate a table showing the balance of covariates before and after matching.

vars <- c("staffing", "prop_temporary", "prop_professional")
matched_tab <- CreateTableOne(vars = vars, strata = "treatment", data = matched_data, test = FALSE)
print(matched_tab, smd = TRUE)

# Visualizing propensity scores
#We can graph the propensity scores to see how they were distributed before
# and after matching:

ggplot(matched_data, aes(x = distance, fill = factor(treatment))) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.5) +
  labs(title = "Propensity Scores Distribution", x = "Propensity Score", fill = "Treatment") +
  theme_minimal()

# MEDIATION ANALYSIS

#  Fit the mediation model
# Model 1: Effect of treatment on the proportion of temporary employees
mediator_model <- lm(prop_temporary ~ treatment +
                       staffing + prop_professional, data = df)

# Model 2: Effect of treatment and mediator (proportion of temporary employees) on turnover
outcome_model <- lm(avg_turnover ~ treatment +
                      prop_temporary + staffing + prop_professional, data = df)

# Run the mediation analysis

med_analysis <- mediate(mediator_model, outcome_model, treat = "treatment",
                        mediator = "prop_temporary", boot = TRUE, sims = 1000)

# View the results
summary(med_analysis)

# INTERPRETATION:
# The mediation analysis provides detailed insights into how much of the total effect of the treatment
# on the outcome (turnover) is mediated through a specific mediator variable. Here’s a breakdown of
# the key components from the analysis:
### 1. ACME (Average Causal Mediation Effect):
# Estimate: -0.111
# This value represents the portion of the treatment effect that is mediated through the
# mediator variable. In other words, it quantifies the indirect effect of the treatment
# (e.g., government transition) on turnover, which occurs because the treatment affects the mediator
# (e.g., the proportion of temporary staff), which in turn affects turnover.
# 95% CI: [-0.148, -0.080]
# The confidence interval indicates that we are 95% confident that the true mediated effect lies
# between -0.148 and -0.080. Since the interval does not include 0, the mediated effect is statistically
# significant p-value**: < 2e-16
# The extremely low p-value confirms that the ACME is highly significant, meaning that the mediator significantly contributes to the treatment's impact on turnover.
### 2. ADE (Average Direct Effect):
# Estimate: 0.630
# This value represents the direct effect of the treatment on the outcome (turnover),
# controlling for the mediator. In other words, it quantifies how much the treatment
# influences turnover directly, without passing through the mediator.
# 95% CI: [0.429, 0.830]
# The confidence interval shows that the direct effect is statistically significant since
#it does not contain 0 p-value: < 2e-16
# The direct effect is also highly significant, indicating that even after accounting for the mediator,
# the treatment has a substantial direct impact on turnover.
### 3. *Total Effect:
# Estimate: 0.519
# This is the overall effect of the treatment on the outcome (turnover), combining both the
# direct and mediated effects.
# 95% CI: [0.315, 0.730]
# The confidence interval indicates that the total effect is statistically significant.
# p-value**: < 2e-16
# The very low p-value confirms that the total effect of the treatment is highly significant.
### 4. Proportion Mediated:
# Estimate: -0.214
# This value represents the proportion of the total effect that is mediated through the mediator.
# Here, approximately 21.4% of the treatment effect is explained by the mediator, with a negative sign
# indicating an inverse relationship between the mediator and the outcome.
# 95% CI: [-0.377, -0.130]
# The confidence interval confirms the statistical significance of the proportion mediated,
# as it does not include 0.
# p-value: < 2e-16
# The highly significant p-value suggests that the mediator plays a meaningful role in
# explaining the overall effect of the treatment on turnover.
### 5. Nonparametric Bootstrap Confidence Intervals:
# This analysis uses 1000 simulations and employs nonparametric bootstrapping to compute
# confidence intervals. Bootstrapping is a resampling technique that provides a robust estimate
#of confidence intervals, especially in mediation analysis, where normality assumptions may not hold. T
# he percentile method is used here to calculate the 95% confidence intervals.

### Interpretation:
# The results suggest that the treatment (e.g., government transitions) has a significant effect on
# turnover both directly and through the mediator (e.g., proportion of temporary staff).
# Specifically, the **direct effect** (ADE) of the treatment on turnover is positive and substantial,
# meaning that even when controlling for the mediator, the treatment continues to have a significant impact.
# The mediated effect (ACME) is negative, implying that the mediator negatively contributes
# to the relationship between the treatment and turnover. In this context, this could mean that an
# increase in the mediator (such as a higher proportion of temporary staff) may reduce the total
# effect of the treatment on turnover.
# Approximately 21.4% of the total effect is mediated through this mediator, suggesting that the
# mediator accounts for a meaningful portion of the relationship between the treatment and turnover.
# In summary, the mediation analysis reveals that both the direct and indirect effects of the treatment
# (through the mediator) are significant. The treatment influences turnover directly,
# but also indirectly through its effect on the mediator, which in turn affects turnover.

#  SENSITIVITY ANALYSIS:

sens.cont <- medsens(med_analysis, rho.by = 0.05)
summary(sens.cont)

# INTERPRETATION:

# The sensitivity analysis for the Average Causal Mediation Effect (ACME) tests how
# robust the mediation results are to potential violations of the assumption that the
# mediator is not confounded by unobserved variables (i.e., the assumption that there
# are no hidden confounders affecting both the mediator and the outcome).
#1. Rho:
#  - This is the correlation between the error terms of the mediator and the outcome
# models. A rho of 0 would mean there is no unobserved confounding between the mediator
# and the outcome. As rho increases, the assumption of no unobserved confounders is
# weakened, meaning that the mediator and the outcome are increasingly correlated due to
# unmeasured factors.
#- In this result, rho is set to 0.2, which means there is some level of unmeasured
# confounding being simulated.
#2. ACME (Average Causal Mediation Effect):
 # - Value: -0.0092
#- This is the estimated indirect effect (the effect of the treatment on the outcome
# through the mediator) when rho is 0.2. This value suggests that when accounting for
# unobserved confounding (rho = 0.2), the mediated effect is small and negative, but the
# confidence interval suggests it is not statistically significant at this level of
# unobserved confounding.
#3. 95% Confidence Interval (CI): Lower Bound: -0.026. Upper Bound: 0.0077
#- These values indicate the range in which the true ACME lies with 95% confidence,
# given the level of unobserved confounding (rho = 0.2). Since the interval includes
# zero, the ACME is not statistically significant at this level of rho, implying that
# the mediated effect might be diminished or even null if unobserved confounding exists
# at this level.
#4. Rho at which ACME = 0, Value: 0.2
#- This value indicates the level of rho (correlation between the error terms of the
# mediator and the outcome) at which the ACME becomes zero. In other words, if the level
# of unobserved confounding (rho) reaches 0.2, the mediation effect vanishes.
# This provides a threshold for how sensitive the mediation effect is to hidden
# confounding. A lower threshold means the results are more sensitive to unobserved
# confounding.
#5. R²_M*R²_Y* at which ACME = 0, Value: 0.04
#- This refers to the proportion of variance in both the mediator (M) and the outcome
# (Y) explained by unmeasured confounders. When this value reaches 0.04, the ACME
# becomes zero. In other words, if unmeasured confounders explain 4% of the variance
# in both the mediator and the outcome, the mediation effect would be null.
#6. R²_M~R²_Y~ at which ACME = 0, Value: 0.0331
#- This represents an alternative measure of the proportion of variance in the mediator
# and the outcome attributable to unmeasured confounders. When 3.31% of the variance
# is explained by unobserved variables, the ACME becomes zero. This also provides
# a threshold for how much unobserved confounding could potentially invalidate the
# mediation effect.
#Interpretation:
 # Sensitivity to Unobserved Confounding: The ACME becomes zero when the correlation
# between the mediator and outcome error terms (rho) reaches 0.2. This suggests that
# the mediation effect is moderately sensitive to unobserved confounding. If unmeasured
# factors affect both the mediator and the outcome with a correlation of 0.2 or higher,
# the mediation effect would no longer exist.
#Proportion of Variance Explained: If unobserved confounders explain more than 4% of
# the variance in both the mediator and the outcome, the mediation effect would become
# zero. This shows that the mediation effect is relatively robust unless a significant
# proportion of the variance is due to unobserved factors.
#Practical Significance: Since the ACME remains negative but close to zero with rho = 0.2,
# this suggests that while there is some mediation, it is relatively weak and sensitive
# to unmeasured confounding. The mediation effect should be interpreted with caution,
# as it could be invalidated by moderate levels of hidden confounding.
#Conclusion:
#The mediation effect is moderately sensitive to unobserved confounders.
# If unobserved variables explain a moderate proportion of the variance (around 4%)
# or have a correlation of 0.2, the mediation effect becomes zero. Therefore, while
# the mediation results are statistically significant under the assumption of no c
# onfounding, the effect may be invalidated by unobserved factors, and these results
# should be interpreted carefully.


# VISUALIZATION OF TURNOVER FOR ALL AGENCIES, MONTHS-YEARS

label1='12_2010'
label2='12_2014'
label3='12_2018'

(a1=max(data_months_summary$avg_turnover_year[which(data_months_summary$Label==label1)]))
(a2=max(data_months_summary$avg_turnover_year[which(data_months_summary$Label==label2)]))
(a3=max(data_months_summary$avg_turnover_year[which(data_months_summary$Label==label3)]))


data_months_summary%>%filter(!Service%in%service_subset)%>%
  group_by(Label,year_month,avg_turnover_year,Service,avg_turnover)%>%
  summarise(avg_turnover_month=mean(avg_turnover))%>%
  ggplot()+
  geom_point(aes(x=year_month,y=avg_turnover))+
  geom_point(aes(x=year_month,y=avg_turnover_month),color='orange',size=2.0)+
  geom_line(aes(x=year_month,y=avg_turnover_month))+
  geom_line(aes(x=year_month,y=avg_turnover_year),color='green')+theme01+
  geom_vline(xintercept = ym(2009,12), linetype="longdash",color = "red", linewidth=1.0)+
  geom_vline(xintercept = ym(2010,12), linetype="longdash",color = "blue", linewidth=1.0)+
  geom_vline(xintercept = ym(2013,12), linetype="longdash",color = "red", linewidth=1.0)+
  geom_vline(xintercept = ym(2014,12), linetype="longdash",color = "blue", linewidth=1.0)+
  geom_vline(xintercept = ym(2017,12), linetype="longdash",color = "red", linewidth=1.0)+
  geom_vline(xintercept = ym(2018,12), linetype="longdash",color = "blue", linewidth=1.0) +
  annotate('segment',x=ym(2009,12),xend=ym(2010,12),y=a1,yend=a1,color='green',linewidth =2)+
  annotate('segment',x=ym(2013,12),xend=ym(2014,12),y=a2,yend=a2,color='green',linewidth =2)+
  annotate('segment',x=ym(2017,12),xend=ym(2018,12),y=a3,yend=a3,color='green',linewidth =2)->p
p+xlab('Month - Year')+ylab('Turnover')+theme02

# VISUALIZATION OF MEAN TURNOVER MONTHS-YEARS

data_months_summary%>%filter(!Service%in%service_subset)%>%
  group_by(Label,year_month,avg_turnover_year)%>%
  summarise(avg_turnover_month=mean(avg_turnover))%>%
  ggplot()+
  geom_point(aes(x=year_month,y=avg_turnover_month),color='orange',size=2)+
  geom_line(aes(x=year_month,y=avg_turnover_month))+
  geom_line(aes(x=year_month,y=avg_turnover_year),color='green')->p
  p=p+
  geom_vline(xintercept = ym(2009,12), linetype="longdash",color = "red", linewidth=1.0)+
  geom_vline(xintercept = ym(2010,12), linetype="longdash",color = "blue", linewidth=1.0)+
  geom_vline(xintercept = ym(2013,12), linetype="longdash",color = "red", linewidth=1.0)+
  geom_vline(xintercept = ym(2014,12), linetype="longdash",color = "blue", linewidth=1.0)+
  geom_vline(xintercept = ym(2017,12), linetype="longdash",color = "red", linewidth=1.0)+
  geom_vline(xintercept = ym(2018,12), linetype="longdash",color = "blue", linewidth=1.0) +
  annotate('segment',x=ym(2009,12),xend=ym(2010,12),y=a1,yend=a1,color='green',linewidth =2)+
  annotate('segment',x=ym(2013,12),xend=ym(2014,12),y=a2,yend=a2,color='green',linewidth =2)+
  annotate('segment',x=ym(2017,12),xend=ym(2018,12),y=a3,yend=a3,color='green',linewidth =2)
p+xlab('Month - Year')+ylab('Month Average Turnover')+theme02


# MIXED SURVIVAL COX MODELS (WITH FRAILITY) FOR LONGEVITY
set.seed(123)
longevity_data=read.csv('data_tenure.csv')
names(longevity_data)
longevity_data$Sex=relevel(as.factor(longevity_data$Sex),ref= 'Male')
longevity_data$Rank=relevel(as.factor(longevity_data$Rank),ref= "Professional")
longevity_data$Regimen=relevel(as.factor(longevity_data$Regimen),ref= "contract")

# MODEL 0: MODEL WITHOUT COVARIABLES (for frailty calculation)
model0=coxme(Surv(time_start,time_end, Status) ~1+(1|Service), data=longevity_data)
summary(model0)
table_frailties=tibble(Service=names(ranef(model0)[[1]]),
          bk=ranef(model0)[[1]],
          exp_bk=exp(ranef(model0)[[1]]))
table_frailties%>%gt()
# MODEL 1: MODEL WITH COVARIABLES RELATED TO THE OFFICIAL
model1= coxme(Surv(time_start,time_end, Status) ~ Sex+
                                Rank+Regimen+
                                Age+Age2+
                                (1|Service), data=longevity_data)
summary(model1)
table_frailties_after_cov=tibble(Service=names(ranef(model1)[[1]]),
         bk=ranef(model1)[[1]],
         exp_bk=exp(ranef(model1)[[1]]))
table_frailties_after_cov%>%gt()
# Extract the variance of the frailty from the model1
(var_frailty_model0 <- VarCorr(model0)$Service[1])
# Extract the variance of the frailty from the model1
(var_frailty_model1 <- VarCorr(model1)$Service[1])

# Create a data.frame to compare the variances of frailty
var_comparison_df <- data.frame(
  Model = c("Model0", "Model1"),
  Variance_Frailty = c(var_frailty_model0 , var_frailty_model1)
)
# Print the variance comparison data.frame
print(var_comparison_df)


# Calculate the decrease in variance
decrease_variance<- var_frailty_model0 - var_frailty_model1
(percentage_decrease <- (decrease_variance / var_frailty_model0) * 100)

### The decrease in variance when considering individual variables such as age, rank and contract regime is 17.7%.

# Visualization 1: Services Frailties

glimpse(table_frailties)

table_frailties%>%mutate(sign=as.factor(ifelse(bk>0,1,0)))%>%rename(service=Service)%>%
  mutate(exp_bk1=round(exp_bk,2))%>%left_join(service_names,by='service')%>%
  na.omit()%>%ggplot(aes(x=reorder(NameService ,exp_bk),y=exp_bk,fill=sign))+
  scale_fill_cosmic(name = "sign")+
  geom_bar(stat = "identity", show.legend = F)+
  geom_text(aes(label=exp_bk1),
            hjust=-0.05, vjust=0.35,size=4,family = "Arial Narrow")->p

p+coord_flip()+theme02+ xlab('Service')+ ylab('Frailty')


# Visualization 2: Services Frailties

# FIGURE 8: Frailties computed for the mixed Cox model of staff longevity using a
#sample of twenty agencies. The figure illustrates the frailty values derived from the
#mixed Cox model, representing the longevity of state officials in a randomly selected
#sample of twenty state agencies. Each bar label corresponds to the exponent of the
#random intercept fk = exp (bk). (Source: authors.)

table_frailties%>%mutate(sign=as.factor(ifelse(bk>0,1,0)))%>%rename(service=Service)%>%
  mutate(exp_bk1=round(exp_bk,2))%>%left_join(service_names,by='service')%>%
  na.omit()%>%ggplot(aes(x=reorder(NameService ,exp_bk),y=bk,fill=sign))+
  scale_fill_d3(name = "sign")+
  geom_bar(stat = "identity", show.legend = F)+
  geom_text(aes(label=exp_bk1),
            hjust=-0.05, vjust=0.35,size=4,family = "Arial Narrow")->p
p+coord_flip()+theme02+ xlab('Service')+ ylab(expression(Frailty~(b[k])))




