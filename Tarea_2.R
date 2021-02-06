rm(list = ls()) #borramos memoria
library('dplyr')
library('tidyverse')
library('ggplot2')    # para gráficas
library('scales')     # para alterar escala del eje x en histograma
library('ggthemes')   # para tema de gráficas
library('stargazer')  # para tablas en formato .tex
library('ggpmisc')
library('ggpubr')    
library('quantreg')     #para regresiones cuantílicas
library('sandwich')
library('modelr')     # para manipulación de data frames con base en resultados de regresiones
library('equatiomatic')     # para generar ecuaciones de regresión en formato LaTeX
library('margins')          # para intervalo de confianza de efecto parcial
library('moderndive')       # para modelo de pendientes paralelas (dummy categórica)
library('broom')       # para modelo de pendientes paralelas (dummy categórica)
library('interactions')
library('car')
library('estimatr') #tarde pero útil para regresiones con errores heteroscedásticos



# PAQUETES INSTALADOS
# install.packages('ggpmisc')   # para mostrar ecuación de regresiones en gráficas
# install.packages('ggpubr')   # para mostrar ecuación de regresiones en gráficas
# install.packages('quantreg')  #para regresiones cuantílicas 
#install.packages('sandwich')
#install.packages('equatiomatic')
# install.packages('margins')
# remotes::install_github("datalorax/equatiomatic")
# install.packages('moderndive' ) # para modelo de pendientes paralelas (dummy categórica)
# install.packages('car') # para pruebas de hipótesis


# DIRECTORIO

setwd("C:/Users/Rafael/Google Drive/MAESTRÍA ITAM/Econometría Aplicada/Tareas/Tarea2")


#####################################################################################################################
#PREGUNTA 1
#####################################################################################################################

# BASE DE DATOS
datos <- read.csv(file='T2_covid.csv')

# 1.  estadísticas descriptivas básicas
summary(datos)

datos %>% 
  select(-gdp_pc_er)

stargazer(datos %>% select(-gdp_pc_er),title="Estadística Descriptiva",digits=2, out="descriptive.tex",summary.stat = c("n", "mean", "sd", "min", "max"))

#####################################################################################################################
#PREGUNTA 2
#####################################################################################################################

datos <- datos %>% mutate(tests_per_mil=(tests_performed*confirmed_per_mil)/confirmed)


##################### a. Diagrama de dispersión

ggplot(datos, aes(x=tests_per_mil, y=confirmed_per_mil)) +
  geom_point(size=2)+
  geom_smooth(method="lm",formula = y~x, se=FALSE, color='red' )+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Diagrama de dispersión",x="Pruebas por millón de habitantes",y="Casos confirmados por millón de habitantes")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, vjust=2.25))+
  scale_x_continuous(labels=comma)+
  scale_y_continuous(labels=comma)



ggsave("p2_diagrama_dispersion.pdf",width = 6, height = 4.25 ,units="in")

# Regresión

reg<- lm(confirmed_per_mil ~ tests_per_mil,data=datos)
round(summary(reg)$coefficients,4)

##################### b. Diagrama de dispersión tomando log(x)

ggplot(datos, aes(x=log(tests_per_mil), y=confirmed_per_mil)) +
  geom_point(size=2)+
  geom_smooth(method="lm",formula = y~x, se=FALSE, color='red' )+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Diagrama de dispersión",x="ln(Pruebas por millón de habitantes)",y="Casos confirmados por millón de habitantes")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, vjust=2.25))+
  scale_x_continuous()+
  scale_y_continuous(labels=comma)



ggsave("p2b_diagrama_dispersion.pdf",width = 6, height = 4.25 ,units="in")

# Regresión

reg<- lm(confirmed_per_mil ~ log(tests_per_mil),data=datos)
summary(reg)
round(summary(reg)$coefficients,4)


##################### c. Diagrama de dispersión tomando log(x) Y

ggplot(datos, aes(x=log(tests_per_mil), y=log(confirmed_per_mil))) +
  geom_point(size=2)+
  geom_smooth(method="lm",formula = y~x, se=FALSE, color='red' )+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Diagrama de dispersión",x="ln(Pruebas por millón de habitantes)",y="ln(Casos confirmados por millón de habitantes)")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, vjust=2.25))+
  scale_x_continuous()+
  scale_y_continuous()

ggsave("p2c_diagrama_dispersion.pdf",width = 6, height = 4.25 ,units="in")

# Regresión

reg<- lm(log(confirmed_per_mil) ~ log(tests_per_mil),data=datos)
summary(reg)
round(summary(reg)$coefficients,4)

##################### d. Regresión cuantílica
# curiosidad de valores de cuartiles
q1 <- quantile(datos$confirmed_per_mil,0.25)
q2 <- quantile(datos$confirmed_per_mil,0.5)
q3 <- quantile(datos$confirmed_per_mil,0.75)
cuartiles<- c(q1,q2,q3)

log_q1 <- quantile(log(datos$confirmed_per_mil),0.25)
log_q2 <- quantile(log(datos$confirmed_per_mil),0.5)
log_q3 <- quantile(log(datos$confirmed_per_mil),0.75)
log_cuartiles<- c(log_q1,log_q2,log_q3)
 # solo quiero ver que si den igual
round(cuartiles,0)
log_cuartiles
round(log(cuartiles),2)

regq <- rq(formula = log(confirmed_per_mil)~log(tests_per_mil),data=datos,tau = c(0.25,0.5,0.75) )
regq
coef(regq)


ggplot(datos, aes(x=log(tests_per_mil), y=log(confirmed_per_mil))) +
  geom_point(size=2,show.legend=FALSE)+
  geom_smooth(method="lm",formula = y~x, se=FALSE, color='black')+
  labs(title = "Diagrama de dispersión",x="ln(Pruebas por millón de habitantes)",y="ln(Casos confirmados por millón de habitantes)",color="Cuartil")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, vjust=2.25),legend.position = c(0.1, 0.8),legend.title = element_text(size = 12),legend.text = element_text(size = 10),
        legend.background = element_rect(fill = "white"))+
  scale_x_continuous()+
  scale_y_continuous()+
  geom_abline(aes(intercept=coef(regq)[1], slope=coef(regq)[2],color="Q1"),show.legend=TRUE,size=1)+
  geom_abline(aes(intercept=coef(regq)[3], slope=coef(regq)[4],color="Q2"),show.legend=TRUE,size=1)+
  geom_abline(aes(intercept=coef(regq)[5], slope=coef(regq)[6],color="Q3"),show.legend=TRUE,size=1)
  
ggsave("p2d_diagrama_dispersion.pdf",width = 6, height = 4.25 ,units="in")


#####################################################################################################################
#PREGUNTA 3
#####################################################################################################################


##################### a. Diagrama de dispersión PIB per cápita vs. total c asos confirmados
ggplot(datos, aes(x=log(gdp_pc_er), y=log(confirmed_per_mil))) +
  geom_point(size=2)+
  geom_smooth(method="lm",formula = y~x, se=FALSE, color='red',fullrange=TRUE )+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Diagrama de dispersión",x="ln(PIB per cápita)",y="ln(Casos confirmados por millón de habitantes)")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, vjust=2.25))+
  scale_x_continuous()+
  scale_y_continuous()

ggsave("p3a_diagrama_dispersion.pdf",width = 6, height = 4.25 ,units="in")

# regresión
reg<- lm(log(confirmed_per_mil) ~ log((gdp_pc)),data=datos)
summary(reg)
round(summary(reg)$coefficients,4)


##################### b. Error de medición

ggplot(datos, aes(x=log(gdp_pc_er), y=log(confirmed_per_mil))) +
  geom_point(aes(x=log(gdp_pc_er), y=log(confirmed_per_mil)),size=2,color='red')+
  geom_point(aes(x=log(gdp_pc), y=log(confirmed_per_mil)),size=1)+
  geom_smooth(method="lm",formula = y~x, se=FALSE, color='black',fullrange=TRUE )+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Diagrama de dispersión",x="ln(PIB per cápita)",y="ln(Casos confirmados por millón de habitantes)")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, vjust=2.25))+
  scale_x_continuous()+
  scale_y_continuous()

reg<- lm(log(confirmed_per_mil) ~ log((gdp_pc_er)),data=datos)
summary(reg)
round(summary(reg)$coefficients,4)


##################### d.Partial Out
# regresión gdp_pc = b_0 + b_1 * tests_per_mil
reg<- lm(log(gdp_pc) ~ log((tests_per_mil)),data=datos)
summary(reg)
resid <- residuals(reg)

extract_eq(reg, use_coefs=TRUE)

reg$residuals
datos<- add_residuals(data=datos, reg, var="resid_gdp_tests")

# ahora regresión confirmed_per_mil = b_0 + b_1 * residuos
reg2<- lm(log(confirmed_per_mil) ~ resid_gdp_tests,data=datos)
summary(reg2)

ggsave("p3a_diagrama_dispersion.pdf",width = 6, height = 4.25 ,units="in")

# diagrama de dispersión
ggplot(datos, aes(x=log(confirmed_per_mil), y=resid_gdp_tests )) +
  geom_point(size=2)+
  geom_smooth(method="lm",formula = y~x, se=FALSE, color='red',fullrange=TRUE )+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Diagrama de dispersión",x="resid_gdp_test",y="ln(Casos confirmados por millón de habitantes)")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, vjust=2.25))+
  scale_x_continuous()+
  scale_y_continuous()
ggsave("p3d_diagrama_dispersion.pdf",width = 6, height = 4.25 ,units="in")


##################### e.Entendiendo partial-out
reg3<- lm(log(confirmed_per_mil) ~ log(gdp_pc) + log(tests_per_mil) ,data=datos)
summary(reg3)

extract_eq(reg3, use_coefs=TRUE)



#####################################################################################################################
#PREGUNTA 4 #########################################################################################################
#####################################################################################################################

# nuevas variables
datos <- datos %>%
  mutate(cfr=deaths/confirmed) %>%
  mutate(eur=as.numeric(continent == "EU")) %>%
  mutate(asia=as.numeric(continent == "AS")) %>%
  mutate(nam=as.numeric(continent=="NAM")) %>%
  mutate(std_hdi = (hdi-mean(hdi,na=TRUE))/sd(hdi,na=TRUE)) %>%
  mutate(hm_cfr=as.numeric(cfr>0.019))

quantile(datos$cfr,0.5) # no coincide con la que dan en la tarea



# modelos a estimar
m1 <- lm(log(deaths) ~ overwgh_prev + log(cardio_dr) + diab_prev + log(tests_per_mil) + log(gdp_pc) ,data=datos)
m2 <- lm(deaths_per_mil ~ overwgh_prev + log(cardio_dr) + diab_prev + log(tests_per_mil) + eur + asia + median_age ,data=datos)
m3 <- lm(cfr ~ overwgh_prev + log(cardio_dr) + diab_prev + log(tests_per_mil) +  std_hdi + I(std_hdi^2) + median_age ,data=datos)
m4 <- lm(hm_cfr ~ overwgh_prev + log(cardio_dr) + diab_prev + log(hosp_beds_per_thou) + log(tests_per_mil) + aged_65_older + log(gdp_pc)+nam,data=datos)

# lista con ee robustos para cada modelo, luego los indico en stargazer
rob_se <- list(sqrt(diag(vcovHC(m1,type='HC1'))),
               sqrt(diag(vcovHC(m2,type='HC1'))),
               sqrt(diag(vcovHC(m3,type='HC1'))),
               sqrt(diag(vcovHC(m4,type='HC1'))))

# tabla con regresiones
stargazer(m1, m2, m3, m4, type="latex", digits=3, 
          se=rob_se,omit.stat=c("f","adj.rsq","ser"),
          order=c("overwgh_prev","cardio_dr","diab_prev","hosp_beds_per_thou", "tests_per_mil"
                  ,"aged_65_older","gdp_pc","eur","asia","nam"
                  ,"std_hdi","I(std_hdi2)","median_age"),
          dep.var.caption="Variable dependiente:",
          report="vc*s",out="tabla2reg.tex")

# ec. de regresión a LaTeX
extract_eq(m2, use_coefs=TRUE)


fitted_m2 <- fitted(m2)

datos <- datos %>%
  mutate(fitted_deats_per_mil = fitted_m2, na.rm = FALSE)

# agrego columna de fitted values al data frame
datos <- datos %>% add_predictions(m2,var="deaths_per_mil_fitted")

view(list(datos$country,datos$deaths_per_mil))

deaths_per_mil_df <- select(datos,country,deaths_per_mil, deaths_per_mil_fitted)

# ahora vemos el real vs el estimado (ajustado)
deaths_per_mil_df %>% filter(country=="Mexico") 

# Inciso 6b #################################################################
summary(m3)
summary(margins(m3))


#OJO: errores heteroscedásticos

#de aquí saco el IC
summary(margins(m3,vcov = vcovHC(m3,type='HC1')))

# Inciso 6c #################################################################
summary(m4)

vector <- c(0,1.5,0,2,0,15,0,0,0)

linearHypothesis(m4,vector,white.adjust="hc1",0)



#####################################################################################################################
#PREGUNTA 7 #########################################################################################################
#####################################################################################################################

datos2 <- datos
promedio_europa_asia <- datos2 %>%
  filter(continent == "EU" | continent=="AS") %>%
  group_by(continent) %>%
  summarise_all(mean, na.rm=TRUE) 

promedio_europa_asia[c(1:3)] <- c("ASIA","EUROPA")

# data frame con 2 nuevas observaciones: PAÍS ASIA = promedio países asiáticos y PAÍS EUROPA = promedio países europeos
datos_p7a <- bind_rows(datos,promedio_europa_asia)
datos_p7a <- datos_p7a %>% add_predictions(m2,var="deaths_per_mil_fitted")

datos_p7a <- datos_p7a %>% add_predictions(m2,var="deaths_per_mil_fitted")

deaths_per_mil_df_eur_asia <- select(datos_p7a,country,deaths_per_mil, deaths_per_mil_fitted)

# ahora vemos el real vs el estimado (ajustado)
deaths_per_mil_df_eur_asia %>% filter(country=="ASIA"| country=="EUROPA") 


# europa menos asia para prueba de hipótesis al 5%m2
resta <- promedio_europa_asia[2,4:31] - promedio_europa_asia[1,4:31]

m2 # para ver orden de coeficientes

ele <- c(0,
         resta$overwgh_prev,
         log(resta$cardio_dr),
             resta$diab_prev,
         log(resta$tests_per_mil),
         resta$eur,
         resta$asia,
         resta$median_age)

#   Buscamos rechazar H_0: Diferencia entre europa y asia es igual a cero
linearHypothesis(m2, ele, white.adjust="hc1",0)

# inciso a.II)

m2b <- lm(deaths_per_mil ~ overwgh_prev + log(cardio_dr) + diab_prev + log(tests_per_mil) + eur + asia + median_age 
         ,data=datos %>% filter(continent == "EU" | continent=="AS") )

summary(m2b)
fitted(m2b)

rob_se_m2b <- list(sqrt(diag(vcovHC(m2b,type='HC1'))))
               

stargazer(m2b ,type="latex", digits=3, 
          se=rob_se_m2b,omit.stat=c("f","adj.rsq","ser"),
          order=c("overwgh_prev","cardio_dr","diab_prev","hosp_beds_per_thou", "tests_per_mil"
                  ,"aged_65_older","gdp_pc","eur","asia","nam"
                  ,"std_hdi","I(std_hdi2)","median_age"),
          dep.var.caption="Variable dependiente:",
          report="vc*s")

# inciso a.III) LOOP ###################################################

# VALORES AJUSTADOS USANDO ESPECIFICACIÓN DE LA COLUMNA 2
fitted_m2 <- fitted(m2)

datos <- add_predictions(datos,m2,var="fitted_m2")

# agrego columna de fitted values al data frame
datos <- datos %>% add_predictions(m2,var="deaths_per_mil_fitted_m2")
datos <- datos %>% add_predictions(m2b,var="deaths_per_mil_fitted_m2b")

#calcul residuos
datos <- datos %>% 
  mutate(resid_m2 = deaths_per_mil - deaths_per_mil_fitted_m2) %>%
  mutate(resid_m2b = deaths_per_mil - deaths_per_mil_fitted_m2b)

# LOOP PARA ELEGIR
muestreo <- matrix(nrow=0,ncol=24) # matriz vacia de 1000 x 1 (tomaré 1,000 muestras de tamaño n=100 y a cada una le calculo el Q1)
set.seed(5) #para poder replicar resultados
for (i in 1:200)  {
x <- sample_n(datos %>% filter(continent=="AS" | continent=="EU"),1, replace=TRUE)
muestreo <- rbind(muestreo,x)
 }              

simulaciones <- as.data.frame(muestreo) #para tener un dataframe

# HISTOGRAMAS
# media de cada residuo
media_resid_m2<- mean(simulaciones$resid_m2, na.rm=TRUE)
media_resid_m2b<- mean(simulaciones$resid_m2b, na.rm=TRUE)

ggplot()+
  geom_histogram(aes(x=resid_m2),data=simulaciones,col='black',bins=20)+
  geom_vline(xintercept= media_resid_m2,color='red')+
  geom_text(aes(x=media_resid_m2+40,y=38,label="Media: 10.32"),size=4,color='red',angle=90)+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Histograma de residuos con modelo (I)",x="resid_m2",y="")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, vjust=2.25))
ggsave("p7a_histograma1.pdf",width = 6, height = 4.25 ,units="in")

               
ggplot()+
  geom_histogram(aes(x=resid_m2b),data=simulaciones,col='black',bins=20)+
  geom_vline(xintercept= media_resid_m2b,color='red')+
  geom_text(aes(x=media_resid_m2b+40,y=32,label="Media: 4.86"),size=4,color='red',angle=90)+
    theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Histograma de residuos con modelo (II)",x="resid_m2b",y="")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, vjust=2.25))             
ggsave("p7a_histograma2.pdf",width = 6, height = 4.25 ,units="in")

# los dos en una gráfica para poder compararlos         
ggplot() + 
  geom_histogram(aes(x=resid_m2, fill="r", colour="r"), alpha=.4, data=simulaciones,stat = "bin",bins=20) +
  geom_histogram(aes(x=resid_m2b, fill="b", colour="b"), alpha=.4, data=simulaciones,stat = "bin",bins=20)  +
  scale_colour_manual(name="Modelo", values=c("r" = "red", "b"="blue"), labels=c("b"="(II): muestra restringida", "r"="(I): total muestra")) +
  scale_fill_manual(name="Modelo", values=c("r" = "red", "b"="blue"), labels=c("b"="(II): muestra restringida", "r"="(I): total muestra" ))+
  labs(title="Q1 PPI (1,000 submuestras)",x="resid",y="")+
  theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5), legend.position = "top")+
  ggsave("p7a_histograma3.pdf",width = 6, height = 4.25 ,units="in")

               
 ########### pregunta 7b inciso I##
#opcion1
par_slopes <- lm(deaths_per_mil ~ factor(continent) + median_age,data=datos)
summary(par_slopes)

augmented_mod<- augment(par_slopes)
glimpse(augmented_mod)


ggplot(data= augmented_mod, aes(x=median_age,y=deaths_per_mil,color=factor.continent.))+
geom_point()+
  geom_line(aes(x=median_age,y=.fitted),lwd = 1)+
  theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5), legend.position = "top")+
  scale_x_continuous()+
  scale_y_continuous()
  ggsave("pb1_parallel_slopes.pdf",width = 6, height = 4.25 ,units="in")

  
  
#opcion 2
ggplot(data=datos, aes(x=median_age,y=deaths_per_mil,color=continent))+
  geom_point()+
  geom_parallel_slopes(se=FALSE)

#opcion 3
#guardo coeficientes en un atabla
par_slopes_coeff <- par_slopes %>%
  tidy() %>%
  select(term, estimate)

intercepto <- par_slopes_coeff %>%
  filter(term=="(Intercept)") %>%
  select(estimate) %>%
  pull()

coef_deaths_AS <- par_slopes_coeff %>%
  filter(term == "factor(continent)AS")%>%
  select(estimate) %>%
  pull()

coef_deaths_EU <- par_slopes_coeff %>%
  filter(term == "factor(continent)EU") %>%
  select(estimate) %>%
  pull()

coef_deaths_NAM <- par_slopes_coeff %>%
  filter(term == "factor(continent)NAM") %>%
  select(estimate) %>%
  pull()

coef_deaths_OC <- par_slopes_coeff %>%
  filter(term == "factor(continent)OC")  %>%
  select(estimate) %>%
  pull()

coef_deaths_SAM <- par_slopes_coeff %>%
  filter(term == "factor(continent)SAM") %>%
select(estimate) %>%
  pull()

slope <-   par_slopes_coeff %>%
  filter(term == "median_age") %>%
  select(estimate) %>%
  pull()            
               
ggplot(data = datos,
       aes(y = deaths_per_mil, x = median_age, color = factor(continent))) +
  geom_point(alpha = 0.3) +
  geom_abline(intercept = coef_deaths_AS, slope = slope, color = "#F57670", lwd = 1) +
  geom_abline(intercept = coef_deaths_EU, slope = slope, color = "#1FBEC3", lwd = 1) +
  labs(x = "Surface", y = "Log(price)", color = "Living artist")

               
# tabla columna 2
par_slopes <- lm(deaths_per_mil ~ (continent) + median_age,data=datos)

stargazer(par_slopes)

rob_se_par_slopes <- list(sqrt(diag(vcovHC(par_slopes,type='HC1'))))


stargazer(par_slopes ,type="text", digits=3, 
          se=rob_se_par_slopes,omit.stat=c("f","adj.rsq","ser"),
          dep.var.caption="Variable dependiente:",
          report="vc*s",out="par_slopes.tex")



summary(lm(deaths_per_mil ~ factor(continent)*median_age + median_age,data=datos))

modelo <- lm(deaths_per_mil ~  median_age + median_age*continent, data=datos)
interact_plot(modelo,pred=median_age,modx = continent)
interact_plot(modelo,pred=median_age,modx = continent, interval=TRUE)

rob_se_modelo <- list(sqrt(diag(vcovHC(modelo,type='HC1'))))

stargazer(modelo ,type="text", digits=3, 
          se=rob_se_modelo,omit.stat=c("f","adj.rsq","ser"),
          dep.var.caption="Variable dependiente:",
          report="vc*s",out="modelo.tex")

linearHypothesis(modelo,c("median_age:continentAS=0","median_age:continentEU=0"
                          ,"median_age:continentNAM=0","median_age:continentOC=0"
                          ,"median_age:continentSAM=0"), white.adjust = "hc1") 



nam <- lm(deaths_per_mil ~ median_age + I(continent=="NAM") +median_age*I(continent=="NAM"), data=datos)

#names(nam$coefficients) <- c('Constant','median_age', 'continentNAM')

rob_se_nam <- list(sqrt(diag(vcovHC(nam,type='HC1'))))



stargazer(nam ,type="text", digits=3, 
          se=rob_se_nam,omit.stat=c("f","adj.rsq","ser"),
          dep.var.caption="Variable dependiente:",
          report="vc*s",out="nam.tex")




rob_se2 <- list(sqrt(diag(vcovHC(par_slopes,type='HC1'))),
               sqrt(diag(vcovHC(modelo,type='HC1'))),
               sqrt(diag(vcovHC(nam,type='HC1'))))


stargazer(par_slopes,modelo, nam ,type="text", digits=3, 
          se=rob_se2,omit.stat=c("f","adj.rsq","ser"),
          dep.var.caption="Variable dependiente:",
          report="vc*s",out="dummy.tex")


ggplot(aes(x=median_age,y=deaths_per_mil,color=continent),data=datos)+
  geom_point()+
  geom_abline(slope=12.477, intercept=3.304)+
  geom_abline(slope=3.304-10.180, intercept=508.733+12.477,color='red')+
  scale_colour_manual(values=c("black", "black", "black","red","black","black"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")
ggsave("p7b3_dispersion.pdf",width = 6, height = 4.25 ,units="in")


ggplot(aes(x=median_age,y=deaths_per_mil,color=continent),data=datos)+
  geom_smooth(method="lm_robust",formula = y~x, se=TRUE, color='black',fullrange=TRUE )
  


#################################################################### FIN #########################################################
