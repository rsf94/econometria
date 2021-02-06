rm(list = ls()) #borramos memoria
library('dplyr')
library('tidyverse')
library('ggplot2')    # para gráficas
library('scales')     # para alterar escala del eje x en histograma


# DIRECTORIO

setwd("C:/Users/Rafael/Google Drive/MAESTRÍA ITAM/Econometría Aplicada/Tareas/Tarea1")


#####################################################################################################################
#PREGUNTA 1 #########################################################################################################
#####################################################################################################################

# BASE DE DATOS
datam <- read.csv(file='BaseCOVIDm.csv')

# 1.  CALCULAR NUEVA VARIABLE: TASA DE POSITIVIDAD = pos
datam <-mutate(datam,pos=Confirmed/Tests)
hist(datam$pos)

##################### a. Intervalos de confianza

media_pos <- mean(datam$pos, na.rm = TRUE)
varianza_pos <- var(datam$pos, na.rm = TRUE)


# Intervalo de Confianza para la media al 95%
n <- length(datam$pos[!is.na(datam$pos)]) #solo considero observaciones con valores, quité NA's
s <- sqrt(varianza_pos)
error <- qnorm(0.975)*s/sqrt(n) #ojo el IC es al 95% por lo que en cada cola queremos 2.5% de probabilidad

left <- media_pos-error
right <- media_pos+error

c(left,right)


##################### b. Prueba de Hipotesis para pos

pos_mex <- subset(datam,Country == "Mexico", select=c(pos))

t = sqrt(n)*(media_pos-pos_mex)/s
pnorm(as.matrix(t)) # Valor-p


##################### c. Prueba de Hipotesis para cfr

#CALCULAR NUEVA VARIABLE: TASA DE FATALIDAD
datam <-mutate(datam,cfr=Deaths/Confirmed)

n2 <- length(datam$cfr) 
media_cfr <- mean(datam$cfr, na.rm = TRUE)
varianza_cfr <- var(datam$cfr, na.rm = TRUE)
s_cfr <- sqrt(varianza_cfr)

cfr_sars2 = 0.096/4
#Valor t
t2 = sqrt(n2)*(media_cfr-cfr_sars2)/s_cfr
t2

2*(1-pnorm(as.matrix(t2))) # Valor-p

##################### d. Relación prueba hipótesis con IC

error2 <- qnorm(0.9954)*s_cfr/sqrt(n2) #ojo el IC es al 95% por lo que en cada cola queremos 2.5% de probabilidad
left2 <- media_cfr-error2
right2 <- media_cfr+error2

c(left2,right2) # es el intervalo de confianza

#####################################################################################################################
#PREGUNTA 2 #########################################################################################################
#####################################################################################################################

#rm(list = ls()) #borramos memoria
data <- read.csv(file='BaseCOVIDp.csv')      # cargamos Covid POBLACION
data <-mutate(data,ppi=Confirmed/Population) #crear variable PPI 

##################### a. Histograma PPI COVIDp

q<- quantile(data$ppi)
cuartil_pob<-q[2]

# gráfica histograma
ggplot(data, aes(x=ppi)) +
  geom_histogram(binwidth=0.005,alpha=0.9,col = 'black') + 
    labs(title="PPI COVIDp",x="PPI",y="Frecuencia")+
    theme(plot.title = element_text(hjust = 0.5))+
    theme_economist_white()+
    theme(plot.title = element_text(hjust = 0.5, vjust=2.25))+
  geom_vline(xintercept = cuartil_pob,size=1,colour="red", linetype = "dashed")+
    geom_text(aes(x=cuartil_pob, label="Q1", y=125), colour="red", vjust = -1, hjust = 1.2)
  
  
##################### b. Histograma PPI COVIDm

datam <- read.csv(file='BaseCOVIDm.csv') #cargamos covid muestra
datam <-mutate(datam,ppi=Confirmed/Population) #calculamos variable

q<- quantile(datam$ppi)
cuartil_muestra<- q[2]

# gráfica histograma
ggplot(datam, aes(x=ppi)) +
  geom_histogram(binwidth=0.005,alpha=0.9,col = 'black') + 
  labs(title="PPI COVIDm",x="PPI",y="Frecuencia")+
  theme_economist_white()+
  theme(plot.title = element_text(hjust = 0.5, vjust=2.25))+
  geom_vline(xintercept = cuartil_pob,size=1,colour="red", linetype = "dashed")+
  geom_text(aes(x=cuartil_pob, label="Q1", y=62), colour="red", vjust = -1, hjust = 1.2)


##################### c. Comparación 2 histogramas anteriores

data <- mutate(data,ppi2=ppi*100)
datam<- mutate(datam, ppi2=ppi*100)


#comparar frecuencias relativas
ggplot() + 
  geom_histogram(aes(x=ppi, y=(..count..)/sum(..count..), fill="r", colour="r"), alpha=.4, data=data, stat = "bin", binwidth=0.005) +
  geom_histogram(aes(x=ppi,y=(..count..)/sum(..count..), fill="b", colour="b"), alpha=.4, data=datam, stat = "bin", binwidth=0.005) +
  scale_colour_manual(name="", values=c("r" = "red", "b"="blue"), labels=c("b"="muestra n=100", "r"="población n=182")) +
  scale_fill_manual(name="", values=c("r" = "red", "b"="blue"), labels=c("b"="muestra n=100", "r"="población n=182"))+
  labs(title="Comparación",x="PPI",y="frecuencia relativa")+
  theme(plot.title = element_text(hjust = 0.5,vjust=2.25), legend.position = "right")+
  theme_economist_white()


##################### d. Bootstrap de primer cuartil con n=100
  
#rm(list = ls()) #borrar memoria
datam <- read.csv(file='BaseCOVIDm.csv') #cargar basecovidm
datam <-mutate(datam,ppi=(Confirmed/Population))


# Indicamos algoritmo aleatorio y creamos vector de 1,000 filas
set.seed(5) #para poder replicar resultados
q1 <- matrix(nrow=1000,ncol=1) # matriz vacia de 1000 x 1 (tomaré 1,000 muestras de tamaño n=100 y a cada una le calculo el Q1)

# loop para muestrear y va llenando cada fila del vector con el quantile de la muestra
for (i in 1:1000) {
  x <- sample(sample(datam$ppi,100, replace = TRUE))
  q1[i] <- quantile(x,0.25)
}

#para que ggplot pueda leer un dataframe
q1.df <- as.data.frame(q1) 

# graficamos histograma de los 1,000 primeros cuartiles
ggplot(q1.df,aes(x=V1))+
  geom_histogram(binwidth=0.00005,col="black")+
  labs(title="Q1 (1,000 muestras de tamaño n=100) ",x="Q1 PPI",y="Frecuencia")+
  scale_x_continuous(label=number,name = "Q1 PPI",breaks = seq(0,0.0007,0.0001)) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme_economist_white()+
  theme(plot.title = element_text(hjust = 0.5, vjust=2.25))


##################### e. Bootstrap de primer cuartil con n=70

set.seed(5) #para poder replicar resultados
q1s <- matrix(nrow=1000,ncol=1) # matriz vacia de 1000 x 1 (tomaré 1,000 muestras de tamaño n=100 y a cada una le calculo el Q1)

for (i in 1:1000) {
  x <- sample(sample(datam$ppi,70, replace = TRUE))
  q1s[i] <- quantile(x,0.25)
}


# data frame de 1 columna conteniendo el Q1 de c/u de las 1000 muestras
q1.dfs <- as.data.frame(q1s) #para que ggplot pueda leer un dataframe

# graficamos histograma de los 1,000 primeros cuartiles
ggplot(q1.dfs,aes(x=V1))+
  geom_histogram(binwidth=0.00005,col="black",fill='light blue')+
  labs(title="Q1 (1,000 muestras de tamaño n=70) ",x="Q1 PPI",y="Frecuencia")+
  scale_x_continuous(label=number,name = "Q1 PPI", breaks = seq(0,0.0007,0.0001)) +
  theme(plot.title = element_text(hjust = 0.5),panel.grid.minor=element_blank())+
  theme_economist_white()+
  theme(plot.title = element_text(hjust = 0.5, vjust=2.25))

##################### e2. Comparamos ambos histogramas del cuartil

# creamos un data frame con ambos submuestros de 1,000 repeticiones
# V1 es el de n=100 y V2 es el de n=70

df_comparar <- data.frame(q1.df,q1.dfs)
names(df_comparar) <- c("V1","V2")

ggplot() + 
  geom_histogram(aes(x=V1, fill="r", colour="r"), alpha=.4, data=df_comparar, stat = "bin", binwidth=0.00005) +
  geom_histogram(aes(x=V2, fill="b", colour="b"), alpha=.4, data=df_comparar, stat = "bin", binwidth=0.00005) +
  scale_x_continuous(label=number,name = "Q1 PPI", breaks = seq(0,0.0007,0.0001)) +
  scale_colour_manual(name="Tamaño submuestra", values=c("r" = "red", "b"="blue"), labels=c("b"="n=70", "r"="muestra n=100")) +
  scale_fill_manual(name="Tamaño submuestra", values=c("r" = "red", "b"="blue"), labels=c("b"="n=70", "r"="muestra n=100"))+
  labs(title="Q1 PPI (1,000 submuestras)",x="PPI",y="Frecuencia ")+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "right")+
  theme_economist_white()+
  theme(plot.title = element_text(hjust = 0.5, vjust=2.25))

##################### f. Intervalo de confianza bootstrap
# cuartil de la muestra CovidM (tomada de la población)
q<- quantile(datam$ppi)
cuartil_muestra<- q[2]

# Cuartil de la población CovidP
q<- quantile(data$ppi)
cuartil_pob<-q[2]


# Para n=100
# vector con valor de cuartil_muestra repetido 1,000 veces (n=100)
qm<- matrix(cuartil_muestra[1],nrow=1000,ncol=1)

# Intervalo de confianza con bootstrap
q1.df # es el vector de medianas de cada muestra (c/u de las 1,000)

se_q1 <- sqrt(var(q1.df$V1)*999/1000) #ajuste para que sea varianza poblacional y divida entre n, y no n-1

error <- qnorm(0.995)*se_q1 #ojo el IC es al 99% por lo que en cada cola queremos 0.5% de probabilidad

IC_median_left = cuartil_muestra - error
IC_median_right = cuartil_muestra + error

c(IC_median_left, IC_median_right)


#ahora para n=70
#q1.dfs es el vector de medianas de cada muestra (c/u de las 1,000 con n=70)

se_q1s <- sqrt(var(q1.dfs$V1)*(999/1000)*(70/100)) # el 999/100 es para tener la varianza poblacinoal y el 70/100 el ajuste porque n=70

error2 <- qnorm(0.995)*se_q1s #ojo el IC es al 99% por lo que en cada cola queremos 0.5% de probabilidad

IC_median_leftb = cuartil_muestra - error2
IC_median_rightb = cuartil_muestra + error2

c(IC_median_leftb, IC_median_rightb)


#Ahora construimos la matriz que contenga info relevante para hacer la gráfica comparando los IC

barras <- data.frame("muestra" = c("n = 100","n = 70"), "cuartil"=c(cuartil_pob,cuartil_pob) ,"left" = c(IC_median_left, IC_median_leftb),"right" = c(IC_median_right, IC_median_rightb))


# gráfica de IC 
ggplot(data = barras, aes(x = muestra, y = cuartil)) +
  geom_errorbar(aes(ymin = left, ymax = right), width = 0.05, size  = 0.75) +
  geom_point(shape = 18, size  = 4) +
  theme_bw() +
  theme(axis.title = element_text(face  = "bold")) +
  labs(title = "IC de Q1(PPI)",x="Tamaño de submuestras",y="Q1 ")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label = scales::percent_format(accuracy=0.0001)(right), y = right), vjust = -0.5)+
  geom_text(aes(label = scales::percent_format(accuracy=0.0001)(left), y = left), vjust = +1.25)


# misma gráfica PERO horizontal # poner esta en LaTeX
ggplot(data = barras, aes(x = cuartil, y = muestra)) +
  geom_errorbarh(aes(xmin = left, xmax = right), height=0.5,size=1) +
  geom_point(shape = 17, size  = 4, color = "red") +
  theme_bw() +
  theme(axis.title = element_text(face  = "bold")) +
  labs(title = "Intervalo de confianza de Q1(PPI)",x="Q1",y="Tamaño de submuestras ")+
  scale_x_continuous(labels = scales::number_format(accuracy = 0.000001))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label = scales::number_format(accuracy=0.000001)(right), x = right), hjust = .80, vjust=-5)+
  geom_text(aes(label = scales::number_format(accuracy=0.000001)(left), x = left), hjust = +.35, vjust=-5)+
  geom_text(aes(label = scales::number_format(accuracy=0.000001)(cuartil), x = cuartil), vjust=-2,color="red")+
  theme_economist_white()+
  theme(plot.title = element_text(hjust = 0.5, vjust=2.25))

# Sin suponer normalidad:

# para n=100

quantile(q1.df$V1,0.005)
quantile(q1.df$V1,0.995)

quantile(q1.dfs$V1,0.005)
quantile(q1.dfs$V1,0.995)

barras2 <- data.frame("muestra" = c("n = 100","n = 70"), "cuartil"=c(cuartil_pob,cuartil_pob) ,"left" = c(quantile(q1.df$V1,0.005), quantile(q1.dfs$V1,0.005)),"right" = c(quantile(q1.df$V1,0.995), quantile(q1.dfs$V1,0.995)))

ggplot(data = barras2, aes(x = cuartil, y = muestra)) +
  geom_errorbarh(aes(xmin = left, xmax = right), height=0.5,size=1) +
  geom_point(shape = 17, size  = 4, color = "red") +
  theme_bw() +
  theme(axis.title = element_text(face  = "bold")) +
  labs(title = "Intervalo de confianza de Q1(PPI)",subtitle="sin suponer normalidad" ,x="Q1",y="Tamaño de submuestras ")+
  scale_x_continuous(labels = scales::number_format(accuracy = 0.000001))+
  geom_text(aes(label = scales::number_format(accuracy=0.000001)(right), x = right), hjust = .80, vjust=-5)+
  geom_text(aes(label = scales::number_format(accuracy=0.000001)(left), x = left), hjust = +.35, vjust=-5)+
  geom_text(aes(label = scales::number_format(accuracy=0.000001)(cuartil), x = cuartil), vjust=-2,color="red")+
  theme_economist_white()+
  theme(plot.title = element_text(hjust = 0.5, vjust=2.25),plot.subtitle = element_text(hjust = 0.5,vjust = 2))

#ggsave("Rplot08b.png", width = 21.16, height = 15.34, units = "cm") # para guardar gráficas



#####################################################################################################################
#PREGUNTA 3 #########################################################################################################
#####################################################################################################################

##################### a. Diagrama de dispersión (BaseCOVIDm guardada como datam)

datam <- read.csv(file='BaseCOVIDm.csv')
view(datam)

datam <- mutate(datam,pt=Tests/Population, ppi=Confirmed/Population)
n <- length(datam$pt[!is.na(datam$pt)]) #solo considero observaciones con valores, quité NA's

ggplot(datam, aes(x=pt, y=ppi)) +
  geom_point(size=2)+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Diagrama de dispersión",x="PT",y="PPI")+
  theme_economist_white()+
  theme(plot.title = element_text(hjust = 0.5, vjust=2.25))



##################### b. Histograma
pt_mex <- subset(datam,Country == "Mexico", select=c(pt))*1
ppi_mex <- subset(datam, Country == "Mexico", select=c(ppi))

datam_filter <- datam %>% filter(between(pt,pt_mex-0.005,pt_mex+0.005))


pt_mex<- as.matrix(pt_mex)
ppi_mex<- as.matrix(ppi_mex)

ggplot(datam_filter, aes(x=ppi)) +
  geom_histogram(binwidth=0.0005,alpha=0.9,col='black') + 
  labs(title="PPI",x="PPI",y="Frecuencia")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="PPI ",x="PPI",y="Frecuencia")+
  scale_x_continuous(breaks = seq(0, 0.0055, 0.0005), labels = scales::number_format(accuracy=0.0001))+
  geom_vline(xintercept=ppi_mex, size = 1, linetype = "dashed",colour="red")+
  geom_text(aes(x=ppi_mex, label="México: 0.0032", y=3.5), colour="red", angle=90, vjust = -1)+
  geom_vline(xintercept = mean(datam_filter$ppi),size=1,colour="black")+
  geom_text(aes(x=mean(ppi), label="Media: 0.0011", y=3.5), colour="black", angle=90, vjust = -1)+
  scale_y_continuous(limits = c(0,4.2),expand = c(0,0))+
  theme_economist_white()+
  theme(plot.title = element_text(hjust = 0.5, vjust=2.25))
  
##################### b. Regresión MCO

mod <- lm(ppi ~ pt, data = datam)
mod

datam_mexico <- datam %>% filter(Country == "Mexico")


ggplot()+
  geom_point(data = datam,size=2,aes(x=pt, y=ppi), color='blue')+
  geom_point(data= datam_mexico, aes(x=pt,y=ppi),color='red', size = 4)+
  geom_smooth(data=datam,aes(x=pt, y=ppi), method='lm', formula = y~x, se=FALSE, color='black' )+
  labs(title="Recta de ajuste por MCO",x="PT",y="PPI")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(data= datam_mexico,aes(x=pt, y=ppi, label=Country),hjust=0.2, vjust=-0.9,color='red')+
  theme_economist_white()  

ggplot()+
  geom_point(data = datam,size=2,aes(x=pt, y=ppi))+
  geom_point(data= datam_mexico, aes(x=pt,y=ppi),color='red', size = 4)+
  geom_smooth(data=datam,aes(x=pt, y=ppi), method='lm', formula = y~x, se=FALSE, color='blue', linetype='dashed' )+
  labs(title="Recta de ajuste por MCO",x="PT",y="PPI")+
  geom_text(data= datam_mexico,aes(x=pt, y=ppi, label=Country),hjust=0.25, vjust=-1.4,color='red',angle = 50)+
  theme_economist_white() +
  theme(plot.title = element_text(hjust = 0.5, vjust=2.25))
  
#################################################################### FIN #########################################################
