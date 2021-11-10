# PRUEBA
PRUEBA DE REPOSITORIO
getwd()
setwd("curso_R_basico")
url <- "https://datos.cdmx.gob.mx/dataset/b0d4230e-f37b-463e-8c16-3565aa78cbfc/resource/8b29f1ab-6245-42f1-878b-78e9a4b02374/download/personas_hospitalizadas_con_diagnostico_covid19-series_totales.csv"
download.file(url, destfile = "hospitalizadoscovid19CDMX.csv")
hospitalizados <- read.table("./hospitalizadoscovid19CDMX.csv", sep = ",", header = TRUE)
write.table(hospitalizados, file = "hospitalizadoscovid19CDMX.csv", append = FALSE, quote=TRUE, sep = "", eol= "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE)
##Convirtiendo la variable mes en una variable categórica y ordenando los niveles de acuerdo al orden de los meses 
hospitalizados$mes<-factor(hospitalizados$mes, levels = c("marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre"))
#Modificando el nombre de la columna de los años
names(hospitalizados)<-gsub("aÃ.o","año",names(hospitalizados))

##creando un diagrama de cajas y bigotes de hospitalizados totales en CDMX para cada mes del 2020
hospitalizados2020<-(hospitalizados[which(hospitalizados$año<=2020),])
png("/Users/robertoramirez/curso_R_basico/box.png.png", width = 1000)
boxplot(hospitalizados2020$hospitalizados_totales_cdmx~mes,data=hospitalizados2020, ylim = c(0,10000), col="10", ylab = NULL)#variables categóricas 
title(main = "Edwin Ramírez", ylab="Hospitalizados totales CDMX")
dev.off()
##Creando un histograma de hospitalizados totales CDMX en el 2020
png("/Users/robertoramirez/curso_R_basico/histograma.png")
hospitalizados2020CDMX_T<-hospitalizados2020[,6]
hist(hospitalizados2020CDMX_T,main =NULL, ylim = c(0,100))
abline(v=mean(hospitalizados2020CDMX_T), col="red", lty=2)
title(main="hospitalizados CDMX (Edwin Ramírez)")
dev.off()
##Creando diagrama de dispersión entre las variables: 
#camas intubadas_CDMX y camas generales_CDMX de junio y diciembre  2020
png("/Users/robertoramirez/curso_R_basico/dispersion.png")
hospitalizados2020<-(hospitalizados[which(hospitalizados$año<=2020),])
hospitalizados2020junio_diciembre <- hospitalizados2020[hospitalizados2020$mes %in% c("junio", "diciembre"),]
plot(hospitalizados2020junio_diciembre$camas_intubados_cdmx,hospitalizados2020junio_diciembre$hospitalizados_totales, type = "n", xlab="No. personas intubadas", ylab="hospitalizados_totales")
points(hospitalizados2020junio_diciembre[hospitalizados2020junio_diciembre$mes=="junio","camas_intubados_cdmx"],
       hospitalizados2020junio_diciembre[hospitalizados2020junio_diciembre$mes=="junio", "hospitalizados_totales"], col="blue", pch=20)
points(hospitalizados2020junio_diciembre[hospitalizados2020junio_diciembre$mes=="diciembre", "camas_intubados_cdmx"],
       hospitalizados2020junio_diciembre[hospitalizados2020junio_diciembre$mes=="diciembre","hospitalizados_totales"], col="red", pch=1)
legend("bottomright", legend = c("junio","diciembre"), pch=c(20,1), col=c("blue","red"))
title(main = "CDMX (Edwin Ramírez)")
dev.off()
##Agrando un etiqueta 
hospitalizados$mes<-factor(hospitalizados$mes, levels = c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre"))
png("/Users/robertoramirez/curso_R_basico/etiquetas.png", width = 800)
plot(tapply(hospitalizados[hospitalizados$año=="2021", "hospitalizados_totales_cdmx"], hospitalizados[hospitalizados$año=="2021", "mes"], mean), pch=20, xlab = "meses", ylab = "Prom. Hospitalizados CDMX", ylim=c(0,8000), main= "CDMX (Edwin Ramírez)")
text(c(1,2,3,4,5,6,7,8,9,10,11),c(6986.3226,  5538.3214,  3692.8065,  2271.5667,  1098.354,   657.1333,  1724.3871,  2724.1290,  1627.4000,   753.9630,   416.5714)+200, labels = c("enero","febrero","marzo", "abril", "mayo","junio","julio","agosto","septiembre","octubre","noviembre"))
dev.off()


### gráfica de violín 
install.packages("sm")
library(vioplot)
hospitalizados$mes<-factor(hospitalizados$mes, levels = c("marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre"))
hospitalizados2020<-(hospitalizados[which(hospitalizados$año<=2020),])
hospitalizados2020_CDMX<-hospitalizados2020[,c(3:6)]
hospitalizados2020_CDMX<-hospitalizados2020_CDMX[,-2]
hospitalizados2020_CDMX$mes<-factor(hospitalizados2020_CDMX$mes, levels=c("marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre"))
png("/Users/robertoramirez/curso_R_basico/violin.png", width = 1000)
vioplot(hospitalizados2020_CDMX$hospitalizados_totales_cdmx ~ hospitalizados2020_CDMX$mes, col = 2:length(levels(hospitalizados2020_CDMX$mes)), 
        xlab = "meses", ylab = "hospitalizados totalescdmx")
stripchart(hospitalizados2020_CDMX$hospitalizados_totales_cdmx~ hospitalizados2020_CDMX$mes, add = TRUE, method = "jitter", pch = 20, vertical = TRUE, col = 3:8)
dev.off()

##utilizando el paquete ggplot
hospitalizados$mes<-factor(hospitalizados$mes, levels = c("enero", "febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre"))
install.packages("ggplot2")
library(ggplot2)
png("/Users/robertoramirez/curso_R_basico/ggplot.png", width = 800) 
ggdata2 <- ggplot(data = hospitalizados, aes(mes, camas_intubados_totales,fill=mes))
ggdata2 + geom_boxplot(aes(color=mes),alpha=.4)+facet_grid(.~año)  + xlab("mes") + ylab("camas intubadas")+ggtitle("COVID Edwin Ramírez Benítez") + theme(axis.text.x = element_text(angle = 90))
stripchart(hospitalizados$camas_intubados_totales~hospitalizados$mes, add = TRUE, method = "jitter", pch = 19, vertical = TRUE, col= hospitalizados$mes)
dev.off()

