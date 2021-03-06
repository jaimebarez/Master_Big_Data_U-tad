

datos<-read.csv("eurovision_1998 to 2012.csv",header=TRUE,sep=";",strip.white=TRUE)
datos$duration<-as.numeric(datos$duration)

spain<-subset(datos, as.character(datos$Country)=="Spain")

#7a) Media de las puntuaciones de Espa�a
media_puntuaciones_espana<-mean(spain$Points)
print("Media puntuaciones Spain:")
print(media_puntuaciones_espana)

#7b) Media de las puntuaciones de Espa�a
mediana_puntuaciones_espana<-median(spain$Points)
print("Mediana puntuaciones Spain:")
print(mediana_puntuaciones_espana)

#7c) Boxplot de todas las duraciones
boxplot(datos$duration)

#7d) Cuartiles de las duraciones (cualquiera de las dos soluciones)
quantile(datos$duration, prob = c(0, 0.25, 0.5, 0.75, 1))
summary(datos$duration)

#7e) Evoluci�n de las posiciones en que qued� espa�a
install.packages("googleVis")
library(googleVis)

plot(gvisLineChart(spain,xvar = "Year", yvar = "Place"))

#7f) Frecuencias de las posiciones en que qued� espa�a
freq_esp<-as.data.frame(table(spain$Place))
freq_esp$Var1<-as.numeric(as.character(freq_esp$Var1))

plot(gvisBarChart(freq_esp, xvar = "Var1", yvar = "Freq", 
                  options=list(
                    legend="none",
                    title="Posiciones Spain", vAxis="{title:''}",
                    hAxis="{title:'Numero de veces'}")
))

#7g) Gr�fico de sectores con el g�nero de los artistas de francia
france<-subset(datos, as.character(datos$Country)=="France")
freq_fra<-as.data.frame(table(france$Artist.gender))

freq_fra$Var1<-as.character(freq_fra$Var1)
freq_fra$Var1<-ifelse(freq_fra$Var1=="", "N/A", freq_fra$Var1)
plot(gvisPieChart(freq_fra, labelvar = "Var1", numvar = "Freq"))

#7h) Qu� pa�s es el mejor?

#Entiendo que el mejor pa�s es el que m�s puntos ha conseguido en total en la historia
totales_puntos<-aggregate(datos$Points, by=list(datos$Country),FUN=sum, na.rm=TRUE)
totales_puntos<-totales_puntos[order(totales_puntos$x, decreasing=TRUE),]
ganador<-as.data.frame(head(totales_puntos,1))
ganador$Group.1<-as.character(ganador$Group.1)

print("El ganador es")
print(ganador$Group.1)

print("El top 10 es:")

head(totales_puntos,10)


