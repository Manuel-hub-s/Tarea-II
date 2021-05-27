#datos hidrológicos Tarea II

inp <- read.csv("FDC.csv", na.strings ="") En esta seccion se busca adjuntar e importar los datos hidrologicos a la plataforma o servidor R. 

head(inp) En esta parte se pide como el limite maximo y el minimo de toda la lista de datos
dim(inp) Reporta la dimension del data frame (en este caso cantidad de datos, 3846 de dos columnas)


inp[!complete.cases(inp),]

#newinp <- na.omit(inp)

plot(inp[,2], type = "l", col="blue")
lines(inp[,3], col="green")
En esta seccion nos grafica los datos de una y la otra columna cada una de un color durante los años o el periodo de tiempo indicado 
summary(inp[,2:3]) resume 
hist(inp[,2]) Nos hace un histograma de los datos para Pandora
hist(inp[,3]) Nos hace un histograma de los datos para Banano

names(inp) <- c("fecha", "Pandora", "Banano")
attach(inp)
plot(Pandora)

Template <- strptime(inp[,1], format= "%d/%m/%Y")
Provee con las fechas el registro de cada dato ordenado segun su antiguedad

MAQ_Pandora <- tapply(Pandora, format(Template, format="%Y"), FUN=sum) Le da formato de lectura en tabla y suma los datos segun cada año para cada columna segun rio Estrella o como se guardo Pandora y el rio banano
MAQ_Banano <- tapply(Banano, format(Template, format="%Y"), FUN=sum)

write.csv(rbind(MAQ_Pandora,MAQ_Banano), file="MAQ.csv") 

plot(MAQ_Banano, ylim=c(100,3000))
lines(MAQ_Pandora, col=2)

MMQ_Pandora <- tapply(Pandora, format(Template, format="%m"), FUN=sum)
MMQ_Banano <- tapply(Banano, format(Template, format="%m"), FUN=sum)

# Análisis de correlacion
corinp <- cor(inp[,2:3],method = "spearman") 



inp.lm <- lm(inp[,2] ~ inp[,3], data=inp)
summary(inp.lm)
plot(inp.lm) Buscamos un patron por lo cual el plot establece un promedio de los datos generados al final de las sumas y dibuja una linea que define ese patron   


