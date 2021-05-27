#datos hidrológicos Tarea II

inp <- read.csv("FDC.csv", na.strings ="")

head(inp)
dim(inp)


inp[!complete.cases(inp),]

#newinp <- na.omit(inp)

plot(inp[,2], type = "l", col="blue")
lines(inp[,3], col="green")

summary(inp[,2:3])
hist(inp[,2])
hist(inp[,3])

names(inp) <- c("fecha", "Pandora", "Banano")
attach(inp)
plot(Pandora)

Template <- strptime(inp[,1], format= "%d/%m/%Y")

MAQ_Pandora <- tapply(Pandora, format(Template, format="%Y"), FUN=sum)
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
plot(inp.lm)


