library(readxl)


iris <- read_excel("iris.xls")
alimentos <- read_excel("alimentos.xls")
trigo <- read_excel("trigo.xls")
carros <- read_excel("carros.xls")

### numéricos

iris1 <- iris[,2:5]
iris1

alimentos1 <- alimentos[,3:7]
alimentos1

trigo1 <- trigo[,3:10]
trigo1

carros1 <- carros[3:8]
carros1


####padronizados

irisp <- scale(iris1)
irisp

alimentosp <- scale(alimentos1)
alimentosp

trigop <- scale(trigo1)
trigop

carrosp <- scale(carros1)
carrosp

help(hclust)



### categorizados
trigo$Localidade <- as.factor(trigo$Localidade)

### ANÁLISE DE AGRUPAMENTOS


library(cluster)


### MÉTODOS HIERÁRQUICOS - 

d<-dist(carrosp,method = "euclidean")

#Métodos
#euclidean
#maximum
#manhattan
#canberra
#binary
#minkowski

d


hc<-hclust(d,method="complete")
hw<-hclust(d,method="ward.D")
#Métodos
#ward.D
#ward.D2
#single
#complete
#average
#mcquitty
#median
#centroid

hc
hw
hce
#summary(hc)


#dendrograma

plot(hc, labels = carros$Nome)

#plot(hang=-1) - muda a escala
plot(hc, hang=-1, labels = carros$Nome)
plot(hw, hang=-1, labels = carros$Nome)
plot(hce, hang=-1, labels = carros$Nome)

library(NbClust)
help(NbClust)

NbClust(data=carrosp, min.nc=2, max.nc=5, index="all", method="ward.D")
NbClust(data=carrosp, min.nc=2, max.nc=5, index="all", method="complete")

grupo <- cutree(hc,2)
grupo
#mostra em que cluster estão as unidades

hc$merge
#mostra as junções dos grupos
#(sinal "-" indica unidade - sem sinal indica grupo)

hc$height
#nível das junções

hc$order
#mostra a ordem para fazer dendrograma

hc$labels
#mostra rótulos das unidades

hc$call
#mostra a função do R

hw$method
#mostra o método de agrupamento utilizado

hw$dist.method
#mostra a distância utilizada

d.coph <- cophenetic(hc)
cor(d, d.coph)
#calcula a correlação cofenética


silhuetac <- silhouette(cutree(hc,2), d)
silhuetac
plot(silhuetac)

dados <- data.frame(carros, grupo)
dados

tapply(dados$cilindrada, dados$grupo, summary)
tapply(dados$desempenho, dados$grupo, summary)
tapply(dados$consumo, dados$grupo, summary)
tapply(dados$autonomia, dados$grupo, summary)
tapply(dados$potencia, dados$grupo, summary)
tapply(dados$aceleracao, dados$grupo, summary)


library(lattice)


# todos os grupos em um mesmo painel com cores diferentes
help("xyplot")
xyplot(consumo ~ potencia, groups=grupo, data = dados, col = c("green","black"))
xyplot(aceleracao ~ cilindrada, groups=grupo, data = dados, col = c("green","black"))
xyplot(Caloria ~ Gordura, groups=grupo, data = dados)
plot(alimentos1, col=dados$grupo)

### K-MÉDIAS para 3 grupos

dt<-dist(irisp,method = "euclidean")
dt

trigod <-as.data.frame(trigop)




library(factoextra)
library(ggplot2)
fviz_nbclust(irisp, kmeans, method="wss")+
  geom_vline(xintercept=3, linetype=2)



km<-kmeans(irisp,3)

km

summary(km)

kmc <- km$cluster
kmc
#mostra em que cluster estão as unidades

dados1 <- data.frame(iris, kmc)
dados1

km$centers
#mostra as médias dos grupos por variável

km$withinss
#mostra as somas de quadrados dentro dos grupos

km$size
#mostra os tamanhos dos grupos

plot(iris, col = kmc)
plot(iris1, col = kmc)
#plota os dados usando as duas primeiras variáveis (col indica que as unidades
#serão coloridas segundo o grupo)


points(km$centers, col = 1:3, pch = 8, cex=2)
#plota o centro dos grupos


plot(x=trigo$Area_a, y=trigo$Area_b, col = km$cluster)


silhuetak <- silhouette(kmc, dt)
silhuetak
plot(silhuetak)




tapply(dados1$CS, dados1$kmc, summary)
tapply(dados1$LS, dados1$kmc, summary)
tapply(dados1$CP, dados1$kmc, summary)
tapply(dados1$LP, dados1$kmc, summary)








