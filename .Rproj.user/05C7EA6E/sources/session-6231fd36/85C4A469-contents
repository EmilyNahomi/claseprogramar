## realizado por Emily Martinez 

## intalar paqueteria
install.packages("tidyverse")
install.packages("gapminder")
install.packages("viridis")
install.packages("ggthemes")
install.packages("datos")
install.packages("skimr")
install.packages("janitor")
install.packages("palmerpenguins")
## cargar librerias 
## para saber en que directorio estoy trabajando
getwd()
setwd("c:\\cursoR")

## para ver los archivos del directorio
list.files()

## para crear un objeto
miprimerobjeto <- 54
miprimerobjeto
misegundoobjeto <- "hola"
misegundoobjeto

## vectores
nombres <- c("Raul", "Diego", "Maria", "Fernanda", "Pablo")
class(nombres)

peso <- c(58, 65, 89, 75, 56)
class(peso)

altura <- c(1.65, 1.45, 1.84, 1.56, 1.75)
class(altura)

imc <- peso/altura^2
imc

## sacar media de peso 
sum(peso)
length(peso)

media_peso <- sum(peso)/length(peso)
media_peso

## data frame 
nombre_altura_peso <- data.frame(nombres, altura, peso, imc)
nombre_altura_peso

## seleccionar elementos de nustra data frame o vectores
peso[4]
peso[-4]
peso[2:4]
peso[c(1,3)]

##secuencia de numeros 
secuencia <- (1:10)
secuencia

##secuencia por rango
seq(1,10, by=0,5)
## ver el numero de filas de data frame
nrow(nombre_altura_peso)

## ver el numero de columnas de df
ncol(nombre_altura_peso)

##llamar base de datos
data("iris")

##dar un vistazo de los primeros 5 observaciones
head(iris)

## ver las ultimas observaciones de 1 df
tail(iris)

## dar vistazo de como estan conformados los datos 
glimpse(iris)

## para ver la base de datos precargados en r
data()

## ver la estructura de la base de datos
str(iris)

## seleccionar elementos 
summary(iris$Sepal.Length)

## ver resultados estadisticos 
summary(largosepalo)
summary(iris)

## operadores aritmedicos 
## suma +
## resta -
## division / 
## multiplicacion *
## esponente ^
## residuo de division %%

o1<- 5+6
11-20
o2<- 2*20
45%%6

o1 + o2

vector1 <- c(4,5,6,6)
vector2 <- c(8,4,6,2)
vector1 + vector2

## operadores booleanos o condicionales 
5==4
5==5
objeto1 <- 5
objeto2 <- 11
objeto1==objeto2
objeto1!=objeto2

## libreria palmerpenguins
## janitor y skinr
data("penguins")
?penguins
skim_without_characts(penguins)
glimpse(penguins)

## atajo para sacar pipeline
## si¿hift + ctrol + m%>%
## seleccionar especies
## pipeline no sirve para concatenar funciones 
penguins %>% select(species)

##solo ver especies 
view(penguins)
unique(penguins$species)
unique(penguins$island)
colname(penguins)
penguins %>%
  distinct(species)

## seleccionar todas las variables menos especies
penguins %>%
  select(-species) %>%
  rename(isla = island)

##libreria janitor
rename_with(penguins, toupper)
rename_with(penguins, tolower)

## funcion para limpiar el nombre de nuetras variables
dataset_limpio<- clean_names(penguins)

penguins %>%
  drop_na()

mayoresa35 <- penguins%>%
  drop_na() %>%
  filter(bill_length_mm>35)
view(mayoresa35)

is.na(mayoresa35)
is.na(penguins)
verperdidos <- sum(is.na(penguins))
verperdidos

##organizar datod por tamaño el largo del pico
penguins %>%
  arrange(bill_length_mm)

## de forma descendente 
penguins %>%
  arrage(- bill_length_mm)

##medidas del largo del pico por isla
penguins %>%
  group_by(island) %>%
  drop_na %>%
  summarize(mean_largopico = mean(bill_length_mm))

penguins %>%
  group_by(island) %>%
  drop_na %>%
  summarize(max_largopico = max(bill_length_mm))

##filtrar datos de especie adeline
soloAdelie <- penguins %>%
  filter(species == "Adelie") %>%
  drop_na()

soloAdelieenDream <- penguins %>%
  filter(species == "Adelie", island=="dream") %>%
  drop_na()

view(pesoenkg)

soloAdelieenDream %>%
  mutate(pesokg= body_mass_g/1000, aleta_m= flipper_length_mm/1000)

## dplyr %>% 
## concatenar funciones 

##filter 
## objeto %>% filter(atributo<=1)

## mutate tranformar nuevas variables de ml a m 
##objeto %>% mutate(atributo_kg=atributo_g/100)
## guardar variable 
## objeto <- objeto %>% mutate(atributo_kg=atributo_g/100)

## group_by agrupar objetos variables categoricas 
## medias, minimos, maximos, conteos
## objeto %>% group_by(atributo) %>% summaries(atriburo_media=mean(atributo))


## data set gapmider
data(gapminder)
?gapminder
skim_without_charts(gapminder)
skim(gapminder)


view(gapminder)

## filtrar informacion del continente Europeo en el año 2007

europa2007 <- gapminder %>%
  filter(continent=="Europe" & year==2007)

view(europa20007)
mean(europa2007$lifeExp)
mean(europa2007$pop)

europa1957 <- gapminder %>% 
  filter(continent=="Europe" & year==1957)

view(europa1957)
mean(europa1957$lifeExp)
mean(europa1957$pop)

europa_asia <- gapminder %>% 
  filter(continent=="Europe" | continent=="Asia")

summary(europa_asia)

##funcion mutate 
gapminder %>%  mutate(gdp=gdpPercap*pop)

##saber el gdp con mas de 800
gapminder %>% 
  filter(gdpPercap>800) %>% 
  mutate(gpp=gdpPercap*pop)

summary(gpp800)

##funcion group_by
gapminder %>% 
  filter(continent=="Africa") %>% 
  group_by(year) %>% 
  summary(exp_mean=mean(lifeExp, na.rm = T))

##agrupas por pais 
gapminder %>% 
  filter(continent=="Africa") %>% 
  group_by(country) %>% 
  summary(exp_mean=mean(lifeExp, na.rm = T)) %>% 
  arrange(desc(exp_mean))
  
##agregar la mediana
gapminder %>% 
  filter(continent=="Africa") %>% 
  group_by(country) %>% 
  summary(exp_mean=mean(lifeExp, na.rm = T), exp_median(lifeExp, na.rm = T)) %>% 
  arrange(desc(exp_mean))

## agregar la desviacion estandar 
gapminder %>% 
  filter(continent=="Africa") %>% 
  group_by(country) %>% 
  summary(exp_mean=mean(lifeExp, na.rm = T), exp_median(lifeExp, na.rm = T), exp_sd=sd(lifeExp, na.rmT)) %>% 
  arrange(desc(exp_mean))

## funcion arrange
gapminder %>% 
  filter(continent=="Africa") %>% 
  group_by(country) %>% 
  summary(exp_mean=mean(lifeExp, na.rm = T), exp_median(lifeExp, na.rm = T), exp_sd=sd(lifeExp, na.rmT)) %>% 
  arrange(desc(exp_mean)) %>% 
  arrange(desc(exp_mean))
  
## se puede guardar en un objeto 
exp_africa <- gapminder %>% 
  filter(continent=="Africa") %>% 
  group_by(country) %>% 
  summary(exp_mean=mean(lifeExp, na.rm = T), exp_median(lifeExp, na.rm = T), exp_sd=sd(lifeExp, na.rmT)) %>% 
  arrange(desc(exp_mean)) %>% 
  arrange(desc(exp_mean))

## obtener directorio de trabajo
getwd()

## para guardar set de datos 
write.csv(exp_africa, "expectativaafrica.csv")

## para leer una base de datos
read.csv()

## graficos basicos 
## histograma
hist(gapminder$pop)
hist(gapminder$lifeExp)

## poner color o etiquetas 
hist(gapminder$lifeExp, col = "blue", xlab = "Expectativa de vida (años)",ylab = "Frecuencia", main="Histograma de expectativa de vida")

plot(gapminder$gdpPercap, gapminder$lifeExp)

##grafica en ggplot
ggplot(gapminder, aes(x=gdpPercap, y=lifeExp))+ geom_point()+
  ylab("Expectativa de vida")+ xlab("dgp per capita")

## con logaritmo
ggplot(gapminder, aes(x=log(dpPercap, y=lifeExp))+ geom_point()+
  ylab("Expectativa de vida")+ xlab("dgp per capita")

## con scale 
ggplot(gapminder, aes(x=gdpPercap, y=lifeExp))+
  geom_point()+
  ylab("Expectativa de vida")+ xlab("dgp per capita")+
  scale_x_log10()

## para agregar temas
## theme_minimal
## theme_ligth
## theme_gray
## theme _bw
## theme_linedraw

ggplot(gapminder, aes(x=gdpPercap, y=lifeExp))+
  geom_point()+
  ylab("Expectativa de vida")+ xlab("dgp per capita")+
  scale_x_log10()+ theme_classic()+
  ggtitle("Relacion entre GDP per capita y expectativa de vida")

## filtrado por continente 
gapminder %>% filter(continent=="Americas") %>% 
  ggplot(aes(x=gdpPercap, y=lifeExp))+
  geom_point()+
  ylab("Expectativa de vida")+ xlab("gdp per capita")+
  scale_x_log10()+ theme_classic()+
  ggtitle("Relacion entre GDP per capita y expectativa de vida ")

##tamaño de poblacion
gapminder %>% filter(continent=="Americas") %>% 
  ggplot(aes(x=gdpPercap, y=lifeExp, size=pop))+
  geom_point()+
  ylab("Expectativa de vida")+ xlab("gdp per capita")+
  scale_x_log10()+ theme_classic()+
  ggtitle("Relacion entre GDP per capita y expectativa de vida ")

## ver pais por color 
gapminder %>% filter(continent=="Americas") %>% 
  ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, color=country))+
  geom_point()+
  ylab("Expectativa de vida")+ xlab("gdp per capita")+
  scale_x_log10()+ theme_classic()+
  ggtitle("Relacion entre GDP per capita y expectativa de vida ")

## filtrar por año 
gapminder %>% filter(continent=="Americas" & year==2007) %>% 
  ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, color=country))+
  geom_point()+
  ylab("Expectativa de vida")+ xlab("gdp per capita")+
  scale_x_log10()+ theme_classic()+
  ggtitle("Relacion entre GDP per capita y expectativa de vida ")

##
gapminder %>% filter(continent=="Americas") %>% 
  ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, color=country))+
  geom_point()+
  ylab("Expectativa de vida")+ xlab("gdp per capita")+
  scale_x_log10()+ theme_classic()+
  facet_wrap(~year)+
  ggtitle("Relacion entre GDP per capita y expectativa de vida ")

## expectativas de vida por continente del año 2007
gapminder %>% 
  filter(year==2007) %>% 
  ggplot(aes(x=continent, lifeExp, fill_continent))+
  geom_boxplot()+
  theme_classic()+
  ylab("Expectativa de vida")+ xlab("gdp per capita")+
  labs(fills="Continente")
  
## expectativa de vida por año en asia
gapminder %>% 
  filter(continent=="Asia") %>% 
  mutate(year1=as.factor(year)) %>% 
  ggplot(aes(x=year1, lifeExp, fill=year))+
  geom_boxplot()+
  theme_classic()+
  ylab("Expectativa de vida")+ xlab("gdp per capita")+
  labs(fills="Continente")

##sacar las medias 
gapminder %>% 
  filter(continent=="Asia") %>% 
  group_by(year) %>% summarise(mean_lifeExp=mean(lifeExp, na.rm = T)) %>% 
  ggplot(aes(x=year,y=mean_lifeExp))+geom_point()+geom_line()
  theme_economist_white()+ylab("espectativa de vida")+xlab("Año")
  ggtitle("Cambios de la expectativa de vida por año")+
  labs(subtitle = "Datos de gapminder",
       caption)
  
  
  
  
  
  