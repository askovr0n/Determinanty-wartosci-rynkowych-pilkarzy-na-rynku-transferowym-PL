library(lmtest)
library(tseries)
library(car)
library(stargazer)
library(dplyr)

regres<-lm(formula = value~age+rating_fifa21+height+contract_left+instagram_followers+europe+south_america+twolegs+leftleg+striker+midfielder+defender, data=model)
summary(regres)

#model istotny, b0, wiek, rating_fifa21, instagram followers istotne
#ale najpierw zrobmy sobie funkcj avPlots sprawdzenie overfitting/underfitting

avPlots(regres)

# Pora na testy

#1. Test na forme funkcyjna

resettest(regres, power=2:3, type="fitted")
#F=19.398 pvalue=0.0000 wiec odrzucam hipoteze h0

resettest(regres, power=2:3, type="regressor")
# w tym przypadku F=2.5064 i pvalue=0. wiec tez odrzucam

#2. Homoskedastycznosc (homogenicznosc wariancji)

# narysuje sobie najpierw jak to wyglada

plot(regres, 3)  #residua powinny być równomiernie rozlozone
bptest(regres)
#wyszlo mi BP=25.438 i pvalue=0.0004 wiec też odrzucam h0

#3. Test na normalnosc reszt

jarque.bera.test(regres$residuals)
#X squared=37.572 a pvalue=6.941e-09 wiec odrzucam h0

#4. Autokorelacja reszt

dwtest(regres)
#DW=1.331 pvalue=1.2288e-05 wiec musze odrzucic h0

qqPlot(regres$residuals, main="QQ plot autocorrelation")

##############

##W zwiazku z tym ze nie mam homoskedastycznosci postanowilem dodac log do price
regreslog<-lm(formula = log(value)~age+rating_fifa21+height+contract_left+instagram_followers+europe+south_america+twolegs+leftleg+striker+midfielder+defender, data=model)
summary(regreslog)

#1. Test na forme funkcyjna

resettest(regreslog, power=2:3, type="fitted")
#F=1.7905 pvalue=0.1708 wiec nie ma podstaw do odrzucenia h0
resettest(regreslog, power=2:3, type="regressor")
#F=1.2668, pvalue=0.2037 ponownie brak podstaw do odrzucenia h0 czyli mamy prawidlowa forme funkcyjna

#2.Homoskedastycznosc

bptest(regreslog)
#BP=18.046 oraz pvalue=0.1143 czyli brak podstaw do odrzucenia h0

plot(regreslog, 3)
#widac ze jest homoskedastycznosc bo z wykresu obrazujacego reszty tez widac homoskedastycznosc, bo zaburzenia losowe wydaja sie byc jednakowo rozproszeone wokol wartosci zerowej

#3. Test na normalnosc reszt

jarque.bera.test(regreslog$residuals)
#Xsquared=7.9341 pvalue=0.01893, wiec h0 trzeba odrzucic

#4 Autokorelacja

dwtest(regreslog)
#DW=1.3285 pvalue=0.00001135 wiec h0 musze odrzucic
qqPlot(regreslog$residuals, main="QQ plot autocorrelation")

######################

####spróbujmy zatem dac wiek^2, a potem jesli dalej nie pyknie to dodac zmienna striker_europe

regreslogage2<-lm(formula = log(value)~age2+rating_fifa21+height+contract_left+instagram_followers+europe+south_america+twolegs+leftleg+striker+midfielder+defender, data=addedage2)

#sposob drugi, czyli mam age i age2 (ale wtedy cienko to wyglada przy pvalue)
regreslogageage2<-lm(formula = log(value)~age+age2+rating_fifa21+height+contract_left+instagram_followers+europe+south_america+twolegs+leftleg+striker+midfielder+defender, data=addedage2)
#######

#zajme sie tym pierwszym czyli regreslogage2

#1. Test na forme funkcyjna
resettest(regreslogage2, power=2:3, type="fitted")
#RESET=2.2688 pvalue=0.1074 brak podstaw do odrzucenia h0

resettest(regreslog, power=2:3, type="regressor")
#RESET=1.2688 pvalue=0.2037 to samo-brak podstaw do odrzucenia h0

#2. Homoskedastycznosc

bptest(regreslogage2)
#BP=18.84 pvalue=0.09245 wiec brak podstaw do odrzucenia h0

plot(regreslogage2, 3)
#widac ze jest homoskedastycznosc bo z wykresu obrazujacego reszty tez widac homoskedastycznosc, bo zaburzenia losowe wydaja sie byc jednakowo rozproszeone wokol wartosci zerowej

#3. Test na normalnosc reszt

jarque.bera.test(regreslogage2$residuals)
#Xsquared=8.9319 pvalue=0.01149 brak normalnosci reszt

#4 Autokorelacja

dwtest(regreslogage2)
#DW=1.3251 pvalue=0.00001032 czyli musze odrzucic h0

########
#Dodalem jeszcze zmienna zlozona europe i striker jesli tak to 1, 0 w.p.p
regreseustr<-lm(formula = log(value)~age+rating_fifa21+height+contract_left+instagram_followers+europe+south_america+twolegs+leftleg+striker+midfielder+defender+europe_striker, data=europeage)

#1. Test na forme funkcyjna

resettest(regreseustr, power=2:3, type="fitted")
#RESET=1.8763 pvalue=0.1572 czyli h0 nie odrzucam

resettest(regreseustr, power=2:3, type="regressor")
#RESET=1.1424 pvalue=0.3088
#tak samo, nie odrzucam

#2.Homoskedastycznosc

bptest(regreseustr)
#BP=20.368 i pvalue=0.0864 nie odrzucam h0

#3. Test na normalnosc reszt

jarque.bera.test(regreseustr$residuals)
#Xsquared=8.4998 pvalue=0.01427 wiec h0 odrzucam

#4 Autokorelacja

dwtest(regreseustr)
#DW=1.3277 pvalue=0.00001086 h0 odrzucam

europeage<-mutate(europeage, age2=age^2)

#model z dodatkowymi zmiennymi
regres<-lm(formula = value ~ age + rating_fifa21 + height + contract_left + 
             instagram_followers + europe + south_america + twolegs + 
             leftleg + striker + midfielder + defender + goals+europe_striker, data = model1)
regres1<-lm(formula = log(value)~age+rating_fifa21+height+contract_left+instagram_followers+europe+south_america+twolegs+leftleg+striker+midfielder+defender+goals+europe_striker, data=model1)
regres2<-lm(formula = value~age+age2+rating_fifa21+height+contract_left+instagram_followers+europe+south_america+twolegs+leftleg+striker+midfielder+defender+goals+europe_striker, data=model1)
regres3<-lm(formula = log(value)~age+age2+rating_fifa21+height+contract_left+instagram_followers+europe+south_america+twolegs+leftleg+striker+midfielder+defender+goals+europe_striker, data=model1)
regres4<-lm(formula = log(value)~age+age2+rating_fifa21+height+contract_left+log1p(instagram_followers)+europe+south_america+twolegs+leftleg+striker+midfielder+defender+goals+europe_striker, data=model1)
regost<-lm(formula = log(value)~age+rating_fifa21+log1p(instagram_followers), data=model1)
regostg<-lm(formula = log(value)~age2+rating_fifa21+log1p(instagram_followers)+goals, data=model1)
#######################

#Zajmuje sie usuwaniem zmiennych nieistotnych- model z log(value), age,age^2 i log(instagram)
reg5a<-lm(formula = log(value)~age+age2+height+contract_left+europe+south_america+twolegs+leftleg+striker+midfielder+defender+goals+europe_striker, data=model1)

# GENERAL TO SPECIFIC
# KROK 0
#Sprawdzam czy wszystkie nieistotne zmienne sa lacznie istotne

anova(regres4,reg5a)
#H0 mówi, że wszystkie zmienne ktore pominalem sa lacznie nieistotne
#pvalue gdy jest <0.05 to h0 odrzucam, u mnie 0.0000 wiec odrzucam
#wiec musze usuwac po kolei, zaczynam od south_america

reg5se<-lm(formula = log(value)~age+age2+rating_fifa21+height+contract_left+log1p(instagram_followers)+europe+twolegs+leftleg+striker+midfielder+defender+goals+europe_striker, data=model1)
summary(reg5se)
#model dalej ma zmienne nieistotne, teraz wywalam height
#sprawdzam laczna nieistotnosc tych dwoch zmiennych

reg5he<-lm(formula = log(value)~age+age2+rating_fifa21+contract_left+log1p(instagram_followers)+europe+twolegs+leftleg+striker+midfielder+defender+goals, data=model1)
anova(regres4, reg5he)
#pvalue 0.0.8968 wiec moge wyrzucic height
#to biore nastepna najwieksza ktora jest europe

reg5eu<-lm(formula = log(value)~age+age2+rating_fifa21+contract_left+log1p(instagram_followers)+twolegs+leftleg+striker+midfielder+defender+goals, data=model1)
anova(regres5,reg5eu)
#pvalue =0.9857 wyrzucam eu
#biore nastepna najwieksza pvalue czyli striker

reg5str<-lm(formula = log(value)~age+age2+rating_fifa21+contract_left+log1p(instagram_followers)+leftleg+twolegs+midfielder+defender+goals+europe_striker, data=model1)
anova(regres4,reg5str)
#pvalue=0.99 wyrzucam
#teraz wyrzucam twolegs

reg5tl<-lm(formula = log(value)~age+age2+rating_fifa21+contract_left+log1p(instagram_followers)+leftleg+midfielder+defender+goals+europe_striker, data=model1)
anova(regres4,reg5tl)
#pvalue 0.99
#teraz wyrzucam leftleg

reg5ll<-lm(formula = log(value)~age+age2+rating_fifa21+contract_left+log1p(instagram_followers)+midfielder+defender+goals+europe_striker, data=model1)
anova(regres4,reg5ll)
#pvalue=0.99 \ #16.01.2021 age

reg5age2<-lm(formula = log(value)~age2+rating_fifa21+contract_left+log1p(instagram_followers)+midfielder+defender+goals+europe_striker, data=model1)
anova(regres4,reg5age2)
#pvalue=0.99 age2 odrzucam, teraz biore midfielder

reg5midfielder<-lm(formula = log(value)~age2+rating_fifa21+contract_left+log1p(instagram_followers)+defender+goals+europe_striker, data=model1)
anova(regres4, reg5midfielder)
#pvalue=0.99 wiec odrzucam i biore europe_striker

reg5eustr<-lm(formula = log(value)~age2+rating_fifa21+contract_left+log1p(instagram_followers)+defender+goals, data=model1)
anova(regres4, reg5eustr)
#pvalue=0.99 odrzucam striker i biore contract_left

reg5cl<-lm(formula = log(value)~age2+rating_fifa21+log1p(instagram_followers)+defender+goals, data=model1)
anova(regres4, reg5cl)
#pvalue=0.98 wiec odrzucam contract left i biore defender

reg5def<-lm(formula = log(value)~age2+rating_fifa21+log1p(instagram_followers)+goals, data=model1)
anova(regres4, reg5def)
#pvalue=0.97 wiec odrzucam i biore goals

reg5goals<-lm(formula = log(value)~age+rating_fifa21+log1p(instagram_followers), data=model1)
anova(regres5, reg5goals)
#pvalue=0.81 wiec odrzut i moj OSTATECZNY MODEL

regost<-lm(formula = log(value)~age+rating_fifa21+log1p(instagram_followers), data=model1)
regostg<-lm(formula = log(value)~age2+rating_fifa21+log1p(instagram_followers)+goals, data=model1)
#ZOSTAWIAM GOLE

bptest(regostg,~log1p(instagram_followers), data=model1)
#wspólliniowosc
vif(regres)

#VIFY<10 wiec brak problemu wspolliniowosci

#Sprawdzam odleglosc Cooka
plot(regostg, which=4, cook.levels=cutoff)
cutoff <- 4/((nrow(model1)))

#wyszlo mi podejrzanie dla 3 obserwacji nr.18 czyli Rodri(moze to wynikac z tego ze nie ma instagrama),
#musi byc wieksze od 4/N (Cook)

#Badam teraz reszty standaryzowane
rstandard(regostg)[abs(rstandard(regostg)) > 2]

# niepokojace wtedy gdy ich wartosci bezwgledne >2 
# mam takich 6 przypadkow

#dzwignia 
leveragePlots(regostg)

# wydaje mi sie ze nie powinienem nic usuwac

# Porownanie modeli
stargazer(regres, regres2, regres3,regres4, regostg,  type="html", out="C:/Users/SKOWRON/Desktop/WNE/stargazer.html")
leveragePlots(regostg)

# age histogram
library(ggplot2)
library(knitr)
library(ggthemes)
hist(age$regres)
hist(x)
xfit<-seq(min(x),max(x))
hist(model$age)
hist(model$age2)
View(europeage)
hist(model1$age, 
      main="Histogram dla zmiennej wiek", 
      xlab="age", 
      border="blue", 
      col="red")
    
hist(model1$age, main="Histogram", xlab="age")
lines(density(model1$age))

x <- model1$age2
h<-hist(x, col="red", xlab="Miles Per Gallon",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
summary(model1$age)

# Histogram dla instagram_followers
Obserwujacy<-log(model1$instagram_followers)
hist(Obserwujacy, main="log_Obserwujacy")
hist(model1$instagram_followers)
summary(model1$instagram_followers)

# Histogram dla rating fifa21
hist(model1$rating_fifa21)

# Histogram dla wzrostu
hist(model1$height)

# Histogram dla goli
hist(model1$goals)

# Histogram dla followersow
hist(model1$instagram_followers, xlab="Obserwujacy", main="Obserwujacy")
