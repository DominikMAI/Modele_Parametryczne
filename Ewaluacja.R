




# [1] "istotna TRAVTIME pvalue: 5.425129e-07"
# [1] "istotna CAR_USE pvalue: 5.353845e-28"
# [1] "istotna BLUEBOOK pvalue: 2.554077e-14"
# [1] "istotna TIF pvalue: 3.066795e-09" - 
# [1] "istotna CAR_TYPE pvalue: 4.643908e-20" - BEZ SENSU
# [1] "nieistotna RED_CAR pvalue: 0.48"
# [1] "istotna OLDCLAIM pvalue: 4.806921e-23"
# [1] "istotna CLM_FREQ pvalue: 4.713843e-78"
# [1] "istotna REVOKED pvalue: 1.327e-28"
#[1] "nieistotna CLM_AMT pvalue: 0.99"
#[1] "istotna CAR_AGE pvalue: 1.231955e-18"
#[1] "istotna URBANICITY pvalue: 5.618925e-54"
#[1] "istotna MVR_PTS_GRP pvalue: 5.363152e-32"
#[1] "istotna OCCUPATION_GRP pvalue: 3.650535e-22"
#[1] "istotna AGE_tr pvalue: 3.091067e-19"
#[1] "istotna HOME_VAL_D pvalue: 3.678084e-30"

### Weryfikacja występowania interakcji

v <- list(m1, m2, m3,m4, m5, m6,m7, m8,m9, m10, m11, m12, m13)

for (ml in v){
print(ml)
  #print(summary(ml)$aic)
}

### Ewalucja jakości modelu
Istotność całego modelu weryfikuje się za pomocą **testu ilorazu wiarygodnodści**, którego hipotezy mają postać: <br/>
$H_0 : β_1 = β_2 = ... = β_k = 0$ <br/>
$H_1 :∃_{1j≤k≤j} ≤ β_{j} ≠ 0$ <br/>
Hipoteza zerowa mówi, że wszystkie parametry przy zmiennych objaśniających są zerami, to znaczy prawdziwy jest jedynie model z wyrazem wolnym. 


### Ewaluacja zbiorcza - TO DO
**Podstawowe miary oceny skutecznosci modelu regresji logistycznej**
Skuteczność wyprowadzonych modeli zweryfikowano w oparciu o poniższe miary:
$PP$ - wskazania prawdziwie pozytywne - poprawnie zaklasyfikowane z klasy pozytywnej (1) <br />
$FP$ - wskazania fałszywie pozytywne - niepoprawnie sklasyfkowane wskazania z klasy pozytywnej <br />
$FN$ - wskazania fałszywie negatywne - niepoprawnie wskazania klasy negatywnej <br/>
$PN$ - wskazania prawdziwie negatywne - poprawne wskazania klasy negatywnej <br/>

$Dokładność = \frac{PP+PN}{N}$ - Iloraz wskazań poprawnie sklasyfikowanych do wszystkich elementów. 
Ze względu na to, że dokładność nie odzwierciedla dobrze skuteczności modelu z danymi niezbalansowanymi, wykorzystano również
miary czułości i specyficzności.<br />
Czułość $= \frac{PP}{PP+FN}$ - określa udział przypadków prawidłowo zaklasyfikowanych wśród wszystkich objętych przewidywaniem pozytywnym. </br>

Specyficzność $= \frac{PN}{PN+FP}$
<br />

**Miary dopasowania R2** <br />
Pseudo-$R^2$ McFaddena<br />
Opiera się on na porównaniu modelu pełnego z modelem zredukowanym tylko dla wyrazu wolnego. Oblicza się go według wzoru: <br />
$R^2 = 1 - \frac{\sum_i (y_i - \beta x_i)^2}{\sum_i (y_i - \bar y)^2}$ <br />
W praktyce wartości $R^2$ McFaddena są niewielkie, bliższe 0 niż 1. 

Pseudo-$R^2$ Maddala<br />



```{r}
p<-0.5

##
ocena_modelu_dwum <- function(model) {
  kryterium_AIC <- c(model$aic)
  McFadden<-pR2(model)[4]
  Cragg_Uhler<-pR2(model)[6]
  ocena <- data.frame(kryterium_AIC, McFadden, Cragg_Uhler)
  return(ocena)
}

ocena_modelu_dwum(m1)

##
miary_pred <- function(model, dane, Y, p = 0.5) {
  tab <- table(obserwowane = Y, przewidywane = ifelse(predict(model, dane, type = "response") > p, 1, 0))
  ACC <- (tab[1,1]+tab[2,2])/sum(tab)
  ER <- (tab[1,2]+tab[2,1])/sum(tab)
  SENS <- tab[2,2] / (tab[2,2] + tab[2,1])
  SPECI <- tab[1,1] / (tab[1,1] + tab[1,2])
  PPV <- tab[2,2] / (tab[2,2] + tab[1,2])
  NPV <- tab[1,1] / (tab[1,1] + tab[2,1])
  #Proszę dodpisać pozostałe miary jakości predykcji
  miary <- data.frame(ACC, ER,SENS, SPECI, PPV, NPV)
  return(miary)
}


wyniki_miary_pred <- rbind(model_logit = miary_pred(model = m, dane = dane_testowy, Y = dane_testowy$CLAIM_FLAG, p))
wyniki_miary_pred

###
rocobj1 <- roc(m$y, m$fitted.values)
rocobj1_t <- roc(dane_testowy$CLAIM_FLAG, predict(m, dane_testowy, type = "response"))

###
plot(rocobj1, main = "krzywe ROC dla modelu logitowego", col="red")
lines(rocobj1_t, col="blue")

```
```{r}

#tab <- table(obserwowane = dane['CLAIM_FLAG'], przewidywane = ifelse(predict(m, dane, type = "response") > p, 1, 0))
#tab
```
**Reference** <br />
**Wykorzystanie modeli logitowych w analizie czynników aktywności zawodowej ludności**, Dominik ŚLIWICKI, Marek RĘKLEWSKI, 2006

