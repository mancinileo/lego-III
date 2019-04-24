
rm(list=ls())

options(scipen=999)

pacman::p_load(tidyverse, haven, janitor, sjPlot)

#==============================
# Abrir a base
#==============================

eseb <- read_sav("eseb_2014.sav")

glimpse(eseb)

#==============================
# Selecionar as variáveis
#==============================

# ##Avaliação Restrospectiva:
# 
# Q3 - Considera que a situação econômica do pais está melhor, 
# igual ou pior que há 12 mesess
# 
# PC2  AVALIAÇÃO GOV.DILMA ULTIMOS 4 ANOS
# 
# PC18 - nos últimos três anos, foi beneficiário do Bolsa Familia   
# 
# PC11B - E QUAL É A SUA CLASSE SOCIAL HOJE?
# 
# ## Avaliação Prospectiva 
# 
# Q2 Perspectiva de futuro melhor padrão de vida 
# 
# ## Identidade Política 
# 
# Q6B - Em quem votou no 1º turno em 2010 (Dilma == 1)
# 
# Q6D - Em quem votou no 2º turno em 2010 (Dilma == 1)
# 
# Q16B - QUAL PARTIDO GOSTA? (PSDB = 20. PT = 25)
# 
# Q12 - AUTO_LOCALIZAÇÃO ESQUERDA OU DIREITA
# 
# ## Contexto 
# 
# REGIÃO REGIÃO
# 
# ## Socio-demográfica 
# 
# D27A_COR_IBGE - COR DO ENTREVISTADO_CLASSIFICAÇÃO IBGE
#   
# D24_RELIGIÃO - RELIGIÃO DO ENTREVISTADO
# 
# D2_SEXO - SEXO
# 
# D3_ESCOLA - ESCOLARIDADE
# 
# D1A_IDADE - IDADE
# 
# D20A_FXRENDFAM - FAIXA RENDA FAMILIAR



eseb2turno <- eseb %>% 
  select(voto2turno = Q5P2B, 
         sit_economica = Q3, 
         aval_gov = PC2, 
         bolsa_familia = PC18, 
         classe_social = PC11B, 
         padrao_futuro = Q2, 
         voto1turno_2010 = Q6B, 
         voto2turno_2010 = Q6D, 
         gosto_partido = Q16B, 
         ideologia = Q12, 
         regiao = REGIÃO, 
         cor = D27A_COR_IBGE, 
         religiao = D24_RELIGIÃO, 
         sexo = D2_SEXO,
         escolaridade = D3_ESCOLA, 
         idade = D1A_IDADE, 
         renda_familiar = D20A_FXRENDFAM, 
         estado = ESTADO,
         fpond = FPOND)

glimpse(eseb2turno)

summary(eseb2turno)

#===========================================================
# Filtrar somente quem votou Dilma ou Aécio no segundo turno
#===========================================================

eseb2turno$voto2turno

library(labelled)

glimpse(to_factor(eseb2turno))

eseb2turno <- eseb2turno %>% 
  dplyr::filter(voto2turno == "1" | voto2turno == "2") 

# No artigo, a seleção resultou em 2.120 casos... 
# > 2617-2120
# [1] 497

#===========================================================
# Limpar as variáveis
#===========================================================

glimpse(eseb2turno)

# eseb2turno %>% 
#   mutate_all(.,~na_if(.,99))


eseb2turno <- eseb2turno %>%
  mutate(sit_economica = case_when(sit_economica == "1" ~ "melhor",
                                   sit_economica == "2" ~ "igual",
                                   sit_economica == "3" ~ "pior")) %>%
  filter(!is.na(sit_economica))%>%
  mutate(aval_gov = case_when(aval_gov %in% c("1","2") ~ "otimo_bom",
                              aval_gov == "3" ~ "regular",
                              aval_gov %in% c("3","4") 
                              ~ "ruim_pessimo")) %>%
  filter(!is.na(aval_gov))%>%
  mutate(bolsa_familia = case_when(bolsa_familia == "1" ~ "sim",
                                   bolsa_familia=="2" ~ "nao")) %>%
  filter(!is.na(bolsa_familia))%>%
  mutate(classe_social = case_when(classe_social %in% c("1", "2") 
                                   ~ "subiu",
                                   TRUE ~ "nao_subiu_nao_sabe"))%>%
  mutate(padrao_futuro = case_when(padrao_futuro %in% c("1", "2") 
                                   ~ "muitoprovavel_provavel",
                                   padrao_futuro == "8" ~ "nao_sabe",
                                   padrao_futuro %in% c("3", "4") 
                                   ~ "improvavel_muitoimp")) %>%
  filter(!is.na(padrao_futuro)) %>%
  mutate(voto_dilma2010 = case_when(voto1turno_2010 =="1" 
                                    | voto2turno_2010 == "1" ~ "sim",
                                    TRUE ~ "nao"))%>%
  mutate(gosto_partido = case_when (gosto_partido == "20" ~ "psdb",
                                    gosto_partido == "25" ~ "pt",
                                    gosto_partido %in% c ("98", "99", "9999") 
                                    ~ "sem_partido",
                                    TRUE ~ "outros")) %>%
  mutate(ideologia = case_when (ideologia %in% c("0", "1", "2", "3") 
                                ~ "esquerda",
                                ideologia %in% c("4", "5", "6") ~ "centro",
                                ideologia %in% c("7", "8", "9", "10") 
                                ~ "direita",
                                ideologia %in% c("95", "96", "98") 
                                ~ "nao_sabe")) %>%
  filter(!is.na(ideologia)) %>%
  mutate(regiao = case_when(regiao == "1" ~ "norte",
                            regiao == "2" ~ "nordeste",
                            regiao == "3" ~ "sudeste",
                            regiao == "4" ~ "sul",
                            regiao == "5" ~ "centro-oeste")) %>%
  mutate(cor = case_when(cor =="3" ~ "branco",
                         cor %in% c("1", "2", "3", "4", "5", "8") 
                         ~ "nao_branco")) %>%
  filter(!is.na(cor)) %>%
  filter(!religiao %in% c("99", "9999")) %>%
  mutate(religiao = case_when(religiao == "3" ~ "catolico",
                              religiao == "5" ~ "evangelico",
                              TRUE ~ "outros"))%>%
  mutate(sexo = ifelse(sexo == "1", "masculino", "feminino")) %>%
  mutate(escolaridade = case_when(escolaridade %in% c("0", "1", "2","3") 
                                  ~ "fundamental_incompleto",
                                  escolaridade %in% c("4", "5") 
                                  ~ "fund_completo_medio_inc",
                                  escolaridade %in% c("6", "7") 
                                  ~ "med_compl_sup_inc",
                                  TRUE ~ "sup_completo")) %>%
  mutate(renda_familiar = case_when(renda_familiar == "1" ~ "ate_1_sm",
                                    renda_familiar == "2" ~ "1_a_2_sm",
                                    renda_familiar == "3" ~ "2_a_5_sm",
                                    renda_familiar == "4" ~ "5_a_10_sm",
                                    renda_familiar %in% 
                                      c("5", "6", "7", "99") 
                                    ~ "mais_de_10sm")) %>% 
  filter(!is.na(renda_familiar))

eseb2turno %>% 
  ggplot(aes(idade))+
  geom_density()

eseb2turno %>% 
  ggplot(aes(log(idade)))+
  geom_density()



# OBS: teria muito NA em Classe Social e em Predileção partidária..
# 
# classe_social    n      percent
# 1    7 0.0026748185
# 2  109 0.0416507451
# 3  389 0.1486434849
# 4  296 0.1131066106
# 5   72 0.0275124188
# 6   18 0.0068781047
# 98    2 0.0007642339
# 99    6 0.0022927016
# 9999 1718 0.6564768819

# gosto_partido    n      percent
# 1   14 0.0053496370
# 3    1 0.0003821169
# 4    4 0.0015284677
# 5    9 0.0034390524
# 7    1 0.0003821169
# 8   77 0.0294230034
# 10    2 0.0007642339
# 12    3 0.0011463508
# 13    1 0.0003821169
# 14    1 0.0003821169
# 15    1 0.0003821169
# 17   10 0.0038211693
# 18    4 0.0015284677
# 19    2 0.0007642339
# 20  196 0.0748949178
# 21    2 0.0007642339
# 22   21 0.0080244555
# 25  492 0.1880015285
# 26    1 0.0003821169
# 27    1 0.0003821169
# 28    1 0.0003821169
# 30   13 0.0049675201
# 31    1 0.0003821169
# 32    1 0.0003821169
# 98   20 0.0076423386
# 99    5 0.0019105846
# 9999 1733 0.6622086358

# renda_familiar   n     percent
# 1 243 0.111212815
# 2 515 0.235697941
# 3 915 0.418764302
# 4 205 0.093821510
# 5  28 0.012814645
# 6   6 0.002745995
# 7  27 0.012356979
# 98  73 0.033409611
# 99 165 0.075514874
# 9999   8 0.003661327


#===========================================================
# Ajustar as categorias de referência 
#===========================================================

eseb2turno <- eseb2turno %>% 
  mutate(sit_economica = relevel(as.factor(sit_economica), 
                                 ref = "melhor")) %>% 
  mutate(aval_gov = relevel(as.factor(aval_gov), 
                            ref = "otimo_bom")) %>% 
  mutate(bolsa_familia = relevel(as.factor(bolsa_familia), 
                                 ref = "sim")) %>% 
  mutate(classe_social = relevel(as.factor(classe_social), 
                                 ref = "subiu")) %>% 
  mutate(padrao_futuro = relevel(as.factor(padrao_futuro), 
                                 ref = "improvavel_muitoimp")) %>% 
  mutate(voto_dilma2010 = relevel(as.factor(voto_dilma2010), 
                                  ref = "sim")) %>% 
  mutate(gosto_partido = relevel(as.factor(gosto_partido), 
                                 ref = "sem_partido")) %>% 
  mutate(ideologia = relevel(as.factor(ideologia), 
                             ref = "nao_sabe")) %>% 
  mutate(regiao = relevel(as.factor(regiao), ref="sudeste")) %>%
  mutate(cor = relevel(as.factor(cor), ref = "nao_branco")) %>% 
  mutate(religiao = relevel(as.factor(religiao), ref = "catolico")) %>% 
  mutate(sexo = relevel (as.factor(sexo), ref = "feminino")) %>% 
  mutate(escolaridade = relevel (as.factor(escolaridade), 
                                 ref = "fundamental_incompleto")) %>% 
  mutate(renda_familiar = relevel(as.factor(renda_familiar), 
                                  ref = "ate_1_sm"))


#===========================================================
# Ajustar a veriável dependente
#===========================================================

eseb2turno$voto2turno

# Labels:
#   value                      label
# 1        Aécio Neves-45-PSDB
# 2                Dilma-13-PT
# 50              Anulou o voto
# 60            Votou em branco
# 98       Não sabe/ Não lembra
# 99 Não respondeu (espontânea)
# 9999              Não se Aplica

eseb2turno <-  eseb2turno %>% 
  mutate(voto2turno = ifelse(voto2turno=="1", "1", "0")) %>% 
  mutate(voto2turno = relevel(as.factor(voto2turno), ref = "0")) %>% 
  mutate(voto2turno = as.numeric(as.character(voto2turno)))


#===========================================================
# Ajustar o peso de SP 
#===========================================================
# No artigo, a seleção resultou em 2.120 casos... 
# > 2617-2120
# [1] 497

# Essa diferença se dá por conta de SP ter 630 entrevistas a mais e, por
# isso, estar super-representado. 

# Estado          Prop Sample   Sample     Weight
# SP - Capital    154           406        0,3793103
# SP - Suburb     112           378        0,2962963
# SP - Interior   308           420        0,7333333

eseb2turno$fpond

library(survey)

eseb2turnopond <- svydesign(id = ~estado,
                            weights = ~fpond,
                            data = eseb2turno)

eseb2turnopond2 <- svydesign(id = ~1,
                            weights = ~fpond,
                            data = eseb2turno)

library(srvyr)

eseb2turno_we <- eseb2turno %>% 
  as_survey(weights = fpond)
  
#===========================================================
# Modelo 
#===========================================================


logit1 <- glm(as.integer(voto2turno) ~ sit_economica + aval_gov + 
                bolsa_familia + classe_social + padrao_futuro + 
                gosto_partido + ideologia + regiao + cor + religiao + 
                sexo + escolaridade + idade + renda_familiar + 
                voto_dilma2010,data = eseb2turno, 
              family = "binomial"(link = "logit"))
summary(logit1)

logit5 <- glm(as.integer(voto2turno) ~ sit_economica + aval_gov + bolsa_familia + 
                classe_social + padrao_futuro + gosto_partido + 
                ideologia + regiao + cor + religiao + sexo + 
                escolaridade + idade + renda_familiar + voto_dilma2010, 
              data = eseb2turno, family = "binomial"(link = "logit"), weights = fpond)
summary(logit5)

# logit2 <- svyglm(as.integer(voto2turno) ~ sit_economica + aval_gov + bolsa_familia + 
#                 classe_social + padrao_futuro + gosto_partido + 
#                 ideologia + regiao + cor + religiao + sexo + 
#                 escolaridade + idade + renda_familiar + voto_dilma2010, 
#               data = eseb2turno, design = eseb2turnopond, 
#               family = "binomial"(link = "logit"))
# 
# summary(logit2)

logit3 <- svyglm(as.integer(voto2turno) ~ sit_economica + aval_gov + bolsa_familia + 
                   classe_social + padrao_futuro + gosto_partido + 
                   ideologia + regiao + cor + religiao + sexo + 
                   escolaridade + idade + renda_familiar + voto_dilma2010, 
                 data = eseb2turno, design = eseb2turnopond2, 
                 family = "binomial"(link = "logit"))

summary(logit3)

logit4 <- svyglm(as.integer(voto2turno) ~ sit_economica + aval_gov + bolsa_familia + 
                   classe_social + padrao_futuro + gosto_partido + 
                   ideologia + regiao + cor + religiao + sexo + 
                   escolaridade + idade + renda_familiar + voto_dilma2010, 
                 data = eseb2turno, design = eseb2turno_we, 
                 family = "binomial"(link = "logit"))

summary(logit4)

probit1 <- glm(as.integer(voto2turno) ~ sit_economica + aval_gov + bolsa_familia + 
                classe_social + padrao_futuro + gosto_partido + 
                ideologia + regiao + cor + religiao + sexo + 
                escolaridade + idade + renda_familiar + voto_dilma2010, 
              data = eseb2turno, family = "binomial"(link = "probit"))
  
summary(probit1)

AIC(logit1, probit1)
BIC(logit1, probit1)

AIC(logit2, logit3, logit4)
BIC(logit2, logit3, logit4)


#===========================================================
# Interpretação 
#===========================================================

exp(coef(logit1))

get_model_data(logit1, type = "pred")

plot_model(logit1)


#===========================================================
# Diagnósticos 
#===========================================================

# Tabela de contingência 

probabilities <- logit1 %>% 
  predict(eseb2turno, type = "response")

predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

mean(predicted.classes == eseb2turno$voto2turno, na.rm = T)

## Curva de ROC 

library(pROC)

rocplot <- roc(voto2turno ~ fitted(logit1), data = eseb2turno)

plot.roc(rocplot, legacy.axes = F)

auc(rocplot)

rocplot2 <- roc(voto2turno ~ fitted(logit2), data = eseb2turno)
plot.roc(rocplot2, legacy.axes = F)
auc(rocplot2)

## Heatmap

pred <- predict(logit1, type = "response")

heatmapFit::heatmap.fit(eseb2turno$voto2turno, pred, reps = 1000)


pred2 <- predict(logit2, type = "response")

heatmapFit::heatmap.fit(eseb2turno$voto2turno, pred2, reps = 1000)