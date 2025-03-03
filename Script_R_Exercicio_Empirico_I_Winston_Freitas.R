#Used libraries
install.packages("readcsv2")
install.packages("ggplot2")
install.packages("ggridges")
install.packages("tydeverse")
install.packages("broom")
install.packages("corrplot")
install.packages("car")
install.packages("heaven")

library(readr)
library(ggplot2)
library(ggridges)

#Loading the ENEM microdata
library(data.table)
ENEM_df <- fread(file.choose(), sep = ";", encoding = "Latin-1")

#Descriptive statistics of grades
attach(ENEM_df)

#Natural Sciences
median(NU_NOTA_CN, na.rm = TRUE)
mean(NU_NOTA_CN, na.rm = TRUE)
sd(NU_NOTA_CN, na.rm = TRUE)

#Human sciences
median(NU_NOTA_CH, na.rm = TRUE)
mean(NU_NOTA_CH, na.rm = TRUE)
sd(NU_NOTA_CH, na.rm = TRUE)

#Languages and codes
median(NU_NOTA_LC, na.rm = TRUE)
mean(NU_NOTA_LC, na.rm = TRUE)
sd(NU_NOTA_LC, na.rm = TRUE)

#Mathmatics
median(NU_NOTA_MT, na.rm = TRUE)
mean(NU_NOTA_MT, na.rm = TRUE)
sd(NU_NOTA_MT, na.rm = TRUE)

#Essay
#The final grade of essay is the sum of 5 Competências(Skills).
library(dplyr)
ENEM_df <- ENEM_df %>%
  mutate(RED_FINAL = NU_NOTA_COMP1 + NU_NOTA_COMP2 + NU_NOTA_COMP3 + NU_NOTA_COMP4 + NU_NOTA_COMP5)

attach(ENEM_df)
median(RED_FINAL, na.rm = TRUE)
mean(RED_FINAL, na.rm = TRUE)
sd(RED_FINAL, na.rm = TRUE)

###criando gráficos de densidade

##Filtrando para ficar somente com escolas públicas e privadas
dados_escola <- dados%>% filter(tp_escola %in% c(2,3))

library(ggplot2) #a biblioteca usada para fazer os gráficos


#Nota de ciências da Natureza

ggplot(dados_escola, aes(x = nu_nota_cn, fill = as.factor(tp_escola))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red"),
                    labels = c("Pública", "Privada")) +
  labs(title = "Distribuição da Nota de Ciências da Natureza por Tipo de Escola",
       x = "Nota de Ciências da Natureza",
       y = "Densidade",
       fill = "Tipo de Escola") +
  theme_minimal()



#Nota de Ciências Humanas

ggplot(dados_escola, aes(x = nu_nota_ch, fill = as.factor(tp_escola))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red"),
                    labels = c("Pública", "Privada")) +
  labs(title = "Distribuição da Nota de Ciências Humanas por Tipo de Escola",
       x = "Nota de Ciências Humanas",
       y = "Densidade",
       fill = "Tipo de Escola") +
  theme_minimal()

#Nota de Linguagens e Códigos

ggplot(dados_escola, aes(x = nu_nota_lc, fill = as.factor(tp_escola))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red"),
                    labels = c("Pública", "Privada")) +
  labs(title = "Distribuição da Nota de Linguagens e Códigos por Tipo de Escola",
       x = "Nota de Linguagens e Códigos",
       y = "Densidade",
       fill = "Tipo de Escola") +
  theme_minimal()


#Nota de Matemática

ggplot(dados_escola, aes(x = nu_nota_mt, fill = as.factor(tp_escola))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red"),
                    labels = c("Pública", "Privada")) +
  labs(title = "Distribuição da Nota de Matemática por Tipo de Escola",
       x = "Nota de Matemática",
       y = "Densidade",
       fill = "Tipo de Escola") +
  theme_minimal()


#Nota de Redação

ggplot(dados_escola, aes(x = redação_final, fill = as.factor(tp_escola))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red"),
                    labels = c("Pública", "Privada")) +
  labs(title = "Distribuição da Nota Final da Redação por Tipo de Escola",
       x = "Nota Final da Redação",
       y = "Densidade",
       fill = "Tipo de Escola") +
  theme_minimal()

##Gráfico de densidadde por sexo(Mas e Fem)

dados_sexo <- dados%>% filter(tp_sexo %in% c("M", "F"))

#Nota de Ciências da Natureza"
ggplot(dados_sexo, aes(x = nu_nota_cn, fill = tp_sexo)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "pink"),
                    labels = c("Masculino", "Feminino")) +
  labs(title = "Distribuição da Nota de Ciências da Natureza por Sexo",
       x = "Nota de Ciências da Natureza",
       y = "Densidade",
       fill = "Sexo") +
  theme_minimal()

#Nota de Ciências Humanas

ggplot(dados_sexo, aes(x = nu_nota_ch, fill = tp_sexo)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "pink"),
                    labels = c("Masculino", "Feminino")) +
  labs(title = "Distribuição da Nota de Ciências Humanas por Sexo",
       x = "Nota de Ciências Humanas",
       y = "Densidade",
       fill = "Sexo") +
  theme_minimal()

#Nota de Linguagens e Códigos

ggplot(dados_sexo, aes(x = nu_nota_lc, fill = tp_sexo)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "pink"),
                    labels = c("Masculino", "Feminino")) +
  labs(title = "Distribuição da Nota de Linguagens e Códigos por Sexo",
       x = "Nota de Linguagens e Códigos",
       y = "Densidade",
       fill = "Sexo") +
  theme_minimal()

#Nota de Matemática
ggplot(dados_sexo, aes(x = nu_nota_mt, fill = tp_sexo)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "pink"),
                    labels = c("Masculino", "Feminino")) +
  labs(title = "Distribuição da Nota de Matemática por Sexo",
       x = "Nota de Matemática",
       y = "Densidade",
       fill = "Sexo") +
  theme_minimal()

#Nota de redação
ggplot(dados_sexo, aes(x = redação_final, fill = tp_sexo)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "pink"),
                    labels = c("Masculino", "Feminino")) +
  labs(title = "Distribuição da Nota de Redação por Sexo",
       x = "Nota de Redação",
       y = "Densidade",
       fill = "Sexo") +
  theme_minimal()

#criando colunas de pai e mãe educada, ambos e somente 1 com o critério escolhido.

library(dplyr)

dados <- dados%>%
  mutate(
    pai_educado = ifelse(q001 %in% c("E", "F", "G"), 1, 0),
    mae_educada = ifelse(q002 %in% c("E", "F", "G"), 1, 0),
    pais_educados = ifelse(pai_educado == 1 & mae_educada == 1, 1, 0),
    somente_pai_educado = ifelse(pai_educado == 1 & mae_educada == 0, 1, 0),
    somente_mae_educada = ifelse(mae_educada == 1 & pai_educado == 0, 1, 0)
  )
#aqui reside um problema com a resposta H(Quem selecionou "H(não sei)" foi considerada como não educada)


#gráficos de densidade filtrando pela educação dos pais

##Condicionado somente à educação do Pai

#Nota de Ciências da Natureza
ggplot(dados, aes(x = nu_nota_cn, fill = factor(pai_educado))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuição da Nota de Ciências da Natureza",
    x = "Nota em Ciências da Natureza",
    y = "Densidade",
    fill = "Pai Educado"
  ) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Não", "Sim")) +
  theme_minimal()

#Nota de ciências Humanas
ggplot(dados, aes(x = nu_nota_ch, fill = factor(pai_educado))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuição da Nota de Ciências Humanas",
    x = "Nota em Ciências Humanas",
    y = "Densidade",
    fill = "Pai Educado"
  ) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Não", "Sim")) +
  theme_minimal()

#Nota de Linguagens e Códigos
ggplot(dados, aes(x = nu_nota_lc, fill = factor(pai_educado))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuição da Nota de Linguagens e Códigos",
    x = "Nota em Linguagens e Códigos",
    y = "Densidade",
    fill = "Pai Educado"
  ) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Não", "Sim")) +
  theme_minimal()

#Nota de Matemática
ggplot(dados, aes(x = nu_nota_mt, fill = factor(pai_educado))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuição da Nota de Matemática",
    x = "Nota em Matemática",
    y = "Densidade",
    fill = "Pai Educado"
  ) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Não", "Sim")) +
  theme_minimal()

#Nota de Redação
ggplot(dados, aes(x = redação_final, fill = factor(pai_educado))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuição da Nota da Redação",
    x = "Nota da Redação",
    y = "Densidade",
    fill = "Pai Educado"
  ) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Não", "Sim")) +
  theme_minimal()

##Condicionado somente à educação da Mãe

#Nota de Ciências da Natureza
ggplot(dados, aes(x = nu_nota_cn, fill = factor(mae_educada))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuição da Nota de Ciências da Natureza",
    x = "Nota em Ciências da Natureza",
    y = "Densidade",
    fill = "Mãe Educada"
  ) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Não", "Sim")) +
  theme_minimal()

#Nota de ciências Humanas
ggplot(dados, aes(x = nu_nota_ch, fill = factor(mae_educada))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuição da Nota de Ciências Humanas",
    x = "Nota em Ciências Humanas",
    y = "Densidade",
    fill = "Mãe Educada"
  ) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Não", "Sim")) +
  theme_minimal()

#Nota de Linguagens e Códigos
ggplot(dados, aes(x = nu_nota_lc, fill = factor(mae_educada))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuição da Nota de Linguagens e Códigos",
    x = "Nota em Linguagens e Códigos",
    y = "Densidade",
    fill = "Mãe Educada"
  ) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Não", "Sim")) +
  theme_minimal()

#Nota de Matemática
ggplot(dados, aes(x = nu_nota_mt, fill = factor(mae_educada))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuição da Nota de Matemática",
    x = "Nota em Matemática",
    y = "Densidade",
    fill = "Mãe Educada"
  ) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Não", "Sim")) +
  theme_minimal()

#Nota de Redação
ggplot(dados, aes(x = redação_final, fill = factor(mae_educada))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuição da Nota da Redação",
    x = "Nota da Redação",
    y = "Densidade",
    fill = "Mãe Educada"
  ) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Não", "Sim")) +
  theme_minimal()

###Caso Somente o pai for educado

#Nota de ciências da Natureza 
ggplot(dados, aes(x = nu_nota_cn, fill = factor(somente_pai_educado))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuição da Nota de Ciências da Natureza",
    x = "Nota da Ciências da Natureza",
    y = "Densidade",
    fill = "Somente o Pai é educado"
  ) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Não", "Sim")) +
  theme_minimal()

#Nota de ciências Humanas
ggplot(dados, aes(x = nu_nota_ch, fill = factor(somente_pai_educado))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuição da Nota de Ciências Humanas",
    x = "Nota da Nota de Ciências Humanas",
    y = "Densidade",
    fill = "Somente o Pai é educado"
  ) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Não", "Sim")) +
  theme_minimal()

#Nota de Linguagens e Códigos
ggplot(dados, aes(x = nu_nota_lc, fill = factor(somente_pai_educado))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuição da Nota de Linguagens e Códigos",
    x = "Nota da Nota de Linguagens e Códigos",
    y = "Densidade",
    fill = "Somente o Pai é educado"
  ) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Não", "Sim")) +
  theme_minimal()

#Nota de Matemática
ggplot(dados, aes(x = nu_nota_mt, fill = factor(somente_pai_educado))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuição da Nota de Matemática",
    x = "Nota da Nota de Matemática",
    y = "Densidade",
    fill = "Somente o Pai é educado"
  ) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Não", "Sim")) +
  theme_minimal()

#Nota de redação
ggplot(dados, aes(x = redação_final, fill = factor(somente_pai_educado))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuição da Nota de Redação",
    x = "Nota da Nota de Redação",
    y = "Densidade",
    fill = "Somente o Pai é educado"
  ) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Não", "Sim")) +
  theme_minimal()

##Somente a mãe é educada

#Nota de ciências da Natureza
ggplot(dados, aes(x = nu_nota_cn, fill = factor(somente_mae_educada))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuição da Nota de Ciências da Natureza",
    x = "Nota de Ciências da Natureza",
    y = "Densidade",
    fill = "Somente a Mãe é educada"
  ) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Não", "Sim")) +
  theme_minimal()

#Nota de ciências Humanas
ggplot(dados, aes(x = nu_nota_ch, fill = factor(somente_mae_educada))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuição da Nota de Ciências Humanas",
    x = "Nota de Ciências Humanas",
    y = "Densidade",
    fill = "Somente a Mãe é educada"
  ) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Não", "Sim")) +
  theme_minimal()

#Nota de Linguagens e Códigos
ggplot(dados, aes(x = nu_nota_lc, fill = factor(somente_mae_educada))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuição da Nota de Linguagens e Códigos",
    x = "Nota de Linguagens e Códigos",
    y = "Densidade",
    fill = "Somente a Mãe é educada"
  ) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Não", "Sim")) +
  theme_minimal()

#Nota de Matemática
ggplot(dados, aes(x = nu_nota_mt, fill = factor(somente_mae_educada))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuição da Nota Mátemática",
    x = "Nota de Matemática",
    y = "Densidade",
    fill = "Somente a Mãe é educada"
  ) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Não", "Sim")) +
  theme_minimal()

#Nota de redação
ggplot(dados, aes(x = redação_final, fill = factor(somente_mae_educada))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuição da Nota de Redação",
    x = "Nota de Redação",
    y = "Densidade",
    fill = "Somente a Mãe é educada"
  ) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Não", "Sim")) +
  theme_minimal()

##5° caso onde ambos são educados

#Nota de Ciências da Natureza
ggplot(dados, aes(x = nu_nota_cn, fill = factor(pais_educados))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuição da Nota de Ciências da natureza",
    x = "Nota de Ciências da natureza",
    y = "Densidade",
    fill = "Ambos os pais são educados"
  ) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Não", "Sim")) +
  theme_minimal()


# nota de Ciências Humanas

ggplot(dados, aes(x = nu_nota_ch, fill = factor(pais_educados))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuição da Nota de Ciências Humanas",
    x = "Nota de Ciências Humanas",
    y = "Densidade",
    fill = "Ambos os pais são educados"
  ) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Não", "Sim")) +
  theme_minimal()

#Nota de Linguagens e Códigos
ggplot(dados, aes(x = nu_nota_lc, fill = factor(pais_educados))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuição da Nota de Linguagens e Códigos",
    x = "Nota de Linguagens e Códigos",
    y = "Densidade",
    fill = "Ambos os pais são educados"
  ) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Não", "Sim")) +
  theme_minimal()

#Nota de matemática
ggplot(dados, aes(x = nu_nota_mt, fill = factor(pais_educados))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuição da Nota de Matemática",
    x = "Nota de Matemática",
    y = "Densidade",
    fill = "Ambos os pais são educados"
  ) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Não", "Sim")) +
  theme_minimal()

#NOta da redação
ggplot(dados, aes(x = redação_final, fill = factor(pais_educados))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuição da Nota de Redação",
    x = "Nota de Redação",
    y = "Densidade",
    fill = "Ambos os pais são educados"
  ) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Não", "Sim")) +
  theme_minimal()

#Estimação da equação tipo rank(Regressão)
#farei primeiro para ambos os pais educados em cada uma das áreas
#Ciências da Natureza 
attach(dados)
modelo_rank_ENEM_CN <- lm(nu_nota_cn ~ pais_educados)
summary(modelo_rank_ENEM_CN)
coef(modelo_rank_ENEM_CN)
modelo_rank_ENEM_CH <- lm(nu_nota_ch ~ pais_educados)
summary(modelo_rank_ENEM_CH)

modelo_rank_ENEM_LC <- lm(nu_nota_lc ~ pais_educados)
summary(modelo_rank_ENEM_LC)

modelo_rank_ENEM_MT <- lm(nu_nota_mt ~ pais_educados)
summary(modelo_rank_ENEM_MT)

modelo_rank_ENEM_Redacao <- lm(redação_final ~ pais_educados)
summary(modelo_rank_ENEM_Redacao)

#Para o caso onde somente 1 dos pais é educado
modelo_SPE_rank_ENEM_CN <- lm(nu_nota_cn ~ somente_pai_educado)
summary(modelo_SPE_rank_ENEM_CN)

modelo_rank_SPE_ENEM_CH <- lm(nu_nota_ch ~ somente_pai_educado)
summary(modelo_rank_SPE_ENEM_CH)

modelo_rank_SPE_ENEM_LC <- lm(nu_nota_lc ~ somente_pai_educado)
summary(modelo_rank_SPE_ENEM_LC)

modelo_rank_SPE_ENEM_MT <- lm(nu_nota_mt ~ somente_pai_educado)
summary(modelo_rank_SPE_ENEM_MT)

modelo_rank_SPE_ENEM_Redacao <- lm(redação_final ~ somente_pai_educado)
summary(modelo_rank_SPE_ENEM_Redacao)
#### as estaísticas para somente pai educado foram bem incertas

#para somente mãe educada
modelo_rank_SME_ENEM_CN <- lm(nu_nota_cn ~ somente_mae_educada)
summary(modelo_rank_SME_ENEM_CN)

modelo_rank_SME_ENEM_CH <- lm(nu_nota_ch ~ somente_mae_educada)
summary(modelo_rank_SME_ENEM_CH)

modelo_rank_SME_ENEM_LC <- lm(nu_nota_lc ~ somente_mae_educada)
summary(modelo_rank_SME_ENEM_LC)

modelo_rank_SME_ENEM_MT <- lm(nu_nota_mt ~ somente_mae_educada)
summary(modelo_rank_SME_ENEM_MT)

modelo_rank_SME_ENEM_Redacao <- lm(redação_final ~ somente_mae_educada)
summary(modelo_rank_SME_ENEM_Redacao)
#estimativas bem imprecisas


#colocando ambos no modelo como var independentes
attach(dados)
modelo_rank_ENEM_CN <- lm(nu_nota_cn ~ mae_educada + pai_educado)
summary(modelo_rank_ENEM_CN)

modelo_rank_ENEM_CH <- lm(nu_nota_ch ~ mae_educada + pai_educado)
summary(modelo_rank_ENEM_CH)

modelo_rank_ENEM_LC <- lm(nu_nota_lc ~ mae_educada + pai_educado)
summary(modelo_rank_ENEM_LC)

modelo_rank_ENEM_MT <- lm(nu_nota_mt ~ mae_educada + pai_educado)
summary(modelo_rank_ENEM_MT)

modelo_rank_ENEM_Redacao <- lm(redação_final ~ mae_educada + pai_educado)
summary(modelo_rank_ENEM_Redacao)

#verificando a multicolinearidade perfeita do modelo que soma pai educado e mae educada
library(car)
vif(modelo_rank_ENEM_CN)
vif(modelo_rank_ENEM_CH)
vif(modelo_rank_ENEM_LC)
vif(modelo_rank_ENEM_MT)
vif(modelo_rank_ENEM_Redacao)

#Verificando e estimando se há diferenças para negros e não-negros
##Preto = Preto + Pardo(lei)
library(dplyr)
library(dplyr)
dados$preto_lei <- ifelse(dados$tp_cor_raca%in% c(2, 3), 1, 0) 
#Talvez será usada posteriormente

#gráficos de densidade
#Ciências da natureza
library(ggplot2)
ggplot(dados %>% filter(tp_cor_raca %in% c(1, 2)), 
       aes(x = nu_nota_cn, fill = factor(tp_cor_raca))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Nota de Ciências da Natureza Brancos X Pretos",
    x = "Nota em Ciências da Natureza",
    y = "Densidade",
    fill = "Grupo Racial"
  ) +
  scale_fill_manual(values = c("1" = "blue", "2" = "red"), 
                    labels = c("Brancos", "Pretos por Lei")) +
  theme_minimal()

#Ciências Humanas
ggplot(dados %>% filter(tp_cor_raca %in% c(1, 2)), 
       aes(x = nu_nota_ch, fill = factor(tp_cor_raca))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Nota de Ciências Humanas Brancos X Pretos",
    x = "Nota em Ciências da Natureza",
    y = "Densidade",
    fill = "Grupo Racial"
  ) +
  scale_fill_manual(values = c("1" = "blue", "2" = "red"), 
                    labels = c("Brancos", "Pretos")) +
  theme_minimal()

#Linguagens e códigos
ggplot(dados %>% filter(tp_cor_raca %in% c(1, 2)), 
       aes(x = nu_nota_lc, fill = factor(tp_cor_raca))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Nota de Linguagens e Códigos Brancos X Pretos",
    x = "Nota em Ciências da Natureza",
    y = "Densidade",
    fill = "Grupo Racial"
  ) +
  scale_fill_manual(values = c("1" = "blue", "2" = "red"), 
                    labels = c("Brancos", "Pretos")) +
  theme_minimal()

#Matemática
ggplot(dados %>% filter(tp_cor_raca %in% c(1, 2)), 
       aes(x = nu_nota_mt, fill = factor(tp_cor_raca))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Nota de Matemática Brancos X Pretos",
    x = "Nota em Matemática",
    y = "Densidade",
    fill = "Grupo Racial"
  ) +
  scale_fill_manual(values = c("1" = "blue", "2" = "red"), 
                    labels = c("Brancos", "Pretos")) +
  theme_minimal()

#redação
ggplot(dados %>% filter(tp_cor_raca %in% c(1, 2)), 
       aes(x = redação_final, fill = factor(tp_cor_raca))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Nota de Redação Brancos X Pretos",
    x = "Nota em Redação",
    y = "Densidade",
    fill = "Grupo Racial"
  ) +
  scale_fill_manual(values = c("1" = "blue", "2" = "red"), 
                    labels = c("Brancos", "Pretos")) +
  theme_minimal()

#Achar uma forma colocar juntos os Pretos e pardos(Vou pensar) 
##Criando uma variável caso a escola seja privada.

dados$escola_privada <- ifelse(dados$tp_escola == 3, 1, 0)

#regressões
#Ciências da Natureza
modelo_rank_CN_com_escola_privada <- lm(nu_nota_cn ~ pai_educado + mae_educada + escola_privada)
summary(modelo_rank_CN_com_escola_privada)

#ciências Humanas
modelo_rank_CH_com_escola_privada <- lm(nu_nota_ch ~ pai_educado + mae_educada + escola_privada)
summary(modelo_rank_CH_com_escola_privada)

#linguagens e códigos
modelo_rank_LC_com_escola_privada <- lm(nu_nota_lc ~ pai_educado + mae_educada + escola_privada)
summary(modelo_rank_LC_com_escola_privada)

#matemática
modelo_rank_MT_com_escola_privada <- lm(nu_nota_mt ~ pai_educado + mae_educada + escola_privada)
summary(modelo_rank_MT_com_escola_privada)

#redação
modelo_rank_Red_com_escola_privada <- lm(redação_final ~ pai_educado + mae_educada + escola_privada)
summary(modelo_rank_Red_com_escola_privada)

##Homens vs mulheres
#ciando coluna caso for mulher
dados$mulher <- ifelse(dados$tp_sexo == "F", 1, 0)
attach(dados)
#regressões
#Ciências da Natureza
modelo_rank_CN_com_sexo <- lm(nu_nota_cn ~ pai_educado + mae_educada + mulher)
summary(modelo_rank_CN_com_sexo)

#Ciências humanas
modelo_rank_CH_com_sexo <- lm(nu_nota_ch ~ pai_educado + mae_educada + mulher)
summary(modelo_rank_CH_com_sexo)

#Linguagens e códigos
modelo_rank_LC_com_sexo <- lm(nu_nota_lc ~ pai_educado + mae_educada + mulher)
summary(modelo_rank_LC_com_sexo)

#Matemática
modelo_rank_MT_com_sexo <- lm(nu_nota_mt ~ pai_educado + mae_educada + mulher)
summary(modelo_rank_MT_com_sexo)

#redação
modelo_rank_red_com_sexo <- lm(redação_final ~ pai_educado + mae_educada + mulher)
summary(modelo_rank_red_com_sexo)

##brancos vs Negros
#criando uma coluna caso for preto
dados$preto <- ifelse(tp_cor_raca == 2, 1, 0)

#regressões
attach(dados)
#Ciências da Natureza
modelo_rank_CN_com_raca <- lm(nu_nota_cn ~ pai_educado + mae_educada + preto)
summary(modelo_rank_CN_com_raca)

#Ciências humanas
modelo_rank_CH_com_raca <- lm(nu_nota_ch ~ pai_educado + mae_educada + preto)
summary(modelo_rank_CH_com_raca)

#Linguagens e códigos
modelo_rank_LC_com_raca <- lm(nu_nota_lc ~ pai_educado + mae_educada + preto)
summary(modelo_rank_LC_com_raca)

#Matemática
modelo_rank_MT_com_raca <- lm(nu_nota_mt ~ pai_educado + mae_educada + preto)
summary(modelo_rank_MT_com_raca)

#redação
modelo_rank_red_com_raca <- lm(redação_final ~ pai_educado + mae_educada + preto)
summary(modelo_rank_red_com_raca)


#questão 5
##Tratando a variável renda familiar

dados$renda_familiar <- factor(dados$q006, 
                               levels = c("A", "B", "C", "D", "E", "F", "G", 
                                          "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q"),
                               labels = c("Nenhuma",
                                          "Até R$ 1.212", 
                                          "R$ 1.212 a R$ 1.818",
                                          "R$ 1.818 a R$ 2.424",
                                          "R$ 2.424 a R$ 3.030",
                                          "R$ 3.030 a R$ 3.636",
                                          "R$ 3.636 a R$ 4.848",
                                          "R$ 4.848 a R$ 6.060",
                                          "R$ 6.060 a R$ 7.272",
                                          "R$ 7.272 a R$ 8.484",
                                          "R$ 8.484 a R$ 9.696",
                                          "R$ 9.696 a R$ 10.908",
                                          "R$ 10.908 a R$ 12.120",
                                          "R$ 12.120 a R$ 14.544",
                                          "R$ 14.544 a R$ 18.180",
                                          "R$ 18.180 a R$ 24.240",
                                          "Acima de R$ 24.240"))
##tratando a variável idade
dados$idade <- factor(dados$tp_faixa_etaria,  
                       levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 
                                  11, 12, 13, 14, 15, 16, 17, 18, 19, 20), 
                       labels = c("Menor de 17 anos",
                                  "17 anos",
                                  "18 anos",
                                  "19 anos",
                                  "20 anos",
                                  "21 anos",
                                  "22 anos",
                                  "23 anos",
                                  "24 anos",
                                  "25 anos",
                                  "Entre 26 e 30 anos",
                                  "Entre 31 e 35 anos",
                                  "Entre 36 e 40 anos",
                                  "Entre 41 e 45 anos",
                                  "Entre 46 e 50 anos",
                                  "Entre 51 e 55 anos",
                                  "Entre 56 e 60 anos",
                                  "Entre 61 e 65 anos",
                                  "Entre 66 e 70 anos",
                                  "Maior de 70 anos"))

##regressões
attach(dados)
#Ciências da Natureza
modelo_rank_CN_renda_idade <- lm(nu_nota_cn ~ pai_educado + mae_educada + renda_familiar + idade)
summary(modelo_rank_CN_renda_idade)

#Ciências humanas
modelo_rank_CH_renda_idade <- lm(nu_nota_ch ~ pai_educado + mae_educada + renda_familiar + idade)
summary(modelo_rank_CH_renda_idade)

#Linguagens e códigos
modelo_rank_LC_renda_idade <- lm(nu_nota_lc ~ pai_educado + mae_educada + renda_familiar + idade)
summary(modelo_rank_LC_renda_idade)

#Matemática
modelo_rank_MT_renda_idade <- lm(nu_nota_mt ~ pai_educado + mae_educada + renda_familiar + idade)
summary(modelo_rank_MT_renda_idade)

#redação
modelo_rank_red_renda_idade <- lm(redação_final ~ pai_educado + mae_educada + renda_familiar + idade)
summary(modelo_rank_red_renda_idade)

#box plot da distribuição da renda da notas

library(ggplot2) #biblioteca usada
ggplot(dados, aes(x = renda_familiar, y = nu_nota_mt)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribuição da Nota de Matemática por Faixa de Renda",
       x = "Faixa de Renda Familiar",
       y = "Nota de Matemática") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



