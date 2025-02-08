#Exercício Empírico I
#Tóipcos Especiais em Econometria I

#Instalação das bibliotecas que serão usadas
install.packages("tidyverse")
install.packages("heaven") 
install.packages("ggplot2")
install.packages("dplyr")

#Etapa de carregamento dos dados e visualização

library(haven)
dados <- read_dta("data.dta") #O arquivo estava na mesma pasta que o script
View(dados)


#Médias e desvio-padrão das notas(excluindo faltosos)
attach(dados)


##Ciências da Natureza
mean(nu_nota_cn, na.rm = TRUE)
sd(nu_nota_cn, na.rm = TRUE)

##Ciências da Humanas
mean(nu_nota_ch, na.rm = TRUE)
sd(nu_nota_ch, na.rm = TRUE)

##Linguagens e Códigos
mean(nu_nota_lc, na.rm = TRUE)
sd(nu_nota_lc, na.rm = TRUE)

##Média da nota de Matemática
mean(nu_nota_mt, na.rm = TRUE)
sd(nu_nota_mt, na.rm = TRUE)

#criando uma coluna com notas de Redação as notas das 5 competências
library(dplyr)
dados <- dados %>%
  mutate(redação_final = nu_nota_comp1 + nu_nota_comp2 + nu_nota_comp3 + nu_nota_comp4 + nu_nota_comp5)


head(dados$redação_final)  # Mostra as primeiras linhas da nova coluna
summary(dados$redação_final)  # Estatísticas da nova variável

sd(redação_final, na.rm = TRUE)

##criando gráficos de densidade

#Filtrando para ficar somente com escolas públicas e privadas
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



