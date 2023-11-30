source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #

#Ajeitando o banco de dados:

dados <- dados %>%
  slice(-48)

#Análise da variável sexo:

dados$SEXO <- dados$SEXO %>%
  str_replace('F', 'Feminino') %>%
  str_replace('M', 'Masculino')

sexo <- dados %>%
  filter(!is.na(SEXO)) %>%
  count(SEXO) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(sexo) +
  aes(x = fct_reorder(SEXO, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  expand_limits(y = c(0, 30)) +
  labs(x = "Sexo", y = "Frequência") +
  theme_estat()
ggsave("colunas-uni-sexo-febap.pdf", width = 158, height = 93, units = "mm")

##Análise da variável idade:

#Gráfico:
ggplot(dados) +
  aes(x=factor(""), y=IDADE) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Idade")+
  scale_y_continuous(breaks = seq(from = 20, to = 90, by = 10), limits=c(20, 90))
  theme_estat()
ggsave("boxplot-uni-idade-febap.pdf", width = 158, height = 93, units = "mm")
  
#Quadro de medidas resumo:
  
quadro_resumo_idade <- dados %>% 
  summarize(Média = round(mean(IDADE),2),
            `Desvio Padrão` = round(sd(IDADE),2),
            `Variância` = round(var(IDADE),2),
            `Mínimo` = round(min(IDADE),2),
            `1º Quartil` = round(quantile(IDADE, probs = .25),2),
            Mediana = round(quantile(IDADE, probs = .5),2),
            `3º Quartil` = round(quantile(IDADE, probs = .75),2),
            `Máximo` = round(max(IDADE),2)) %>% t() %>% as.data.frame() %>% 
  mutate(V1 = str_replace(V1,"\\.",",")) 
  
xtable::xtable(quadro_resumo_idade)

##Análise do número de dias no hospital X severidade:

dados$SEVERIDADE <- dados$SEVERIDADE %>%
  str_replace('MENOR', 'Menor') %>%
  str_replace('MAIOR', 'Maior') %>%
  str_replace('INTERMEDIÁRIO', 'Intermediário')

#Gráfico:

tabela_SEVERIDADE <- dados %>%
  select(SEVERIDADE, `DIAS NO HOSPITAL`) %>%
  group_by(SEVERIDADE)

tabela_SEVERIDADE$SEVERIDADE <- as.factor(tabela_SEVERIDADE$SEVERIDADE)

tabela_SEVERIDADE$SEVERIDADE <- factor(tabela_SEVERIDADE$SEVERIDADE,
                                       levels = c("Menor", "Intermediário", "Maior"))

ggplot(tabela_SEVERIDADE) +
  aes(x = SEVERIDADE, y = `DIAS NO HOSPITAL`) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Severidade", y = "Dias no Hospital") +
  theme_estat()
ggsave("boxplot-bi-severidade-hospital-febap.pdf", width = 158, height = 93, units = "mm")

#Quadro de medidas resumo:

quadro_resumo_hospital <- tabela_SEVERIDADE %>% 
  group_by(SEVERIDADE) %>% 
  summarize(Média = round(mean(`DIAS NO HOSPITAL`),2),
            `Desvio Padrão` = round(sd(`DIAS NO HOSPITAL`),2),
            `Variância` = round(var(`DIAS NO HOSPITAL`),2),
            `Mínimo` = round(min(`DIAS NO HOSPITAL`),2),
            `1º Quartil` = round(quantile(`DIAS NO HOSPITAL`, probs = .25),2),
            Mediana = round(quantile(`DIAS NO HOSPITAL`, probs = .5),2),
            `3º Quartil` = round(quantile(`DIAS NO HOSPITAL`, probs = .75),2),
            `Máximo` = round(max(`DIAS NO HOSPITAL`),2)) %>% t() %>% as.data.frame() %>% 
  mutate(V1 = str_replace(V1,"\\.",",")) 

xtable::xtable(quadro_resumo_hospital)

#Teste de associação:

tabela_SEVERIDADE2 <- tabela_SEVERIDADE

tabela_SEVERIDADE2$SEVERIDADE <- tabela_SEVERIDADE2$SEVERIDADE %>%
  str_replace('Menor', '1') %>%
  str_replace('Maior', '3') %>%
  str_replace('Intermediário', '2')

tabela_SEVERIDADE2$`DIAS NO HOSPITAL` <- as.numeric(as.character(tabela_SEVERIDADE2$`DIAS NO HOSPITAL`))
tabela_SEVERIDADE2$SEVERIDADE <- as.numeric(as.character(tabela_SEVERIDADE2$SEVERIDADE))

cor.test(tabela_SEVERIDADE2$SEVERIDADE, tabela_SEVERIDADE2$`DIAS NO HOSPITAL`, method = "spearman", exact = F)

##Análise da ASA X urgencia:

dados$URGÊNCIA <- dados$URGÊNCIA %>%
  str_replace('ELETIVA', 'Eletiva') %>%
  str_replace('URGÊNCIA', 'Urgência') 

#Gráfico:

tabela_asa <- dados %>%
  group_by(URGÊNCIA, ASA) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

porcentagens1 <- str_c(tabela_asa$freq_relativa, "%") %>% str_replace("\\.", ",")
legendas1 <- str_squish(str_c(tabela_asa$freq, " (", porcentagens1, ")"))

ggplot(tabela_asa) +
  aes(
    x = fct_reorder(URGÊNCIA, freq, .desc = T), y = freq,
    fill = ASA, label = legendas1
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Urgência", y = "Frequência") +
  theme_estat()
ggsave("colunas-bi-asa-urgencia-febap.pdf", width = 158, height = 93, units = "mm")

#Teste qui-quadrado de independencia:

tabela_asa2 <- tabela_asa %>%
  select(ASA, URGÊNCIA, freq)

tabela_contingencia <- as.table(xtabs(freq ~ ASA + URGÊNCIA, data = tabela_asa2))

chisq.test(tabela_contingencia)

##Analise da ASA X horas na RPA:

#Gráfico:

tabela_horas_RPA <- dados %>%
  group_by(`HORAS NA RPA`, ASA) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

tabela_horas_RPA$ASA <- as.factor(tabela_horas_RPA$ASA)
tabela_horas_RPA$`HORAS NA RPA` <- as.factor(tabela_horas_RPA$`HORAS NA RPA`)

tabela_horas_RPA$ASA <- factor(tabela_horas_RPA$ASA,
                                       levels = c("I", "II", "III"))
tabela_horas_RPA$`HORAS NA RPA` <- factor(tabela_horas_RPA$`HORAS NA RPA`,
                               levels = c("0", "1", "2", "3", "4"))

porcentagens2 <- str_c(tabela_horas_RPA$freq_relativa, "%") %>% str_replace("\\.", ",")
legendas2 <- str_squish(str_c(tabela_horas_RPA$freq, " (", porcentagens2, ")"))

ggplot(tabela_horas_RPA) +
  aes(
    x = fct_reorder(`HORAS NA RPA`, freq, .desc = T), y = freq,
    fill = ASA, label = legendas2
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = 0.2, hjust = -0.05,
    size = 2.05
  ) +
  labs(x = "Horas na RPA", y = "Frequência")+
  coord_flip()+
  theme_estat() +
  scale_y_continuous(breaks = seq(from = 0, to = 10, by = 2), limits=c(0, 10))  
ggsave("barras-bi-asa-horas-febap.pdf", width = 158, height = 93, units = "mm")

# Teste de correlação:

tabela_horas_RPA2 <- dados %>%
  select(ASA, `HORAS NA RPA`) %>%
  group_by(ASA)

tabela_horas_RPA2$ASA <- tabela_horas_RPA2$ASA %>%
  str_replace('III', '3')
tabela_horas_RPA2$ASA <- tabela_horas_RPA2$ASA %>%
  str_replace('II', '2')
tabela_horas_RPA2$ASA <- tabela_horas_RPA2$ASA %>%
  str_replace('I', '1')

tabela_horas_RPA2$`HORAS NA RPA` <- as.numeric(as.character(tabela_horas_RPA2$`HORAS NA RPA`))
tabela_horas_RPA2$ASA <- as.numeric(as.character(tabela_horas_RPA2$ASA))

cor.test(tabela_horas_RPA2$`HORAS NA RPA`, tabela_horas_RPA2$ASA, method = "spearman", exact = F)

## Desfecho X ASA:

tabela_desfecho <- dados %>%
  group_by(`DESFECHOS (APENAS SE POSITIVOS)`, ASA) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )


## Desfecho X Tipo de Anestesia

tabela_anestesia <- dados %>%
  group_by(`DESFECHOS (APENAS SE POSITIVOS)`, ANESTESIA) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )


## Idade X Comorbidade
