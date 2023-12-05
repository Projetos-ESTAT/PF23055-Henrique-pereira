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

caminho_lucas <- "resultados/Lucas"


banco_LASOS <- read_excel("banco/banco LASOS.xlsx", 
                          sheet = "LASOS - OBSTETRÍCIA")


comorb_obs <- banco_LASOS %>%
  filter(!is.na(COMORBIDADES)) %>%
  count(COMORBIDADES) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(comorb_obs) +
  aes(
    x = fct_reorder(COMORBIDADES, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Comorbidade", y = "Frequência") +
  theme_estat()

ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-comorb-obstetricia.pdf"), width = 158, height = 93, units = "mm")

# Sexo 
Sexo_obs <- banco_LASOS %>%
  filter(!is.na()) %>%
  count(COMORBIDADES) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

# Dias no Hospital e Sofrimento Feral 

colnames(banco_LASOS)[colnames(banco_LASOS) == "SOFRIMENTO FETAL"] <- "Sofrimento Fetal"
banco_LASOS$`DIAS NO HOSPITAL` <- as.factor(banco_LASOS$`DIAS NO HOSPITAL`)
banco_LASOS$`Sofrimento Fetal` <- as.factor(banco_LASOS$`Sofrimento Fetal`)

DH_SF <- banco_LASOS %>%
  group_by(`DIAS NO HOSPITAL`, `Sofrimento Fetal`) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

porcentagens <- str_c(DH_SF$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(DH_SF$freq, " (", porcentagens, ")"))

ggplot(DH_SF) +
  aes(
    x = fct_reorder(`DIAS NO HOSPITAL`, freq, .desc = T),
    y = freq,
    fill = `Sofrimento Fetal`,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Dias no Hospital", y = "Frequência Sofrimento Fetal") +
  theme_estat()+
  scale_y_continuous(limits = c(0, 4))
ggsave(filename = file.path(caminho_lucas, "colunas-bi-freq-DH_SF-obstetricia.pdf"), width = 158, height = 93, units = "mm")


#Urgência X Horas na RPA

colnames(banco_LASOS)[colnames(banco_LASOS) == "URGÊNCIA"] <- "Urgência"
banco_LASOS$Urgência <- as.factor(banco_LASOS$Urgência)
banco_LASOS$`HORAS NA RPA` <- as.factor(banco_LASOS$`HORAS NA RPA`)


URG_HRPA <- banco_LASOS %>%
  group_by(Urgência, `HORAS NA RPA`) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

porcentagens <- str_c(URG_HRPA$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(URG_HRPA$freq, " (", porcentagens, ")"))

ggplot(URG_HRPA) +
  aes(
    x = fct_reorder(`HORAS NA RPA`, freq, .desc = F),
    y = freq,
    fill = Urgência,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Horas no Hospital", y = "Urgência") +
  theme_estat()+
  scale_y_continuous(limits = c(0, 3))
ggsave(filename = file.path(caminho_lucas, "colunas-bi-freq-URG_HRPA-obstetricia.pdf"), width = 158, height = 93, units = "mm")

#Urgência X Idade gestacional

#colnames(banco_LASOS)[colnames(banco_LASOS) == "URGÊNCIA"] <- "Urgência"
colnames(banco_LASOS)[colnames(banco_LASOS) == "IDADE GESTACIONAL"] <- "Idade Gestacional"
banco_LASOS$Urgência <- as.factor(banco_LASOS$Urgência)
banco_LASOS$`Idade Gestacional` <- as.factor(banco_LASOS$`Idade Gestacional`)


URG_IG <- banco_LASOS %>%
  group_by(Urgência, `Idade Gestacional`) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

porcentagens <- str_c(URG_IG$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(URG_IG$freq, " (", porcentagens, ")"))

ggplot(URG_IG) +
  aes(
    x = fct_reorder(`Idade Gestacional`, freq, .desc = F),
    y = freq,
    fill = Urgência,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Idade Gestacional", y = "Urgência") +
  theme_estat()+
  scale_y_continuous(limits = c(0, 3))

ggsave(filename = file.path(caminho_lucas, "colunas-bi-freq-URG_IG-obstetricia.pdf"), width = 158, height = 93, units = "mm")


# ANÁLISE 2 

banco_LASOS1 <- read_excel("banco/banco LASOS.xlsx", 
                           sheet = "LASOS 1 (2)")

# ASA X SEXO

# Exemplo de dados
banco_LASOS1$ASA <- as.factor(banco_LASOS1$ASA)

# Mapeando os valores
banco_LASOS1$ASA <- factor(banco_LASOS1$ASA, 
                           levels = c("I", "II", "III"),
                           labels = c("Saudáveis", "Doenças brandas", "Patologias mais graves"))

banco_LASOS1 <- rename(banco_LASOS1, Sexo = SEXO)


banco_LASOS1$Sexo <- factor(banco_LASOS1$Sexo, 
                           levels = c("F", "M"),
                           labels = c("Feminino", "Masculino"))



asa_sexo <- banco_LASOS1 %>%
  filter(!is.na(Sexo), !is.na(ASA)) %>% 
  group_by(ASA, Sexo) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

porcentagens <- str_c(asa_sexo$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(asa_sexo$freq, " (", porcentagens, ")"))

ggplot(asa_sexo) +
  aes(
    x = fct_reorder(ASA, freq, .desc = T),
    y = freq,
    fill = Sexo,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Classificação Pacientes", y = "Frequência") +
  theme_estat()+
  scale_y_continuous(limits = c(0, 20))

ggsave(filename = file.path(caminho_lucas, "colunas-bi-freq-asa-sexo-lasos1(2).pdf"), width = 158, height = 93, units = "mm")


# ASA X Indicação de Cirurgia 

banco_LASOS1$`INDICAÇÃO DA CIRURGIA` <- gsub("^OUTR[OA].*", "Outro", banco_LASOS1$`INDICAÇÃO DA CIRURGIA`)

banco_LASOS1$`INDICAÇÃO DA CIRURGIA` <- factor(banco_LASOS1$`INDICAÇÃO DA CIRURGIA`, 
                           levels = c("CÂNCER", "Outro", "INFECÇÃO", "NÃO", "TRAUMA"),
                           labels = c("Câncer", "Outro", "Infecção", "Não", "Trauma"))

banco_LASOS1 <- rename(banco_LASOS1, "Indicação da Cirurgia" = "INDICAÇÃO DA CIRURGIA")


asa_ic <- banco_LASOS1 %>%
  filter(!is.na(`Indicação da Cirurgia`), !is.na(ASA)) %>% 
  group_by(`Indicação da Cirurgia`,ASA) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

porcentagens <- str_c(asa_ic$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(asa_ic$freq, " (", porcentagens, ")"))

ggplot(asa_ic) +
  aes(
    x = fct_reorder(ASA, freq, .desc = T),
    y = freq,
    fill = `Indicação da Cirurgia`,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = 0.5, hjust = -0.1,
    size = 3
  ) +
  coord_flip()+
  labs(x = "Classificação Pacientes", y = "Frequência") +
  theme_estat()+
  scale_y_continuous(limits = c(0, 20))

ggsave(filename = file.path(caminho_lucas, "colunas-bi-freq-asa-ic-lasos1(2).pdf"), width = 158, height = 93, units = "mm")

ggplot(asa_ic) +
  aes(
    x = fct_reorder(ASA, freq, .desc = T),
    y = freq,
    fill = `Indicação da Cirurgia`,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Classificação Pacientes", y = "Frequência") +
  theme_estat()+
  scale_y_continuous(limits = c(0, 20))


ggplot(asa_ic) +
  aes(
    x = fct_reorder(`Indicação da Cirurgia`, freq, .desc = T),
    y = freq,
    fill = ASA,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = 0.5, hjust = -0.1,
    size = 3
  ) +
  coord_flip()+
  labs(x = "Indicação da Cirurgia", y = "Frequência") +
  theme_estat()+
  scale_y_continuous(limits = c(0, 20))
ggsave(filename = file.path(caminho_lucas, "colunas-bi-freq-asa-ic-lasos1(2).pdf"), width = 158, height = 93, units = "mm")
