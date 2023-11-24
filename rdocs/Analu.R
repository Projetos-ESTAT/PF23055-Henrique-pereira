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

banco_LASOS <- read_excel("banco/banco LASOS.xlsx", 
                          sheet = "LASOS - OBSTETRÍCIA")

banco_LASOS2 <- read_excel("banco/banco LASOS.xlsx", 
                          sheet = "LASOS 1 (2)")

caminho_ana <- "resultados/Ana Lu"

# Idade

min(banco_LASOS$IDADE)

max(banco_LASOS$IDADE)

mean(banco_LASOS$IDADE)

median(banco_LASOS$IDADE)

quantile(banco_LASOS$IDADE, 0.25)

quantile(banco_LASOS$IDADE, 0.75)

sd(banco_LASOS$IDADE)

# Urgência X Sofrimento fetal

Urg_SofriFetal <- banco_LASOS %>%
  select(URGÊNCIA, 'SOFRIMENTO FETAL')

colnames(Urg_SofriFetal)[colnames(Urg_SofriFetal) == "SOFRIMENTO FETAL"] <- "Sofrimento Fetal"

Urg_SofriFetal <- Urg_SofriFetal %>%
  group_by(`URGÊNCIA`, `Sofrimento Fetal`) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

Urg_SofriFetal$URGÊNCIA <- Urg_SofriFetal$URGÊNCIA %>%
  str_replace("URGENTE", "Urgente") %>%
  str_replace("ELETIVA", "Eletiva")

Urg_SofriFetal$`Sofrimento Fetal` <- Urg_SofriFetal$`Sofrimento Fetal` %>%
  str_replace("SIM", "Sim") %>%
  str_replace("NÃO", "Não")

Urg_SofriFetal$URGÊNCIA <- factor(Urg_SofriFetal$URGÊNCIA, levels = c("Eletiva", "Urgente"))


porcentagens <- str_c(Urg_SofriFetal$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(Urg_SofriFetal$freq, " (", porcentagens, ")"))

ggplot(Urg_SofriFetal) +
  aes(
    x = URGÊNCIA,
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
  labs(x = "Urgência", y = "Frequência de sofrimento fetal") +
  theme_estat() +
  scale_y_continuous(limits = c(0, 3))
ggsave(filename = file.path(caminho_ana, "Urgencia_SofrimentoFetal.pdf"), width = 158, height = 93, units = "mm")

# urgencia

urgencia <- banco_LASOS2 %>%
  filter(!is.na(URGÊNCIA)) %>%
  count(URGÊNCIA) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

urgencia$URGÊNCIA <- urgencia$URGÊNCIA %>%
  str_replace("URGÊNCIA", "Urgência") %>%
  str_replace("ELETIVA", "Eletiva")

urgencia$URGÊNCIA <- factor(urgencia$URGÊNCIA, levels = c("Eletiva", "Urgência"))

ggplot(urgencia) +
  aes(x = URGÊNCIA, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  expand_limits(y = c(0, 30)) +
  labs(x = "Urgência", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_ana, "Urgencia.pdf"), width = 158, height = 93, units = "mm")

# severidade

severidade <- banco_LASOS2 %>%
  filter(!is.na(SEVERIDADE)) %>%
  count(SEVERIDADE) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

severidade$SEVERIDADE <- severidade$SEVERIDADE %>%
  str_replace("MENOR", "Menor") %>%
  str_replace("INTERMEDIÁRIO", "Intermediário") %>%
  str_replace("MAIOR", "Maior")

severidade$SEVERIDADE <- factor(severidade$SEVERIDADE, levels = c("Menor", "Intermediário", "Maior"))

ggplot(severidade) +
  aes(x = SEVERIDADE, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  expand_limits(y = c(0, 30)) +
  labs(x = "Severidade", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_ana, "Severidade.pdf"), width = 158, height = 93, units = "mm")


# indicação de cirurgia x anestesia

cirur_anes <- banco_LASOS2 %>%
  select(`INDICAÇÃO DA CIRURGIA`, "ANESTESIA")

cirur_anes <- cirur_anes %>%
  mutate(`INDICAÇÃO DA CIRURGIA` = ifelse(grepl("^OUTR", `INDICAÇÃO DA CIRURGIA`), "Outro", `INDICAÇÃO DA CIRURGIA`))

cirur_anes$`INDICAÇÃO DA CIRURGIA` <- cirur_anes$`INDICAÇÃO DA CIRURGIA` %>%
  str_replace("INFECÇÃO", "Infecção") %>%
  str_replace("TRAUMA", "Trauma") %>%
  str_replace("CÂNCER", "Câncer")

cirur_anes$ANESTESIA <- cirur_anes$ANESTESIA %>%
  str_to_title()

cirur_anes <- cirur_anes[c(-17, -48),]

cirur_anes <- cirur_anes %>%
  group_by(`ANESTESIA`, `INDICAÇÃO DA CIRURGIA`) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

porcentagens <- str_c(cirur_anes$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(cirur_anes$freq, " (", porcentagens, ")"))

colnames(cirur_anes)[colnames(cirur_anes) == "INDICAÇÃO DA CIRURGIA"] <- "Indicação de cirurgia"


ggplot(cirur_anes) +
  aes(
    x = ANESTESIA,
    y = freq,
    fill = `Indicação de cirurgia`,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = 0.3, hjust = -0.1,
    size = 2.35
  ) +
  labs(x = "Anestesia", y = "Frequência") +
  theme_estat() +
  scale_y_continuous(limits = c(0, 20)) +
  scale_x_discrete(labels = wrap_format(12)) +
  coord_flip()
ggsave(filename = file.path(caminho_ana, "anaste_cirur.pdf"), width = 158, height = 93, units = "mm")



