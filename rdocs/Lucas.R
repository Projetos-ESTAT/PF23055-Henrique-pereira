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

caminho_lucas <- "resultados"


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

