source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #-------------

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


#Primeria entrega parcial 
#análise univariada da variável COMORBIDADE do banco  "LASOS 1 (2)"; 


#leitura e manipulação do banco caso necessária

banco<-read_excel("banco/banco LASOS.xlsx")
View(banco)



#observando os textos----------------- 

classes <- banco %>%
  filter(!is.na(COMORBIDADES)) %>%
  count(COMORBIDADES) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

#fazendo as manipulações de texto: 
classes_separadas <- classes %>%
  separate_rows(COMORBIDADES, sep = "/")

classes_separadas <- classes_separadas %>%
  mutate(COMORBIDADES = case_when(
    grepl("OUTRA", COMORBIDADES, ignore.case = TRUE) ~ "Outras comorbidades",
    grepl("OUTRO", COMORBIDADES, ignore.case = TRUE) ~ "Outras comorbidades",
    grepl("OUTRAS", COMORBIDADES, ignore.case = TRUE) ~ "Outras comorbidades",
    grepl("OUTROS", COMORBIDADES, ignore.case = TRUE) ~ "Outras comorbidades",
    TRUE ~ COMORBIDADES
  ))

#teste

classes_2 <- classes_separadas %>%
  filter(!is.na(COMORBIDADES)) %>%
  count(COMORBIDADES) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

#agrupando novamente

classes_separadas <- classes_separadas %>%
  mutate(COMORBIDADES = case_when(
    grepl("DIABETES MELITUS", COMORBIDADES, ignore.case = TRUE) ~ "DIABETES MELLITUS",
    grepl("CÂNCER METASTÁTICA", COMORBIDADES, ignore.case = TRUE) ~ "CÂNCER METASTÁTICO",
    grepl("GLAUCOMA)", COMORBIDADES, ignore.case = TRUE) ~ "GLAUCOMA",
    grepl("ICC", COMORBIDADES, ignore.case = TRUE) ~ "INSUFICIÊNCIA CARDÍACA",
    grepl("HIPERTENSÃO ARTERIAL", COMORBIDADES, ignore.case = TRUE) ~ "HIPERTENSÃO",
    TRUE ~ COMORBIDADES)) %>%
  mutate(Comorbidades = str_to_title(COMORBIDADES))%>%
  mutate(Comorbidades = ifelse(tolower(Comorbidades) == "não", NA, Comorbidades))
#Pegando as siglas 

capitalizar_palavras <- function(siglas) {
  palavras <- str_split(siglas, "\\s")[[1]]  # Divide o texto em palavras
  palavras_capitalizadas <- ifelse(nchar(palavras) %in% c(3, 4),
                                   str_to_upper(palavras),
                                   str_to_title(palavras))
  return(str_c(palavras_capitalizadas, collapse = " "))
}

classes_separadas <- classes_separadas %>%
  mutate(Comorbidades = sapply(Comorbidades, capitalizar_palavras))


#verificando novamente 

classes_2 <- classes_separadas %>%
  filter(!is.na(Comorbidades)) %>%
  count(Comorbidades) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )


ggplot(classes_2) +
  aes(
    x = fct_reorder(Comorbidades, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.1, hjust = -0.1,
    size = 3
  ) +
  labs(x = "Comorbidades", y = "Frequência") +
  expand_limits(y = c(0, 20)) + 
  theme_estat() + 
  coord_flip()

caminho <- "C:\\Git-ESTAT\\PF23055-Henrique-pereira\\resultados\\Letícia"
ggsave(file.path(caminho,"comorbidades.pdf")
       , width = 158, height = 93, units = "mm")



#Categoria (let) ------------------

cat_limpas <- banco %>%
  mutate(`DIAS NO HOSPITAL` = case_when(
    CATEGORIA2 ==  "APARELHO DIGESTIVO (BAIXO)" ~ "APARELHO DIGESTIVO",
    CATEGORIA2 == "APARELHO DIGESTIVO (ALTO)" ~ "APARELHO DIGESTIVO",
    CATEGORIA2 == "OUTRA (ENDOSCOPIA)" ~ "OUTROS",
    CATEGORIA2 == "OUTRO (OFTALMO)" ~ "OFTALMOLOGIA",
    CATEGORIA2 == "OUTRO" ~ "OUTROS",
    TRUE ~ as.character(CATEGORIA2)  # Se não houver correspondência, mantenha o valor original
  ))%>%
  mutate(`DIAS NO HOSPITAL` = str_to_title(`DIAS NO HOSPITAL`))

#endoscopia vai pra aparelho digestivo ou outros?
cat_cont <- cat_limpas %>%
  filter(!is.na(`DIAS NO HOSPITAL`)) %>%
  count(`DIAS NO HOSPITAL`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

tabela_2 <- xtable(cat_cont[, c("`DIAS NO HOSPITAL`","n", "freq")])
print(tabela_2, include.rownames = FALSE, 
      comment = FALSE, booktabs = TRUE)


# ASA X Categoria---------------
cont_2 <- cat_limpas %>%
  filter(!is.na(`DIAS NO HOSPITAL`)) %>%
  filter(!is.na(ASA))%>%
  group_by(`DIAS NO HOSPITAL`,ASA) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

porcentagens <- str_c(cont_2$freq_relativa, "%") %>% str_replace("\\.", ",")
legendas <- str_squish(str_c(cont_2$freq, " (", porcentagens, ")"))



ggplot(cont_2) +
  aes(
    x = fct_reorder(`DIAS NO HOSPITAL`, freq, .desc = T),
    y = freq,
    fill = ASA,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", 
                                      padding = 0)) +
  geom_text(
    position = position_dodge(width = 1),
    vjust = 0.5, hjust = -0.1,
    size = 2
  ) +
  labs(x = "Categorias", y = "Frequência") +
  theme_estat() +
  scale_y_continuous(name = "Frequência", limits = c(0, 7)) +
  coord_flip()

caminho <- "C:\\Git-ESTAT\\PF23055-Henrique-pereira\\resultados\\Letícia"
ggsave(file.path(caminho,"asa_cat.pdf")
       , width = 158, height = 93, units = "mm")


cont_table <- table(cat_limpas$ASA, cat_limpas$`DIAS NO HOSPITAL`)
chi_square_test <- chisq.test(cont_table)
cramer_v <- sqrt(chi_square_test$statistic / sum(cont_table) * (min(dim(cont_table)) - 1))
cat("Coeficiente V de Cramér:", cramer_v)


#ASA X TABAGISMO --------------------

cont_3 <- cat_limpas %>%
  filter(!is.na(`TABAGISTA ATUAL`)) %>%
  filter(!is.na(ASA))%>%
  group_by(`TABAGISTA ATUAL`,ASA) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

porcentagens <- str_c(cont_3$freq_relativa, "%") %>% str_replace("\\.", ",")
legendas <- str_squish(str_c(cont_3$freq, " (", porcentagens, ")"))



ggplot(cont_3) +
  aes(
    x = fct_reorder(`TABAGISTA ATUAL`, freq, .desc = T),
    y = freq,
    fill = ASA,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single",
                                      padding = 0),width = .9) +
  geom_text(
    aes(y= ifelse(ASA == "Sim", freq, freq+0.2)),
    position = position_dodge(width = .85),
    vjust = -0.5, hjust = 0.5,
    size = 2.8
  ) +
  labs(x = "Tabagismo", y = "Frequência") +
  theme_estat() +
  scale_y_continuous(name = "Frequência", limits = c(0, 25)) +
  scale_x_discrete(labels = c("Não", "Sim"))
  #+coord_flip()

caminho <- "C:\\Git-ESTAT\\PF23055-Henrique-pereira\\resultados\\Letícia"
ggsave(file.path(caminho,"asa_tab.pdf")
       , width = 158, height = 93, units = "mm")


cont_table <- table(cat_limpas$ASA, cat_limpas$`TABAGISTA ATUAL`)
chi_square_test <- chisq.test(cont_table)
cramer_v <- sqrt(chi_square_test$statistic / sum(cont_table) * (min(dim(cont_table)) - 1))
cat("Coeficiente V de Cramér:", cramer_v)


# Categoria X Indicação de cirurgia---------

#observando o que preciso mudar
classes_3 <- cat_limpas %>%
  filter(!is.na(`INDICAÇÃO DA CIRURGIA`)) %>%
  count(`INDICAÇÃO DA CIRURGIA`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ind_limpas <- cat_limpas %>%
  mutate(`INDICAÇÃO DA CIRURGIA` = ifelse(grepl("Outro|Outros|Outra",
                                                `INDICAÇÃO DA CIRURGIA`, 
                                                ignore.case = TRUE), "Outras", 
                                          `INDICAÇÃO DA CIRURGIA`))%>%
  mutate(`INDICAÇÃO DA CIRURGIA` = str_to_title(`INDICAÇÃO DA CIRURGIA`))

ind_limpas <- ind_limpas %>%
filter(!grepl("Não", `INDICAÇÃO DA CIRURGIA`, ignore.case = TRUE))
#verificando

classes_3 <- ind_limpas %>%
  filter(!is.na(`INDICAÇÃO DA CIRURGIA`)) %>%
  count(`INDICAÇÃO DA CIRURGIA`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

cont_3 <- ind_limpas %>%
  filter(!is.na(`INDICAÇÃO DA CIRURGIA`)) %>%
  filter(!is.na(`DIAS NO HOSPITAL`))%>% #n fez diferença, apenas um NA
  group_by(`DIAS NO HOSPITAL`, `INDICAÇÃO DA CIRURGIA`) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

cont_3 <- cont_3 %>%
  mutate(`DIAS NO HOSPITAL` = str_replace(`DIAS NO HOSPITAL`, " E ", " e "))

porcentagens <- str_c(cont_3$freq_relativa, "%") %>% str_replace("\\.", ",")
legendas <- str_squish(str_c(cont_3$freq, " (", porcentagens, ")"))



ggplot(cont_3) +
  aes(
    x = fct_reorder(``DIAS NO HOSPITAL``, freq, .desc = T),
    y = freq,
    fill = `INDICAÇÃO DA CIRURGIA`,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single",
                                      padding = 0),width = .9) +
  geom_text(
    position = position_dodge(width = .85),
    vjust = 0.3, hjust = -0.4,
    size = 2.15
  ) +
  labs(x = "Categorias Cirúrgicas", y = "Frequência",
       fill = "Indicação da Cirurgia") +
  theme_estat() +
  scale_y_continuous(name = "Frequência", limits = c(0, 8)) +
coord_flip()

caminho <- "C:\\Git-ESTAT\\PF23055-Henrique-pereira\\resultados\\Letícia"
ggsave(file.path(caminho,"cat_ind.pdf")
       , width = 158, height = 93, units = "mm")


cont_table <- table(ind_limpas$`DIAS NO HOSPITAL`, 
                    ind_limpas$`INDICAÇÃO DA CIRURGIA`)
chi_square_test <- chisq.test(cont_table)
cramer_v <- sqrt(chi_square_test$statistic / sum(cont_table) * (min(dim(cont_table)) - 1))
cat("Coeficiente V de Cramér:", cramer_v)

install.packages(rcompanion)
library(rcompanion)
cramerV(cont_table)

# severidade X HB: (let)----------------------
# gráfico de colunas empilhada, onde as cores se referem à severidade e o eixo X 
#se refere ao HB (pode separar o HB por intervalos pela quantidade de valores diferentes)


hb_limpas <- ind_limpas %>%
  mutate(HB = ifelse(HB == "NÃO", NA, HB)) %>%
  filter(!is.na(`HB`))%>%
  mutate(`Severidade` = str_to_title(`SEVERIDADE`))



teste <- data.frame(HB = c(13.4, 13.2, 15.3, 12.9, 16.4, 
                           12.3, 9.1, 8.5, 14.9, 13.1, 
                           12.6,12.6, 9.9, 15.0, 12.1, 
                           12.7, 10.7, 12.6, 10.8, 13.5, 
                           14.0, 12.9,13.2, 12.8, 13.6,
                           11.2, 12.8, 14.8, 9.9, 9.8, 
                           14.0, 17.2, 12.4))

limites <- seq(8, 18, by = 3)
teste$Intervalo <- paste0(limites[findInterval(teste$HB, limites)], " a ", limites[findInterval(teste$HB, limites)] + 2)

print(teste) #ok deu certo 


hb_sev <- cbind(hb_limpas, teste["Intervalo"])


#HB e asa----------------------------

#gráfico 

cont_empilhado <- hb_sev %>%
  group_by(`Intervalo`, Severidade) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent())

porcentagens <- str_c(cont_empilhado$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(cont_empilhado$freq, " (", porcentagens, ")"))


ggplot(cont_empilhado) +
  aes(x = `Intervalo`, y = freq, fill = Severidade, label = legendas) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Nível de hemoglobina (em g/dl)", y = "Frequência") +
  geom_text(position = position_fill(vjust = 0.5), size = 3.5,
            colour = "white") +
  theme_estat()
caminho <- "C:\\Git-ESTAT\\PF23055-Henrique-pereira\\resultados\\Letícia"
ggsave(file.path(caminho,"hb_sev.pdf")
       , width = 158, height = 93, units = "mm")



#asa x hb--------------------------

#acredito que não preciso fazer uma nova manpulação, por conta do HB. só temos 33 observações numericas de HB
#as outras são 'NÃO", entçao novamente o total de obs vai ser 33, o ASA só possui um NA


#gráfico 

cont_empilhado_2 <- hb_sev %>%
  group_by(`Intervalo`, ASA) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent())

porcentagens <- str_c(cont_empilhado_2$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(cont_empilhado_2$freq, " (", porcentagens, ")"))


ggplot(cont_empilhado_2) +
  aes(x = `Intervalo`, y = freq, fill = ASA, label = legendas) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Nível de hemoglobina (em g/dl)", y = "Frequência") +
  geom_text(position = position_fill(vjust = 0.5), size = 3.5,
            colour = "white") +
  theme_estat()
caminho <- "C:\\Git-ESTAT\\PF23055-Henrique-pereira\\resultados\\Letícia"
ggsave(file.path(caminho,"hb_asa.pdf")
       , width = 158, height = 93, units = "mm")


#diasnohospitalxtipodeanestesia------------------

banco <- banco %>%
  mutate(ANESTESIA = ifelse(grepl("GERAL +", ANESTESIA), 
                            "Geral + Outra", ANESTESIA)) %>%
  mutate(ANESTESIA = str_to_title(ANESTESIA))

banco <- banco %>%
  mutate(`DIAS NO HOSPITAL` = ifelse(`DIAS NO HOSPITAL` < 10, "Até 10 dias", "Mais de 10 dias"))

cont_dias <- banco %>%
  filter(!is.na(`DIAS NO HOSPITAL`)) %>%
  filter(!is.na(ANESTESIA))%>%
  group_by(`DIAS NO HOSPITAL`,ANESTESIA) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

porcentagens <- str_c(cont_dias$freq_relativa, "%") %>% str_replace("\\.", ",")
legendas <- str_squish(str_c(cont_dias$freq, " (", porcentagens, ")"))



ggplot(cont_dias) +
  aes(
    x = fct_reorder(`DIAS NO HOSPITAL`, freq, .desc = T),
    y = freq,
    fill = ANESTESIA,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", 
                                      padding = 0)) +
  geom_text(
    position = position_dodge(width = 1),
    vjust = -0.5, hjust = 0.5,
    size = 2
  ) +
  labs(x = "Tipos de anestesia", y = "Frequência") +
  theme_estat()

caminho <- "C:\\Git-ESTAT\\PF23055-Henrique-pereira\\resultados\\Letícia"
ggsave(file.path(caminho,"ANESTESIA_cat.pdf")
       , width = 158, height = 93, units = "mm")