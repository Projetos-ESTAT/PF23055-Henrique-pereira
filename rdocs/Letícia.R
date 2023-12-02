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
  mutate(CATEGORIA_2 = case_when(
    CATEGORIA2 ==  "APARELHO DIGESTIVO (BAIXO)" ~ "APARELHO DIGESTIVO",
    CATEGORIA2 == "APARELHO DIGESTIVO (ALTO)" ~ "APARELHO DIGESTIVO",
    CATEGORIA2 == "OUTRA (ENDOSCOPIA)" ~ "OUTROS",
    CATEGORIA2 == "OUTRO (OFTALMO)" ~ "OFTALMOLOGIA",
    CATEGORIA2 == "OUTRO" ~ "OUTROS",
    TRUE ~ as.character(CATEGORIA2)  # Se não houver correspondência, mantenha o valor original
  ))%>%
  mutate(CATEGORIA2 = str_to_title(CATEGORIA_2))

#endoscopia vai pra aparelho digestivo ou outros?
cat_cont <- cat_limpas %>%
  filter(!is.na(CATEGORIA_2)) %>%
  count(CATEGORIA_2) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

tabela_2 <- xtable(cat_cont[, c("n", "freq")])
print(tabela_2, include.rownames = FALSE, 
      comment = FALSE, booktabs = TRUE)

