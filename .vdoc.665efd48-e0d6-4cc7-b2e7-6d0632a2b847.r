#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: Pacotes
#| code-summary: Pacotes
#| warning: false
#| message: false

library(pacman)
p_load(tidyverse, haven, here, survey, srvyr, gtsummary, kableExtra, glmmTMB)
#
#
#
#| label: Separadores
#| code-summary: Separadores
#| warning: false
#| message: false

theme_gtsummary_language("en", decimal.mark = ",", big.mark = ".")
#
#
#
#| label: Knitr
#| code-summary: Knitr

knitr::opts_chunk$set(fig.align = "center", out.width = "100%")
#
#
#
#| label: PSU
#| code-summary: PSU

options(survey.lonely.psu = "adjust")
#
#
#
#| label: Banco de dados
#| code-summary: Banco de dados

df <- read_dta(here("elsi_brasil_auditoria_virtual.dta"))
#
#
#
#
#
#
#
#
#
#
#
#| label: Tabela sexo
#| code-summary: Tabela

df %>%
  filter(zona == 1) %>%
  mutate(
    sexo = factor(sexo, levels = c(0, 1), labels = c("Feminino", "Masculino")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  select(sexo, cidade_auditoria) %>%
  tbl_summary(
    by = cidade_auditoria,
    label = list(sexo ~ "Sexo")
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**<br>N = {N}"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  )
#
#
#
#| label: Tabela sexo com desenho amostral
#| code-summary: Tabela com desenho amostral

df %>%
  filter(zona == 1) %>%
  mutate(
    sexo = factor(sexo, levels = c(0, 1), labels = c("Feminino", "Masculino")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  as_survey_design(ids = upa, strata = estrato, weights = peso_calibrado) %>%
  tbl_svysummary(
    by = cidade_auditoria,
    include = c(sexo, cidade_auditoria),
    label = list(sexo ~ "Sexo"),
    statistic = list(all_categorical() ~ "{p}%")
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**",
    stat_1 = "**Não**",
    stat_2 = "**Sim**"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(all_stat_cols() ~ NA)
#
#
#
#
#
#
#| label: Histograma idade
#| code-summary: Histograma

df %>%
  filter(zona == 1) %>%
  mutate(
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  ggplot(aes(x = idade, fill = cidade_auditoria)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~cidade_auditoria) +
  theme_minimal() +
  labs(
    title = "Distribuição da idade \n",
    x = "\n Idade (anos)",
    y = "Densidade \n",
    fill = "Auditoria virtual"
  ) +
  theme(strip.text = element_blank())
#
#
#
#| label: Teste de normalidade idade
#| code-summary: Teste de normalidade (Shapiro-Wilk)

df %>%
  filter(zona == 1) %>%
  mutate(
    idade = as.numeric(idade),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  group_by(cidade_auditoria) %>%
  summarise(
    p_val_raw = shapiro.test(idade)$p.value,
    `p-valor` = formatC(p_val_raw, format = "e", digits = 2), 
    Resultado = if_else(p_val_raw > 0.05, "Distribuição normal", "Distribuição não normal")
  ) %>%
  select(-p_val_raw) %>%
  knitr::kable(
    col.names = c("Auditoria virtual", "p-valor", "Resultado"),
    align = "c"
  ) %>%
  kable_styling(
    full_width = FALSE, 
    position = "center", 
    font_size = 24
  ) %>%
  row_spec(0, bold = TRUE, align = "c")
#
#
#
#| label: Tabela idade
#| code-summary: Tabela

df %>%
  filter(zona == 1) %>%
  mutate(
    idade = as.numeric(idade), 
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  select(idade, cidade_auditoria) %>%
  tbl_summary(
    by = cidade_auditoria,
    label = list(idade ~ "Idade (anos)"),
    statistic = list(idade ~ "{median} ({p25}, {p75})"),
    digits = list(idade ~ 1)
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3), test = list(idade ~ "wilcox.test")) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**<br>N = {N}"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  )
#
#
#
#| label: Tabela idade com desenho amostral
#| code-summary: Tabela com desenho amostral

df %>%
  filter(zona == 1) %>%
  mutate(
    idade = as.numeric(idade), 
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  as_survey_design(ids = upa, strata = estrato, weights = peso_calibrado) %>%
  tbl_svysummary(
    by = cidade_auditoria,
    include = c(idade, cidade_auditoria),
    label = list(idade ~ "Idade (anos)"),
    statistic = list(idade ~ "{median} ({p25}, {p75})"),
    digits = list(idade ~ 1)
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**",
    stat_1 = "**Não**",
    stat_2 = "**Sim**"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  )
#
#
#
#| label: Tabela faixa etária
#| code-summary: Tabela

df %>%
  filter(zona == 1) %>%
  mutate(
    faixaetaria = factor(faixaetaria, 
                         levels = c(1, 2, 3, 4), 
                         labels = c("50-59", "60-69", "70-79", "≥80")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  select(faixaetaria, cidade_auditoria) %>%
  tbl_summary(
    by = cidade_auditoria,
    label = list(faixaetaria ~ "Faixa Etária (anos)")
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**<br>N = {N}"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  )
#
#
#
#| label: Tabela faixa etária com desenho amostral
#| code-summary: Tabela com desenho amostral

df %>%
  filter(zona == 1) %>%
  mutate(
    faixaetaria = factor(faixaetaria, 
                         levels = c(1, 2, 3, 4), 
                         labels = c("50-59", "60-69", "70-79", "≥80")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  as_survey_design(ids = upa, strata = estrato, weights = peso_calibrado) %>%
  tbl_svysummary(
    by = cidade_auditoria,
    include = c(faixaetaria, cidade_auditoria),
    label = list(faixaetaria ~ "Faixa Etária (anos)"),
    statistic = list(all_categorical() ~ "{p}%")
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**",
    stat_1 = "**Não**",
    stat_2 = "**Sim**"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(all_stat_cols() ~ NA)
#
#
#
#
#
#
#| label: Tabela cor/raça
#| code-summary: Tabela

n_miss_cor_raca <- df %>% 
  filter(zona == 1) %>% 
  summarise(n = sum(is.na(cor_raca))) %>% 
  pull(n)

df %>%
  filter(zona == 1) %>%
  mutate(
    cor_raca = factor(cor_raca, levels = c(0, 1), labels = c("Branca", "Não branca")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  select(cor_raca, cidade_auditoria) %>%
  tbl_summary(
    by = cidade_auditoria,
    label = list(cor_raca ~ "Cor/raça"),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**<br>N = {N}"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(
    label ~ paste0("Dados ausentes: ", n_miss_cor_raca)
  )
#
#
#
#| label: Tabela cor/raça com desenho amostral
#| code-summary: Tabela com desenho amostral

df %>%
  filter(zona == 1) %>%
  mutate(
    cor_raca = factor(cor_raca, levels = c(0, 1), labels = c("Branca", "Não branca")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  as_survey_design(ids = upa, strata = estrato, weights = peso_calibrado) %>%
  tbl_svysummary(
    by = cidade_auditoria,
    include = c(cor_raca, cidade_auditoria),
    label = list(cor_raca ~ "Cor/raça"),
    statistic = list(all_categorical() ~ "{p}%"),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**",
    stat_1 = "**Não**",
    stat_2 = "**Sim**"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(
    label ~ paste0("Dados ausentes: ", n_miss_cor_raca),
    all_stat_cols() ~ NA
  )
#
#
#
#
#
#
#| label: Tabela escolaridade
#| code-summary: Tabela

n_miss_escolaridade <- df %>% 
  filter(zona == 1) %>% 
  summarise(n = sum(is.na(escolaridade2))) %>% 
  pull(n)

df %>%
  filter(zona == 1) %>%
  mutate(
    escolaridade2 = factor(escolaridade2, 
                           levels = c(1, 2, 3), 
                           labels = c("<4", "4-7", "≥8")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  select(escolaridade2, cidade_auditoria) %>%
  tbl_summary(
    by = cidade_auditoria,
    label = list(escolaridade2 ~ "Escolaridade (anos)"),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>% 
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**<br>N = {N}"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(
    label ~ paste0("Dados ausentes: ", n_miss_escolaridade)
  )
#
#
#
#| label: Tabela escolaridade com desenho amostral
#| code-summary: Tabela com desenho amostral

n_miss_escolaridade <- df %>% 
  filter(zona == 1) %>% 
  summarise(n = sum(is.na(escolaridade2))) %>% 
  pull(n)

df %>%
  filter(zona == 1) %>%
  mutate(
    escolaridade2 = factor(escolaridade2, 
                           levels = c(1, 2, 3), 
                           labels = c("<4", "4-7", "≥8")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  as_survey_design(ids = upa, strata = estrato, weights = peso_calibrado) %>%
  tbl_svysummary(
    by = cidade_auditoria,
    include = c(escolaridade2, cidade_auditoria),
    label = list(escolaridade2 ~ "Escolaridade (anos)"),
    statistic = list(all_categorical() ~ "{p}%"),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>% 
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**",
    stat_1 = "**Não**",
    stat_2 = "**Sim**"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(
    label ~ paste0("Dados ausentes: ", n_miss_escolaridade),
    all_stat_cols() ~ NA
  )
#
#
#
#
#
#
#| label: Tabela estado civil
#| code-summary: Tabela

df %>%
  filter(zona == 1) %>%
  mutate(
    est_civil = factor(est_civil, levels = c(0, 1), labels = c("Não casado", "Casado ou união estável")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  select(est_civil, cidade_auditoria) %>%
  tbl_summary(
    by = cidade_auditoria,
    label = list(est_civil ~ "Estado civil")
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**<br>N = {N}"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  )
#
#
#
#| label: Tabela estado civil com desenho amostral
#| code-summary: Tabela com desenho amostral

df %>%
  filter(zona == 1) %>%
  mutate(
    est_civil = factor(est_civil, levels = c(0, 1), labels = c("Não casado", "Casado ou união estável")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  as_survey_design(ids = upa, strata = estrato, weights = peso_calibrado) %>%
  tbl_svysummary(
    by = cidade_auditoria,
    include = c(est_civil, cidade_auditoria),
    label = list(est_civil ~ "Estado civil"),
    statistic = list(all_categorical() ~ "{p}%")
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**",
    stat_1 = "**Não**",
    stat_2 = "**Sim**"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(all_stat_cols() ~ NA)
#
#
#
#
#
#
#| label: Tabela DCNT
#| code-summary: Tabela

n_miss_dcnt <- df %>% 
  filter(zona == 1) %>% 
  summarise(n = sum(is.na(dcnt))) %>% 
  pull(n)

df %>%
  filter(zona == 1) %>%
  mutate(
    dcnt = factor(dcnt, levels = c(0, 1, 2), labels = c("Nenhuma", "1", "≥2")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  select(dcnt, cidade_auditoria) %>%
  tbl_summary(
    by = cidade_auditoria,
    label = list(dcnt ~ "DCNT"),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**<br>N = {N}"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(
    label ~ paste0("Dados ausentes: ", n_miss_dcnt)
  )
#
#
#
#| label: Tabela DCNT com desenho amostral
#| code-summary: Tabela com desenho amostral

df %>%
  filter(zona == 1) %>%
  mutate(
    dcnt = factor(dcnt, levels = c(0, 1, 2), labels = c("Nenhuma", "1", "≥2")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  as_survey_design(ids = upa, strata = estrato, weights = peso_calibrado) %>%
  tbl_svysummary(
    by = cidade_auditoria,
    include = c(dcnt, cidade_auditoria),
    label = list(dcnt ~ "DCNT"),
    statistic = list(all_categorical() ~ "{p}%"),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**",
    stat_1 = "**Não**",
    stat_2 = "**Sim**"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(
    label ~ paste0("Dados ausentes: ", n_miss_dcnt),
    all_stat_cols() ~ NA
  )
#
#
#
#
#
#
#| label: Tabela consultas
#| code-summary: Tabela

n_miss_consultas <- df %>% 
  filter(zona == 1) %>% 
  summarise(n = sum(is.na(consultas))) %>% 
  pull(n)

df %>%
  filter(zona == 1) %>%
  mutate(
    consultas = factor(consultas, levels = c(0, 1, 2), labels = c("Nenhuma", "1 a 2", "≥3")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  select(consultas, cidade_auditoria) %>%
  tbl_summary(
    by = cidade_auditoria,
    label = list(consultas ~ "Consulta médica (12 meses)"),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**<br>N = {N}"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(
    label ~ paste0("Dados ausentes: ", n_miss_consultas)
  )
#
#
#
#| label: Tabela consultas com desenho amostral
#| code-summary: Tabela com desenho amostral

n_miss_consultas <- df %>% 
  filter(zona == 1) %>% 
  summarise(n = sum(is.na(consultas))) %>% 
  pull(n)

df %>%
  filter(zona == 1) %>%
  mutate(
    consultas = factor(consultas, levels = c(0, 1, 2), labels = c("Nenhuma", "1 a 2", "≥3")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  as_survey_design(ids = upa, strata = estrato, weights = peso_calibrado) %>%
  tbl_svysummary(
    by = cidade_auditoria,
    include = c(consultas, cidade_auditoria),
    label = list(consultas ~ "Consulta médica (12 meses)"),
    statistic = list(all_categorical() ~ "{p}%"),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**",
    stat_1 = "**Não**",
    stat_2 = "**Sim**"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(
    label ~ paste0("Dados ausentes: ", n_miss_consultas),
    all_stat_cols() ~ NA
  )
#
#
#
#
#
#
#| label: Tabela frutas e hortaliças
#| code-summary: Tabela

n_miss_regularFH <- df %>% 
  filter(zona == 1) %>% 
  summarise(n = sum(is.na(regularFH))) %>% 
  pull(n)

df %>%
  filter(zona == 1) %>%
  mutate(
    regularFH = factor(regularFH, 
                       levels = c(0, 1), 
                       labels = c("<5", "≥5")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  select(regularFH, cidade_auditoria) %>%
  tbl_summary(
    by = cidade_auditoria,
    label = list(regularFH ~ "Consumo regular de frutas e hortaliças (dias/semana)"),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**<br>N = {N}"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(
    label ~ paste0("Dados ausentes: ", n_miss_regularFH)
  )
#
#
#
#| label: Tabela frutas e hortaliças com desenho amostral
#| code-summary: Tabela com desenho amostral

df %>%
  filter(zona == 1) %>%
  mutate(
    regularFH = factor(regularFH, 
                       levels = c(0, 1), 
                       labels = c("<5", "≥5")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  as_survey_design(ids = upa, strata = estrato, weights = peso_calibrado) %>%
  tbl_svysummary(
    by = cidade_auditoria,
    include = c(regularFH, cidade_auditoria),
    label = list(regularFH ~ "Consumo regular de frutas e hortaliças (dias/semana)"),
    statistic = list(all_categorical() ~ "{p}%"),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**",
    stat_1 = "**Não**",
    stat_2 = "**Sim**"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(
    label ~ paste0("Dados ausentes: ", n_miss_regularFH),
    all_stat_cols() ~ NA
  )
#
#
#
#
#
#
#| label: Histograma renda
#| code-summary: Histograma
#| warning: false
#| message: false

df %>%
  filter(zona == 1) %>%
  mutate(
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  ggplot(aes(x = rendadompc, fill = cidade_auditoria)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~cidade_auditoria) +
  theme_minimal() +
  labs(
    title = "Distribuição da renda mensal domiciliar per capita \n",
    x = "\n Renda (R$)",
    y = "Densidade \n",
    fill = "Auditoria virtual"
  ) +
  theme(strip.text = element_blank())
#
#
#
#| label: Teste de normalidade renda
#| code-summary: Teste de normalidade (Shapiro-Wilk)

df %>%
  filter(zona == 1) %>%
  mutate(
    rendadompc = as.numeric(rendadompc),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  group_by(cidade_auditoria) %>%
  summarise(
    p_val_raw = shapiro.test(rendadompc)$p.value,
    `p-valor` = formatC(p_val_raw, format = "e", digits = 2), 
    Resultado = if_else(p_val_raw > 0.05, "Distribuição normal", "Distribuição não normal")
  ) %>%
  select(-p_val_raw) %>%
  knitr::kable(
    col.names = c("Auditoria virtual", "p-valor", "Resultado"),
    align = "c"
  ) %>%
  kable_styling(
    full_width = FALSE, 
    position = "center", 
    font_size = 24
  ) %>%
  row_spec(0, bold = TRUE, align = "c")
#
#
#
#| label: Tabela renda
#| code-summary: Tabela

n_miss_renda <- df %>% 
  filter(zona == 1) %>% 
  summarise(n = sum(is.na(rendadompc))) %>% 
  pull(n)

df %>%
  filter(zona == 1) %>%
  mutate(
    rendadompc = as.numeric(rendadompc), 
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  select(rendadompc, cidade_auditoria) %>%
  tbl_summary(
    by = cidade_auditoria,
    label = list(rendadompc ~ "Renda per capita (R$)"),
    statistic = list(rendadompc ~ "{median} ({p25}, {p75})"),
    digits = list(rendadompc ~ 2),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>% 
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3), test = list(rendadompc ~ "wilcox.test")) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**<br>N = {N}"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(
    label ~ paste0("Dados ausentes: ", n_miss_renda)
  )
#
#
#
#| label: Tabela renda com desenho amostral
#| code-summary: Tabela com desenho amostral

n_miss_renda <- df %>% 
  filter(zona == 1) %>% 
  summarise(n = sum(is.na(rendadompc))) %>% 
  pull(n)

df %>%
  filter(zona == 1) %>%
  mutate(
    rendadompc = as.numeric(rendadompc), 
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  as_survey_design(ids = upa, strata = estrato, weights = peso_calibrado) %>%
  tbl_svysummary(
    by = cidade_auditoria,
    include = c(rendadompc, cidade_auditoria),
    label = list(rendadompc ~ "Renda per capita (R$)"),
    statistic = list(rendadompc ~ "{median} ({p25}, {p75})"),
    digits = list(rendadompc ~ 2),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>% 
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**",
    stat_1 = "**Não**",
    stat_2 = "**Sim**"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(
    label ~ paste0("Dados ausentes: ", n_miss_renda)
  )
#
#
#
#| label: Tabela renda (tercis)
#| code-summary: Tabela

n_miss_renda2 <- df %>% 
  filter(zona == 1) %>% 
  summarise(n = sum(is.na(rendadompc2corr))) %>% 
  pull(n)

df %>%
  filter(zona == 1) %>%
  mutate(
    rendadompc2corr = factor(rendadompc2corr, 
                             levels = c(1, 2, 3), 
                             labels = c("1º tercil", "2º tercil", "3º tercil")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  select(rendadompc2corr, cidade_auditoria) %>%
  tbl_summary(
    by = cidade_auditoria,
    label = list(rendadompc2corr ~ "Renda per capita (tercis)"),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**<br>N = {N}"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(
    label ~ paste0("Dados ausentes: ", n_miss_renda2)
  )
#
#
#
#| label: Tabela renda (tercis) com desenho amostral
#| code-summary: Tabela com desenho amostral

df %>%
  filter(zona == 1) %>%
  mutate(
    rendadompc2corr = factor(rendadompc2corr, 
                             levels = c(1, 2, 3), 
                             labels = c("1º tercil", "2º tercil", "3º tercil")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  as_survey_design(ids = upa, strata = estrato, weights = peso_calibrado) %>%
  tbl_svysummary(
    by = cidade_auditoria,
    include = c(rendadompc2corr, cidade_auditoria),
    label = list(rendadompc2corr ~ "Renda per capita (tercis)"),
    statistic = list(all_categorical() ~ "{p}%"),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**",
    stat_1 = "**Não**",
    stat_2 = "**Sim**"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(
    label ~ paste0("Dados ausentes: ", n_miss_renda),
    all_stat_cols() ~ NA
  )
#
#
#
#
#
#
#| label: Tabela programa AF
#| code-summary: Tabela

n_miss_programa <- df %>% 
  filter(zona == 1) %>% 
  summarise(n = sum(is.na(programa))) %>% 
  pull(n)

df %>%
  filter(zona == 1) %>%
  mutate(
    programa = factor(programa, 
                      levels = c(0, 1, 2), 
                      labels = c("Não conhece", "Conhece, mas não participa", "Conhece e participa")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  select(programa, cidade_auditoria) %>%
  tbl_summary(
    by = cidade_auditoria,
    label = list(programa ~ "Programa público de AF"),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**<br>N = {N}"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(
    label ~ paste0("Dados ausentes: ", n_miss_programa)
  )
#
#
#
#| label: Tabela programa AF com desenho amostral
#| code-summary: Tabela com desenho amostral

df %>%
  filter(zona == 1) %>%
  mutate(
    programa = factor(programa, 
                      levels = c(0, 1, 2), 
                      labels = c("Não conhece", "Conhece, mas não participa", "Conhece e participa")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  as_survey_design(ids = upa, strata = estrato, weights = peso_calibrado) %>%
  tbl_svysummary(
    by = cidade_auditoria,
    include = c(programa, cidade_auditoria),
    label = list(programa ~ "Programa público de AF"),
    statistic = list(all_categorical() ~ "{p}%"),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**",
    stat_1 = "**Não**",
    stat_2 = "**Sim**"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(
    label ~ paste0("Dados ausentes: ", n_miss_programa),
    all_stat_cols() ~ NA
  )
#
#
#
#
#
#
#| label: Tabela região
#| code-summary: Tabela

df %>%
  filter(zona == 1) %>%
  mutate(
    regiao = factor(regiao, 
                    levels = c(1, 2, 3, 4, 5), 
                    labels = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")),
    cidade_auditoria = factor(cidade_auditoria, 
                              levels = c(0, 1), 
                              labels = c("Não", "Sim"))
  ) %>%
  select(regiao, cidade_auditoria) %>%
  tbl_summary(
    by = cidade_auditoria,
    label = list(regiao ~ "Região")
  ) %>%
  add_overall(last = TRUE) %>% 
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**<br>N = {N}"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  )
#
#
#
#| label: Tabela região com desenho amostral
#| code-summary: Tabela com desenho amostral

df %>%
  filter(zona == 1) %>%
  mutate(
    regiao = factor(regiao, 
                    levels = c(1, 2, 3, 4, 5), 
                    labels = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")),
    cidade_auditoria = factor(cidade_auditoria, 
                              levels = c(0, 1), 
                              labels = c("Não", "Sim"))
  ) %>%
  as_survey_design(ids = upa, strata = estrato, weights = peso_calibrado) %>%
  tbl_svysummary(
    by = cidade_auditoria,
    include = c(regiao, cidade_auditoria),
    label = list(regiao ~ "Região"),
    statistic = list(all_categorical() ~ "{p}%")
  ) %>%
  add_overall(last = TRUE) %>% 
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**",
    stat_1 = "**Não**",
    stat_2 = "**Sim**"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(all_stat_cols() ~ NA)
#
#
#
#
#
#
#| label: Tabela AF global
#| code-summary: Tabela

n_miss_AF_global <- df %>% 
  filter(zona == 1) %>% 
  summarise(n = sum(is.na(AF_global))) %>% 
  pull(n)

df %>%
  filter(zona == 1) %>%
  mutate(
    AF_global = factor(AF_global, 
                       levels = c(0, 1), 
                       labels = c("Inativo", "Ativo")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  select(AF_global, cidade_auditoria) %>%
  tbl_summary(
    by = cidade_auditoria,
    label = list(AF_global ~ "AF global"),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**<br>N = {N}"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(
    label ~ paste0("Dados ausentes: ", n_miss_AF_global)
  )
#
#
#
#| label: Tabela AF global com desenho amostral
#| code-summary: Tabela com desenho amostral

df %>%
  filter(zona == 1) %>%
  mutate(
    AF_global = factor(AF_global, 
                       levels = c(0, 1), 
                       labels = c("Inativo", "Ativo")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  as_survey_design(ids = upa, strata = estrato, weights = peso_calibrado) %>%
  tbl_svysummary(
    by = cidade_auditoria,
    include = c(AF_global, cidade_auditoria),
    label = list(AF_global ~ "AF global"),
    statistic = list(all_categorical() ~ "{p}%"),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**",
    stat_1 = "**Não**",
    stat_2 = "**Sim**"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(
    label ~ paste0("Dados ausentes: ", n_miss_AF_global),
    all_stat_cols() ~ NA
  )
#
#
#
#
#
#
#| label: Tabela caminhada
#| code-summary: Tabela

n_miss_caminhada_cat <- df %>% 
  filter(zona == 1) %>% 
  summarise(n = sum(is.na(caminhada_cat))) %>% 
  pull(n)

df %>%
  filter(zona == 1) %>%
  mutate(
    caminhada_cat = factor(caminhada_cat, 
                           levels = c(0, 1), 
                           labels = c("Inativo", "Ativo")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  select(caminhada_cat, cidade_auditoria) %>%
  tbl_summary(
    by = cidade_auditoria,
    label = list(caminhada_cat ~ "Caminhada"),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**<br>N = {N}"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(
    label ~ paste0("Dados ausentes: ", n_miss_caminhada_cat)
  )
#
#
#
#| label: Tabela caminhada com desenho amostral
#| code-summary: Tabela com desenho amostral

df %>%
  filter(zona == 1) %>%
  mutate(
    caminhada_cat = factor(caminhada_cat, 
                           levels = c(0, 1), 
                           labels = c("Inativo", "Ativo")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  as_survey_design(ids = upa, strata = estrato, weights = peso_calibrado) %>%
  tbl_svysummary(
    by = cidade_auditoria,
    include = c(caminhada_cat, cidade_auditoria),
    label = list(caminhada_cat ~ "Caminhada"),
    statistic = list(all_categorical() ~ "{p}%"),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**",
    stat_1 = "**Não**",
    stat_2 = "**Sim**"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(
    label ~ paste0("Dados ausentes: ", n_miss_caminhada_cat),
    all_stat_cols() ~ NA
  )
#
#
#
#
#
#
#| label: Tabela sintomas depressivos
#| code-summary: Tabela

n_miss_sint_dep <- df %>% 
  filter(zona == 1) %>% 
  summarise(n = sum(is.na(sint_dep))) %>% 
  pull(n)

df %>%
  filter(zona == 1) %>%
  mutate(
    sint_dep = factor(sint_dep, levels = c(0, 1), labels = c("Não", "Sim")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  select(sint_dep, cidade_auditoria) %>%
  tbl_summary(
    by = cidade_auditoria,
    label = list(sint_dep ~ "Sintomas depressivos"),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**<br>N = {N}"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(
    label ~ paste0("Dados ausentes: ", n_miss_sint_dep)
  )
#
#
#
#
#| label: Tabela sintomas depressivos com desenho amostral
#| code-summary: Tabela com desenho amostral

df %>%
  filter(zona == 1) %>%
  mutate(
    sint_dep = factor(sint_dep, levels = c(0, 1), labels = c("Não", "Sim")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  as_survey_design(ids = upa, strata = estrato, weights = peso_calibrado) %>%
  tbl_svysummary(
    by = cidade_auditoria,
    include = c(sint_dep, cidade_auditoria),
    label = list(sint_dep ~ "Sintomas depressivos"),
    statistic = list(all_categorical() ~ "{p}%"),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**",
    stat_1 = "**Não**",
    stat_2 = "**Sim**"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(
    label ~ paste0("Dados ausentes: ", n_miss_sint_dep),
    all_stat_cols() ~ NA
  )
#
#
#
#
#
#
#| label: Tabela autoavaliação de saúde
#| code-summary: Tabela

n_miss_aas <- df %>% 
  filter(zona == 1) %>% 
  summarise(n = sum(is.na(aas))) %>% 
  pull(n)

df %>%
  filter(zona == 1) %>%
  mutate(
    aas = factor(aas, levels = c(0, 1), labels = c("Muito ruim/ruim", "Excelente/Muito boa/Boa/Regular")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  select(aas, cidade_auditoria) %>%
  tbl_summary(
    by = cidade_auditoria,
    label = list(aas ~ "Autoavaliação de saúde"),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**<br>N = {N}"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(
    label ~ paste0("Dados ausentes: ", n_miss_aas)
  )
#
#
#
#
#| label: Tabela autoavaliação de saúde com desenho amostral
#| code-summary: Tabela com desenho amostral

df %>%
  filter(zona == 1) %>%
  mutate(
    aas = factor(aas, levels = c(0, 1), labels = c("Muito ruim/ruim", "Excelente/Muito boa/Boa/Regular")),
    cidade_auditoria = factor(cidade_auditoria, levels = c(0, 1), labels = c("Não", "Sim"))
  ) %>%
  as_survey_design(ids = upa, strata = estrato, weights = peso_calibrado) %>%
  tbl_svysummary(
    by = cidade_auditoria,
    include = c(aas, cidade_auditoria),
    label = list(aas ~ "Autoavaliação de saúde"),
    statistic = list(all_categorical() ~ "{p}%"),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  modify_header(
    label = "**Variável**",
    stat_0 = "**Total**",
    stat_1 = "**Não**",
    stat_2 = "**Sim**"
  ) %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Auditoria virtual**"
  ) %>%
  modify_footnote(
    label ~ paste0("Dados ausentes: ", n_miss_aas),
    all_stat_cols() ~ NA
  )
#
#
#
#
#
#
#
#
#
#
#
#| label: Modelos multinivel AF global
#| code-summary: "Regressão logística com efeito aleatório para setor"
#| warning: false
#| message: false

# Base de dados (sem survey design)
dados <- df %>%
  filter(zona == 1,
         !is.na(AF_global),
         !is.na(identificador)) %>%
  mutate(setor = factor(setor))

# Variáveis e rótulos
rotulos <- c(
  dom_urbana1_escala = "Pathway accessibility",
  dom_urbana2_escala = "Surface and continuity",
  dom_transito1_escala = "Markings and controls",
  dom_transito2_escala = "Speed and bus lane",
  dom_desordem1_escala = "Absence of infrastructure and vegetation",
  dom_desordem2_escala = "Absence of building and green spaces"
)
exposicoes <- names(rotulos)
ajustes <- c("sexo", "idade", "cor_raca", "escolaridade")

# Função para rodar e salvar os modelos
modelos <- function(exp_var) {
  # Fórmula dinâmica com intercepto aleatório do setor
  form <- as.formula(paste("AF_global ~", exp_var, "+", paste(ajustes, collapse = " + "),
                           "+ (1 | setor)"))
  # Modelo
  fit_glmmTMB_af <- glmmTMB(
    formula = form,
    data = dados,
    family = binomial(link = "logit")
  )
  # Salva o modelo
  assign(paste0("fit_glmmTMB_af_", exp_var), fit_glmmTMB_af, envir = .GlobalEnv)
  # Rótulo apenas para a exposição atual
  rotulo_atual <- setNames(list(as.character(rotulos[exp_var])), exp_var)
  # Tabela de regressão
  fit_glmmTMB_af %>%
    tbl_regression(
      exponentiate = TRUE,
      include = all_of(exp_var),
      pvalue_fun = ~style_pvalue(.x, digits = 3),
      label = rotulo_atual
    ) %>%
    remove_row_type(type = "header") 
}

# Tabela
tabela_glmmTMB_af <- map(exposicoes, modelos) %>%
  tbl_stack() %>%
  modify_header(
    label = "**Neighborhood indicators**",
    estimate = "**OR**",
    conf.low = "**95% CI**"
  ) %>%
  modify_spanning_header(
    c(estimate, conf.low, conf.high, p.value) ~ "**Global physical activity**"
  ) %>%
  modify_footnote(
    label ~ "Each row represents a separate model with a random intercept for census tract, adjusted for sex, age, race/color, and education."
  ) %>%
  modify_footnote(
    c(estimate) ~ "OR = Odds Ratio",
    abbreviation = TRUE
  )

tabela_glmmTMB_af
#
#
#
#| label: Modelos AF global com desenho amostral 
#| code-summary: "Regressão logística com desenho amostral complexo"

# Desenho amostral
design_modelo <- df %>%
  filter(zona == 1) %>%
  filter(!is.na(AF_global), 
         !is.na(identificador)) %>%
  as_survey_design(ids = upa, strata = estrato, weights = peso_calibrado)

# Variáveis e rótulos
rotulos <- c(
  dom_urbana1_escala = "Pathway accessibility",
  dom_urbana2_escala = "Surface and continuity",
  dom_transito1_escala = "Markings and controls",
  dom_transito2_escala = "Speed and bus lane",
  dom_desordem1_escala = "Absence of infrastructure and vegetation",
  dom_desordem2_escala = "Absence of building and green spaces"
)
exposicoes <- names(rotulos)
ajustes <- c("sexo", "idade", "cor_raca", "escolaridade")

# Função para rodar e salvar os modelos
modelos <- function(exp_var) {
  # Fórmula dinâmica
  form <- as.formula(paste("AF_global ~", exp_var, "+", paste(ajustes, collapse = " + ")))
  # Modelo
  fit_glm_af <- svyglm(
    formula = form,
    design = design_modelo,
    family = quasibinomial(link = "logit")
  )
  # Salva o modelo, exemplo: fit_glm_af_dom_urbana1_escala
  assign(paste0("fit_glm_af_", exp_var), fit_glm_af, envir = .GlobalEnv)
  # Rótulo apenas para a exposição atual
  rotulo_atual <- setNames(list(as.character(rotulos[exp_var])), exp_var)
  # Tabela de regressão
  fit_glm_af %>%
    tbl_regression(
      exponentiate = TRUE,
      include = all_of(exp_var),
      pvalue_fun = ~style_pvalue(.x, digits = 3),
      label = rotulo_atual
    )
}

# Tabela
tabela_glm_af <- map(exposicoes, modelos) %>%
  tbl_stack() %>%
  modify_header(
    label = "**Neighborhood indicators**"
  ) %>%
  modify_spanning_header(
    c(estimate, conf.low, conf.high, p.value) ~ "**Global physical activity**"
  ) %>%
  modify_footnote(
    label ~ "Each row represents a separate model adjusted for sex, age, race/color, and education."
  )

tabela_glm_af
#
#
#
#
#
#
#| label: Modelos multinivel caminhada
#| code-summary: "Regressão logística com efeito aleatório para setor"
#| warning: false
#| message: false

# Base de dados (sem survey design)
dados <- df %>%
  filter(zona == 1,
         !is.na(caminhada_cat),
         !is.na(identificador)) %>%
  mutate(setor = factor(setor))

# Variáveis e rótulos
rotulos <- c(
  dom_urbana1_escala = "Pathway accessibility",
  dom_urbana2_escala = "Surface and continuity",
  dom_transito1_escala = "Markings and controls",
  dom_transito2_escala = "Speed and bus lane",
  dom_desordem1_escala = "Absence of infrastructure and vegetation",
  dom_desordem2_escala = "Absence of building and green spaces"
)
exposicoes <- names(rotulos)
ajustes <- c("sexo", "idade", "cor_raca", "escolaridade")

# Função para rodar e salvar os modelos
modelos <- function(exp_var) {
  # Fórmula dinâmica com intercepto aleatório do setor
  form <- as.formula(paste("caminhada_cat ~", exp_var, "+", paste(ajustes, collapse = " + "),
                           "+ (1 | setor)"))
  # Modelo
  fit_glmmTMB_cam <- glmmTMB(
    formula = form,
    data = dados,
    family = binomial(link = "logit")
  )
  # Salva o modelo
  assign(paste0("fit_glmmTMB_cam_", exp_var), fit_glmmTMB_cam, envir = .GlobalEnv)
  # Rótulo apenas para a exposição atual
  rotulo_atual <- setNames(list(as.character(rotulos[exp_var])), exp_var)
  # Tabela de regressão
  fit_glmmTMB_cam %>%
    tbl_regression(
      exponentiate = TRUE,
      include = all_of(exp_var),
      pvalue_fun = ~style_pvalue(.x, digits = 3),
      label = rotulo_atual
    ) %>%
    remove_row_type(type = "header") 
}

# Tabela
tabela_glmmTMB_cam <- map(exposicoes, modelos) %>%
  tbl_stack() %>%
  modify_header(
    label = "**Neighborhood indicators**",
    estimate = "**OR**",
    conf.low = "**95% CI**"
  ) %>%
  modify_spanning_header(
    c(estimate, conf.low, conf.high, p.value) ~ "**Walking**"
  ) %>%
  modify_footnote(
    label ~ "Each row represents a separate model with a random intercept for census tract, adjusted for sex, age, race/color, and education."
  ) %>%
  modify_footnote(
    c(estimate) ~ "OR = Odds Ratio",
    abbreviation = TRUE
  )

tabela_glmmTMB_cam
#
#
#
#| label: Modelos caminhada com desenho amostral
#| code-summary: "Regressão logística com desenho amostral complexo"

# Desenho amostral
design_modelo <- df %>%
  filter(zona == 1) %>%
  filter(!is.na(caminhada_cat), 
         !is.na(identificador)) %>%
  as_survey_design(ids = upa, strata = estrato, weights = peso_calibrado)

# Variáveis e rótulos
rotulos <- c(
  dom_urbana1_escala = "Pathway accessibility",
  dom_urbana2_escala = "Surface and continuity",
  dom_transito1_escala = "Markings and controls",
  dom_transito2_escala = "Speed and bus lane",
  dom_desordem1_escala = "Absence of infrastructure and vegetation",
  dom_desordem2_escala = "Absence of building and green spaces"
)
exposicoes <- names(rotulos)
ajustes <- c("sexo", "idade", "cor_raca", "escolaridade")

# Função para rodar e salvar os modelos
modelos <- function(exp_var) {
  # Fórmula dinâmica
  form <- as.formula(paste("caminhada_cat ~", exp_var, "+", paste(ajustes, collapse = " + ")))
  # Modelo
  fit_glm_cam <- svyglm(
    formula = form,
    design = design_modelo,
    family = quasibinomial(link = "logit")
  )
  # Salva o modelo
  assign(paste0("fit_glm_cam_", exp_var), fit_glm_cam, envir = .GlobalEnv)
  # Rótulo apenas para a exposição atual
  rotulo_atual <- setNames(list(as.character(rotulos[exp_var])), exp_var)
  # Tabela de regressão
  fit_glm_cam %>%
    tbl_regression(
      exponentiate = TRUE,
      include = all_of(exp_var),
      pvalue_fun = ~style_pvalue(.x, digits = 3),
      label = rotulo_atual
    )
}

# Tabela
tabela_glm_cam <- map(exposicoes, modelos) %>%
  tbl_stack() %>%
  modify_header(
    label = "**Neighborhood indicators**"
  ) %>%
  modify_spanning_header(
    c(estimate, conf.low, conf.high, p.value) ~ "**Walking**"
  ) %>%
  modify_footnote(
    label ~ "Each row represents a separate model adjusted for sex, age, race/color, and education."
  )

tabela_glm_cam
#
#
#
#
#
#
#| label: Modelos multinivel sintomas depressivos
#| code-summary: "Regressão logística com efeito aleatório para setor"
#| warning: false
#| message: false

# Base de dados (sem survey design)
dados <- df %>%
  filter(zona == 1,
         !is.na(sint_dep),
         !is.na(identificador)) %>%
  mutate(setor = factor(setor))

# Variáveis e rótulos
rotulos <- c(
  dom_urbana1_escala = "Pathway accessibility",
  dom_urbana2_escala = "Surface and continuity",
  dom_transito1_escala = "Markings and controls",
  dom_transito2_escala = "Speed and bus lane",
  dom_desordem1_escala = "Absence of infrastructure and vegetation",
  dom_desordem2_escala = "Absence of building and green spaces"
)
exposicoes <- names(rotulos)
ajustes <- c("sexo", "idade", "cor_raca", "escolaridade")

# Função para rodar e salvar os modelos
modelos <- function(exp_var) {
  form <- as.formula(paste("sint_dep ~", exp_var, "+", paste(ajustes, collapse = " + "),
          "+ (1 | setor)"))
  fit_glmmTMB_dep <- glmmTMB(
    formula = form,
    data = dados,
    family = binomial(link = "logit")
  )
  assign(paste0("fit_glmmTMB_dep_", exp_var), fit_glmmTMB_dep, envir = .GlobalEnv)
  rotulo_atual <- setNames(list(as.character(rotulos[exp_var])), exp_var)
  fit_glmmTMB_dep %>%
    tbl_regression(
      exponentiate = TRUE,
      include = all_of(exp_var),
      pvalue_fun = ~style_pvalue(.x, digits = 3),
      label = rotulo_atual
    ) %>%
    remove_row_type(type = "header")
}

# Tabela
tabela_glmmTMB_dep <- map(exposicoes, modelos) %>%
  tbl_stack() %>%
  modify_header(
    label = "**Neighborhood indicators**",
    estimate = "**OR**",
    conf.low = "**95% CI**"
  ) %>%
  modify_spanning_header(
    c(estimate, conf.low, conf.high, p.value) ~ "**Depressive symptoms**"
  ) %>%
  modify_footnote(
    label ~ "Each row represents a separate model with a random intercept for census tract, adjusted for sex, age, race/color, and education."
  ) %>%
  modify_footnote(
    c(estimate) ~ "OR = Odds Ratio",
    abbreviation = TRUE
  )

tabela_glmmTMB_dep
#
#
#
#| label: Modelos sintomas depressivos com desenho amostral
#| code-summary: "Regressão logística com desenho amostral complexo"

# Desenho amostral
design_modelo <- df %>%
  filter(zona == 1) %>%
  filter(!is.na(sint_dep),
         !is.na(identificador)) %>%
  as_survey_design(ids = upa,
                   strata = estrato,
                   weights = peso_calibrado)

# Variáveis e rótulos
rotulos <- c(
  dom_urbana1_escala = "Pathway accessibility",
  dom_urbana2_escala = "Surface and continuity",
  dom_transito1_escala = "Markings and controls",
  dom_transito2_escala = "Speed and bus lane",
  dom_desordem1_escala = "Absence of infrastructure and vegetation",
  dom_desordem2_escala = "Absence of building and green spaces"
)
exposicoes <- names(rotulos)
ajustes <- c("sexo", "idade", "cor_raca", "escolaridade")

# Função para rodar e salvar os modelos
modelos <- function(exp_var) {
  form <- as.formula(
    paste("sint_dep ~", exp_var, "+", paste(ajustes, collapse = " + ")))
  fit_glm_dep <- svyglm(
    formula = form,
    design = design_modelo,
    family = quasibinomial(link = "logit")
  )
  assign(paste0("fit_glm_dep_", exp_var), fit_glm_dep, envir = .GlobalEnv)
  rotulo_atual <- setNames(list(as.character(rotulos[exp_var])), exp_var)
  fit_glm_dep %>%
    tbl_regression(
      exponentiate = TRUE,
      include = all_of(exp_var),
      pvalue_fun = ~style_pvalue(.x, digits = 3),
      label = rotulo_atual
    )
}

# Tabela
tabela_glm_dep <- map(exposicoes, modelos) %>%
  tbl_stack() %>%
  modify_header(
    label = "**Neighborhood indicators**"
  ) %>%
  modify_spanning_header(
    c(estimate, conf.low, conf.high, p.value) ~ "**Depressive symptoms**"
  ) %>%
  modify_footnote(
    label ~ "Each row represents a separate model adjusted for sex, age, race/color, and education."
  )

tabela_glm_dep
#
#
#
#
#
#
#| label: Modelos multinivel autoavaliação de saúde
#| code-summary: "Regressão logística com efeito aleatório para setor"
#| warning: false
#| message: false

# Base de dados (sem survey design)
dados <- df %>%
  filter(zona == 1,
         !is.na(aas),
         !is.na(identificador)) %>%
  mutate(setor = factor(setor))

# Variáveis e rótulos
rotulos <- c(
  dom_urbana1_escala = "Pathway accessibility",
  dom_urbana2_escala = "Surface and continuity",
  dom_transito1_escala = "Markings and controls",
  dom_transito2_escala = "Speed and bus lane",
  dom_desordem1_escala = "Absence of infrastructure and vegetation",
  dom_desordem2_escala = "Absence of building and green spaces"
)
exposicoes <- names(rotulos)
ajustes <- c("sexo", "idade", "cor_raca", "escolaridade")

# Função para rodar e salvar os modelos
modelos <- function(exp_var) {
  form <- as.formula(paste("aas ~", exp_var, "+", paste(ajustes, collapse = " + "),
          "+ (1 | setor)"))
  fit_glmmTMB_aas <- glmmTMB(
    formula = form,
    data = dados,
    family = binomial(link = "logit")
  )
  assign(paste0("fit_glmmTMB_aas_", exp_var), fit_glmmTMB_aas, envir = .GlobalEnv)
  rotulo_atual <- setNames(list(as.character(rotulos[exp_var])), exp_var)
  fit_glmmTMB_aas %>%
    tbl_regression(
      exponentiate = TRUE,
      include = all_of(exp_var),
      pvalue_fun = ~style_pvalue(.x, digits = 3),
      label = rotulo_atual
    ) %>%
    remove_row_type(type = "header")
}

# Tabela
tabela_glmmTMB_aas <- map(exposicoes, modelos) %>%
  tbl_stack() %>%
  modify_header(
    label = "**Neighborhood indicators**",
    estimate = "**OR**",
    conf.low = "**95% CI**"
  ) %>%
  modify_spanning_header(
    c(estimate, conf.low, conf.high, p.value) ~ "**Self-rated health**"
  ) %>%
  modify_footnote(
    label ~ "Each row represents a separate model with a random intercept for census tract, adjusted for sex, age, race/color, and education."
  ) %>%
  modify_footnote(
    c(estimate) ~ "OR = Odds Ratio",
    abbreviation = TRUE
  )

tabela_glmmTMB_aas
#
#
#
#| label: Modelos autoavaliação de saúde com desenho amostral
#| code-summary: "Regressão logística com desenho amostral complexo"

# Desenho amostral
design_modelo <- df %>%
  filter(zona == 1) %>%
  filter(!is.na(aas),
         !is.na(identificador)) %>%
  as_survey_design(ids = upa,
                   strata = estrato,
                   weights = peso_calibrado)

# Variáveis e rótulos
rotulos <- c(
  dom_urbana1_escala = "Pathway accessibility",
  dom_urbana2_escala = "Surface and continuity",
  dom_transito1_escala = "Markings and controls",
  dom_transito2_escala = "Speed and bus lane",
  dom_desordem1_escala = "Absence of infrastructure and vegetation",
  dom_desordem2_escala = "Absence of building and green spaces"
)
exposicoes <- names(rotulos)
ajustes <- c("sexo", "idade", "cor_raca", "escolaridade")

# Função para rodar e salvar os modelos
modelos <- function(exp_var) {
  form <- as.formula(
    paste("aas ~", exp_var, "+", paste(ajustes, collapse = " + ")))
  fit_glm_aas <- svyglm(
    formula = form,
    design = design_modelo,
    family = quasibinomial(link = "logit")
  )
  assign(paste0("fit_glm_aas_", exp_var), fit_glm_aas, envir = .GlobalEnv)
  rotulo_atual <- setNames(list(as.character(rotulos[exp_var])), exp_var)
  fit_glm_aas %>%
    tbl_regression(
      exponentiate = TRUE,
      include = all_of(exp_var),
      pvalue_fun = ~style_pvalue(.x, digits = 3),
      label = rotulo_atual
    )
}

# Tabela
tabela_glm_aas <- map(exposicoes, modelos) %>%
  tbl_stack() %>%
  modify_header(
    label = "**Neighborhood indicators**"
  ) %>%
  modify_spanning_header(
    c(estimate, conf.low, conf.high, p.value) ~ "**Self-rated health**"
  ) %>%
  modify_footnote(
    label ~ "Each row represents a separate model adjusted for sex, age, race/color, and education."
  )

tabela_glm_aas
#
#
#
