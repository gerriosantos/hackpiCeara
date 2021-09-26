
library(tidyverse)

# reg <- readr::read_rds('data-raw/d_dash_geom.rds') |>
#   as_tibble() |> select(-geom)


# reg <- haven::read_dta('data-raw/d_dash.dta') |>
#   select(cod_ibge, reg_plan)
#
#
# dados <- readr::read_csv('data-raw/TS_ALUNO.csv') |>
#   dplyr::mutate(uf = as.integer(substr(ID_MUNICIPIO,1,2))) |>
#   dplyr::filter(uf == 23) |>
#   dplyr::select(
#     ID_UF:IN_SALA_SEPARADA_MT,
#     TX_RESPOSTA_LP:TX_RESPOSTA_MT,
#     PROFICIENCIA_LPO, PROFICIENCIA_MT,
#     PROFICIENCIA_LPO_ANA, PROFICIENCIA_MT_ANA
#
#   ) |> left_join(reg, c('ID_MUNICIPIO'='cod_ibge'))
#
#
#
# # Funcao ----
#
# getInfo <- function(data = dados, id_group, id_cont_distinct = ID_ALUNO){
#
#   data |>
#     group_by({{id_group}}) |>
#     summarise(matricula = n_distinct({{id_cont_distinct}}),
#               across(PROFICIENCIA_LPO:PROFICIENCIA_MT_ANA,
#                      ~ mean(.x, na.rm = T)))
# }
#
# # Regiao ----
#
# regiao <- getInfo(id_group = reg_plan)
#
# # Municipios -----
# mun <- getInfo(id_group = ID_MUNICIPIO)
#
# # Escolas ----
# escola <- getInfo(id_group = ID_ESCOLA)
#
# # Turma ----
# turma <- getInfo(id_group = ID_TURMA)



              ### BASE DO LEPES -----


d <- readr::read_csv('base_dados_simulada_ eapi.txt',
                     locale = readr::locale(encoding = 'latin1')) |>
  dplyr::select(-c("...1"))


readr::write_rds(d, 'base.rds')

# readr::write_csv(d, 'data/d_hackPI.csv')
# openxlsx::write.xlsx(d, 'data/d_hackPI.xlsx')



# Analisando da questão od_q19 a od_q30 (exceto od_q22),
# fizemos um compilação
# para saber a proporção de oportunidades de aprendizagem não
# realizada por turma.

d1 <- d |>
  select(nome_turma, od_q19:od_q30, -od_q22) |>
  mutate(across(od_q19:od_q30, ~ if_else(.x == 1, 1, 0))) |>
  pivot_longer(-nome_turma) |>
  group_by(nome_turma) |>
  summarise(oportu_aprend = mean(value, na.rm = T))

d1 <- d |>
  select(nome_turma, od_q19:od_q30, -od_q22) |>
  mutate(across(od_q19:od_q30, ~ if_else(.x == 1, 1, 0))) |>
  group_by(nome_turma) |>
  summarise(across(od_q19:od_q30, ~ round(mean(.x)*100,2))) |>
  pivot_longer(cols = -nome_turma,
               names_to = 'q_oport_aprend',
               values_to = 'perc_vezes_opor_n_observ')

#readr::write_csv(d1, 'bases_limpas/produto_od19_od30_exceto_od22.csv')
openxlsx::write.xlsx(d1, 'produto_od19_od30_exceto_od22.xlsx')
readr::write_rds(d1, 'produto1.rds')



# Base que pega a quantidade de oportunidades de aprendizagem
# por categoria (escala de 1 a 4). Onde 1 é o mais ruim e 4 o melhor.


d2 <- d |>
  select(nome_turma, od_q19:od_q30, -od_q22) |>
  mutate(across(od_q19:od_q30,
                ~ case_when(
                  .x == 1 ~ 'Não Desejado',
                  .x == 2 ~ 'Qualidade Indesejada',
                  .x == 3 ~ 'Qualidade Intermediária',
                  .x == 4 ~ 'Qualidade Adequada')
  )) |>pivot_longer(-nome_turma) |>
  group_by(nome_turma, name) |>
  mutate(n_desej = ifelse(value == 'Não Desejado', 1, 0),
         qual_indes = ifelse(value == 'Qualidade Indesejada', 1, 0),
         qual_interm = ifelse(value == 'Qualidade Intermediária', 1, 0),
         qual_adeq =  ifelse(value == 'Qualidade Adequada',1,0)) |>
  summarise(across(n_desej:qual_adeq, ~ sum(.x)))

readr::write_rds(d2, 'escalas_oportun_aprend.rds')


openxlsx::write.xlsx(d2, 'oportunidde_aprend_escala.xlsx')





# Quantidade de crianças faltantes e presentes
## Fazer uma média para todas as turmas

d3 <- d |>
  select(nome_turma, ano_inicio, n_crianca_matriculada = od_q1,
         n_crianca_presente = od_q12, criancas_especiais = od_q2) |>
  group_by(nome_turma, ano_inicio) |>
  summarise(across(n_crianca_matriculada:criancas_especiais,
                   ~ round(sum(.x, na.rm = T),2))) |>
  mutate(
    crianca_faltantes = n_crianca_matriculada-n_crianca_presente,
    crianca_faltante_percent = round((crianca_faltantes/n_crianca_matriculada)*100,1),
    crianca_presente_percent = round((n_crianca_presente/n_crianca_matriculada)*100,1),
    percent_crianca_especiais = round((criancas_especiais/n_crianca_presente)*100,1))


openxlsx::write.xlsx(d3, 'matriculas_acessibilidade.xlsx')

