library(tidyverse)
library(tibble)
library(ciceroUI)

model <- feather::read_feather('../spider/data/metadata/indicators.feather')
model <-
    model %>%
    mutate_at(
        vars(indicator.name, indicator.name.in.source, Definition, Explanation),
        as.character
    ) %>%
    filter(str_replace_all(indicator.name, '\\W', '_') %in% colnames(indicators))
fragility_model <- tibble(
    dimension = 'fragility',
    subdimension = 'fragility',
    indicator.name = 'fragility',
    scaling = 'metric',
    scale.direction = 'low',
    source = 'Statistical model'
)
dim_model <- tibble(
    dimension = c('authority', 'legitimacy', 'capacity', 'social cohesion', 'other')
) %>%
    mutate(subdimension = dimension,
           indicator.name = dimension,
           scaling = 'metric',
           scale.direction = 'high',
           source = 'Statistical model')
subdim_model <-
    model %>%
    select(dimension, subdimension) %>%
    unique() %>%
    mutate(indicator.name = subdimension,
           scaling = 'metric',
           scale.direction = 'high',
           source = 'Statistical model')
pop_deaths <-
    tibble(
        dimension = 'other',
        subdimension = 'other',
        indicator.name = c('log_population', 'log_deaths'),
        scaling = 'metric',
        scale.direction = c('high', 'low'),
        source = c('V-DEM', 'UCDP')
    )
model <- bind_rows(
    fragility_model, dim_model, subdim_model, model
)

usethis::use_data(model, overwrite=TRUE)
