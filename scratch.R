#grouped mutate example

data("gapminder") # from dslabs package
gapminder
gapminder <- as_tibble(gapminder)

gapminder %>% count(year) %>% arrange(desc(year))

gapminder %>%
  filter(year == max(year)) %>%
  print(n=Inf)


# when is the last year with good data?
gapminder %>%
  group_by(year) %>%
  summarise(gdp_data_count = sum(!(gdp %>% is.na()))) %>%
  arrange(year %>% desc())

year_for_analysis <- gapminder %>%
  group_by(year) %>%
  summarise(gdp_data_count = sum(!(gdp %>% is.na()))) %>%
  filter(gdp_data_count > 100) %>%
  .$year %>%
  max()

gapminder %>%
  filter(year == year_for_analysis, !is.na(gdp)) %>%
  #select(-(year:fertility)) %>%
  group_by(continent) %>%
  mutate(
    gdp_per_capita = gdp / population,
    gdp_pc_rank = row_number(gdp_per_capita %>% desc())
  ) %>%
  filter(gdp_pc_rank <= 3) %>%
  arrange(
    gdp_pc_rank, continent
  )


population_growth <- gapminder %>%
  arrange(country, year) %>%
  filter(!is.na(population)) %>%
  group_by(country) %>%
  mutate(
    prev_population = lag(population),
    population_growth = (population - prev_population) / prev_population
  ) %>%
  filter(!is.na(population_growth))# filter population_growth not missing


population_growth %>%
  print(n=300)

population_growth %>%
  #filter(region != 'Western Asia') %>%
  arrange(population_growth %>% desc()) %>%
  print(n=100)

# what about china?
population_growth %>%
  filter(country == 'China') %>%
  ggplot(aes(year, population_growth)) +
  geom_line()


## finally, what about doing more complicated stuff to groups
population_growth %>%
  split(.$country) %>%
  map(function(df) {
    lm(year ~ population_growth, data = df)
  })


library(broom)
population_growth %>%
  split(.$country) %>%
  map(function(df) {
    lm(year ~ population_growth, data = df) %>%
      tidy()
  })


adjusted_pop_growth <- population_growth %>%
  split(.$country) %>%
  map_dfr(function(df) {
    lm(year ~ population_growth, data = df) %>%
      tidy() %>%
      as_tibble()
  }, .id = 'country') %>%
  filter(term == 'population_growth') %>%
  mutate(adjusted_p_value = p.adjust(p.value, 'fdr')) %>%
  filter(adjusted_p_value < 0.05)

adjusted_pop_growth %>%
  arrange(estimate %>% desc())


