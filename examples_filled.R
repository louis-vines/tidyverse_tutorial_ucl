 ## Motivating example ------------------------------------------------------------
library(dslabs) # some cool motivating examples
# checkout: https://simplystatistics.org/2018/01/22/the-dslabs-package-provides-datasets-for-teaching-data-science/ 
library(tidyverse)
 
data(polls_us_election_2016)
as_tibble(polls_us_election_2016)

polls_us_election_2016 %>%
  as_tibble() %>%
  filter(state == "U.S." & enddate>="2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>% 
  mutate(candidate = factor(candidate, levels = c("Trump","Clinton"))) %>%
  group_by(pollster) %>%
  filter(n()>=10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +  
  geom_point(show.legend = FALSE, alpha=0.4)  + 
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30,50))
 
 
# NOTE: Clear environment!

## GOOD THINGS ABOUT R: --------------------------------------------------------

# Loads of packages that are super good at data analysis etc...

# stats packages are native and pre-included

# data frames are native and main focal point:
mtcars

# Vectorisation is native
mtcars$cyl * 2

mtcars$wt / mtcars$hp

as.Date('2020-01-01') + lubridate::days(0:30)

paste('letter number', seq_along(letters), 'in the alphabet is', letters)

## functions are very first class ----------------------------------------------------------------------

# how I assign a variable
string <- 'a'

# how I assign a fn
st_dev_fn <- function(input_data){
  sum_squares <- sum((input_data - mean(input_data)) ^ 2)
  sqrt(sum_squares)
}

class(st_dev_fn)

st_dev_fn(1:20)
st_dev_fn

(function(input_data){
  sum_squares <- sum((input_data - mean(input_data)) ^ 2)
  sqrt(sum_squares)
})(1:20)


# --------------------------------------------------------------------------
## lazy argument evaluation & non-standard evaluation
x <- seq(0, 2 * pi, length.out = 360)
plot(x, sin(x), type = 'l')

# This is a very appealing feature for writing DSLs in the language
'aaa' + 'bbb'

string_math <- function(x) {
  e <- rlang::env(
    rlang::caller_env(),
    `+` = function(x, y) paste0(x, y),
    `*` = function(x, y) strrep(x, y)
  )
  
  eval(rlang::enexpr(x), e)
}

string_math('aaa' + 'bbb')
string_math('rep' * 2)

## BAD THINGS ABOUT BASE R --------------------------------------------------------

# - Is a memory hog (very keen to copy data around
# (not that much of an issue if data is of OK size i.e. < several million rows)

# - Many ways to do the same thing
 
# - Some of these are convoluted and obscure intent

#e.g. selecting all integer columns from mtcars:
iris

integer_column_index <- rep(NA, dim(iris)[2])

for (i in seq_along(integer_column_index)) {
  integer_column_index[i] <- is.numeric(iris[,i])
}

iris[, integer_column_index]


# how we can do this using packages I'm about to show you...
dplyr::select_if(iris, is.numeric)

# - Base language is carrying around a ton of warts (that won't get fixed)
1 + 2
1 + NA

paste0(1, 2)
paste0(1, NA)

# When you don't understand R's internals, things like this can happen:
bad_df <- read.csv('data/bad_data.csv')
bad_df$x + bad_df$y

# multiple choice:
  # throw an error
  # parse with NA for a and throw a warning
  # parse with NA for a and don't throw a warning
  # none of the above

bad_df$y_numeric <- as.numeric(bad_df$y)
bad_df

# what happened here?
  
# R encodes any non-numeric data in dataframe as factor (categorical data) by default(!!!)
# The underlying encoding of these are integers assigned in alphabetical order

slightly_better_df <- read.csv('data/bad_data.csv', stringsAsFactors = FALSE)
slightly_better_df$y_numeric <- as.numeric(slightly_better_df$y)
slightly_better_df

# this whole factor thing even happens when we're just creating dataframes
char_df <- data.frame(letters = letters)
class(char_df$letters)

char_df <- data.frame(letters = letters, stringsAsFactors = FALSE)
class(char_df$letters)

# could go on and on...
# see here: https://www.talyarkoni.org/blog/2012/06/08/r-the-master-troll-of-statistical-languages/
# https://news.ycombinator.com/item?id=15868786


# TIDYVERSE TO THE RESCUE!!!! -------------------------------------------

# NOTE: you should only ever include library at top of file
# this just for presentation
library(tidyverse)

# show quick slide on packages
better_df <- read_csv('data/bad_data.csv')
better_df

better_df %>% print(n=Inf)

better_df %>% mutate(y = as.numeric(y)) %>% print(n=Inf)

# and the previous example...
str_c('a', 'b')
str_c('a', NA)

## LOOK AT DPLYR VERBS -----
# * select columns with `select` function
# * filter rows with `filter` function
# * sort data using `arrange` function
# * new columns with `mutate` function
# * create summary statistics with summarise
# * group by mechanism...

# this set of functions replaces most of the base R wrangling with subsets [,]
data("starwars")
starwars

# a lot better than this!
starwars %>% select(name:species) %>% as.data.frame()

# a little note on farmiliarising with data...
starwars %>% glimpse()
starwars %>% View()
starwars %>% select_if(is.numeric) %>% map(summary)
starwars %>% count(gender)

# select verb -------------------------------------------------------
# base R
starwars[, 'hair_color']

# numeric indexing is prone to bugs, but more powerful :'(
starwars[, 4:6]
starwars[, -3]

# not with select!
starwars %>% select(hair_color)
starwars %>% select(hair_color:birth_year)
starwars %>% select(-mass)

# string/regex matches
starwars %>% select(contains('_'))
starwars %>% select(matches('[rn]_c'))
?select

# logical selection - will revisit this later
starwars %>% select_if(is.numeric)


## A pipe operator !! ---------------------------
select(starwars, hair_color)

#f(x, y) <--> x %>% f(y)
starwars %>%
  select(hair_color)

# output of all these functions is also another data frame to can chain!
starwars %>% # object
  # chain of commands (verbs) to be done on the object:...
  select(name, height, gender) %>%
  arrange(gender)


# what about if the argument you want to pipe into isn't the first one?
starwars %>%
  lm(mass ~ height, data = .) %>%
  summary()

# filter, arrange --------------------------------------
starwars %>%
  filter(hair_color == 'blond')

# base version:
starwars[starwars$hair_color == 'blond', ]
starwars[!is.na(starwars$hair_color) & starwars$hair_color == 'blond', ]

starwars %>%
  arrange(height)

starwars %>%
  arrange(desc(height))

## mutate - new columns -------
star_wars_reduced <- starwars %>%
  select(name, height, mass)

star_wars_reduced %>%
  mutate(bmi = mass / height ^ 2)

# isn't this better than this???
star_wars_reduced$bmi <- star_wars_reduced$mass / star_wars_reduced$height ^ 2

# also now I've saved that on my original dataframe
star_wars_reduced <- starwars %>%
  select(name, species, height, mass)

# so mucher nicer using dplyr way as we can prototype....
OBESITY_BMI <- 30

star_wars_reduced %>%
  mutate(
    bmi = mass / height ^ 2
  )
  

#  select(-(height:height_sq))
#  filter(bmi > OBESITY_BMI)

## summarise, summary functions ----------------
starwars %>%
  summarise(
    row_counts = n(),
    mass_mean = mean(mass, na.rm = TRUE),
    missing_mass_count = sum(mass %>% is.na()),
    max_bmi = max(mass / (height / 100) ^ 2, na.rm = TRUE),
    height_mean_human = mean(height[species == 'Human'], na.rm = TRUE)
  )

## special note: count function ----------------------------
starwars %>%
  count(species, sort = TRUE)

## SPLIT APPLY COMBINE (GROUP_BY)--------------------
# this is the magic....

# grouped summarise: -----
starwars %>%
  mutate(
    height_sq = (height / 100) ^ 2,
    bmi = mass / height_sq
  ) %>%
  group_by(species, gender) %>%
  summarise(
    row_counts = n(),
    mass_mean = mean(mass, na.rm = TRUE),
    height_mean = mean(height, na.rm = TRUE),
    bmi_mean = mean(bmi, na.rm=TRUE)
  ) %>%
  arrange(row_counts %>% desc()) %>%
  print(n=Inf)


# grouped mutate: ----
library(dslabs)
data("gapminder") # from dslabs package

gapminder
gapminder <- as_tibble(gapminder)


# let's calculate gdp_per_capita in most recent year and calculate top 3 by continent

# start by inspecting the data
gapminder %>%
  count(year) %>% 
  arrange(desc(year))

gapminder %>%
  count(year) %>% 
  arrange(desc(year)) %>%
  ggplot(aes(year, n)) + geom_line()


gapminder %>%
  filter(year == max(year)) %>%
  print(n=Inf)


gapminder %>%
  group_by(year) %>%
  summarise(gdp_data_count = sum(!(gdp %>% is.na()))) %>%
  arrange(year %>% desc())

# around 2011 we have enough data - let's use this year but not hard code it...
year_for_analysis <- gapminder %>%
  group_by(year) %>%
  summarise(gdp_data_count = sum(!(gdp %>% is.na()))) %>%
  filter(gdp_data_count > 100) %>%
  pull(year) %>%
  max()


# lets look at ranking countries by gdp per capita...
gapminder %>%
  filter(year == year_for_analysis, !is.na(gdp)) %>%
  # select(-(year:fertility)) %>%
  # group_by(continent) %>%
  mutate(
    gdp_per_capita = gdp / population,
    gdp_pc_rank = dense_rank(gdp_per_capita %>% desc())
  ) %>%
  filter(gdp_pc_rank <= 3) %>%
  arrange(
    continent, gdp_pc_rank
  ) %>%
  ungroup()


top_gdp_per_capita <- gapminder %>%
  filter(year == year_for_analysis, !is.na(gdp)) %>%
  select(-(year:fertility)) %>%
  group_by(continent) %>%
  mutate(
    gdp_per_capita = gdp / population,
    gdp_pc_rank = row_number(gdp_per_capita %>% desc())
  ) %>%
  filter(gdp_pc_rank <= 3) %>%
  arrange(
    continent, gdp_pc_rank
  ) %>%
  ungroup()

# bit hard to take in so let's plot it....
top_gdp_per_capita %>%
#  mutate(country = fct_reorder(country, gdp_per_capita)) %>%
  ggplot(aes(country, gdp_per_capita)) +
  geom_col() +
  facet_wrap(. ~ continent, scales = 'free_y', ncol=1)  +
  coord_flip()

# other uses of grouped mutate: lag, lead etc... and finding proportions of totals...
# https://rstudio.com/resources/cheatsheets/
# data transformation

# https://r4ds.had.co.nz/

## doing more complicated stuff with map----------------------------------------------------------------------

# let's explore how life expectancy has changed since 1960

gapminder %>%
  ggplot(aes(year, life_expectancy)) +
  geom_line(aes(group=country), alpha = 0.4) +
  geom_smooth()

# overall increase in life expectancy:
gapminder %>%
  lm(life_expectancy ~ year, data = .) %>%
  summary()


# how about if we want to see how life expectancy has changed by country?
# we want to fit a regression model for each country
# this is split apply combine but more complicated....

gapminder %>%
  split(.$country)


# for loop... 
all_countries_df <- gapminder %>%
  split(.$country)

countries_lm_results <- list()
for (country_df in all_countries_df) {
  # let's not bother...
}


# use map function to iterate over each element and return something...
gapminder %>%
  split(.$country) %>%
  map(function(country_df) {
    lm(life_expectancy ~ year, data = country_df)
  })


# how do we format into a data frame for further analysis
gapminder %>%
  split(.$country) %>%
  map(function(country_df) {
    lm(life_expectancy ~ year, data = country_df) %>%
      summary()
  })


library(broom)

gapminder %>%
  split(.$country) %>%
  map(function(country_df) {
    lm(life_expectancy ~ year, data = country_df) %>%
      tidy()
  })


# turn this into one data frame at the end
gapminder %>%
  split(.$country) %>%
  map(function(country_df) {
    lm(life_expectancy ~ year, data = country_df) %>%
      tidy()
  }) %>%
  bind_rows(.id = 'country')


# we can do that conversion with a specialised map function
gapminder %>%
  split(.$country) %>%
  map_dfr(function(country_df) {
    lm(life_expectancy ~ year, data = country_df) %>%
      tidy()
  }, .id = 'country')


countrywise_life_expectancy <- gapminder %>%
  split(.$country) %>%
  map_dfr(function(country_df) {
    lm(life_expectancy ~ year, data = country_df) %>%
      tidy() %>%
      as_tibble()
  }, .id = 'country')


# which countries haven't had a significant change in life expectancy?
country_le_adjusted_p <- countrywise_life_expectancy %>%
  filter(term == 'year') %>%
  mutate(
    p_adjusted = p.adjust(p.value, method = 'fdr'),
    significant = p_adjusted < 0.05
  )

country_le_adjusted_p %>%
  count(significant)


country_le_adjusted_p %>%
  filter(!significant)


# which have had a significant decrease
country_le_adjusted_p %>%
  filter(significant) %>%
  count(estimate < 0)

country_le_adjusted_p %>%
  filter(significant) %>%
  filter(estimate < 0)


# which have increased the most?
country_le_adjusted_p %>%
  filter(significant) %>%
  arrange(estimate %>% desc()) %>%
  print(n = 50)


# other functionality of the broom package: 
gapminder %>%
  split(.$country) %>%
  map(function(country_df) {
    lm(life_expectancy ~ year, data = country_df) %>%
      glance()
  })


gapminder %>%
  split(.$country) %>%
  map(function(country_df) {
    lm(life_expectancy ~ year, data = country_df) %>%
      augment()
  })

