# Final R Script

# All Libraries:

library(tidyverse)
library(ggthemes)
library(scales)
library(plotly)
library(geomtextpath)

# Loading all the data:

# https://www.kaggle.com/datasets/piterfm/olympic-games-medals-19862018
athletes_1896_2022 <- read_csv("data/olympic_athletes.csv")
hosts_1896_2022 <- read_csv("data/olympic_hosts.csv")
medals_1896_2022 <- read_csv("data/olympic_medals.csv")
results_1896_2022 <- read_csv("data/olympic_results.csv")

# https://www.kaggle.com/datasets/piterfm/paris-2024-olympic-summer-games
medal_count_2024 <- read_csv("data/paris_2024/medals_total.csv")
detailed_medals_2024 <- read_csv("data/paris_2024/medals.csv")
schedule_2024 <- read_csv("data/paris_2024/schedules.csv")
nocs_2024 <- read_csv("data/paris_2024/nocs.csv")
medalists_2024 <- read_csv("data/paris_2024/medallists.csv")
events_2024 <- read_csv("data/paris_2024/events.csv")
athletes_2024 <- read_csv("data/paris_2024/athletes.csv")

#Also have lots of data for specific sports but idk how I could use it

# https://www.kaggle.com/datasets/piterfm/tokyo-2020-paralympics
para_medal_count_2024 <- read_csv("data/para_paris_2024/medals_total.csv")
para_detailed_medals_2024 <- read_csv("data/para_paris_2024/medals.csv")
para_schedule_2024 <- read_csv("data/para_paris_2024/schedules.csv")
para_nocs_2024 <- read_csv("data/para_paris_2024/nocs.csv")
para_medalists_2024 <- read_csv("data/para_paris_2024/medallists.csv")
para_sports_2024 <- read_csv("data/para_paris_2024/sports.csv")
para_athletes_2024 <- read_csv("data/para_paris_2024/athletes.csv")

#Also have lots of data for specific sports but idk how I could use it

# https://www.kaggle.com/datasets/piterfm/milano-cortina-2026-olympic-winter-games
athletes_2026 <- read_csv("data/milano_cortina_2026/athletes.csv")
medallists_2026 <- read_csv("data/milano_cortina_2026/medallists.csv")
medal_count_2026 <- read_csv("data/milano_cortina_2026/medals.csv")
schedule_2026 <- read_csv("data/milano_cortina_2026/schedules.csv")

# Other datasets (treating all 4 above as one since all from piterfm)

# https://www.kaggle.com/datasets/dataanalyst001/world-population-by-country-2024
world_pop_2024 <- read_csv("data/world_population_by_country_2024.csv")

# https://www.kaggle.com/datasets/enwelumchiamaka/world-population-by-country-2026
world_pop_2026 <- read_csv("data/world_population_by_country_2026.csv")

# https://www.kaggle.com/datasets/codebynadiia/gdp-per-country-2020-2024-csv
gdp_2020_2024 <- read_csv("data/gdp_2020_2024.csv")



# ************************************************************************************************************
# ************************************************************************************************************
# ************************************************************************************************************


# Lookign at overall medal trends


# CHANGE TO USING SEPARTE POSITION THING!!!!!!!!
# Did not work since since rio-de-jinerio-2016 for example has multiple "-"

results_1896_2022 <- results_1896_2022 |> 
  mutate(year = as.numeric(
    substr(slug_game, nchar(slug_game) - 3, nchar(slug_game))),
    .before = 4)

medals_1896_2022 <- medals_1896_2022 |> 
  mutate(year = as.numeric(
    substr(slug_game, nchar(slug_game) - 3, nchar(slug_game))),
    .before = 3)

medals_1896_2022 <- medals_1896_2022 |> 
  mutate(
    season = if_else(year < 1924, "Summer",
                     if_else( year <= 1992, "Combined",
                              if_else(year %% 4 == 2, "Winter", 
                                      "Summer"))))

medals_per_year_1896_2022 <- medals_1896_2022 |> 
  count(year, season)


ggplot(medals_1896_2022, aes(x = year)) +
  geom_histogram(binwidth = 4)

ggplot(medals_per_year_1896_2022, aes(x = year, y = n)) +
  geom_point() +
  labs(
    title = "How has the number of olympic medals changed over time?",
    x = "Year", y = "Number of Medals"
  )


ggplot(medals_per_year_1896_2022, aes(x = year, y = n, color = season)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("Summer" = "red", "Winter" = "blue", "Combined" = "purple")) +
  labs(
    title = "Medals Per Year Split by Type of Olympic Game",
    x = "Year", y = "Number of Medals",
    color = "Type of Olympic Game"
  )


ggplot(medals_per_year_1896_2022, aes(x = year, y = n)) +
  geom_point(aes(color = season)) +
  geom_textvline(xintercept = 1922, label = "Added Winter Sports to Summer Games", hjust = .95,
                 linetype = 3, vjust = 1.3, color = "black", size = 3) +
  geom_textvline(xintercept = 1993, label = "Seperate Summer and Winter Games", hjust = .4,
                 linetype = 3, vjust = 1.3, color = "black", size = 3) +
  scale_color_manual(values = c("Summer" = "red", "Winter" = "blue", "Combined" = "purple")) +
  labs(
    title = "Medals Per Year Split by Type of Olympic Game",
    x = "Year", y = "Number of Medals",
    color = "Type of Olympic Game"
  )



ggplot(medals_per_year_1896_2022, aes(x = year, y = n, color = season)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("Summer" = "red", "Winter" = "blue", "Combined" = "purple")) +
  facet_wrap(~season)

split_medals_per_year <- ggplot(medals_per_year_1896_2022, aes(x = year, y = n, color = season)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("Summer" = "red", "Winter" = "blue", "Combined" = "purple")) +
  facet_wrap(~season, scales = "free_x") + 
  labs(
    title = "Medals Per Year Split by Type of Olympic Game",
    x = "Year", y = "Number of Medals",
    color = "Type of Olympic Game"
  )

ggplotly(split_medals_per_year)



# ************************************************************************************************************
# ************************************************************************************************************
# ************************************************************************************************************


# I then got curious to see how gender equality has developed shown in oly data

gender_medal_hist <- ggplot(medals_1896_2022, aes(x = year, fill = event_gender)) +
  geom_histogram(binwidth = 4, position = "fill") +
  labs(
    title = "How has the gender split of olympic medals changed over time?",
    x = "Year", y = "Proportion",
    fill = "Gender of Event"
  )

ggplotly(gender_medal_hist)

ggplot(medals_1896_2022, aes(x = year, fill = event_gender)) +
  geom_histogram(binwidth = 2)

gender_medal_plot_split_by_season <- ggplot(medals_1896_2022, aes(x = year, fill = event_gender)) +
  geom_histogram(binwidth = 4, position = "fill") + 
  facet_wrap(~season) +
  labs(
    title = "How has the gender split of olympic medals changed over time?",
    x = "Year", y = "Number of Medals",
    fill = "Gender of Event"
  )

ggplotly(gender_medal_plot_split_by_season)


# I then wanted to compare gender across the three (sumemr, para, wiinter)

ggplot(athletes_2024, aes(x = fct_infreq(gender))) +
  geom_bar()

ggplot(athletes_2026, aes(x = fct_infreq(gender))) +
  geom_bar()

ggplot(para_athletes_2024, aes(x = fct_infreq(gender))) +
  geom_bar()

small_athletes_2024 <- athletes_2024 |> 
  select(name, gender, country_code) |> 
  mutate(category = "Summer") 

small_para_athletes_2024 <- para_athletes_2024 |> 
  select(name, gender, country_code) |> 
  mutate(category = "Para Summer")

small_athletes_2026 <- athletes_2026 |> 
  mutate(gender = if_else(gender == "M", "Male", "Female")) |> 
  select(name, gender, country_code) |> 
  mutate(category = "Winter")

combined_recent_athletes <- rbind(small_athletes_2024, small_para_athletes_2024, small_athletes_2026)

ggplot(combined_recent_athletes, aes(x = fct_infreq(gender), fill = category)) +
  geom_bar()

ggplot(combined_recent_athletes, aes(x = fct_infreq(gender), fill = category)) +
  geom_bar() +
  facet_wrap(~category)

ggplot(combined_recent_athletes, aes(x = fct_infreq(gender), fill = category)) +
  geom_bar(aes(y = after_stat(prop), group = category)) +
  facet_wrap(~category) +
  scale_fill_manual(values = c("Para Summer" = "hotpink", "Summer" = "red", "Winter" = "blue")) +
  labs(
    title = "Proportion of Athletes Gender in Recent Olympics",
    subtitle = "From Summer Paris 2024: Paralympics and Olympics and Winter Milano-Cortina 2026",
    x = "Gender", y = "Proportion",
    fill = "Olympic Game"
  )


# ************************************************************************************************************
# ************************************************************************************************************
# ************************************************************************************************************


# Countries that have dominated the olympics

small_medal_count_2024 <- medal_count_2024 |> 
  mutate(gold = `Gold Medal`,
         silver = `Silver Medal`,
         bronze = `Bronze Medal`,
         total = Total) |> 
  select(country_code, country, gold, silver, bronze, total) |> 
  mutate(category = "Summer")

small_para_medal_count_2024 <- para_medal_count_2024 |> 
  mutate(gold = `Gold Medal`,
         silver = `Silver Medal`,
         bronze = `Bronze Medal`,
         total = Total) |> 
  select(country_code, country, gold, silver, bronze, total) |> 
  mutate(category = "Para Summer")

small_medal_count_2026 <- medal_count_2026 |> 
  select(country_code, country, gold, silver, bronze, total) |> 
  mutate(category = "Winter")


top_10_medals_2024 <- small_medal_count_2024 |> 
  slice_head(n = 10)

top_10_para_medals_2024 <- small_para_medal_count_2024 |> 
  slice_head(n = 10)

top_10_medals_2026 <- small_medal_count_2026 |> 
  slice_head(n = 10)

combined_top_10 <- rbind(top_10_medals_2024, top_10_medals_2026, top_10_para_medals_2024)


ggplot(top_10_para_medals_2024, aes(x = reorder(country_code, total), y = total)) +
  geom_col(fill = "hotpink") +
  coord_flip() +
  labs(
    title = "Top 10 Countries at Summer Paralympics Paris 2024",
    x = "Country", y = "Total Medal Count"
  )


ggplot(top_10_medals_2024, aes(x = reorder(country_code, total), y = total)) +
  geom_col(fill = "red") +
  coord_flip() +
  labs(
    title = "Top 10 Countries at Summer Olympics Paris 2024",
    x = "Country", y = "Total Medal Count"
  )


ggplot(top_10_medals_2026, aes(x = reorder(country_code, total), y = total)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(
    title = "Top 10 Countries at Winter Olympics Milano-Cortina 2026",
    x = "Country", y = "Total Medal Count"
  )

# How does location play a role in olympic sucess?

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# --- IOC to ISO-3 crosswalk ---
ioc_to_iso3 <- tibble::tribble(
  ~country_code, ~iso_a3,
  "USA", "USA", "CHN", "CHN", "GBR", "GBR", "AUS", "AUS",
  "NED", "NLD", "FRA", "FRA", "GER", "DEU", "ITA", "ITA",
  "KOR", "KOR", "JPN", "JPN", "NZL", "NZL", "CAN", "CAN",
  "HUN", "HUN", "ESP", "ESP", "SWE", "SWE", "NOR", "NOR",
  "BRA", "BRA", "KEN", "KEN", "RSA", "ZAF", "ETH", "ETH",
  "UZB", "UZB", "IRL", "IRL", "BEL", "BEL", "POL", "POL",
  "CZE", "CZE", "SRB", "SRB", "ROU", "ROU", "SUI", "CHE",
  "AUT", "AUT", "FIN", "FIN", "SVK", "SVK", "SLO", "SVN",
  "LAT", "LVA", "LTU", "LTU", "EST", "EST", "RUS", "RUS",
  "TPE", "TWN", "HKG", "HKG", "INA", "IDN", "THA", "THA",
  "IRI", "IRN", "AZE", "AZE", "KAZ", "KAZ", "GEO", "GEO",
  "ALG", "DZA", "TUN", "TUN", "MAR", "MAR", "EGY", "EGY",
  "ARG", "ARG", "CUB", "CUB", "COL", "COL", "MEX", "MEX",
  "GRE", "GRC", "POR", "PRT", "DEN", "DNK", "ISL", "ISL",
  "CRO", "HRV", "BUL", "BGR", "UKR", "UKR", "TUR", "TUR",
  "ISR", "ISR", "IND", "IND", "MGL", "MNG", "NMI", "MNP"
)

world <- ne_countries(scale = "medium", returnclass = "sf")

# --- Helper: build an sf object for one dataset ---
make_map_sf <- function(df, crosswalk, world_sf) {
  df |>
    left_join(crosswalk, by = "country_code") |>
    filter(!is.na(iso_a3)) |>
    left_join(world_sf |> select(iso_a3, geometry), by = "iso_a3") |>
    st_as_sf()
}

summer_sf <- make_map_sf(top_10_medals_2024,      ioc_to_iso3, world)
para_sf   <- make_map_sf(top_10_para_medals_2024, ioc_to_iso3, world)
winter_sf <- make_map_sf(top_10_medals_2026,      ioc_to_iso3, world)

# --- Helper: build one map plot ---
make_olympic_map <- function(highlight_sf, fill_colour, title) {
  ggplot() +
    geom_sf(data = world, fill = "grey88", color = "grey70", linewidth = 0.15) +
    geom_sf(data = highlight_sf, fill = fill_colour, color = "grey20", linewidth = 0.3) +
    coord_sf(crs = st_crs("ESRI:54030")) +
    labs(title = title, caption = "Source: Olympic medal data") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 13),
      axis.text     = element_blank(),
      axis.ticks    = element_blank(),
      panel.grid    = element_blank()
    )
}

# --- Build the three maps ---
map_summer <- make_olympic_map(
  summer_sf,
  fill_colour = "#E63946",
  title       = "2024 Summer Olympics — Top 10 by Total Medals"
)

map_para <- make_olympic_map(
  para_sf,
  fill_colour = "#E040FB",
  title       = "2024 Paralympic Games — Top 10 by Total Medals"
)

map_winter <- make_olympic_map(
  winter_sf,
  fill_colour = "#1D7FCC",
  title       = "2026 Winter Olympics — Top 10 by Total Medals"
)

# --- Display individually ---
map_summer
map_para
map_winter

# --- OR arrange all three stacked with patchwork ---
library(patchwork)
map_summer / map_para / map_winter




# ************************************************************************************************************
# ************************************************************************************************************
# ************************************************************************************************************

# Does having a larger population lead to more medal?
# Looking into GDP and Population data (I know I haven't gotten far but I want to look at medal per GDP and Person as metrics of success)
# I always wannt to look at ranks of pop and see almost like a residual plot to see who is higher pop but lower medals (like India) and who has a low pop rank but a high medal count (idk who to expect here)
# Could also do this with gdp in 2024 too

small_pop_2024 <- world_pop_2024 |> 
  janitor::clean_names() |> 
  select(country, population_2024, density_km2, world_rank)

small_pop_2026 <- world_pop_2026 |> 
  janitor::clean_names() |> 
  rename(world_rank = rank, density_km2 = density_per_km2) |> 
  select(country, population_2026, density_km2, world_rank)



joined_pop_medals_2024 <- medal_count_2024 |> 
  left_join(small_pop_2024, join_by(country))

joined_pop_medals_2026 <- medal_count_2026 |> 
  left_join(small_pop_2026, join_by(country))

para_pop <- para_medal_count_2024 |> 
  left_join(small_pop_2024, join_by(country))


ggplot(joined_pop_medals_2024, aes(x = Total, y= population_2024)) +
  geom_point()

ggplot(joined_pop_medals_2024, aes(x = population_2024, y= Total)) +
  geom_point()

ggplot(joined_pop_medals_2026, aes(x = total, y= population_2026)) +
  geom_point()

ggplot(joined_pop_medals_2024, aes(x = world_rank, y = Total)) +
  geom_point()  +
  geom_smooth(se = FALSE)

ggplot(joined_pop_medals_2026, aes(x = world_rank, y = total)) +
  geom_point() +
  geom_smooth()

# Plots aren't useful so let's make a new metric 

final_pop_medals_2024 <- joined_pop_medals_2024 |> 
  mutate(medal_per_1000000 = (Total/population_2024) * 1000000)


final_pop_medals_2026 <- joined_pop_medals_2026 |> 
  mutate(medal_per_1000000 = (total/population_2026) * 1000000)

final_para_pop <- para_pop |> 
  mutate(medal_per_1000000 = (Total/population_2024) * 1000000)



ggplot(final_pop_medals_2024 |> 
         slice_max(medal_per_1000000, n = 10), aes(x = reorder(country, medal_per_1000000), y = medal_per_1000000)) +
  geom_col(fill = "red") +
  coord_flip() +
  labs(
    title = "Top 10 Countries by Medal per Million people",
    x = "Country",
    y = "Medals per 1,000,000"
  )


ggplot(final_pop_medals_2026 |> 
         slice_max(medal_per_1000000, n = 10), aes(x = reorder(country, medal_per_1000000), y = medal_per_1000000)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(
    title = "Top 10 Countries by Medal per Million people",
    x = "Country",
    y = "Medals per 1,000,000"
  )

ggplot(final_para_pop |> 
         slice_max(medal_per_1000000, n = 10), aes(x = reorder(country, medal_per_1000000), y = medal_per_1000000)) +
  geom_col(fill = "hotpink") +
  coord_flip() +
  labs(
    title = "Top 10 Countries by Medal per Million people",
    x = "Country",
    y = "Medals per 1,000,000"
  )


# ************************************************************************************************************
# ************************************************************************************************************
# ************************************************************************************************************


# How about GDP (can only look into 2024 data rn might use 2025 data for 2026 Olympics since Jan is close enough)

gdp_2024 <- gdp_2020_2024 |> 
  janitor::clean_names() |> 
  select(country, x2024) |> 
  arrange(desc(x2024)) |> 
  mutate(world_rank = row_number())


joined_gdp_2024 <- medal_count_2024 |> 
  left_join(gdp_2024, join_by(country))

ggplot(joined_gdp_2024, aes(x = Total, y= x2024)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Total medal count by GDP (in dollars) 2024",
    subtitle = "GDP Data Source: International Monetary Fund (IMF)",
    x = "Medal Count",
    y = "2024 GDP (in dollars)"
  )



ggplot(joined_gdp_2024, aes(x = Total, y= x2024, text = paste(
  "Country:", country,
  "<br>Medals:", Total,
  "<br>GDP:", dollar(x2024)))) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Total medal count by GDP (in dollars) 2024",
    subtitle = "GDP Data Source: International Monetary Fund (IMF)",
    x = "Medal Count",
    y = "2024 GDP (in dollars)"
  )

# Wanted log sacles 
ggplot(joined_gdp_2024, aes(x = Total, y = x2024)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10() +
  scale_y_log10(labels = label_dollar())

library(ggpmisc)


ggplot(joined_gdp_2024, aes(x = Total, y = x2024)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  scale_x_log10() +
  scale_y_log10(labels = label_dollar()) +
  labs(
    title = "GDP Relationship (Log-Log Scale)",
    x = "Total Medal Count (log scale)",
    y = "GDP (in dollars) in 2024 (log scale)"
  ) +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = after_stat(eq.label)),
    parse = TRUE,
    label.x.npc = "left",
    label.y.npc = "top"
  )




ggplot(joined_gdp_2024, aes(x = x2024, y= Total)) +
  geom_point() +
  geom_smooth()

ggplot(joined_gdp_2024, aes(x = world_rank, y = x2024)) +
  geom_point()



# Do same with para data

joined_para_gdp_2024 <- para_medal_count_2024 |> 
  left_join(gdp_2024, join_by(country))

ggplot(joined_para_gdp_2024, aes(x = Total, y= x2024)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Total Paralympic medal count by GDP (in dollars) 2024",
    subtitle = "GDP Data Source: International Monetary Fund (IMF)",
    x = "Medal Count",
    y = "2024 GDP (in dollars)"
  )

# Same Graph
ggplot(joined_para_gdp_2024, aes(x = Total, y = x2024)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  scale_x_log10() +
  scale_y_log10(labels = label_dollar()) +
  labs(
    title = "GDP Relationship for Paralympic datset(Log-Log Scale)",
    x = "Total Medal Count (log scale)",
    y = "GDP (in dollars) in 2024 (log scale)"
  ) +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = after_stat(eq.label)),
    parse = TRUE,
    label.x.npc = "left",
    label.y.npc = "top"
  )

