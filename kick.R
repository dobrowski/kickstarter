

### Libraries ----

library(tidyverse)
library(here)
library(janitor)
library(ggthemes)
library(plotly)
library(tidytext)


### Import data ------

kick <- read_csv(here("data", "kickstarter-train.csv"))


### Explorations -----

tabyl(kick$category)

tabyl(kick$main_category)

tabyl(kick$currency)

 # Plot goal, percent pledged, currency and category 

ggplot(kick) +
    geom_point(aes(x= log(usd_goal_real), y = log(percent_pledged), color = deadline)) +
    lims(x = c(0,15), y = c(0,10)) +
    facet_grid(main_category ~ currency)

kick %>% tabyl(usd_goal_real, percent_pledged)

## Filter out extremes
kick2 <- kick %>% 
     filter(usd_goal_real > 1,
            usd_goal_real < 100000) %>%
    mutate(goalbin = cut(usd_goal_real, seq(0,100000, 5000)),
           pass = percent_pledged >= 100,
           cancel = str_detect(str_to_lower(name), "cancel" ))

kick2 %>% tabyl(cancel,pass)  # if not is canceled, not pass (most likely)
#if usd_goal_real is 1 or less then pass  



kick2 %>%
    filter(usd_goal_real > 100000) %>%  # Greater than 100000  less than .6 passing
    summarise(mean(pass))

kick2 %>%
    filter(usd_goal_real < 1.01) %>%  # Greater than 100000  less than .6 passing
    summarise(mean(pass))



ggplot(kick2) + 
    geom_density(aes(x = usd_goal_real, weight = percent_pledged, fill = percent_pledged), color = "darkgreen", alpha = 0.3) +
 #   geom_density(aes(x = deadline, weight = usd_goal_real, fill = usd_goal_real), color = "red", alpha = 0.3) +
    theme_hc()



ggplot(kick) + 
    geom_point(aes(x = deadline, y = percent_pledged, fill = percent_pledged), color = "darkgreen", alpha = 0.3) +
    #   geom_density(aes(x = deadline, weight = usd_goal_real, fill = usd_goal_real), color = "red", alpha = 0.3) +
    theme_hc()


ggplot(kick3) +
    geom_histogram(aes(x = usd_goal_real, weight = mean(percent_pledged)), binwidth = 5000 )



kick3 <- kick2  %>%
    group_by(goalbin) %>%
    summarise(mean.pledge = mean(percent_pledged),
              mean.pass = mean(pass))


ggplot(kick3) +
    geom_col(aes(x = goalbin, y = mean.pledge) )


ggplot(kick3) +
    geom_col(aes(x = goalbin, y = mean.pass) )

ggplotly()



ggplot(kick2) + 
    geom_density(aes(x = usd_goal_real, weight = mean(as.numeric( pass))), alpha = 0.3) +
    #   geom_density(aes(x = deadline, weight = usd_goal_real, fill = usd_goal_real), color = "red", alpha = 0.3) +
    theme_hc() +
    facet_grid(currency~main_category)



###  Prepare dataframe for text analysis 

kick.text <- kick2 %>%
    mutate(row_number = row_number()) %>%
    unnest_tokens(word, name) %>%
    anti_join(stop_words) 


###  Top words by category 

kick.text.count <- kick.text %>%
    group_by(main_category) %>%
    count(word, sort = TRUE) %>%
    filter(n > 50) %>%
    mutate(word = reorder(word, n)) %>%
    top_n(10)

ggplot(kick.text.count , aes(word, n)) +
    geom_col() +
        facet_wrap(~main_category, scales = "free") +
    xlab(NULL) +
    coord_flip() +
    theme_hc()


#  Top words for items that passed or did not 

kick.text.pass <- kick.text %>%
    group_by(pass) %>%
    count(word, sort = TRUE) %>%
    filter(n > 50) %>%
    mutate(word = reorder(word, n)) %>%
    top_n(10)

ggplot(kick.text.pass , aes(word, n)) +
    geom_col() +
    facet_wrap(~pass) +
    xlab(NULL) +
    coord_flip() +
    theme_hc()
