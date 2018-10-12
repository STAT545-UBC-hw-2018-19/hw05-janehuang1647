Assignment\_5
================

Part 1 Factor Management
------------------------

``` r
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(gapminder))
suppressPackageStartupMessages(library(forcats))
```

Elaboration for the gapminder data set
======================================

**1. Drop Oceania** Before dropping the unused data the country and continent have 142 and 5 levels repectively. In addition, the row number before dropping is 1704.

``` r
#before dropping Oceania, the number of levels of continent is 5.
nrow(gapminder)
```

    ## [1] 1704

``` r
Ocean_drop <- gapminder %>% 
  filter(continent != "Oceania")
str(Ocean_drop)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1680 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

``` r
nrow(Ocean_drop)  # Number of rows after dropping levels
```

    ## [1] 1680

After filting out the Oceania, the row number drop to 1680. By appling fct\_drop on continent, only the level of continent drop to 4 by removing Oceania. And while droplevels() was applied, it applied to all the factors in the structure, therefore, the level of country drop to 140 which mean the country in Oceania were removed. And the level of continent is also reduced to 4.

``` r
# dropping unused Oceania data
drop1 <- Ocean_drop %>% 
  mutate(continent=fct_drop(continent))
  str(drop1)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1680 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 4 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

``` r
  nrow(drop1)  # the row number remaining at 1680
```

    ## [1] 1680

``` r
drop2 <- Ocean_drop %>% 
  droplevels() 
  str(drop2) 
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1680 obs. of  6 variables:
    ##  $ country  : Factor w/ 140 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 4 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

``` r
  nrow(drop2)# Number of rows after dropping levels
```

    ## [1] 1680

**2. Reorder the levels of `country` or `continent`** The continent is originally ordered alphabetically, here we used different method to reorder the levels of the continient. First, we will check the original looking of the gapminder dataset before reordering. It is obviously that the continent is ordered by alphabetically.

``` r
levels(gapminder$continent)
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"   "Oceania"

Now we try to reordered the levels of the continent in the following different ways:

1.  Reordered by the maximum GDP per capital of each continent

``` r
gapminder %>% 
  mutate(continent=fct_reorder(continent,gdpPercap,max)) %>% 
ggplot(aes(continent,gdpPercap)) + geom_boxplot(aes(fill=continent))
```

![](assignment_5_files/figure-markdown_github/reordered%20by%20max-1.png)

``` r
# to verify that this is reordered using maximum GDP per capital of each continent, we do the following:

gapminder %>% 
  group_by(continent) %>% 
  # calculate the maximum gdpPercap in each continent
  summarize(
    max_gdp = max(gdpPercap)
  ) %>% 
  arrange(max_gdp) %>%  # which is consistent with the order in the box plot
 knitr::kable()
```

| continent |   max\_gdp|
|:----------|----------:|
| Africa    |   21951.21|
| Oceania   |   34435.37|
| Americas  |   42951.65|
| Europe    |   49357.19|
| Asia      |  113523.13|

1.  Reordered by the minimum GDP per capital of each continent

``` r
gapminder %>% 
  mutate(continent=fct_reorder(continent,gdpPercap,min)) %>% 
ggplot(aes(continent,gdpPercap)) + geom_boxplot(aes(fill=continent))
```

![](assignment_5_files/figure-markdown_github/reordered%20by%20min-1.png)

``` r
# to verify that this is reordered using maximum GDP per capital of each continent, we do the following:

gapminder %>% 
  group_by(continent) %>% 
  # calculate the maximum gdpPercap in each continent
  summarize(
    min_gdp = min(gdpPercap)
  ) %>% 
  arrange(min_gdp) %>%  # which is consistent with the order in the box plot
 knitr::kable()
```

| continent |    min\_gdp|
|:----------|-----------:|
| Africa    |    241.1659|
| Asia      |    331.0000|
| Europe    |    973.5332|
| Americas  |   1201.6372|
| Oceania   |  10039.5956|

1.  Reordered by the mean GDP per capital of each continent

``` r
gapminder %>% 
  mutate(continent=fct_reorder(continent,gdpPercap,mean)) %>% 
ggplot(aes(continent,gdpPercap)) + geom_boxplot(aes(fill=continent))
```

![](assignment_5_files/figure-markdown_github/reordered%20by%20mean-1.png)

``` r
gapminder %>% 
  group_by(continent) %>% 
  # calculate the maximum gdpPercap in each continent
  summarize(
    mean_gdp = mean(gdpPercap)
  ) %>% 
  arrange(mean_gdp) %>%  # which is consistent with the order in the box plot
 knitr::kable()
```

| continent |  mean\_gdp|
|:----------|----------:|
| Africa    |   2193.755|
| Americas  |   7136.110|
| Asia      |   7902.150|
| Europe    |  14469.476|
| Oceania   |  18621.609|

Part 2: File I/O
----------------

In order to explore whether this survives the round trip of writing to file then reading back in, we filter the gapminder dataframe and get only the data from Asian country in 2007.

``` r
filterdata <- gapminder %>% 
  filter(continent == "Asia" & year == 2007) 

# drop the unused level
asiadata <- filterdata %>% 
 droplevels() 
# check the level of continent and country to make sure the unused data is successfully dropped.
str(asiadata)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    33 obs. of  6 variables:
    ##  $ country  : Factor w/ 33 levels "Afghanistan",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ continent: Factor w/ 1 level "Asia": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ year     : int  2007 2007 2007 2007 2007 2007 2007 2007 2007 2007 ...
    ##  $ lifeExp  : num  43.8 75.6 64.1 59.7 73 ...
    ##  $ pop      : int  31889923 708573 150448339 14131858 1318683096 6980412 1110396331 223547000 69453570 27499638 ...
    ##  $ gdpPercap: num  975 29796 1391 1714 4959 ...

`write_csv()`/`read_csv()`
==========================

so now we will try to write the **asiadata** into the csv fie and again read it out to see whether the write and read has effected the data.

``` r
#first check the original data frame asiadata
head(asiadata)
```

    ## # A tibble: 6 x 6
    ##   country          continent  year lifeExp        pop gdpPercap
    ##   <fct>            <fct>     <int>   <dbl>      <int>     <dbl>
    ## 1 Afghanistan      Asia       2007    43.8   31889923      975.
    ## 2 Bahrain          Asia       2007    75.6     708573    29796.
    ## 3 Bangladesh       Asia       2007    64.1  150448339     1391.
    ## 4 Cambodia         Asia       2007    59.7   14131858     1714.
    ## 5 China            Asia       2007    73.0 1318683096     4959.
    ## 6 Hong Kong, China Asia       2007    82.2    6980412    39725.

``` r
# wirte the csv file
write_csv(asiadata,"asiadata.csv")
```

Now we found out that the variable **country** and **continent** which used to be factor transfer into a characters after reading back from the csv file. Apart from that, the data remains unchanged.

``` r
# read back the csv file
 read_data <- read_csv("asiadata.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   country = col_character(),
    ##   continent = col_character(),
    ##   year = col_integer(),
    ##   lifeExp = col_double(),
    ##   pop = col_integer(),
    ##   gdpPercap = col_double()
    ## )

``` r
 is.factor(read_data$country)  # which is false
```

    ## [1] FALSE

``` r
 is.factor(read_data$continent)
```

    ## [1] FALSE

``` r
 is.character(read_data$country) 
```

    ## [1] TRUE

``` r
# now check the new read_in data
 head(read_data)
```

    ## # A tibble: 6 x 6
    ##   country          continent  year lifeExp        pop gdpPercap
    ##   <chr>            <chr>     <int>   <dbl>      <int>     <dbl>
    ## 1 Afghanistan      Asia       2007    43.8   31889923      975.
    ## 2 Bahrain          Asia       2007    75.6     708573    29796.
    ## 3 Bangladesh       Asia       2007    64.1  150448339     1391.
    ## 4 Cambodia         Asia       2007    59.7   14131858     1714.
    ## 5 China            Asia       2007    73.0 1318683096     4959.
    ## 6 Hong Kong, China Asia       2007    82.2    6980412    39725.

`saveRDS()/readRDS()`
=====================

while saving and reading RDS file, the class of the data is preserved, therefore the variable continent and country returned are with class factor.

``` r
# save to RDS file
saveRDS(asiadata, "asiadata.rds")

# read from RDS file
read_rdsdata <- readRDS("asiadata.rds")

# check the readin data
 head(read_rdsdata) 
```

    ## # A tibble: 6 x 6
    ##   country          continent  year lifeExp        pop gdpPercap
    ##   <fct>            <fct>     <int>   <dbl>      <int>     <dbl>
    ## 1 Afghanistan      Asia       2007    43.8   31889923      975.
    ## 2 Bahrain          Asia       2007    75.6     708573    29796.
    ## 3 Bangladesh       Asia       2007    64.1  150448339     1391.
    ## 4 Cambodia         Asia       2007    59.7   14131858     1714.
    ## 5 China            Asia       2007    73.0 1318683096     4959.
    ## 6 Hong Kong, China Asia       2007    82.2    6980412    39725.

``` r
  is.factor(read_rdsdata$country)  # which is true
```

    ## [1] TRUE

``` r
  is.factor(read_rdsdata$continent)
```

    ## [1] TRUE

`dput()/dget()`
===============

``` r
# put data into file
dput(asiadata, "asiadata.txt")

# get data from text file
data_txt <- dget("asiadata.txt")
 
head(data_txt) 
```

    ## # A tibble: 6 x 6
    ##   country          continent  year lifeExp        pop gdpPercap
    ##   <fct>            <fct>     <int>   <dbl>      <int>     <dbl>
    ## 1 Afghanistan      Asia       2007    43.8   31889923      975.
    ## 2 Bahrain          Asia       2007    75.6     708573    29796.
    ## 3 Bangladesh       Asia       2007    64.1  150448339     1391.
    ## 4 Cambodia         Asia       2007    59.7   14131858     1714.
    ## 5 China            Asia       2007    73.0 1318683096     4959.
    ## 6 Hong Kong, China Asia       2007    82.2    6980412    39725.

``` r
  is.factor(data_txt$country)  # which is true
```

    ## [1] TRUE

``` r
  is.factor(data_txt$continent)
```

    ## [1] TRUE

This is similar to `saveRDS()/readRDS()` which preserve the class of the data and the data remains unchanged while reading in from the txt file.

Part 3: Visualization design
----------------------------

In this part, I am going to remake the graph I have plotted for the previous assignment.

1.  this is a graph plotted in assignment 2. In this graph, I tried to showed the gdpPerCap of each country grouped by the countinent. But this graph did not show that much useful information and it did not show any information how the gdpPercap varies with time. Therefore, we are going to replot this graph.

``` r
ggplot(gapminder,aes(gdpPercap,continent))+
  geom_point(aes(colour=continent,size=gdpPercap),alpha=0.4)
```

![](assignment_5_files/figure-markdown_github/unnamed-chunk-10-1.png)

in the new graph

``` r
# get the max, min, median and mean for each contient in different year
  new_table <-  gapminder %>% 
  group_by(continent,year) %>% 
summarize(
  min_gdp = min(min(gdpPercap)),
  max_gdp = max(max(gdpPercap)),
  mean_gdp = mean(mean(gdpPercap)),
  median_gdp = median(median(gdpPercap))
)

# then we need to gather the gdp together to tidy up the data

tidy_table <- gather(new_table,key = "Stat_GDP", value="GDP_value", min_gdp, max_gdp,mean_gdp,median_gdp)
# then check the new gathered table
knitr::kable(head(tidy_table))
```

| continent |  year| Stat\_GDP |  GDP\_value|
|:----------|-----:|:----------|-----------:|
| Africa    |  1952| min\_gdp  |    298.8462|
| Africa    |  1957| min\_gdp  |    335.9971|
| Africa    |  1962| min\_gdp  |    355.2032|
| Africa    |  1967| min\_gdp  |    412.9775|
| Africa    |  1972| min\_gdp  |    464.0995|
| Africa    |  1977| min\_gdp  |    502.3197|

``` r
# now the data is ready for plotting

tidy_table %>% 
  ggplot(aes(x = year, y = GDP_value, color = Stat_GDP) ) +
  facet_wrap(~continent) +
  #use log10 in y-axis
  scale_y_log10()+
  geom_point()+
  #use line to show the trend
  geom_line()+
  ylab("GDP distribution")+
  ggtitle("Summarized GDP per Capital vs Year")
```

![](assignment_5_files/figure-markdown_github/modified%20graph%20for%20GDP%20per%20capital-1.png)

The differences between these two version are: \* The graph is faceted which can easier to access the data for each continent \* Various statistic such as maximum, minimum, mean and median were computed and the trend of each was shown instead of raw data. \* The trend against the year is shown.
