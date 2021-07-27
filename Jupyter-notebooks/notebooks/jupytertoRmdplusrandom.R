require('ggplot2')
require('nls.multstart')
require('broom')
require('tidyverse')
require('rTPC')
require('data.table')
require('car')
require('boot')
require('patchwork')
require('minpack.lm')
require("tidyr")
require('purrr')


setwd("/home/primuser/Documents/VBTraining3/Jupyter-notebooks/")
rm(list=ls())
graphics.off()

#take a look at the different models available
get_model_names()

df <- read.csv("../data/csm7I.csv")
df1 <- df %>%
  dplyr::select('originalid', 'originaltraitname', 'originaltraitunit', 'originaltraitvalue', 'interactor1', 'ambienttemp', 'citation')
df2 <- as_data_frame(df1)
#visualize
ggplot(df2, aes(ambienttemp, originaltraitvalue))+
  geom_point()+
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Development Rate',
       title = 'Development Rate across temperatures for Aedes albopictus')

# choose model
mod = 'sharpschoolhigh_1981'
d<- df2 %>%
  rename(temp = ambienttemp,
         rate = originaltraitvalue)



# fit Sharpe-Schoolfield model
d_fit <- nest(d, data = c(temp, rate)) %>%
  mutate(sharpeschoolhigh = map(data, ~nls_multstart(rate~sharpeschoolhigh_1981(temp = temp, r_tref,e,eh,th, tref = 15),
                                                     data = .x,
                                                     iter = c(3,3,3,3),
                                                     start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981') - 10,
                                                     start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981') + 10,
                                                     lower = get_lower_lims(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981'),
                                                     upper = get_upper_lims(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981'),
                                                     supp_errors = 'Y',
                                                     convergence_count = FALSE)),
         
         # create new temperature data
         new_data = map(data, ~tibble(temp = seq(min(.x$temp), max(.x$temp), length.out = 100))),
         # predict over that data,
         preds =  map2(sharpeschoolhigh, new_data, ~augment(.x, newdata = .y)))
# nb1 <- "../Jupyter-notebooks/notebooks/NLLS-Example.ipynb"
# convert_ipynb(nb1)

nb2 <- "../Jupyter-notebooks/notebooks/1-data-wrangling_and_exp-design-NEW_EXAMPLES-Copy-Miles_is_working_on_this_one.ipynb"
#convert_ipynb(nb2)

rm(list=ls())
graphics.off()
nb3 <- "../Jupyter-notebooks/notebooks/2-Linear_models-NEW_EXAMPLES.ipynb"
#convert_ipynb(nb3)

nb4 <- "../Jupyter-notebooks/notebooks/lmpt2.ipynb"
#convert_ipynb(nb4)
