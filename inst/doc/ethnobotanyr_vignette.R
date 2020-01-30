## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

load("ethnobotanydata.rda")
library(ethnobotanyR)
library(ggplot2)
library(ggridges)
library(magrittr)
# in case of rendering issues render with 
# rmarkdown::render('vignettes/ethnobotanyr_vignette.Rmd', output_file='ethnobotanyr_vignette.html', output_dir='vignettes')


## ---- echo= FALSE-------------------------------------------------------------
knitr::kable(head(ethnobotanydata), digits = 2, caption = "First six rows of the example ethnobotany data included with ethnobotanyR")

## ----URs----------------------------------------------------------------------
ethnobotanyR::URs(ethnobotanydata)

## ----URsum--------------------------------------------------------------------
ethnobotanyR::URsum(ethnobotanydata)

## ----CIs----------------------------------------------------------------------
ethnobotanyR::CIs(ethnobotanydata)

## ----FCs----------------------------------------------------------------------
ethnobotanyR::FCs(ethnobotanydata)

## ----NUs----------------------------------------------------------------------
ethnobotanyR::NUs(ethnobotanydata)

## ----RFCs---------------------------------------------------------------------
ethnobotanyR::RFCs(ethnobotanydata)

## ----RIs----------------------------------------------------------------------
ethnobotanyR::RIs(ethnobotanydata)

## ----UVs----------------------------------------------------------------------
ethnobotanyR::UVs(ethnobotanydata)

## ----CVe----------------------------------------------------------------------
ethnobotanyR::CVe(ethnobotanydata)

## ----FLs----------------------------------------------------------------------
ethnobotanyR::FLs(ethnobotanydata)

## ----ethno_boot_histograms----------------------------------------------------
Use_1_boot <- ethno_boot(ethnobotanydata$Use_1, statistic = mean, n1 = 1000, n2 = 100)

Use_2_boot <- ethno_boot(ethnobotanydata$Use_2, statistic = mean, n1 = 1000, n2 = 100)

Use_3_boot <- ethno_boot(ethnobotanydata$Use_3, statistic = mean, n1 = 1000, n2 = 100)

## ----fig.width=7, fig.height=7------------------------------------------------
boot_data <- data.frame(Use_1_boot, Use_2_boot, Use_3_boot)

ethno_boot_melt <- reshape::melt(boot_data)

## ----plot_boot_ridges, fig.width=7, fig.height=4------------------------------
ggplot2::ggplot(ethno_boot_melt, aes(x = value, 
                y = variable, fill = variable)) +
                ggridges::geom_density_ridges() +
                ggridges::theme_ridges() + 
                theme(legend.position = "none") +
                labs(y= "", x = "Example Bayesian bootstraps of three use categores")

## ---- fig.width=7, fig.height=7-----------------------------------------------
URs_plot <- ethnobotanyR::Radial_plot(ethnobotanydata, ethnobotanyR::URs)

NUs_plot <- ethnobotanyR::Radial_plot(ethnobotanydata, ethnobotanyR::NUs)

FCs_plot <- ethnobotanyR::Radial_plot(ethnobotanydata, ethnobotanyR::FCs)

CIs_plot <- ethnobotanyR::Radial_plot(ethnobotanydata, ethnobotanyR::CIs)

cowplot::plot_grid(URs_plot, NUs_plot, FCs_plot, CIs_plot, 
    labels = c('URs', 'NUs', 'FCs', 'CIs'), 
    nrow = 2, 
    align="hv",
    label_size = 12)

## ---- fig.width=7, fig.height=7-----------------------------------------------
Chord_sp <- ethnobotanyR::ethnoChord(ethnobotanydata, by = "sp_name")

## ---- fig.width=7, fig.height=7-----------------------------------------------
Chord_informant <- ethnobotanyR::ethnoChord(ethnobotanydata, by = "informant")

## ----ethno_sp_a---------------------------------------------------------------
ethno_sp_a <- dplyr::filter(ethnobotanydata, sp_name == "sp_a")

## ----answers------------------------------------------------------------------
  answers <- 2

## ----ethno_compet_sp_a--------------------------------------------------------
ethno_compet_sp_a <- dplyr::recode(ethno_sp_a$informant, 
    inform_a = 0.9,inform_b = 0.5,inform_c = 0.5,
    inform_d = 0.9, inform_e = 0.9, inform_f = 0.5,
    inform_g = 0.7,inform_h = 0.5,inform_i = 0.9,
    inform_j= 0.9, inform_eight = 0.9,inform_five = 0.6,
    inform_four = 0.5,inform_nine = 0.9, 
    inform_one = 0.5, inform_seven = 0.5,
    inform_six= 0.9, inform_ten = 0.9, 
    inform_three = 0.9, inform_two = 0.5)

## ----ethno_sp_a_bayes---------------------------------------------------------
ethno_sp_a_bayes <- ethnobotanyR::ethno_bayes_consensus(ethno_sp_a, 
                    answers = 2,
                    prior_for_answers = ethno_compet_sp_a,
                    prior = -1)

## ----heatmap------------------------------------------------------------------
heatmap(ethno_sp_a_bayes)

## ----ethno_sp_a_rich----------------------------------------------------------
set.seed(123) #make random number reproducible
ethno_sp_a_rich <- data.frame(replicate(3,sample(0:10,20,rep=TRUE)))
  names(ethno_sp_a_rich) <- 
  gsub(x = names(ethno_sp_a_rich), 
  pattern = "X", replacement = "Use_")  
  ethno_sp_a_rich$informant <- sample(c('User_1', 'User_2'), 
  20, replace=TRUE)
  ethno_sp_a_rich$sp_name <- sample(c('sp_a'), 
  20, replace=TRUE)

## -----------------------------------------------------------------------------
ethno_compet_sp_a_rich <- 
          dplyr::recode(ethno_sp_a_rich$informant,
          User_1 = 0.9, User_2 = 0.5)

## ----fig.width=7, fig.height=7------------------------------------------------
ethno_sp_a_bayes <- ethnobotanyR::ethno_bayes_consensus(ethno_sp_a_rich,
       answers = 10, 
       prior_for_answers = ethno_compet_sp_a_rich, 
       prior=-1) #keep a normal prior in this example

## ----fig.width=7, fig.height=7------------------------------------------------
ethno_sp_a_bayes_dat <- as.data.frame(ethno_sp_a_bayes)

ethno_sp_a_bayes_melt <- reshape::melt(ethno_sp_a_bayes_dat)

## ----fig.width=7, fig.height=4------------------------------------------------
ggplot2::ggplot(ethno_sp_a_bayes_melt, aes(x = value, 
                y = variable, fill = variable)) +
                ggridges::geom_density_ridges() +
                ggridges::theme_ridges() + 
                theme(legend.position = "none")+
                labs(y= "", x = "Example ethno_bayes_consensus of use categores for sp_a")

