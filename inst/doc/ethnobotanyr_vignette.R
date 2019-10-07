## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

load("ethnobotanydata.rda")

# in case of rendering issues render with 
# rmarkdown::render('vignettes/ethnobotanyr_vignette.Rmd', output_file='ethnobotanyr_vignette.html', output_dir='vignettes')


## ---- echo= FALSE--------------------------------------------------------
knitr::kable(head(ethnobotanydata), digits = 2, caption = "First six rows of the example ethnobotany data included with ethnobotanyR")

## ----URs-----------------------------------------------------------------
ethnobotanyR::URs(ethnobotanydata)

## ----URsum---------------------------------------------------------------
ethnobotanyR::URsum(ethnobotanydata)

## ----CIs-----------------------------------------------------------------
ethnobotanyR::CIs(ethnobotanydata)

## ----FCs-----------------------------------------------------------------
ethnobotanyR::FCs(ethnobotanydata)

## ----NUs-----------------------------------------------------------------
ethnobotanyR::NUs(ethnobotanydata)

## ----RFCs----------------------------------------------------------------
ethnobotanyR::RFCs(ethnobotanydata)

## ----RIs-----------------------------------------------------------------
ethnobotanyR::RIs(ethnobotanydata)

## ----UVs-----------------------------------------------------------------
ethnobotanyR::UVs(ethnobotanydata)

## ----CVe-----------------------------------------------------------------
ethnobotanyR::CVe(ethnobotanydata)

## ----FLs-----------------------------------------------------------------
ethnobotanyR::FLs(ethnobotanydata)

## ---- fig.width=7, fig.height=7------------------------------------------
ethnobotanyR::Radial_plot(ethnobotanydata, ethnobotanyR::URs)

## ---- fig.width=7, fig.height=7------------------------------------------
URs_plot <- ethnobotanyR::Radial_plot(ethnobotanydata, ethnobotanyR::URs)

NUs_plot <- ethnobotanyR::Radial_plot(ethnobotanydata, ethnobotanyR::NUs)

FCs_plot <- ethnobotanyR::Radial_plot(ethnobotanydata, ethnobotanyR::FCs)

CIs_plot <- ethnobotanyR::Radial_plot(ethnobotanydata, ethnobotanyR::CIs)

cowplot::plot_grid(URs_plot, NUs_plot, FCs_plot, CIs_plot, 
    labels = c('URs', 'NUs', 'FCs', 'CIs'), 
    nrow = 2, 
    align="hv",
    label_size = 12)

## ---- fig.width=7, fig.height=7------------------------------------------
Chord_sp <- ethnobotanyR::ethnoChord(ethnobotanydata, by = "sp_name")

## ---- fig.width=7, fig.height=7------------------------------------------
Chord_informant <- ethnobotanyR::ethnoChord(ethnobotanydata, by = "informant")

