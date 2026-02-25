## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

load("ethnobotanydata.rda")
library(bnlearn)
library(broom)
library(causaleffect)
library(circlize)
library(cowplot)
library(dplyr)
library(ethnobotanyR)
library(ggalluvial)
library(ggplot2)
library(ggridges)
library(magrittr)
library(pbapply)
library(purrr)
library(stringr)
library(tibble)
library(tidyr)
library(tidyselect)
library(vegan)
# install.packages(c('bnlearn','broom','causaleffect','circlize','cowplot','dplyr','ethnobotanyR','ggalluvial','ggplot2','ggridges','magrittr','pbapply','purrr','stringr','tibble','tidyr','tidyselect','vegan'))

# in case of rendering issues render with 
# rmarkdown::render('vignettes/ethnobotanyr_vignette.Rmd', output_file='ethnobotanyr_vignette.html', output_dir='vignettes')

knitr::write_bib(c(.packages(), 
                  "bnlearn",
                  "broom",
                  "causaleffect",
                  "circlize",
                  "cowplot",
                  "dplyr",
                  "ethnobotanyR",
                  "ggalluvial",
                  "ggplot2",
                  "ggridges",
                  "knitr",
                  "magrittr",
                  "pbapply",
                  "purrr",
                  "stringr",
                  "tibble",
                  "tidyr",
                  "tidyselect",
                  "vegan" ), file = "references/packages.bib")


## ----cite_ethnobotanyR, comment=NA, echo=FALSE--------------------------------
citation("ethnobotanyR")

## ----echo= FALSE--------------------------------------------------------------
knitr::kable(head(ethnobotanydata), digits = 2, caption = "First six rows of the example ethnobotany data included with ethnobotanyR")

## ----Chord_sp, fig.width=7, fig.height=7--------------------------------------
Chord_sp <- ethnobotanyR::ethnoChord(ethnobotanydata, by = "sp_name")

## ----fig.width=7, fig.height=7------------------------------------------------
Chord_informant <- ethnobotanyR::ethnoChord(ethnobotanydata, by = "informant")

## ----fig.width=7, fig.height=7------------------------------------------------
ethnobotanyR::ethno_alluvial(ethnobotanydata)

## ----fig.width=7, fig.height=7------------------------------------------------

# correct internal assignment for stat = "stratum" 
  StatStratum <- ggalluvial::StatStratum

ethnobotanyR::ethno_alluvial(ethnobotanydata, alpha = 0.2) + 
  ggplot2::theme(legend.position = "none") +  
             ggplot2::geom_label(stat = "stratum", 
                      ggplot2::aes(label = ggplot2::after_stat(stratum)))


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

## ----fig.width=7, fig.height=7------------------------------------------------
URs_plot <- ethnobotanyR::Radial_plot(ethnobotanydata, ethnobotanyR::URs)

NUs_plot <- ethnobotanyR::Radial_plot(ethnobotanydata, ethnobotanyR::NUs)

FCs_plot <- ethnobotanyR::Radial_plot(ethnobotanydata, ethnobotanyR::FCs)

CIs_plot <- ethnobotanyR::Radial_plot(ethnobotanydata, ethnobotanyR::CIs)

cowplot::plot_grid(URs_plot, NUs_plot, FCs_plot, CIs_plot, 
    labels = c('URs', 'NUs', 'FCs', 'CIs'), 
    nrow = 2, 
    align="hv",
    label_size = 12)

## ----create_data--------------------------------------------------------------
eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
eb_data$informant <- sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
eb_data$sp_name <- sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)

## ----remove_zero_DCA----------------------------------------------------------

#remove rows with zero rowsums
ethno_data_complete <- eb_data %>%  
  dplyr::select(-informant, -sp_name) %>% 
  dplyr::filter_all(any_vars(!is.na(.)))%>%
    dplyr::filter_all(any_vars(. != 0))


## ----ordination---------------------------------------------------------------
#Save ordination results
 ethno_ordination <- vegan::decorana(ethno_data_complete) 

## ----plot_ord, fig.width=7, fig.height=7--------------------------------------
#Plot ordination results
plot(ethno_ordination)

## ----nesting------------------------------------------------------------------
nested_ethno_pca <- eb_data %>% 
  tidyr::nest(., data = everything()) %>% 
  dplyr::mutate(pca = purrr::map(data, ~ stats::prcomp(.x %>% 
        dplyr::select(-informant, -sp_name), 
        center = TRUE, scale = TRUE)),
         pca_augmented = map2(pca, data, ~broom::augment(.x, data = .y)))

## ----variance_check-----------------------------------------------------------
var_exp_tidy <- nested_ethno_pca %>% 
  tidyr::unnest(pca_augmented) %>% 
  dplyr::summarize_at(.vars = dplyr::vars(contains("PC", ignore.case = FALSE)), .funs = tibble::lst(var)) %>% 
  tidyr::gather(key = pc, value = variance) %>% 
  dplyr::mutate(var_exp = variance/sum(variance),
         cum_var_exp = cumsum(var_exp),
         pc = stringr::str_replace(pc, ".fitted", ""))

## ----select_components--------------------------------------------------------
var_exp <- var_exp_tidy %>% 
  head(4)

## ----plot_pca, fig.width=7, fig.height=3--------------------------------------
var_exp %>% 
  dplyr::rename(
    `Variance Explained` = var_exp,
    `Cumulative Variance Explained` = cum_var_exp
  ) %>% 
  tidyr::gather(key = key, value = value, `Variance Explained`:`Cumulative Variance Explained`) %>% 
  ggplot2::ggplot(ggplot2::aes(pc, value, group = key)) + 
  ggplot2::geom_point() + 
  ggplot2::geom_line() + 
  ggplot2::facet_wrap(~key, scales = "free_y") +
  ggplot2::theme_minimal() +
  ggplot2::lims(y = c(0, 1)) +
  ggplot2::labs(y = "Variance",
       title = "Variance explained by each principal component")


