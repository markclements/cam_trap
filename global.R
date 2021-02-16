### global.R

library(shiny)
library(tidyverse)
library(fs)
library(exifr)
library(magick)
library(shinyFiles)
library(mime)
library(reactable)
#library(taxize)
#library(wikitaxa)


source("input_folder_module.R")
source("plot_image_module.R")
source("zoom_image_module.R")
source("select_image_module.R")
source("annotate_image.R")
source("display_table_module.R")

