library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinybusy)
library(data.table)
library(tidyverse)
library(plotly)
library(heatmaply)
library(DT)
library(DESeq2)
library(RColorBrewer)
library(clusterProfiler)
library(org.Mm.eg.db)
library(org.Hs.eg.db)
library(here)
for(dir in c("functions", "modules")){
  list.files(here(dir), ".R", recursive = T, full.names = T) |> lapply(source)
}
source("css.R")

debugging = T
options(shiny.maxRequestSize = 2^30)
theme_set(theme_bw())

# Global variables----
.user = "Eric" # Dummy user name; maybe add authentication later

.project = read_last_project() # Last used project name (or "")

.metadata = NULL # Project metadata

.saved_input_state = NULL  # Saved input states (or a new list)

# update_state("tbs_main", "Home") # To avoid panel events triggering

.g = reactiveValues() # Reactives for project-specific constants
.r = reactiveValues() # Dynamic reactives that need reset on project loading

.status_string = "" # For status display
.status_updated = reactiveVal(F) # For real time status update

.project_load_flag = reactiveVal(F) # React to project name change
.project_load_complete = reactiveVal() # For UI refresh; contains the project name
.next_project = .project # Project to be loaded

.re = reactiveValues() # To store reactive data for export
.gg = reactiveValues() # To store ggplot for export
.pl = reactiveValues() # To store plotly for export

# Constants----
# Genes by gene types
.gene_types = c("Coding",
                "Pseudogene",
                "Ig/TR",
                "Mitochondria",
                "Unplaced",
                "LncRNA",
                "Other RNA",
                "Other")

genes_by_type = function(gtf){
  genes = list(
    gtf[gene_biotype %like% "protein_coding"]$gene_id,
    gtf[gene_biotype %like% "pseudo"]$gene_id,
    gtf[gene_biotype %like% "(IG_|TR_)"]$gene_id,
    gtf[seqnames == "MT"]$gene_id,
    gtf[!seqnames %in% c(1:19, "X", "Y", "MT")]$gene_id,
    gtf[gene_biotype %like% "ncRNA"]$gene_id,
    gtf[gene_biotype %like% "RNA" & !gene_biotype %like% "ncRNA"]$gene_id
  )
  genes[[8]] = setdiff(gtf$gene_id, unique(unlist(genes)))
  genes |> setNames(.gene_types)
}




