# Marmot Gut DNA Virome

[![BioRxiv / Journal Status](https://img.shields.io/badge/Status-Manuscript%20under%20review-blue.svg)](#)
[![Language](https://img.shields.io/badge/Language-R%20%7C%20Shell%20%7C%20Python-orange.svg)](#)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

This repository provides source code, processed datasets, and curated viral genomic sequences supporting the findings reported in the study:

> **"Deciphering DNA Viral Communities in Marmot Gut: Antimicrobial Potential, Metabolic Contributions, and Candidate Antibiotic Resistance-Associated Genes"**

---

## Repository Structure

```text
.
├── scripts/                      # R scripts for generating manuscript figures
│   ├── Fig. 1c.R                 # Rarefaction curves
│   ├── Fig. 1e.R                 # Alpha diversity (Shannon index boxplots)
│   ├── Fig. 1f.R                 # Beta diversity (PCoA with Bray-Curtis distance & PERMANOVA)
│   ├── Fig. 1g.R                 # UpSet plot of shared and unique vOTUs
│   ├── Fig. 2a.R                 # Viral family relative abundance bubble plot
│   ├── Fig. 2b.R                 # Predicted phage lifestyle proportions (virulent vs. temperate)
│   ├── Fig. 2c.R                 # Taxonomic distribution of phages across lifestyles
│   ├── Fig. 3a.R                 # Host-phage abundance correlation (linear regression & Pearson r)
│   ├── Fig. 3b.R                 # Virus-to-host abundance ratio (VHR dual-axis plot)
│   └── ...                       # Additional figure-generation scripts
├── Figures_Data/                 # Input tables and metadata corresponding to each figure
│   ├── Fig1/
│   ├── Fig2/
│   ├── Fig3/
│   └── ...
├── Viral_Sequences/              # Fasta sequences generated and analyzed in this study
│   ├── vOTUs.fasta               # Viral operational taxonomic units clustered at 95% ANI
│   └── RVS.fasta                 # Representative viral sequences
├── bioinformatics_resources.xlsx # Comprehensive list of software versions, databases, and URLs
├── LICENSE
└── README.md
