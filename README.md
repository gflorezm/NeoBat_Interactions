# NeoBat_Interactions - Supplement

Supplements to the data paper Florez-Montero *et al*. (2021, *in prep*.).

[Ecological Synthesis Lab](https://marcomellolab.wordpress.com) (SintECO), University of São Paulo.

Authors: Guillermo L. Florez-Montero, Renata L. Muylaert, Marcelo R. Nogueira, Cullen Geiselman, Sharlene E. Santana, Richard D. Stevens, Marco Tschapka, Francisco A. Rodrigues, Marco A. R. Mello.

E-mail: [gflorezmontero\@gmail.com](mailto:gflorezmontero@gmail.com){.email}.

First published on October 14th, 2020 (English version).

Run in R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out".

Disclaimer: You may freely use the software and data provided here for any purposes at your own risk. We assume no responsibility or liability for the use of this material, convey no license or title under any patent, copyright, or mask work right to the product. We reserve the right to make changes in the material without notification. We also make no representation or warranty that such application will be suitable for the specified use without further testing or modification. If this material helps you produce any academic work (paper, book, chapter, monograph, dissertation, report, talk, lecture or similar), please acknowledge the authors and cite the source.

## Functionality and origin

The data, scripts, and functions provided here aim at making or data paper 100% reproducible. You will find code to reproduce both the analysis and figures, as well as the main supplementary material.

## List of folders and files

1.  Data (folder)

    a.  NeoBat_Interactions_Sites.csv -\> data set with study site attributes.

    b.  NeoBat_Interactions_Records.csv -\> data set with interaction records.

    c.  NeoBat_Interactions_References.csv -\> data set with full references of the source studies.

2.  Figures (folder)

    a.  Figure_1.png

    b.  Figure_2.png

    c.  Figure_3.png

    d.  Figure_4.png

    e.  Figure_5.png

3.  Code (folder)

    a.  Figure_1.R -\> script for reproducing Figure 1.

    b.  Figure_2.R -\> script for reproducing Figure 2.

    c.  Figure_3.R -\> script for reproducing Figure 3.

    d.  Figure_4.R -\> script for reproducing Figure 4.

    e.  Figure_5.R -\> script for reproducing Figure 5.

    f.  abbr_name.R -\> custom-made function to abbreviate scientific names.

4.  Metadata (Folder)

    a.  NeoBat_Interactions_metadata.Rmd -\> script for reproducing the metadata document in PDF format.

    b.  NeoBat_Interactions_metadata.aux -\> intermediate file between RMD and LaTeX.

    c.  NeoBat_Interactions_metadata.log -\> intermediate file between RMD and LaTeX.

    d.  NeoBat_Interactions_metadata.pdf -\> metadata in PDF format.

    e.  bibliography.bib -\> references in BibTeX format.

    f.  chunk_1.R -\> script for the basic quantitative analysis reported in the metadata. A child code of the Rmd script.

    g.  chunk_2.R -\> script for building the metadata tables. A child code of the RMD script.

    h.  frontiersec.csl -\> bibliography format.

5.  other (Folder)

    a.  Bats_IUCN.csv -\> data set with the IUCN categories of bats.

    b.  Plants_IUCN.csv -\> data set with the IUCN categories of plants.

## Instructions

### For reproducing the figures

1.  Run the respective script to reproduce the chosen figure;

2.  Follow the instructions provided in the script;

3.  Check the figure in the Figures folder.

### For reproducing the metadata

1.  Knit the RMD script from the Metadata folder;

2.  Check the PDF document in the same folder.

## Feedback

If you have any questions, corrections, or suggestions, please feel free to open an [issue](https://github.com/gflorezm/NeoBat_Interactions/issues) or make a [pull request](https://github.com/gflorezm/NeoBat_Interactions/pulls).

## Acknowledgments

Firstly, we thank Prof. Mauro Galetti and Prof. Milton Ribeiro, from the Sao Paulo State University (UNESP), Brazil, who led the [Atlantic Series of data papers](https://esajournals.onlinelibrary.wiley.com/doi/toc/10.1002/(ISSN)1939-9170.AtlanticPapers) and inspired our entire community to take their data out of their drawers. Our labmates and colleagues helped us at different stages of this project. Our sponsors, especially the Alexander von Humboldt-Stiftung (AvH), Brazilian Council of Scientific and Technological Development (CNPq), Brazilian Coordination for the Improvement of Higher Education Personnel (CAPES), and São Paulo Research Foundation (FAPESP), gave us grants, fellowships, and scholarships. Last, but not least, we thank the [Stack Overflow Community](https://stackoverflow.com), where we solve most of our coding dilemmas.

## Reference

Florez-Montero GL, Muylaert RL, Geiselman C, Nogueira MR, Santana SE, Stevens RD, Tschapka M, Rodrigues FA, Mello MAR. 2021. NeoBat Interactions: a data set of bat-plant interactions in the Neotropics. *In preparation*.
