## ---------------------------------------------------
## GHA RENDER SCRIPT
## ---------------------------------------------------

quarto::quarto_render(output_format = "html", as_job = FALSE)   ## Nov 2023, can't load figures in GHA when knitting to PDF 
