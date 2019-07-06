
# Revised version

## Title page

pandoc ms/revision/title_page.md -o ms/revision/Brice_title_page.pdf --pdf-engine=xelatex

pandoc ms/revision/title_page.md -f markdown -t latex -s -o  ms/revision/Brice_title_page.tex --pdf-engine=xelatex


## Main text

pandoc ms/revision/ms_revised.md -o ms/revision/Brice_ms_revised.pdf  --bibliography=../references.bib --csl ms/GEB.csl --pdf-engine=xelatex

pandoc ms/revision/ms_revised.md -f markdown -t latex -s -o ms/revision/Brice_ms_revised.tex --bibliography=../references.bib --csl ms/GEB.csl --pdf-engine=xelatex

## Answer to reviewers

pandoc ms/reviews/answer_reviewers.md -o ms/reviews/Brice_answer_reviewers.pdf --bibliography=../references.bib --csl ms/GEB.csl --pdf-engine=xelatex

pandoc ms/reviews/answer_reviewers.md -o ms/reviews/Brice_answer_reviewers.docx --bibliography=../references.bib --csl ms/GEB.csl

## Supplementary material

pandoc ms/revision/SI_revised.md -o ms/revision/Brice_SI_revised.pdf --bibliography=../references.bib --csl ms/GEB.csl --pdf-engine=xelatex
