
# Revised version

## Title page

pandoc ms/title_page.md -o ms/revision/Brice_title_page.pdf --pdf-engine=xelatex

## Main text

pandoc ms/revision/ms_revised.md -o ms/revision/Brice_ms_revised.pdf  --bibliography=../references.bib --csl ms/GEB.csl --pdf-engine=xelatex

## Answer to reviewers

pandoc ms/reviews/answer_reviewers.md -o ms/reviews/Brice_answer_reviewers.pdf --bibliography=../references.bib --csl ms/GEB.csl --pdf-engine=xelatex

## Supplementary material

pandoc ms/revision/SI_revised.md -o ms/revision/Brice_SI_revised.pdf --pdf-engine=xelatex