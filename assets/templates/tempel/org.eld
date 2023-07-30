org-mode

(python "#+begin_src python" n> r> n "#+end_src" :post (org-edit-src-code))
(name "#+name: " r> n)
(hwidth "#+attr_html: :width " (p "500px") n)
(width "#+attr_latex: :width " (p "0.5\textwidth") n)
(hheight "#+attr_html: :height " (p "500px") n)
(height "#+attr_latex: :height " (p "0.1\textheight") n)
(dot "#+begin_src dot :file " (p "graph") "." (p "svg") " :results file graphics" n> r> n "#+end_src" :post (org-edit-src-code))
(jupyter "#+begin_src jupyter-" (p "python") " :session " r " :async yes" n> r> n "#+end_src" :post (org-edit-src-code))
(figure "#+caption: " (p "Figure caption") n>
        "#+name: " (p "fig:figure-label") n>
        "#+attr_latex: :width .6\\textwidth :placement [H]" n>
        "[[file:" (p "path/to/figure.png") "]]" n>)
(subfigs "#+caption: " (p "Figure caption") n>
         "#+name: " (p "fig:figure-label") n>
         "#+begin_figure:" n> "[H]" n>
         "#+latex: \\centering" n>
         "#+name: " (p "fig:subfig1-label") n>
         "#+caption: " (p "Subfigure 1 caption")
         "#+attr_latex: :width .3\\textwidth :center nil :options [b]{.28\\textwidth}" n>
         "#+begin_subfigure" n>
         "#+attr_latex: :width \textwidth" n>
         "[[file:" (p "path/to/file1.png") "]]" n>
         "#+end_subfigure" n>
         "#+latex: \\quad" n>
         "#+name: " (p "fig:subfig2-label") n>
         "#+caption: " (p "Subfigure 2 caption")
         "#+attr_latex: :width .3\\textwidth :center nil :options [b]{.28\\textwidth}" n>
         "#+begin_subfigure" n>
         "#+attr_latex: :width \\textwidth" n>
         "[[file:" (p "path/to/file2.png") "]]" n>
         "#+end_subfigure" n>
         "#+end_figure")
