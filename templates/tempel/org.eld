org-mode

(python "#+begin_src python" n> r> n "#+end_src" :post (org-edit-src-code))
(name "#+name: " r> n)
(hwidth "#+attr_html: :width " (p "500px") n)
(width "#+attr_latex: :width " (p "0.5\textwidth") n)
(hheight "#+attr_html: :height " (p "500px") n)
(height "#+attr_latex: :height " (p "0.1\textheight") n)
(dot "#+begin_src dot :file " (p "graph") "." (p "svg") " :results file graphics" n> r> n "#+end_src" :post (org-edit-src-code))
(jupyter "#+begin_src jupyter-" (p "python") " :session " r " :async yes" n> r> n "#+end_src" :post (org-edit-src-code))
