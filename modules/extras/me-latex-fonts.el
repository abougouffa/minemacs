;;; me-latex-fonts.el -*- lexical-binding: t; -*-

;; From Doom Emacs

;; Fontification taken from https://tex.stackexchange.com/a/86119/81279.
(setq font-latex-match-reference-keywords
      '(;; BibLaTeX.
        ("printbibliography" "[{")
        ("addbibresource" "[{")
        ;; Standard commands.
        ("cite" "[{")
        ("citep" "[{")
        ("citet" "[{")
        ("Cite" "[{")
        ("parencite" "[{")
        ("Parencite" "[{")
        ("footcite" "[{")
        ("footcitetext" "[{")
        ;; Style-specific commands.
        ("textcite" "[{")
        ("Textcite" "[{")
        ("smartcite" "[{")
        ("Smartcite" "[{")
        ("cite*" "[{")
        ("parencite*" "[{")
        ("supercite" "[{")
        ;; Qualified citation lists.
        ("cites" "[{")
        ("Cites" "[{")
        ("parencites" "[{")
        ("Parencites" "[{")
        ("footcites" "[{")
        ("footcitetexts" "[{")
        ("smartcites" "[{")
        ("Smartcites" "[{")
        ("textcites" "[{")
        ("Textcites" "[{")
        ("supercites" "[{")
        ;; Style-independent commands.
        ("autocite" "[{")
        ("Autocite" "[{")
        ("autocite*" "[{")
        ("Autocite*" "[{")
        ("autocites" "[{")
        ("Autocites" "[{")
        ;; Text commands.
        ("citeauthor" "[{")
        ("Citeauthor" "[{")
        ("citetitle" "[{")
        ("citetitle*" "[{")
        ("citeyear" "[{")
        ("citedate" "[{")
        ("citeurl" "[{")
        ;; Special commands.
        ("fullcite" "[{")
        ;; Cleveref.
        ("cref" "{")
        ("Cref" "{")
        ("cpageref" "{")
        ("Cpageref" "{")
        ("cpagerefrange" "{")
        ("Cpagerefrange" "{")
        ("crefrange" "{")
        ("Crefrange" "{")
        ("labelcref" "{"))
      font-latex-match-textual-keywords
      '(;; BibLaTeX brackets.
        ("parentext" "{")
        ("brackettext" "{")
        ("hybridblockquote" "[{")
        ;; Auxiliary commands.
        ("textelp" "{")
        ("textelp*" "{")
        ("textins" "{")
        ("textins*" "{")
        ;; Subcaption.
        ("subcaption" "[{"))
      font-latex-match-variable-keywords
      '(;; Amsmath.
        ("numberwithin" "{")
        ;; Enumitem.
        ("setlist" "[{")
        ("setlist*" "[{")
        ("newlist" "{")
        ("renewlist" "{")
        ("setlistdepth" "{")
        ("restartlist" "{")
        ("crefname" "{")))


(provide 'me-latex-fonts)
