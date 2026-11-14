;; -*- mode: lisp-data; coding: utf-8 -*-
;; Tempel templates converted from YASnippet snippets in the `yasnippet-snippets` repository (snippets/ directory)
;; by yas2tempel on 2026-11-13.

ada-ts-mode

;; ada-ts-mode/fctbody
;; WARNING: Multi-line default of field 3 is not re-indented
;; WARNING: Multi-line default of field 6 is not re-indented
(fctbody "-- .................................." n
 "-- .                                ." > n
 "-- .  " (p "Subprogram_name") "               .  BODY" > n
 "-- .                                ." > n
 "-- .................................." > n
 "function " (p "<function identifier>") > n
 "  ( " (p "<parameter specification>;\n    <parameter specification>") " )" > n
 "  return " (p "<type mark>") " is" > n
 n
 "--| Notes (none)" > n
 "--| " (p "[Additional comments on the use of the subprogram.]") > n
 "--|" > n
 "--| Modifications" > n
 "--| " (p "[A list of modifications made to the subprogram BODY.\n--| Each entry in the list should include the date of the change,\n--| the name of the person who made the change, and a description\n--| of the modification.  The first entry in the list should\n--| always be the initial coding of the subprogram body.\n--| The \"Modifications\" section is required only when the\n--| subprogram body is a stand-alone compilation unit\n--| in a file of its own.]") > n
 q n
 
 :ann "function_body")

;; ada-ts-mode/fctspec
;; WARNING: Multi-line default of field 3 is not re-indented
;; WARNING: Multi-line default of field 5 is not re-indented
;; WARNING: Multi-line default of field 6 is not re-indented
;; WARNING: Multi-line default of field 8 is not re-indented
(fctspec "--X1804: CSU" n
 "-- .................................." > n
 "-- .                                ." > n
 "-- .  " (p "Subprogram_name") "               .  SPEC" > n
 "-- .                                ." > n
 "-- .................................." > n
 "function " (p "<function identifier>") > n
 "  ( " (p "<parameter specification>;\n    <parameter specification>") " )" > n
 "  return " (p "<type mark>") ";" > n
 n
 "--| Purpose" > n
 "--| " (p "[This section is a description of all purposes and functions\n--| of the subprogram.]") > n
 "--|" > n
 "--| Exceptions (none)" > n
 "--| " (p "[A list of all exceptions which may propagate out of the\n--| subprogram, and a description of when each would be raised.]") > n
 "--|" > n
 "--| Notes (none)" > n
 "--| " (p "[Additional comments on the use of the subprogram.]") > n
 "--|" > n
 "--| Modifications" > n
 "--| " (p "[A list of modifications made to the subprogram DECLARATION.\n--| Each entry in the list should include the date of the change,\n--| the name of the person who made the change, and a description\n--| of the modification.  The first entry in the list should\n--| always be the initial coding of the subprogram declaration.\n--| The \"Modifications\" section is required only when the\n--| subprogram declaration is a stand-alone compilation unit\n--| in a file of its own.]") > n
 q n
 
 :ann "function_specification")

;; ada-ts-mode/pacbody
;; WARNING: Multi-line default of field 3 is not re-indented
;; WARNING: Multi-line default of field 4 is not re-indented
(pacbody "-- **********************************" n
 "-- *                                *" > n
 "-- *  " (p "Package_name") "                  *  BODY" > n
 "-- *                                *" > n
 "-- **********************************" > n
 "package body " (p "<package identifier>") " is" > n
 n
 "--| Notes (none)" > n
 "--| " (p "[Additional comments on the design, implementation, and\n--| use of the package.]") > n
 "--|" > n
 "--| Modifications" > n
 "--| " (p "[A list of modifications made to the package BODY.\n--| Each entry in the list should include the date of the change,\n--| the name of the person who made the change, and a description\n--| of the modification.  The first entry in the list should\n--| always be the initial coding of the package BODY.\n--| The \"Modifications\" section is required only when the\n--| package BODY is a stand-alone compilation unit\n--| in a file of its own.]") > n
 q n
 
 :ann "package_body")

;; ada-ts-mode/pacspec
;; WARNING: Multi-line default of field 3 is not re-indented
;; WARNING: Multi-line default of field 4 is not re-indented
;; WARNING: Multi-line default of field 6 is not re-indented
(pacspec "--X1804: CSC" n
 "-- **********************************" > n
 "-- *                                *" > n
 "-- *  " (p "Package_name") "                  *  SPEC" > n
 "-- *                                *" > n
 "-- **********************************" > n
 "package " (p "<package identifier>") " is" > n
 n
 "--| Purpose" > n
 "--| " (p "[This section is a description of the purpose and function\n--| of the package.]") > n
 "--|" > n
 "--| Initialization Exceptions (none)" > n
 "--| " (p "[A list of all exceptions which may propagate out of the\n--| package INITIALIZATION PART and a description of when each\n--| would be raised.]") > n
 "--|" > n
 "--| Notes (none)" > n
 "--| " (p "[Additional comments on the use of the package.]") > n
 "--|" > n
 "--| Modifications" > n
 "--| " (p "[A list of modifications made to the package SPECIFICATION.\n--| Each entry in the list should include the date of the change,\n--| the name of the person who made the change, and a description\n--| of the modification.  The first entry in the list should\n--| always be the initial coding of the package specification.\n--| The \"Modifications\" section is required only when the\n--| package specification is a stand-alone compilation unit\n--| in a file of its own.]") > n
 q
 :ann "package_specification")

;; ada-ts-mode/probody
;; WARNING: Multi-line default of field 3 is not re-indented
;; WARNING: Multi-line default of field 5 is not re-indented
(probody "-- .................................." n
 "-- .                                ." > n
 "-- .  " (p "Subprogram_name") "               .  BODY" > n
 "-- .                                ." > n
 "-- .................................." > n
 "procedure " (p "<procedure identifier>") > n
 "  ( " (p "<parameter specification>;\n    <parameter specification>") " ) is" > n
 n
 "--| Notes (none)" > n
 "--| " (p "[Additional comments on the use of the subprogram.]") > n
 "--|" > n
 "--| Modifications" > n
 "--| " (p "[A list of modifications made to the subprogram BODY.\n--| Each entry in the list should include the date of the change,\n--| the name of the person who made the change, and a description\n--| of the modification.  The first entry in the list should\n--| always be the initial coding of the subprogram body.\n--| The \"Modifications\" section is required only when the\n--| subprogram body is a stand-alone compilation unit\n--| in a file of its own.]") > n
 q n
 
 :ann "procedure_body")

;; ada-ts-mode/prospec
;; WARNING: Multi-line default of field 3 is not re-indented
;; WARNING: Multi-line default of field 4 is not re-indented
;; WARNING: Multi-line default of field 5 is not re-indented
;; WARNING: Multi-line default of field 7 is not re-indented
(prospec "--X1804: CSU" n
 "-- .................................." > n
 "-- .                                ." > n
 "-- .  " (p "Subprogram_name") "               .  SPEC" > n
 "-- .                                ." > n
 "-- .................................." > n
 "procedure " (p "<procedure identifier>") > n
 "  ( " (p "<parameter specification>;\n    <parameter specification>") " );" > n
 n
 "--| Purpose" > n
 "--| " (p "[This section is a description of all purposes and functions\n--| of the subprogram.]") > n
 "--|" > n
 "--| Exceptions (none)" > n
 "--| " (p "[A list of all exceptions which may propagate out of the\n--| subprogram, and a description of when each would be raised.]") > n
 "--|" > n
 "--| Notes (none)" > n
 "--| " (p "[Additional comments on the use of the subprogram.]") > n
 "--|" > n
 "--| Modifications" > n
 "--| " (p "[A list of modifications made to the subprogram DECLARATION.\n--| Each entry in the list should include the date of the change,\n--| the name of the person who made the change, and a description\n--| of the modification.  The first entry in the list should\n--| always be the initial coding of the subprogram declaration.\n--| The \"Modifications\" section is required only when the\n--| subprogram declaration is a stand-alone compilation unit\n--| in a file of its own.]") > n
 q n
 
 :ann "procedure_specification")

;; ada-ts-mode/tskbody
;; WARNING: Multi-line default of field 4 is not re-indented
(tskbody "-- **********************************" n
 "-- *                                *" > n
 "-- *  " (p "Task_name") "                     *  BODY" > n
 "-- *                                *" > n
 "-- **********************************" > n
 "task body " (p "<task identifier>") " is" > n
 n
 "--| Notes (none)" > n
 "--| " (p "[Additional comments on the use of the task.]") > n
 "--|" > n
 "--| Modifications" > n
 "--| " (p "[A list of modifications made to the task BODY.\n--| Each entry in the list should include the date of the change,\n--| the name of the person who made the change, and a description\n--| of the modification.  The first entry in the list should\n--| always be the initial coding of the task BODY.\n--| The \"Modifications\" section is required only when the\n--| task BODY is a stand-alone compilation unit\n--| in a file of its own.]") > n
 q n
 
 :ann "task_body")

;; ada-ts-mode/tskspec
;; WARNING: Multi-line default of field 3 is not re-indented
;; WARNING: Multi-line default of field 5 is not re-indented
(tskspec "-- **********************************" n
 "-- *                                *" > n
 "-- *  " (p "Task_name") "                     *  SPEC" > n
 "-- *                                *" > n
 "-- **********************************" > n
 "task " (p "<task identifier>") " is" > n
 n
 "--| Purpose" > n
 "--| " (p "[This section is a description of all purposes and functions\n--| of the task.]") > n
 "--|" > n
 "--| Notes (none)" > n
 "--| " (p "[Additional comments on the use of the task.]") > n
 "--|" > n
 "--| Modifications" > n
 "--| " (p "[A list of modifications made to the task DECLARATION.\n--| Each entry in the list should include the date of the change,\n--| the name of the person who made the change, and a description\n--| of the modification.  The first entry in the list should\n--| always be the initial coding of the task declaration.\n--| The \"Modifications\" section is required only when the\n--| task declaration is a stand-alone compilation unit\n--| in a file of its own.]") > n
 q n
 
 :ann "task_specification")


antlr-mode

;; antlr-mode/project
(proj "<project name=\"" (p "test") "\" default=\"" (p "compile") "\" basedir=\"" (p ".") "\">" n
 n
 q n
 "</project>" >
 :ann "project")

;; antlr-mode/property
(prop "<property name=\"" (p "name") "\" value=\"" (p "value") "\" />" n
 q
 :ann "property")

;; antlr-mode/target
(target "<target name=\"" (p "compile") "\" " (p "other") ">" n
 "        " > q n
 "</target>" >)


apples-mode

;; apples-mode/comment
(comment "(*" n
 q n
 "*)" >)

;; apples-mode/considering
(considering "considering " p n
 q n
 "end considering" >)

;; apples-mode/considering-application-responses
(considering-application-responses "considering application responses" n
 q n
 "end considering" >)

;; apples-mode/display-dialog
(display-dialog "display dialog \"" q "\"")

;; apples-mode/if
(if "if " p " then" n
 p n
 "else" > n
 q n
 "end if" >)

;; apples-mode/ignoring
(ignoring "ignoring " p n
 q n
 "end ignoring" >)

;; apples-mode/ignoring-application-responses
(ignoring-application-responses "ignoring application responses" n
 q n
 "end ignoring" >)

;; apples-mode/on
(on "on " (s f1) n
 q n
 "end " f1 >)

;; apples-mode/repeat
(repeat "repeat " p n
 q n
 "end repeat" >)

;; apples-mode/repeat-until
(repeat-until "repeat until " p n
 q n
 "end repeat" >)

;; apples-mode/repeat-while
(repeat-while "repeat while " p n
 q n
 "end repeat" >)

;; apples-mode/repeat-with
(repeat-with "repeat with " p n
 q n
 "end repeat" >)

;; apples-mode/tell-application
(tell-application "tell application \"" p "\"" n
 q n
 "end tell" >)

;; apples-mode/tell-application-to-activate
(tell-application-to-activate "tell application \"" p "\" to activate" n
 q)

;; apples-mode/try
(try "try" n
 p n
 "on error " p > n
 q n
 "end try" >)

;; apples-mode/using-terms-from-application
(using-terms-from-application "using terms from application \"" p "\"" n
 q n
 "end using terms from" >)

;; apples-mode/with-timeout-of-seconds
(with-timeout-of-seconds "with timeout of " p " seconds" n
 q n
 "end timeout" >)

;; apples-mode/with-transaction
(with-transaction "with transaction" n
 q n
 "end transaction" >)


applescript-mode

;; applescript-mode/comment
(comment "(*" n
 q n
 "*)" >)

;; applescript-mode/considering
(considering "considering " p n
 q n
 "end considering" >)

;; applescript-mode/considering-application-responses
(considering-application-responses "considering application responses" n
 q n
 "end considering" >)

;; applescript-mode/display-dialog
(display-dialog "display dialog \"" q "\"")

;; applescript-mode/if
(if "if " p " then" n
 p n
 "else" > n
 q n
 "end if" >)

;; applescript-mode/ignoring
(ignoring "ignoring " p n
 q n
 "end ignoring" >)

;; applescript-mode/ignoring-application-responses
(ignoring-application-responses "ignoring application responses" n
 q n
 "end ignoring" >)

;; applescript-mode/on
(on "on " (s f1) n
 q n
 "end " f1 >)

;; applescript-mode/repeat
(repeat "repeat " p n
 q n
 "end repeat" >)

;; applescript-mode/repeat-until
(repeat-until "repeat until " p n
 q n
 "end repeat" >)

;; applescript-mode/repeat-while
(repeat-while "repeat while " p n
 q n
 "end repeat" >)

;; applescript-mode/repeat-with
(repeat-with "repeat with " p n
 q n
 "end repeat" >)

;; applescript-mode/tell-application
(tell-application "tell application \"" p "\"" n
 q n
 "end tell" >)

;; applescript-mode/tell-application-to-activate
(tell-application-to-activate "tell application \"" p "\" to activate" n
 q)

;; applescript-mode/try
(try "try" n
 p n
 "on error " p > n
 q n
 "end try" >)

;; applescript-mode/using-terms-from-application
(using-terms-from-application "using terms from application \"" p "\"" n
 q n
 "end using terms from" >)

;; applescript-mode/with-timeout-of-seconds
(with-timeout-of-seconds "with timeout of " p " seconds" n
 q n
 "end timeout" >)

;; applescript-mode/with-transaction
(with-transaction "with transaction" n
 q n
 "end transaction" >)


bazel-mode

;; bazel-mode/alias
;; WARNING: $0 is not the last field; kept as a plain field
(alias "alias(" n
 "    name = \"" p "\"," > n
 "    actual = \"" p "\"" > n
 ")" > n
 )

;; bazel-mode/ccb
;; WARNING: $0 is not the last field; kept as a plain field
(ccb "cc_binary(" n
 "    name = \"" p "\"," > n
 "    srcs = [\"" p "\"]," > n
 "    deps = [\"" p "\"]" > n
 ")" > n
 
 :ann "cc_binary")

;; bazel-mode/cci
;; WARNING: $0 is not the last field; kept as a plain field
(cci "cc_import(" n
 "    name = \"" p "\"," > n
 "    hdrs = [\"" p "\"]" > n
 ")" > n
 
 :ann "cc_import")

;; bazel-mode/ccinc
;; WARNING: $0 is not the last field; kept as a plain field
(ccinc "cc_inc_library(" n
 "    name = \"" p "\"," > n
 "    hdrs = [\"" p "\"]," > n
 "    prefix = \"" p "\"," > n
 "    deps = [\"" p "\"]" > n
 ")" > n
 
 :ann "cc_inc_library")

;; bazel-mode/ccl
;; WARNING: $0 is not the last field; kept as a plain field
(ccl "cc_library(" n
 "    name = \"" p "\"," > n
 "    srcs = [\"" p "\"]," > n
 "    hdrs = [\"" p "\"]," > n
 "    deps = [\"" p "\"]" > n
 ")" > n
 
 :ann "cc_library")

;; bazel-mode/ccp
;; WARNING: $0 is not the last field; kept as a plain field
(ccp "cc_proto_library(" n
 "    name = \"" p "\"," > n
 "    deps = [\"" p "\"]" > n
 ")" > n
 
 :ann "cc_proto_library")

;; bazel-mode/cct
;; WARNING: $0 is not the last field; kept as a plain field
(cct "cc_test(" n
 "    name = \"" p "\"" > n
 "    srcs = [\"" p "\"]," > n
 "    deps = [\"" p "\"]" > n
 ")" > n
 
 :ann "cc_test")

;; bazel-mode/fg
;; WARNING: $0 is not the last field; kept as a plain field
(fg "filegroup(" n
 "    name = \"" p "\"," > n
 "    srcs = \"" p "\"" > n
 ")" > n
 
 :ann "filegroup")

;; bazel-mode/genq
;; WARNING: $0 is not the last field; kept as a plain field
(genq "genquery(" n
 "    name = \"" p "\"," > n
 "    expression = \"" p "\"," > n
 "    scope = \"" p "\"" > n
 ")" > n
 
 :ann "genquery")

;; bazel-mode/jbin
;; WARNING: $0 is not the last field; kept as a plain field
(jbin "java_binary(" n
 "    name = \"" p "\"," > n
 "    srcs = \"" p "\"," > n
 "    deps = \"" p "\"" > n
 ")" > n
 
 :ann "java_binary")

;; bazel-mode/jimp
;; WARNING: $0 is not the last field; kept as a plain field
(jimp "java_import(" n
 "    name = \"" p "\"," > n
 "    jars = [" > n
 "        \"" p "\"" > n
 "    ]" > n
 ")" > n
 
 :ann "java_import")

;; bazel-mode/jlib
;; WARNING: $0 is not the last field; kept as a plain field
(jlib "java_library(" n
 "    name = \"" p "\"," > n
 "    deps = \"" p "\"" > n
 ")" > n
 
 :ann "java_library")

;; bazel-mode/jrun
;; WARNING: $0 is not the last field; kept as a plain field
(jrun "java_runtime(" n
 "    name = \"" p "\"," > n
 "    srcs = \"" p "\"," > n
 "    java_home = \"" p "\"" > n
 ")" > n
 
 :ann "java_runtime")

;; bazel-mode/jtest
;; WARNING: $0 is not the last field; kept as a plain field
(jtest "java_test(" n
 "    name = \"" p "\"," > n
 "    size = \"" p "\"," > n
 "    runtime_deps = [" > n
 "        \"" p "\"" > n
 "    ]" > n
 ")" > n
 
 :ann "java_test")

;; bazel-mode/pybin
;; WARNING: $0 is not the last field; kept as a plain field
(pybin "py_binary(" n
 "    name = \"" p "\"," > n
 "    srcs = \"" p "\"," > n
 "    data = \"" p "\"" > n
 "    deps = [" > n
 "        \"" p "\"" > n
 "    ]" > n
 ")" > n
 
 :ann "py_binary")

;; bazel-mode/pylib
;; WARNING: $0 is not the last field; kept as a plain field
(pylib "py_library(" n
 "    name = \"" p "\"," > n
 "    deps = \"" p "\"," > n
 "    srcs = \"" p "\"" > n
 ")" > n
 
 :ann "py_library")

;; bazel-mode/pyrun
;; WARNING: $0 is not the last field; kept as a plain field
(pyrun "py_runtime(" n
 "    name = \"" p "\"," > n
 "    files = \"" p "\"," > n
 "    interpreter = \"" p "\"" > n
 ")" > n
 
 :ann "py_runtime")

;; bazel-mode/pytest
;; WARNING: $0 is not the last field; kept as a plain field
(pytest "py_test(" n
 "    name = \"" p "\"," > n
 "    srcs = [" > n
 "        \"" p "\"" > n
 "    ]," > n
 "    main = \"" p "\"," > n
 ")" > n
 
 :ann "py_test")

;; bazel-mode/shbin
;; WARNING: $0 is not the last field; kept as a plain field
(shbin "sh_binary(" n
 "    name = \"" p "\"," > n
 "    srcs = \"" p "\"," > n
 "    data = \"" p "\"" > n
 ")" > n
 
 :ann "sh_binary")

;; bazel-mode/shlib
;; WARNING: $0 is not the last field; kept as a plain field
(shlib "sh_library(" n
 "    name = \"" p "\"," > n
 "    data = [" > n
 "        \"" p "\"" > n
 "    ]" > n
 ")" > n
 
 :ann "sh_library")

;; bazel-mode/shtest
;; WARNING: $0 is not the last field; kept as a plain field
(shtest "sh_test(" n
 "    name = \"" p "\"," > n
 "    size = \"" p "\"," > n
 "    srcs = \"" p "\"," > n
 "    deps = \"" p "\"," > n
 "    data = \"" p "\"" > n
 ")" > n
 
 :ann "sh_test")

;; bazel-mode/ws
(ws "workspace(name = \"" q "\")" n
 
 :ann "workspace")


bibtex-mode

;; bibtex-mode/article
;; WARNING: Nested fields flattened into field 
(article "@article{ " (p "title") "," n
 "  author  = {" (p "author") "}," n
 "  title   = {" (p "title") "}," n
 "  journal = {" (p "journal") "}," n
 "  year    = {" (p "year") "}" (p ",\n  volume  = {volume") "}" (p ",\n  number  = {number") "}" (p ",\n  pages   = {pages") "}" (p ",\n  month   = {month") "}" (p ",\n  note    = {note") "}" n
 "}" n
 n
 q)

;; bibtex-mode/book
;; WARNING: Nested fields flattened into field 
(book "@book{ " (p "title") "," n
 "  author    = {" (p "author") "}," n
 "  editor    = {" (p "editor") "}" n
 "  title     = {" (p "title") "}," n
 "  publisher = {" (p "publisher") "}," n
 "  year      = {" (p "year") "}" (p ",\n  volume    = {volume") "}" (p ",\n  number    = {number") "}" (p ",\n  series    = {series") "}" (p ",\n  type      = {type") "}" (p ",\n  address   = {address") "}" (p ",\n  edition   = {edition") "}" (p ",\n  month     = {month") "}" (p ",\n  isbn      = {isbn") "}" (p ",\n  note      = {note") "}" n
 "}" n
 n
 q)

;; bibtex-mode/bookinbook
;; WARNING: Nested fields flattened into field 
(bookinbook "@bookinbook{ " (p "title") "," n
 "author    = {" (p "author") "}," n
 "title     = {" (p "title") "}," n
 "chapter   = {" (p "chapter") "}" (p ",\npages     = {pages") "}," n
 "publisher = {" (p "publisher") "}," n
 "year      = {" (p "year") "}," n
 "volume    = {" (p "volume") "}" (p ",\nseries    = {series") "}" (p ",\ntype      = {type") "}" (p ",\naddress   = {address") "}" (p ",\nedition   = {edition") "}" (p ",\nmonth     = {month") "}" (p ",\nnote      = {note") "}" n
 "}" n
 n
 q)

;; bibtex-mode/booklet
;; WARNING: Nested fields flattened into field 
(booklet "@booklet{ " (p "title") "," n
 "  author       = {" (p "author") "}" (p ",\n  howpublished = {howpublished") "}" (p ",\n  address      = {address") "}" (p ",\n  year         = {year") "}" (p ",\n  month        = {month") "}" (p ",\n  volume       = {volume") "}" (p ",\n  note         = {note") "}" n
 "}" n
 n
 q)

;; bibtex-mode/collection
;; WARNING: Nested fields flattened into field 
(collection "@collection{ " (p "title") "," n
 "  editor    = {" (p "editor") "}," n
 "  title     = {" (p "title") "}," n
 "  year      = {" (p "year") "}" (p ",\n  publisher = {publisher") "}" (p ",\n  volume    = {volume") "}" (p ",\n  series    = {series") "}" (p ",\n  type      = {type") "}" (p ",\n  chapter   = {chapter") "}" (p ",\n  pages     = {pages") "}" (p ",\n  address   = {address") "}" (p ",\n  edition   = {edition") "}" (p ",\n  month     = {month") "}" (p ",\n  note      = {note") "}" n
 "}" n
 n
 q)

;; bibtex-mode/conference
;; WARNING: Nested fields flattened into field 
(conference "@conference{ " (p "title") "," n
 "  author       = {" (p "author") "}," n
 "  title        = {" (p "title") "}," n
 "  booktitle    = {" (p "booktitle") "}," n
 "  year         = {" (p "year") "}," n
 "  editor       = {" (p "editor") "}" (p ",\n  volume       = {volume") "}" (p ",\n  series       = {series") "}" (p ",\n  pages        = {pages") "}" (p ",\n  address      = {address") "}" (p ",\n  month        = {month") "}" (p ",\n  organization = {organization") "}" (p ",\n  publisher    = {publisher") "}" (p ",\n  note         = {note") "}" n
 "}" n
 n
 q)

;; bibtex-mode/dataset
;; WARNING: Nested fields flattened into field 
(dataset "@dataset{ " (p "title") n
 "  title        = {" (p "title") "}," n
 "  editor       = {" (p "editor") "}," n
 "  year         = {" (p "year") "}" (p ",\n  author       = {author") "}" (p ",\n  volume       = {volume") "}" (p ",\n  version      = {version") "}" (p ",\n  publisher    = {publisher") "}" (p ",\n  organization = {organization") "}" (p ",\n  note         = {note") "}" n
 n
 "}" n
 q)

;; bibtex-mode/electronic
;; WARNING: Nested fields flattened into field 
(electronic "@electronic{ " (p "title") "," n
 "author  = {" (p "author") "}," n
 "title   = {" (p "title") "}," n
 "year    = {" (p "year") "}," n
 "url     = {" (p "url") "}" (p ",\neditor  = {editor") "}" (p ",\nurldate = {urldate") "}" (p ",\nnote    = {note") "}" n
 "}" n
 q)

;; bibtex-mode/inbook
;; WARNING: Nested fields flattened into field 
(inbook "@inbook{ " (p "title") "," n
 "  author    = {" (p "author") "}," n
 "  title     = {" (p "title") "}," n
 "  chapter   = {" (p "chapter") "}" (p ",\n  pages     = {pages") "}," n
 "  publisher = {" (p "publisher") "}," n
 "  year      = {" (p "year") "}," n
 "  volume    = {" (p "volume") "}" (p ",\n  series    = {series") "}" (p ",\n  type      = {type") "}" (p ",\n  address   = {address") "}" (p ",\n  edition   = {edition") "}" (p ",\n  month     = {month") "}" (p ",\n  note      = {note") "}" n
 "}" n
 n
 q)

;; bibtex-mode/incollection
;; WARNING: Nested fields flattened into field 
(incollection "@incollection{ " (p "title") "," n
 "  author    = {" (p "author") "}," n
 "  title     = {" (p "title") "}," n
 "  booktitle = {" (p "booktitle") "}," n
 "  publisher = {" (p "publisher") "}," n
 "  year      = {" (p "year") "}" (p ",\n  editor    = {editor") "}" (p ",\n  volume    = {volume") "}" (p ",\n  series    = {series") "}" (p ",\n  type      = {type") "}" (p ",\n  chapter   = {chapter") "}" (p ",\n  pages     = {pages") "}" (p ",\n  address   = {address") "}" (p ",\n  edition   = {edition") "}" (p ",\n  month     = {month") "}" (p ",\n  note      = {note") "}" n
 "}" n
 n
 q)

;; bibtex-mode/inproceedings
;; WARNING: Nested fields flattened into field 
(inproceedings "@inproceedings{ " (p "title") "," n
 "  author       = {" (p "author") "}," n
 "  title        = {" (p "title") "}," n
 "  booktitle    = {" (p "booktitle") "}," n
 "  year         = {" (p "year") "}," n
 "  editor       = {" (p "editor") "}" (p ",\n  volume       = {volume") "}" (p ",\n  series       = {series") "}" (p ",\n  pages        = {pages") "}" (p ",\n  address      = {address") "}" (p ",\n  month        = {month") "}" (p ",\n  organization = {organization") "}" (p ",\n  publisher    = {publisher") "}" (p ",\n  note         = {note") "}" n
 "}" n
 n
 q)

;; bibtex-mode/inreference
;; WARNING: Nested fields flattened into field 
(inreference "@inreference{ " (p "title") "," n
 "author    = {" (p "author") "}," n
 "title     = {" (p "title") "}," n
 "booktitle = {" (p "booktitle") "}," n
 "publisher = {" (p "publisher") "}," n
 "year      = {" (p "year") "}" (p ",\neditor    = {editor") "}" (p ",\nvolume    = {volume") "}" (p ",\nseries    = {series") "}" (p ",\ntype      = {type") "}" (p ",\nchapter   = {chapter") "}" (p ",\npages     = {pages") "}" (p ",\naddress   = {address") "}" (p ",\nedition   = {edition") "}" (p ",\nmonth     = {month") "}" (p ",\nnote      = {note") "}" n
 "}" n
 n
 q)

;; bibtex-mode/manual
;; WARNING: Nested fields flattened into field 
(manual "@manual{ " (p "title") "," n
 "  title        = {" (p "title") "}" (p ",\n  author       = {author") "}" (p ",\n  organization = {organization") "}" (p ",\n  address      = {address") "}" (p ",\n  editor       = {editor") "}" (p ",\n  month        = {month") "}" (p ",\n  year         = {year") "}" (p ",\n  note         = {note") "}" n
 "}" n
 q)

;; bibtex-mode/masterthesis
;; WARNING: Nested fields flattened into field 
(masterthesis "@masterthesis{ " (p "title") "," n
 "  author  = {" (p "author") "}," n
 "  title   = {" (p "title") "}," n
 "  school  = {" (p "school") "}," n
 "  year    = {" (p "year") "}" (p ",\n  type    = {type") "}" (p ",\n  address = {address") "}" (p ",\n  month   = {month") "}" (p ",\n  note    = {note") "}" n
 "}" n
 n
 q)

;; bibtex-mode/misc
;; WARNING: Nested fields flattened into field 
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(misc "@misc{ " (p "title") (p ",\n  author       = {author") "}" (p ",\n  title        = {title") "}" (p ",\n  howpublished = {howpublished") "}" (p ",\n  month        = {month") "}" (p ",\n  year         = {year") "}" (p ",\n  note         = {note") "}" n
 "}" n
 n
 q)

;; bibtex-mode/mvbook
;; WARNING: Nested fields flattened into field 
(mvbook "@mvbook{ " (p "title") "," n
 "  author    = {" (p "author") "}," n
 "  title     = {" (p "title") "}," n
 "  year      = {" (p "year") "}" (p ",\n  volume    = {volume") "}" (p ",\n  editor    = {editor") "}" (p ",\n  publisher = {publisher") "}" (p ",\n  number    = {number") "}" (p ",\n  series    = {series") "}" (p ",\n  type      = {type") "}" (p ",\n  address   = {address") "}" (p ",\n  edition   = {edition") "}" (p ",\n  month     = {month") "}" (p ",\n  isbn      = {isbn") "}" (p ",\n  note      = {note") "}" n
 "}" n
 n
 q n
 )

;; bibtex-mode/mvcollection
;; WARNING: Nested fields flattened into field 
(mvcollection "@mvcollection{ " (p "title") "," n
 "  editor    = {" (p "editor") "}," n
 "  title     = {" (p "title") "}," n
 "  year      = {" (p "year") "}" (p ",\n  publisher = {publisher") "}" (p ",\n  volumes   = {volumes") "}" (p ",\n  series    = {series") "}" (p ",\n  type      = {type") "}" (p ",\n  chapter   = {chapter") "}" (p ",\n  pages     = {pages") "}" (p ",\n  address   = {address") "}" (p ",\n  edition   = {edition") "}" (p ",\n  month     = {month") "}" (p ",\n  note      = {note") "}" n
 "}" n
 n
 q)

;; bibtex-mode/mvreference
;; WARNING: Nested fields flattened into field 
(mvreference "@mvreference{ " (p "title") "," n
 "editor    = {" (p "editor") "}," n
 "title     = {" (p "title") "}," n
 "year      = {" (p "year") "}" (p ",\npublisher = {publisher") "}" (p ",\nvolumes   = {volumes") "}" (p ",\nseries    = {series") "}" (p ",\ntype      = {type") "}" (p ",\nchapter   = {chapter") "}" (p ",\npages     = {pages") "}" (p ",\naddress   = {address") "}" (p ",\nedition   = {edition") "}" (p ",\nmonth     = {month") "}" (p ",\nnote      = {note") "}" n
 "}" n
 n
 q
 :ann "mvereference")

;; bibtex-mode/online
;; WARNING: Nested fields flattened into field 
(online "@online{ " (p "title") "," n
 "  author  = {" (p "author") "}," n
 "  title   = {" (p "title") "}," n
 "  year    = {" (p "year") "}," n
 "  url     = {" (p "url") "}" (p ",\n  editor  = {editor") "}" (p ",\n  urldate = {urldate") "}" (p ",\n  note    = {note") "}" n
 "}" n
 q)

;; bibtex-mode/patent
;; WARNING: Nested fields flattened into field 
(patent "@patent{ " (p "title") "," n
 "  title  = {" (p "title") "}," n
 "  author = {" (p "author") "}," n
 "  number = {" (p "number") "}," n
 "  year   = {" (p "year") "}," n
 "  holder = {" (p "holder") "}" (p ",\n  type   = {type") "}" (p ",\n  url    = {url") "}" (p ",\n  note   = {note") "}" n
 "}" q)

;; bibtex-mode/periodical
;; WARNING: Nested fields flattened into field 
(periodical "@periodical{ " (p "title") n
 "editor     = {" (p "editor") "}," n
 "title      = {" (p "title") "}," n
 "year       = {" (p "year") "}" (p ",\nissue      = {issue") "}" (p ",\nissuetitle = {issuetitle") "}" (p ",\nurl        = {url") "}" (p ",\nurldate    = {urldate") "}" (p ",\nissn       = {issn") "}" (p ",\nnote       = {note") "}" n
 "}" n
 q)

;; bibtex-mode/phdthesis
;; WARNING: Nested fields flattened into field 
(phdthesis "@phdthesis{ " (p "title") "," n
 "  author  = {" (p "author") "}," n
 "  title   = {" (p "title") "}," n
 "  school  = {" (p "school") "}," n
 "  year    = {" (p "year") "}" (p ",\n  type    = {type") "}" (p ",\n  address = {address") "}" (p ",\n  month   = {month") "}" (p ",\n  note    = {note") "}" n
 "}" n
 n
 q)

;; bibtex-mode/proceedings
;; WARNING: Nested fields flattened into field 
(proceedings "@proceedings{ " (p "title") "," n
 "  title        = {" (p "title") "}," n
 "  year         = {" (p "year") "}" (p ",\n  editor       = {editor") "}" (p ",\n  volume       = {volume") "}" (p ",\n  series       = {series") "}" (p ",\n  address      = {address") "}" (p ",\n  month        = {month") "}" (p ",\n  organization = {organization") "}" (p ",\n  publisher    = {publisher") "}" (p ",\n  note         = {note") "}" n
 "}" n
 n
 q)

;; bibtex-mode/reference
;; WARNING: Nested fields flattened into field 
(reference "@reference{ " (p "title") "," n
 "editor    = {" (p "editor") "}," n
 "title     = {" (p "title") "}," n
 "year      = {" (p "year") "}" (p ",\npublisher = {publisher") "}" (p ",\nvolume    = {volume") "}" (p ",\nseries    = {series") "}" (p ",\ntype      = {type") "}" (p ",\nchapter   = {chapter") "}" (p ",\npages     = {pages") "}" (p ",\naddress   = {address") "}" (p ",\nedition   = {edition") "}" (p ",\nmonth     = {month") "}" (p ",\nnote      = {note") "}" n
 "}" n
 n
 q)

;; bibtex-mode/report
;; WARNING: Nested fields flattened into field 
(report "@report{ " (p "title") "," n
 "  author       = {" (p "author") "}," n
 "  title        = {" (p "title") "}," n
 "  institution  = {" (p "institution") "}," n
 "  year         = {" (p "year") "}," n
 "  type         = {" (p "type") "}" (p ",\n  address      = {address") "}" (p ",\n  month        = {month") "}" (p ",\n  note         = {note") "}" n
 "}" n
 n
 q)

;; bibtex-mode/set
(set "@set{" (p "title") "," n
 "  entryset = {" (p "entryset") "}" n
 "}" q)

;; bibtex-mode/software
;; WARNING: Nested fields flattened into field 
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(software "@software{ " (p "title") (p ",\nauthor       = {author") "}" (p ",\ntitle        = {title") "}" (p ",\nhowpublished = {howpublished") "}" (p ",\nmonth        = {month") "}" (p ",\nyear         = {year") "}" (p ",\nnote         = {note") "}" n
 "}" n
 n
 q)

;; bibtex-mode/suppbook
;; WARNING: Nested fields flattened into field 
(suppbook "@suppbook{ " (p "title") "," n
 "author    = {" (p "author") "}," n
 "title     = {" (p "title") "}," n
 "chapter   = {" (p "chapter") "}" (p ",\npages     = {pages") "}," n
 "publisher = {" (p "publisher") "}," n
 "year      = {" (p "year") "}," n
 "volume    = {" (p "volume") "}" (p ",\nseries    = {series") "}" (p ",\ntype      = {type") "}" (p ",\naddress   = {address") "}" (p ",\nedition   = {edition") "}" (p ",\nmonth     = {month") "}" (p ",\nnote      = {note") "}" n
 "}" n
 n
 q)

;; bibtex-mode/suppcollection
;; WARNING: Nested fields flattened into field 
(suppcollection "@suppcollection{ " (p "title") "," n
 "author    = {" (p "author") "}," n
 "title     = {" (p "title") "}," n
 "booktitle = {" (p "booktitle") "}," n
 "publisher = {" (p "publisher") "}," n
 "year      = {" (p "year") "}" (p ",\neditor    = {editor") "}" (p ",\nvolume    = {volume") "}" (p ",\nseries    = {series") "}" (p ",\ntype      = {type") "}" (p ",\nchapter   = {chapter") "}" (p ",\npages     = {pages") "}" (p ",\naddress   = {address") "}" (p ",\nedition   = {edition") "}" (p ",\nmonth     = {month") "}" (p ",\nnote      = {note") "}" n
 "}" n
 n
 q)

;; bibtex-mode/suppperiodical
;; WARNING: Nested fields flattened into field 
(suppperiodical "@suppperiodical{ " (p "title") "," n
 "author  = {" (p "author") "}," n
 "title   = {" (p "title") "}," n
 "journal = {" (p "journal") "}," n
 "year    = {" (p "year") "}" (p ",\nvolume  = {volume") "}" (p ",\nnumber  = {number") "}" (p ",\npages   = {pages") "}" (p ",\nmonth   = {month") "}" (p ",\nnote    = {note") "}" n
 "}" n
 n
 q)

;; bibtex-mode/techreport
;; WARNING: Nested fields flattened into field 
(techreport "@techreport{ " (p "title") "," n
 "  author       = {" (p "author") "}," n
 "  title        = {" (p "title") "}," n
 "  institution  = {" (p "institution") "}," n
 "  year         = {" (p "year") "}," n
 "  type         = {" (p "type") "}" (p ",\n  address      = {address") "}" (p ",\n  month        = {month") "}" (p ",\n  note         = {note") "}" n
 "}" n
 n
 q)

;; bibtex-mode/thesis
;; WARNING: Nested fields flattened into field 
(thesis "@thesis{ " (p "title") "," n
 "author  = {" (p "author") "}," n
 "title   = {" (p "title") "}," n
 "school  = {" (p "school") "}," n
 "year    = {" (p "year") "}" (p ",\ntype    = {type") "}" (p ",\naddress = {address") "}" (p ",\nmonth   = {month") "}" (p ",\nnote    = {note") "}" n
 "}" n
 n
 q)

;; bibtex-mode/unpublished
;; WARNING: Nested fields flattened into field 
(unpublished "@unpublished{ " (p "title") "," n
 "  author = {" (p "author") "}," n
 "  title  = {" (p "title") "}," n
 "  note   = {" (p "note") "}" (p ",\n  month  = {month") "}" (p ",\n  year   = {year") "}" n
 "}" n
 n
 q)

;; bibtex-mode/xdata
(xdata "@xdata{ " (p "title") "," n
 "  " q n
 "}")


c++-mode c++-ts-mode

;; c++-mode/acm
(acm "auto sum = std::accumulate(std::begin(" (p "container" f1) "), std::end(" f1 "), 0);" n
 q n
 
 :ann "accumulate")

;; c++-mode/ajf
(ajf "auto pos = std::adjacent_find(std::begin(" (p "container" f1) "), std::end(" f1 "));" n
 "if (pos != std::end(" f1 ")) {" > n
 "  " > p n
 "}" > n
 q n
 
 :ann "adjacent_find")

;; c++-mode/alo
(alo "if (std::all_of(std::begin(" (p "container" f1) "), std::end(" f1 "), [](" p ") {" n
 "  " > p n
 "})) {" > n
 "  " > p n
 "}" > n
 q n
 
 :ann "all_of")

;; c++-mode/ano
(ano "if (std::any_of(std::begin(" (p "container" f1) "), std::end(" f1 "), [](" p ") {" n
 "  " > p n
 "})) {" > n
 "  " > p n
 "}" > n
 q n
 
 :ann "any_of")

;; c++-mode/assert
(ass "assert(" q ");"
 :ann "assert")

;; c++-mode/beginend
(beginend (p "v" f1) ".begin(), " f1 ".end()" n
 q
 :ann "v.begin(), v.end()")

;; c++-mode/boost_require
(req "BOOST_REQUIRE( " (p "condition") " );" n
 q
 :ann "boost_require")

;; c++-mode/cerr
(err "cerr << " q ";" n
 
 :ann "cerr")

;; c++-mode/cin
(cin (list 'l (format "%s" (or (save-excursion (progn (goto-char (point-min)) (unless (re-search-forward "^using\\s-+namespace std;" nil 'no-error) "std::"))) ""))) "cin >> " q ";")

;; c++-mode/class
;; WARNING: Requires YASnippet function ‘yas-c++-class-name’
(cls "class " (p "Name" f1) n
 "{" > n
 "public:" > n
 "    " > (or (yas-c++-class-name f1) f1) "();" n
 "    " > `(p ,(concat "virtual ~" "();")) n
 "};" > n
 q
 :ann "class")

;; c++-mode/class11
;; WARNING: Multi-line default of field 2 is not re-indented
;; WARNING: Nested fields flattened into field 2
;; WARNING: Multi-line default of field 5 is not re-indented
;; WARNING: Nested fields flattened into field 5
;; WARNING: Multi-line default of field 8 is not re-indented
;; WARNING: Nested fields flattened into field 8
;; WARNING: Multi-line default of field 12 is not re-indented
;; WARNING: Nested fields flattened into field 12
;; WARNING: Multi-line default of field 17 is not re-indented
;; WARNING: Nested fields flattened into field 17
;; WARNING: Multi-line default of field 20 is not re-indented
;; WARNING: Nested fields flattened into field 20
;; WARNING: $0 directly follows a field; Tempel exits at the template end
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(cls11 "class " (p "Name" f1) n
 "{" > n
 "public:" > n>
 `(p ,(concat "  " "//! Default constructor\n  " "()" ";" "\n\n")) `(p ,(concat "  " "//! Copy constructor\n  " "(const " " &other)" ";" "\n\n")) `(p ,(concat "  " "//! Move constructor\n  " "(" " &&other)" " noexcept" ";" "\n\n")) `(p ,(concat "  " "//! Destructor\n  " "virtual " "~" "()" " noexcept" ";" "\n\n")) `(p ,(concat "  " "//! Copy assignment operator\n  " "& operator=(const " " &other)" ";" "\n\n")) `(p ,(concat "  " "//! Move assignment operator\n  " "& operator=(" " &&other)" " noexcept" ";" "\n\n")) n
 n
 "protected:" > n
 "private:" > n
 "};" >
 :ann "class11")

;; c++-mode/cni
(cni "auto n = std::count_if(std::begin(" (p "container" f1) "), std::end(" f1 "), [](" p ") {" n
 "  " > p n
 "});" > n
 q n
 
 :ann "count_if")

;; c++-mode/cnt
(cnt "auto n = std::count(std::begin(" (p "container" f1) "), std::end(" f1 "), " p ");" n
 q n
 
 :ann "count")

;; c++-mode/const_[]
(c\[ "const " (p "Name") "& operator[](" (p "int index") ") const" n
 "{" > n
 "        " > q n
 "}" >
 :ann "const_[]")

;; c++-mode/constructor
;; WARNING: Nested fields flattened into field 3
(ct (p "Name" f1) "::" f1 "(" (p "args") ") " (p " : init") n
 "{" > n
 "        " > q n
 "}" >
 :ann "constructor")

;; c++-mode/cout
;; WARNING: $0 is not the last field; kept as a plain field
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(cout (list 'l (format "%s" (or (save-excursion (progn (goto-char (point-min)) (unless (re-search-forward "^using\\s-+namespace std;" nil 'no-errer) "std::"))) ""))) "cout << " p p " << \"" (p "\\n") "\";" n
 )

;; c++-mode/cpb
(cpb "std::copy_backward(std::begin(" (p "container" f1) "), std::end(" f1 "), std::end(" f1 "));" n
 q n
 
 :ann "copy_backward")

;; c++-mode/cpi
(cpi "std::copy_if(std::begin(" (p "container" f1) "), std::end(" f1 "), std::begin(" p ")," n
 "[](" p ") {" > n
 "  " > p n
 "});" > n
 q n
 
 :ann "copy_if")

;; c++-mode/cpn
(cpn "std::copy_n(std::begin(" (p "container" f1) "), " p ", std::end(" f1 "));" n
 q n
 
 :ann "copy_n")

;; c++-mode/cpp
(cpp "#include \"" (list 'l (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))) ".h\"")

;; c++-mode/cpy
(cpy "std::copy(std::begin(" (p "container" f1) "), std::end(" f1 "), std::begin(" p "));" n
 q n
 
 :ann "copy")

;; c++-mode/cstd
(cstd "#include <cstdlib>")

;; c++-mode/d+=
;; WARNING: Nested fields flattened into field 2
(d+= (p "Name" f1) "& operator+=(" (p "const Name &") ");" q)

;; c++-mode/d_operator
(<< "friend std::ostream& operator<<(std::ostream&, const " (p "Name") "&);" q
 :ann "d_operator<<")

;; c++-mode/d_operator[]
(\[ (p "Name") "& operator[](" (p "int index") ");" q
 :ann "d_operator[]")

;; c++-mode/d_operator[]_const
;; WARNING: Duplicate key ‘c[’ renamed to ‘c[-2’
(c\[-2 "const " (p "Name") "& operator[](" (p "int index") ") const;" q
 :ann "d_operator[]_const")

;; c++-mode/d_operator_istream
(>> "friend std::istream& operator>>(std::istream&, const " (p "Name") "&);" q
 :ann "d_operator>>")

;; c++-mode/delete
(dl "delete " (p "pointer") ";" q
 :ann "delete")

;; c++-mode/delete[]
(dla "delete[] " (p "arr") ";" q
 :ann "delete[]")

;; c++-mode/doc
(doc "/**" n
 " * " q > n
 " */" >)

;; c++-mode/dynamic_casting
(cast "check_and_cast<" (p "Name") " *>(" (p "msg") ");" q
 :ann "dynamic_casting")

;; c++-mode/enum
(enum "enum " (p "NAME") "{" n
 q n
 "};" >)

;; c++-mode/eql
(eql "if (std::equal(std::begin(" (p "container" f1) "), std::end(" f1 "), std::begin(" p "))) {" n
 "  " > p n
 "}" > n
 q n
 
 :ann "equal")

;; c++-mode/erm
(erm (p "container" f1) ".erase(std::remove(std::begin(" f1 "), std::end(" f1 "), " p "), std::end(" f1 "));" n
 q n
 
 :ann "remove")

;; c++-mode/ffo
(ffo "auto pos = std::find_first_of(std::begin(" (p "container" f1) "), std::end(" f1 ")," n
 "  std::begin(" p "), std::end(" p "));" > n
 "if (pos != std::end(" f1 ")) {" > n
 "  " > p n
 "}" > n
 q n
 
 :ann "find_first_of")

;; c++-mode/fil
(fil "std::fill(std::begin(" (p "container" f1) "), std::end(" f1 "), " p ");" n
 q n
 
 :ann "fill")

;; c++-mode/fin
(fin "auto pos = std::find_if_not(std::begin(" (p "container" f1) "), std::end(" f1 "),[](" p ") {" n
 "  " > p n
 "});" > n
 "if (pos != std::end(" f1 ")) {" > n
 "  " > p n
 "}" > n
 q n
 
 :ann "find_if_not")

;; c++-mode/fixture
(fixt "BOOST_FIXTURE_TEST_SUITE( " (p "name") ", " (p "Fixture") " )" n
 n
 q n
 n
 "BOOST_AUTO_TEST_SUITE_END()" >
 :ann "fixture")

;; c++-mode/fln
(fln "std::fill_n(std::begin(" (p "container") "), " p ", " p ");" n
 q n
 
 :ann "fill_n")

;; c++-mode/fnd
(fnd "auto pos = std::find(std::begin(" (p "container" f1) "), std::end(" f1 "), " p ");" n
 "if (pos != std::end(" f1 ")) {" > n
 "  " > p n
 "}" > n
 q n
 
 :ann "find")

;; c++-mode/fne
(fne "auto pos = std::find_std::end(std::begin(" (p "container" f1) "), std::end(" f1 ")," n
 "  std::begin(" p "), std::end(" p "));" > n
 "if (pos != std::end(" f1 ")) {" > n
 "  " > p n
 "}" > n
 q n
 
 :ann "find_end")

;; c++-mode/fni
(fni "auto pos = std::find_if(std::begin(" (p "container" f1) "), std::end(" f1 "), [](" p ") {" n
 "  " > p n
 "});" > n
 "if (pos != std::end(" f1 ")) {" > n
 "  " > p n
 "}" > n
 q n
 
 :ann "find_if")

;; c++-mode/fori
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(fori "for (" (p "auto ") (p "it" f2) " = " (p "var" f3) ".begin(); " f2 " != " f3 ".end(); ++" f2 ") {" n
 "    " > q n
 "}" >)

;; c++-mode/forr
(forr "for (" (p "type") " " (p "name") " : " (p "containerName") ") {" n
 "    " > q n
 "}" >
 :ann "for range")

;; c++-mode/fre
(fre "std::for_each(std::begin(" (p "container" f1) "), std::end(" f1 "), [](" p ") {" n
 "  " > p n
 "});" > n
 q n
 
 :ann "for_each")

;; c++-mode/friend
(fr "friend " q ";"
 :ann "friend")

;; c++-mode/fun_declaration
(f (p "type") " " (p "name") "(" (p "args") ")" (p " const") ";" q
 :ann "fun_declaration")

;; c++-mode/gnn
(gnn "std::generate_n(std::begin(" (p "container") "), " p ", [](" p ") {" n
 "  " > p n
 "});" > n
 q n
 
 :ann "generate_n")

;; c++-mode/gnr
(gnr "std::generate(std::begin(" (p "container" f1) "), std::end(" f1 "), [](" p ") {" n
 "  " > p n
 "});" > n
 q n
 
 :ann "generate")

;; c++-mode/gtest
(gtest "#include <gtest/gtest.h>")

;; c++-mode/ignore
(ignore (p "std::") "cin.ignore(std::numeric_limits<std::streamsize>::max(), '\\n');" q)

;; c++-mode/ihp
(ihp "if (std::is_heap(std::begin(" (p "container" f1) "), std::end(" f1 "))) {" n
 "  " > p n
 "}" > n
 q n
 
 :ann "is_heap")

;; c++-mode/ihu
(ihu "auto pos = std::is_heap_until(std::begin(" (p "container" f1) "), std::end(" f1 "));" n
 "if (pos != std::end(" f1 ")) {" > n
 "  " > p n
 "}" > n
 q n
 
 :ann "is_heap_until")

;; c++-mode/inline
(il "inline " q
 :ann "inline")

;; c++-mode/io
(io "#include <iostream>")

;; c++-mode/ipr
(ipr "if (std::is_permutation(std::begin(" (p "container" f1) "), std::end(" f1 "), std::begin(" p "))) {" n
 "  " > p n
 "}" > n
 q n
 
 :ann "is_permutation")

;; c++-mode/ipt
(ipt "if (std::is_partitioned(std::begin(" (p "container" f1) "), std::end(" f1 "), [](" p ") {" n
 "  " > p n
 "})) {" > n
 "  " > p n
 "}" > n
 q n
 
 :ann "is_partitioned")

;; c++-mode/iss
(iss "if (std::is_sorted(std::begin(" (p "container" f1) "), std::end(" f1 "))) {" n
 "  " > p n
 "}" > n
 q n
 
 :ann "is_sorted")

;; c++-mode/isu
(isu "auto pos = std::is_sorted_until(std::begin(" (p "container" f1) "), std::end(" f1 "));" n
 "if (pos != std::end(" f1 ")) {" > n
 "  " > p n
 "}" > n
 q n
 
 :ann "is_sorted_until")

;; c++-mode/ita
(ita "std::iota(std::begin(" (p "container" f1) "), std::end(" f1 "), " p ");" n
 q n
 
 :ann "iota")

;; c++-mode/iterator
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(iter (p "std::") (p "vector<int>") "::iterator " (p "iter") ";" n
 q
 :ann "iterator")

;; c++-mode/lambda
(lambda "[" (p "capture clause") "](" (p "parameters") ") {" n
 "    " > q n
 "}" >)

;; c++-mode/ltr
(ltr (p "container" f1) ".erase(0, " f1 ".find_first_not_of(\" \\t\\n\\r\"));" n
 q n
 
 :ann "remove whitespace at beginning")

;; c++-mode/lwr
(lwr "std::transform(std::begin(" (p "container" f1) "), std::end(" f1 "), std::begin(" f1 "), [](char c) {" n
 "return std::tolower(c);});" > n
 q n
 
 :ann "string to lower case")

;; c++-mode/lxc
(lxc "if (std::lexigraphical_compare(std::begin(" (p "container" f1) "), std::end(" f1 ")," n
 "  std::begin(" p "), std::end(" p ")) {" > n
 "  " > p n
 "}" > n
 q n
 
 :ann "lexigraphical_compare")

;; c++-mode/map
;; WARNING: $0 is not the last field; kept as a plain field
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(map "std::map<" (p "type1") p "> " (p "var") ";")

;; c++-mode/member_function
(mf (p "type") " " (p "Name") "::" (p "name") "(" (p "args") ")" (p " const") n
 "{" > n
 "        " > q n
 "}" > n
 
 :ann "member_function")

;; c++-mode/mkh
(mkh "std::make_heap(std::begin(" (p "container" f1) "), std::end(" f1 "));" n
 q n
 
 :ann "make_heap")

;; c++-mode/mme
(mme "auto minmax = std::minmax_element(std::begin(" (p "container" f1) "), std::end(" f1 "));" n
 q n
 
 :ann "minmax_element")

;; c++-mode/mne
(mne "auto pos = std::min_element(std::begin(" (p "container" f1) "), std::end(" f1 "));" n
 q n
 
 :ann "min_element")

;; c++-mode/module
(mod "class " (p "Name") " : public cSimpleModule" n
 "{" > n
 "   " > q n
 "}" >
 :ann "module")

;; c++-mode/mpb
(mpb "std::move_backward(std::begin(" (p "container" f1) "), std::end(" f1 "), std::end(" f1 "));" n
 q n
 
 :ann "move_backward")

;; c++-mode/mrg
(mrg "std::merge(std::begin(" (p "container" f1) "), std::end(" f1 ")," n
 "std::begin(" p "), std::end(" p "), std::begin(" p "));" > n
 q n
 
 :ann "merge")

;; c++-mode/msm
(msm "auto values = std::mismatch(std::begin(" (p "container" f1) "), std::end(" f1 "), std::begin(" f1 "));" n
 "if (values.first == std::end(" f1 ")) {" > n
 "  " > p n
 "} else {" > n
 "  " > p n
 "}" > n
 q n
 
 :ann "mismatch")

;; c++-mode/mxe
(mxe "auto pos = std::max_element(std::begin(" (p "container" f1) "), std::end(" f1 "));" n
 q n
 
 :ann "max_element")

;; c++-mode/namespace
(ns "namespace " (p "Namespace" f1) " {" n
 n
 "          " > r n
 n
 "}  // " f1 >
 :ann "namespace")

;; c++-mode/nno
(nno "if (std::none_of(std::begin(" (p "container" f1) "), std::end(" f1 "), [](" p ") {" n
 "  " > p n
 "})) {" > n
 "  " > p n
 "}" > n
 q n
 
 :ann "none_of")

;; c++-mode/ns
;; WARNING: Duplicate key ‘ns’ renamed to ‘ns-2’
(ns-2 "namespace "
 :ann "namespace ...")

;; c++-mode/nth
(nth "std::nth_element(std::begin(" (p "container" f1) "), std::end(" f1 "), std::end(" f1 "));" n
 q n
 
 :ann "nth_element")

;; c++-mode/nxp
(nxp "if (std::next_permutation(std::begin(" (p "container" f1) "), std::end(" f1 "))) {" n
 "  " > p n
 "}" > n
 q n
 
 :ann "next_permutation")

;; c++-mode/operator!=
(!= "bool " (p "Name" f1) "::operator!=(const " f1 " &other) const" n
 "{" > n
 "    return !(*this == other);" > n
 "}" q >
 :ann "operator!=")

;; c++-mode/operator+
(+ (p "Name" f1) " " f1 "::operator+(const " f1 " &other)" n
 "{" > n
 "    " > f1 " result = *this;" n
 "    result += other;" > n
 "    return result;" > n
 "}" q >
 :ann "operator+")

;; c++-mode/operator+=
;; WARNING: Nested fields flattened into field 2
(+= (p "Name" f1) "& " f1 "::operator+=(" (p "const Name &rhs") ")" n
 "{" > n
 "  " > q n
 "  return *this;" > n
 "}" >
 :ann "operator+=")

;; c++-mode/operator=
(= (p "Name" f1) "& " f1 "::operator=(const " f1 " &rhs)" n
 "{" > n
 "    // Check for self-assignment!" > n
 "    if (this == &rhs)" > n
 "      return *this;" > n
 "    " > q n
 "    return *this;" > n
 "}" >
 :ann "operator=")

;; c++-mode/operator==
(== "bool " (p "Name" f1) "::operator==(const " f1 " &other) const" n
 "{" > n
 "     " > q n
 "}" >
 :ann "operator==")

;; c++-mode/operator[]
(\[\] (p "Name") "& operator[](" (p "int index") ")" n
 "{" > n
 "        " > q n
 "}" >
 :ann "operator[]")

;; c++-mode/operator_istream
;; WARNING: Duplicate key ‘>>’ renamed to ‘>>-2’
(>>-2 "std::istream& operator>>(std::istream& is, const " (p "Name") "& " (p "c") ")" n
 "{" > n
 "         " > q n
 "	 return is;" > n
 "}" >
 :ann "operator>>")

;; c++-mode/operator_ostream
;; WARNING: Duplicate key ‘<<’ renamed to ‘<<-2’
(<<-2 "std::ostream& operator<<(std::ostream& os, const " (p "Name") "& " (p "c") ")" n
 "{" > n
 "         " > q n
 "         return os;" > n
 "}" >
 :ann "operator<<")

;; c++-mode/ostream
(os "#include <ostream>"
 :ann "ostream")

;; c++-mode/pack
(pack "void cNetCommBuffer::pack(" (p "type") ")" n
 "{" > n
 n
 "}" > n
 n
 q)

;; c++-mode/phh
(phh "std::push_heap(std::begin(" (p "container" f1) "), std::end(" f1 "));" n
 q n
 
 :ann "push_heap")

;; c++-mode/ppt
(ppt "auto pos = std::partition_point(std::begin(" (p "container" f1) "), std::end(" f1 "), [](" p ") {" n
 "  " > p n
 "});" > n
 "if (pos != std::end(" f1 ")) {" > n
 "  " > p n
 "}" > n
 q n
 
 :ann "partition_point")

;; c++-mode/private
(pr "private:" > n
 "        " > q
 :ann "private")

;; c++-mode/protected
(pt "protected:" > n
 "        " > q
 :ann "protected")

;; c++-mode/prp
(prp "if (std::prev_permutation(std::begin(" (p "container" f1) "), std::end(" f1 "))) {" n
 "  " > p n
 "}" > n
 q n
 
 :ann "prev_permutation")

;; c++-mode/psc
(psc "std::partial_sort_copy(std::begin(" (p "container" f1) "), std::end(" f1 ")," n
 "                  std::begin(" p "), std::end(" p "));" > n
 q n
 
 :ann "partial_sort_copy")

;; c++-mode/pst
(pst "std::partial_sort(std::begin(" (p "container" f1) "), std::end(" f1 "), std::end(" f1 "));" n
 q n
 
 :ann "partial_sort")

;; c++-mode/ptc
(ptc "std::partition_copy(std::begin(" (p "container" f1) "), std::end(" f1 ")," n
 "                  std::begin(" p "), std::end(" p "));" > n
 q n
 
 :ann "partition_copy")

;; c++-mode/ptn
(ptn "auto pos = std::partition(std::begin(" (p "container" f1) "), std::end(" f1 "), [](" p ") {" n
 "  " > p n
 "});" > n
 "if (pos != std::end(" f1 ")) {" > n
 "  " > p n
 "}" > n
 q n
 
 :ann "partition")

;; c++-mode/public
(pb "public:" > n
 "        " > q
 :ann "public")

;; c++-mode/rci
(rci "std::replace_copy_if(std::begin(" (p "container" f1) "), std::end(" f1 ")," n
 "  std::begin(" f1 "), [](" p ") {" > n
 "  " > p n
 "}, " p ");" > n
 q n
 
 :ann "replace_copy_if")

;; c++-mode/rmc
(rmc "std::remove_copy(std::begin(" (p "container" f1) "), std::end(" f1 ")," n
 "  std::begin(" f1 "), " p ");" > n
 q n
 
 :ann "remove_copy")

;; c++-mode/rmf
(rmf "std::remove_copy_if(std::begin(" (p "container" f1) "), std::end(" f1 ")," n
 "  std::begin(" f1 "), [](" p ") {" > n
 "    " > p n
 "});" > n
 q n
 
 :ann "remove_copy_if")

;; c++-mode/rmi
(rmi "auto pos = std::remove_if(std::begin(" (p "container" f1) "), std::end(" f1 "), [](" p ") {" n
 "  " > p n
 "});" > n
 "if (pos != std::end(" f1 ")) {" > n
 "  " > p n
 "}" > n
 q n
 
 :ann "remove_if")

;; c++-mode/rpc
(rpc "std::replace_copy(std::begin(" (p "container" f1) "), std::end(" f1 "), std::begin(" f1 "), " p ", " p ");" n
 q n
 
 :ann "replace_copy")

;; c++-mode/rpi
(rpi "std::replace_if(std::begin(" (p "container" f1) "), std::end(" f1 "), [](" p ") {" n
 p n
 "}, " p ");" > n
 q n
 
 :ann "replace_if")

;; c++-mode/rpl
(rpl "std::replace(std::begin(" (p "container" f1) "), std::end(" f1 "), " p ", " p ");" n
 q n
 
 :ann "replace")

;; c++-mode/rtc
(rtc "std::rotate_copy(std::begin(" (p "container" f1) "), std::begin(" p "), std::end(" f1 ")," n
 "  std::begin(" p "));" > n
 q n
 
 :ann "rotate_copy")

;; c++-mode/rte
(rte "std::rotate(std::begin(" (p "container" f1) "), std::begin(" p "), std::end(" f1 "));" n
 q n
 
 :ann "rotate")

;; c++-mode/rvc
(rvc "std::reverse_copy(std::begin(" (p "container" f1) "), std::end(" f1 "), std::begin(" p "));" n
 q n
 
 :ann "reverse_copy")

;; c++-mode/rvr
(rvr "std::reverse(std::begin(" (p "container" f1) "), std::end(" f1 "));" n
 q n
 
 :ann "reverse")

;; c++-mode/shf
(shf "std::random_shuffle(std::begin(" (p "container" f1) "), std::end(" f1 "));" n
 q n
 
 :ann "random_shuffle")

;; c++-mode/spt
(spt "auto pos = std::stable_partition(std::begin(" (p "container" f1) "), std::end(" f1 "), [](" p ") {" n
 "  " > p "});" n
 "if (pos != std::end(" f1 ")) {" > n
 "  " > p n
 "}" > n
 q n
 
 :ann "stable_partition")

;; c++-mode/srh
(srh "auto pos = std::search(std::begin(" (p "container" f1) "), std::end(" f1 ")," n
 "  std::begin(" p "), std::end(" p "));" > n
 "if (pos != std::end(" f1 ")) {" > n
 "  " > p n
 "}" > n
 q n
 
 :ann "search")

;; c++-mode/srn
(srn "auto pos = std::search_n(std::begin(" (p "container" f1) "), std::end(" f1 ")," p "," p ");" n
 "if (pos != std::end(" f1 ")) {" > n
 "  " > p n
 "}" > n
 q n
 
 :ann "search_n")

;; c++-mode/srt
(srt "std::sort(std::begin(" (p "container" f1) "), std::end(" f1 "));" n
 q n
 
 :ann "sort")

;; c++-mode/sstream
(ss "#include <sstream>"
 :ann "<sstream>")

;; c++-mode/std
(std "using namespace std;")

;; c++-mode/std_colon
(st "std::" q
 :ann "std::")

;; c++-mode/sth
(sth "std::sort_heap(std::begin(" (p "container" f1) "), std::end(" f1 "));" n
 q n
 
 :ann "sort_heap")

;; c++-mode/str
(str "#include <string>")

;; c++-mode/sts
(sts "std::stable_sort(std::begin(" (p "container" f1) "), std::end(" f1 "));" n
 q n
 
 :ann "stable_sort")

;; c++-mode/swr
(swr "std::swap_ranges(std::begin(" (p "container" f1) "), std::end(" f1 "), std::begin(" p "));" n
 q n
 
 :ann "swap_ranges")

;; c++-mode/template
(temp "template<" (p (completing-read "Choose: " (flatten-tree (list '("typename" "class"))))) " " (p "T") ">" n
 q
 :ann "template")

;; c++-mode/test case
(tc "BOOST_AUTO_TEST_CASE( " (p "test_case") " )" n
 "{" > n
 "        " > q n
 "}" >
 :ann "test case")

;; c++-mode/test_main
(test_main "int main(int argc, char **argv)" n
 "{" > n
 "      ::testing::InitGoogleTest(&argc, argv);" > n
 "       return RUN_ALL_TESTS();" > n
 "}" >)

;; c++-mode/test_suite
(ts "BOOST_AUTO_TEST_SUITE( " (p "test_suite1") " )" n
 n
 q n
 n
 "BOOST_AUTO_TEST_SUITE_END()" >
 :ann "test_suite")

;; c++-mode/tfm
(tfm "std::transform(std::begin(" (p "container" f1) "), std::end(" f1 ")," n
 "  std::begin(" f1 "), [](" p ") {" > n>
 p "%" n
 "});" > n
 q n
 
 :ann "transform")

;; c++-mode/this
(th "this"
 :ann "this")

;; c++-mode/throw
(throw "throw " (p "MyError") "(" q ");")

;; c++-mode/try
;; WARNING: $0 is not the last field; kept as a plain field
(try "try {" n
 "    " > p n
 "} catch (" (p "type") ") {" > n
 n
 "}" > n
 )

;; c++-mode/tryw
;; WARNING: ‘yas-selected-text’ inside an expression was replaced by nil
(tryw "try {" n
 "    " > (list 'l (format "%s" (or (or nil (car kill-ring)) ""))) n
 "} catch " (p "Exception") " {" > n
 n
 "}" > n
 q)

;; c++-mode/ucp
(ucp "std::unique_copy(std::begin(" (p "container" f1) "), std::end(" f1 ")," n
 "  std::ostream_iterator<string>(std::cout, \"\\n\"));" > n
 q n
 
 :ann "unique_copy")

;; c++-mode/uqe
(uqe "auto pos = std::unique(std::begin(" (p "container" f1) "), std::end(" f1 "));" n
 q n
 
 :ann "unique")

;; c++-mode/using
(using "using namespace " (p "std") ";" n
 q
 :ann "using namespace ...")

;; c++-mode/vector
;; WARNING: Nested fields flattened into field 3
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(vec "std::vector<" (p "Class" f1) "> " (p "var") (p "(10, Class())") ";" q
 :ann "vector")


c-lang-common c++-mode c++-ts-mode c-mode c-ts-mode

;; c-lang-common/fopen
(fopen "FILE *" (p "fp") " = fopen(" (p "\"file\"") ", \"" (p "r") "\");" q
 :ann "FILE *fp = fopen(..., ...);")

;; c-lang-common/function_doxygen_doc
(doxy "/**" n
 " * @brief      " (p "function description") > n
 " *" > n
 " * @details    " (p "detailed description") > n
 " *" > n
 " * @param      " (p "param") > n
 " *" > n
 " * @return     " (p "return type") > n
 " */" q >
 :ann "Function Doxygen Doc")

;; c-lang-common/ifdef
(ifdef "#ifdef " (p "MACRO" f1) n
 n
 q n
 n
 "#endif // " f1 >)

;; c-lang-common/inc
(incs "#include <" p ">" q
 :ann "#include <...>")

;; c-lang-common/inc.1
(incl "#include \"" p "\"" q
 :ann "#include \"...\"")

;; c-lang-common/main
(main "int main(" (p "int argc, char *argv[]") ")" n
 "{" > n
 "    " > q n
 "    return 0;" > n
 "}" > n
 )

;; c-lang-common/math
(math "#include <math.h>" n
 q)

;; c-lang-common/once
(once "#ifndef " `(p ,(upcase (replace-regexp-in-string "[^A-Za-z0-9_]" "_" (file-name-nondirectory (or (buffer-file-name))))) f1) n
 "#define " f1 > n
 n
 q n
 n
 "#endif /* " f1 " */" >
 :ann "#ifndef XXX; #define XXX; #endif")

;; c-lang-common/typedef
(typedef "typedef " (p "type") " " (p "alias") ";" q)


c-mode c-ts-mode

;; c-mode/assert
(ass "#include <assert.h>" n
 q
 :ann "assert")

;; c-mode/compile
;; WARNING: Nested fields flattened into field 1
(compile "// -*- compile-command: \"" (p "gcc -Wall -o dest file") "\" -*-" q)

;; c-mode/define
(d "#define " q
 :ann "define")

;; c-mode/fgets
(fgets "fgets(" (p "variable") ", " (p "size") ", " (p "stdin") ");" n
 q)

;; c-mode/fprintf
(fprintf "fprintf(" (p "stdout") ", \"" (p "format string") "\", " (p "variable") ");" n
 q)

;; c-mode/malloc
;; WARNING: Nested fields flattened into field 2
(malloc "malloc(sizeof(" p ")" (p " * 3") ");" n
 q)

;; c-mode/packed
(packed "__attribute__((__packed__))" q)

;; c-mode/printf
(pr "printf(\"" (p "format string") "\"" (p " ,a0,a1") ");" q
 :ann "printf")

;; c-mode/scanf
(scanf "scanf(\"" (p "format string") "\", " (p "&variable") ");" n
 q)

;; c-mode/stdio
(io "#include <stdio.h>"
 :ann "stdio")

;; c-mode/stdlib
(std "#include <stdlib.h>"
 :ann "stdlib")

;; c-mode/string
(str "#include <string.h>"
 :ann "string")

;; c-mode/strstr
(strstr "strstr(" (p "string") ", " (p "string") ");" n
 q)

;; c-mode/union
;; WARNING: $0 is not the last field; kept as a plain field
(union "typedef union {" n
 "        " > p n
 "} " (p "name") ";" >)

;; c-mode/unistd
(uni "#include <unistd.h>"
 :ann "unistd")


cc-mode c++-mode c++-ts-mode c-mode c-ts-mode csharp-mode csharp-ts-mode nesc-mode

;; cc-mode/case
(case "case " (p "constexpr") ":" (p " {" f3) > n
 "    " > q n
 "    break;" > n>
 (or (if (string-match "{" f3) "}" "") f3)
 :ann "case : {...}")

;; cc-mode/do
;; WARNING: $0 is not the last field; kept as a plain field
(do "do {" n
 "    " > p n
 "} while (" (p "condition") ");" >
 :ann "do { ... } while (...)")

;; cc-mode/else
;; WARNING: Multi-line default of field 1 is not re-indented
;; WARNING: Nested fields flattened into field 1
(else "else" (p " {\n    \n") "}" q
 :ann "else { ... }")

;; cc-mode/file_description
;; WARNING: Multi-line default of field 3 is not re-indented
;; WARNING: Nested fields flattened into field 3
(\\file "/**" n
 " *   \\file " `(p ,(file-name-nondirectory (buffer-file-name))) > n
 " *   \\brief " (p "A Documented file.") > n
 " " > (p "*\n *  Detailed description\n *\n") "*/" q
 :ann "File description")

;; cc-mode/for
(for "for (" (p "i = 0") "; " (p "i < N") "; " (p "++i") ") {" n
 "    " > q n
 "}" >)

;; cc-mode/for_n
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(forn "for (" (p "auto ") (p "i" f2) " = " (p "0") "; " f2 " < " (p "MAXIMUM") "; ++" f2 ") {" n
 "    " > q n
 "}" >
 :ann "for_n")

;; cc-mode/function_description
;; WARNING: Multi-line default of field 2 is not re-indented
;; WARNING: Nested fields flattened into field 2
(\\brief "/**" n
 " *  \\brief " (p "function description") > n
 " " > (p "*\n *  Detailed description\n *\n ") "*  \\param " (p "param") n
 " *  \\return " (p "return type") > n
 " */" q >
 :ann "Function description")

;; cc-mode/if
;; WARNING: Multi-line default of field 2 is not re-indented
;; WARNING: Nested fields flattened into field 2
(if "if (" (p "condition") ") " (p "{\n    \n}")
 :ann "if (...) { ... }")

;; cc-mode/member_description
(!< "/*!< " (p "Detailed description after the member") " */" q
 :ann "Member description")

;; cc-mode/printf
;; WARNING: A field directly follows a mirror; it may move when the mirror changes
(printf "printf(\"" (p "%s" f1) "\\n\"" (or (if (string-match "%" f1) ", " ");") f1) p (or (if (string-match "%" f1) ");" "") f1))

;; cc-mode/struct
(struct "struct " (p "name") n
 "{" > n
 "    " > q n
 "};" >
 :ann "struct ... { ... }")

;; cc-mode/switch
(switch "switch (" (p "expr") ") {" n
 "case " (p "constexpr") ":" (p " {" f3) > n
 "    " > q n
 "    break;" > n>
 (or (if (string-match "{" f3) "}\n" "") f3) "default:" n
 "    break;" > n
 "}" >
 :ann "switch (...) { case : ... default: ...}")

;; cc-mode/ternary
(\? "(" (p "cond") ") ? " (p "then") " : " (p "else") ";" q
 :ann "ternary")

;; cc-mode/while
(while "while (" (p "condition") ") {" n
 "      " > q n
 "}" >)


chef-mode

;; chef-mode/action
(action "action: " q "nothing")

;; chef-mode/bash
(bash "bash \"" (p "install something") "\" do" n
 "  user \"" (p "root") "\"" > n
 "  cwd \"" (p "/tmp") "\"" > n
 "  creates \"" (p "maybe") "\"" > n
 "  code <<-EOH" > n
 "    STATUS=0" > n
 "    " > q "|| STATUS=1" n
 "    exit $STATUS" > n
 "  EOH" > n
 "end" >)

;; chef-mode/cookbook_file
(cookbook_file "cookbook_file \"" (p "/tmp/file") "\" do" n
 "  owner \"" (p "root") "\"" > n
 "  group \"" (p "root") "\"" > n
 "  mode \"" (p "0644") "\"" > n
 "  source \"" (p "my-filename") "\"" > n
 "end" q >)

;; chef-mode/cron
(cron "cron \"" (p "name") "\" do" n
 "  hour \"" (p "5") "\"" > n
 "  minute \"" (p "0") "\"" > n
 n
 "  command \"" (p "/bin/true") "\"" > n
 "end" q >)

;; chef-mode/cronf
(cronf "cron \"" (p "name") "\" do" n
 "  hour \"" (p "*") "\"" > n
 "  minute \"" (p "*") "\"" > n
 "  day \"" (p "*") "\"" > n
 "  weekday \"" (p "*") "\"" > n
 "  command \"" (p "/bin/true") "\"" > n
 "  user \"" (p "root") "\"" > n
 "  mailto \"" (p "root@example.com") "\"" > n
 "  path \"" (p "/bin:/usr/bin") "\"" > n
 "  home \"" (p "/tmp") "\"" > n
 "  shell \"" (p "/bin/bash") "\"" > n
 "end" > n
 q)

;; chef-mode/deploy
(deploy "deploy \"/my/deploy/dir\" do" n
 "  repo \"git@github.com/whoami/provideroject\"" > n
 "  revision \"abc123\" # or \"HEAD\" or \"TAG_for_1.0\" or (subversion) 	\"1234\"" > n
 "  user \"deploy_ninja\"" > n
 "  enable_submodules true" > n
 "  migrate true" > n
 "  migration_command \"rake db:migrate\"" > n
 "  environment \"RAILS_ENV\" => \"production\", \"OTHER_ENV\" => \"foo\"" > n
 "  shallow_clone true" > n
 "  action :deploy # or :rollback" > n
 "  restart_command \"touch tmp/restart.txt\"" > n
 "  git_ssh_wrapper \"wrap-ssh4git.sh\"" > n
 "  scm_provider Chef::Provider::Git # is the default, for svn: Chefhef::Provider::Subversion" > n
 "end" > n
 )

;; chef-mode/directory
(directory "directory \"" (p "name") "\" do" n
 "  owner \"root\"" > n
 "  group \"root\"" > n
 "  mode \"0755\"" > n
 n
 "  action :create" > n
 "end" > n
 q)

;; chef-mode/directoryf
(directoryf "directory \"" (p "name") "\" do" n
 "  owner \"$create{2:root}\"" > n
 "  group \"" (p "root") "\"" > n
 "  mode \"" (p "0755") "\"" > n
 "  path \"" (p "name") "\"" > n
 "  recursive " (p "false") > n
 n
 "  action :" (p "create") > n
 "end" > n
 q)

;; chef-mode/env
(env "env \"" (p "RAILS_ENV") "\" do" n
 "  value \"" (p "production") "\"" > n
 "end" q >)

;; chef-mode/execute
(execute "execute \"" (p "name") "\" do" n
 "  command \"" (p "ls -la") "\"" > n
 "  creates \"" (p "/tmp/something") "\"" > n
 n
 "  action :" (p "run") > n
 "end" q >)

;; chef-mode/executef
(executef "execute \"" (p "name") "\" do" n
 "  command \"" (p "ls -la") "\"" > n
 "  creates \"$ls{3:/tmp/something}\"" > n
 "  cwd \"" (p "/tmp") "\"" > n
 "  environment ({" (p "'HOME' => '/home/myhome'") "})" > n
 "  user \"" (p "root") "\"" > n
 "  group \"" (p "root") "\"" > n
 "  path \"" (p "['/opt/bin','/opt/sbin']") "\"" > n
 "  timeout " (p "3600") > n
 "  returns " (p "0") > n
 "  umask \"" (p "022") "umask\"" > n
 n
 "  action :" (p "run") > n
 "end" q >)

;; chef-mode/file
(file "file \"" (p "name") "\" do" n
 "  owner \"root\"" > n
 "  group \"root\"" > n
 "  mode \"0644\"" > n
 n
 "  action :create" > n
 "end" q >)

;; chef-mode/filef
;; WARNING: Fields visited in buffer order (1 3 4 5 6 7 8 2) instead of numeric order
(filef "file \"" (p "name") "\" do" n
 "  path \"" (p "path") "\"" > n
 "  backup " (p "5") > n
 "  owner \"" (p "root") "\"" > n
 "  group \"" (p "root") "\"" > n
 "  mode \"" (p "0644") "\"" > n
 "  content \"" (p "content here") "\"" > n
 n
 "  action :" (p "create") > n
 "end" q >)

;; chef-mode/git
(git "git \"" (p "/home/user/deployment") "\" do" n
 "  repository \"" (p "git@github.com:gitsite/deploymentployment.git") "\"" > n
 "  reference \"" (p "master") "\"" > n
 "  user \"" (p "user") "\"" > n
 "  group \"" (p "templateest") "\"" > n
 "  action :sync" > n
 "end" q >)

;; chef-mode/group
(group "group \"" (p "name") "\" do" n
 "  gid " (p "999") > n
 "  members [" (p "'paco','vicente'") "]" > n
 n
 "  action :create" > n
 "end" q >)

;; chef-mode/http_request
(http_request "http_request \"" (p "some message") "\" do" n
 "  url \"" (p "http://example.com/check_in") "\"" > n
 "end" q >)

;; chef-mode/http_requestp
(http_requestp "http_request \"" (p "posting data") "\" do" n
 "  action :post" > n
 "  url 		\"" (p "http://example.com/check_in") "\"" > n
 "  message " (p ":some => \"data\"") > n
 "  headers (" (p "{\"AUTHORIZATION\" => authorization}") ")" > n
 "end" q >)

;; chef-mode/ignore_failure
(ignore_failure "ignore_failure " q "true")

;; chef-mode/inc
(inc "include_recipe \"" (p "example::recipe") "\"" q)

;; chef-mode/link
(link "link \"" (p "/tmp/passwd") "\" do" n
 "  to \"" (p "/etc/passwd") "\"" > n
 "end" q >)

;; chef-mode/linkf
(linkf "link \"" (p "/tmp/passwd") "\" do" n
 "  to \"" (p "/etc/passwd") "\"" > n
 "  link_type :" (p "symbolic") > n
 "  owner \"" (p "root") "\"" > n
 "  group \"" (p "root") "\"" > n
 "end" q >)

;; chef-mode/log
(log "log (\"" (p "your string to log") "\") { level :" (p "debug") " }" q)

;; chef-mode/machine
(machine "machine \"" (p "name") "\" do" n
 "  role '" (p "web") "'" > n
 "  recipe '" (p "web") "'" > n
 "  chef_environment '" (p "_default") "'" > n
 "  converge true" > n
 "end" > n
 q)

;; chef-mode/meta
(meta "maintainer       \"" (p "YOUR_COMPANY_NAME") "\"" n
 "maintainer_email \"" (p "YOUR_EMAIL") "\"" > n
 "license          \"" (p "All rights reserved") "\"" > n
 "description      \"" (p "Installs/Configures example") "\"" > n
 "long_description IO.read(File.join(File.dirname(__FILE__), 'README.rdoc'))" > n
 "version          \"" (p "0.0.1") "\"" q >)

;; chef-mode/not_if
(not_if "not_if \"" p "\"" q)

;; chef-mode/notifies
(notifies "notifies :" (p "restart") ", \"" (p "service") "[" (p "name") "]\"" q)

;; chef-mode/only_if
(only_if "only_if \"" p "\"" q)

;; chef-mode/pac
(pac "package \"" (p "name") "\" do" n
 "  action :" (p "install") > n
 "  version \"" (p "1.0-1") "\"" > n
 "end" q >)

;; chef-mode/pak
(pak "package \"" (p "name") "\" do" n
 "  action :" (p "install") > n
 "  version \"" (p "1.0-1") "\"" > n
 "end" q >)

;; chef-mode/provider
(provider "provider Chef::Provider::" q "Package::Rubygems")

;; chef-mode/python
(python "python \"" (p "install something") "\" do" n
 "  user \"" (p "root") "\"" > n
 "  cwd \"" (p "/tmp") "\"" > n
 "  code <<-EOH" > n
 "    " > q n
 "  EOH" > n
 "end" >)

;; chef-mode/remote_file
(remote_file "remote_file \"" (p "/tmp/remote_file") "\" do" n
 "  owner \"" (p "root") "\"" > n
 "  group \"" (p "root") "\"" > n
 "  mode \"" (p "0644") "\"" > n
 "  source \"" (p "http://www.example.com/remote_file") "\"" > n
 "  checksum \"" (p "sha256checksum") "\"" > n
 "end" q >)

;; chef-mode/retries
(retries "retries " (p "1") n
 "retry_delay " (p "2") >)

;; chef-mode/role
(role "name \"" (p "role name") "\"" n
 "  description \"" (p "Description for the role") "\"" > n
 "  env_run_lists \"" (p "role name") "\" => [" > n
 "  ]" > n
 "  run_list \"\"" > n
 "  default_attributes(" > n
 "  " > (p ":attribute => \"example\"") n
 "  )" > n
 q)

;; chef-mode/ruby
(ruby "ruby_block \"" (p "reload client config") "\" do" n
 "  block do" > n
 "    " > q "Chef::Config.from_file(\"/Chefetc/chef/client.rb\")" n
 "  end" > n
 "end" >)

;; chef-mode/script
(script "script \"" (p "do something") "\" do" n
 "  interpreter \"bash\"" > n
 "  user \"" (p "root") "\"" > n
 "  cwd \"" (p "/tmp") "\"" > n
 "  creates \"" (p "maybe") "\"" > n
 "  code <<-EOH" > n
 "    STATUS=0" > n
 "    " > q "|| STATUS=1" n
 "    exit $STATUS" > n
 "    EOH" > n
 "end" > n
 )

;; chef-mode/service
(service "service \"" (p "name") "\" do" n
 "  supports :status => " (p "true") ", :restart => " (p "true") ", :truereload => " (p "true") > n
 "  action " (p "[ :enable, :start ]") > n
 "end" q >)

;; chef-mode/servicep
(servicep "service \"" (p "name") "\" do" n
 "  pattern \"" (p "pattern") "\"" > n
 "  supports :status => " (p "true") ", :restart => " (p "true") ", :reload => " (p "true") > n
 "  action " (p "[ :enable, :start ]") > n
 "end" q >)

;; chef-mode/subscribes
(subscribes "subscribes :" (p "restart") ", \"" (p "template") "[" (p "name") "]\"" q)

;; chef-mode/supports
(supports "supports :status => :" (p "true") ", :restart => :" (p "true") ", :reload => :" (p "true"))

;; chef-mode/template
(template "template \"" (p "name") "\" do" n
 "  source \"" (p "source") ".erb\"" > n
 "  owner \"root\"" > n
 "  group \"root\"" > n
 "  mode \"0644\"" > n
 "end" > n
 q)

;; chef-mode/templatev
(templatev "template \"" (p "name") "\" do" n
 "  source \"" (p "source") ".erb\"" > n
 "  owner \"root\"" > n
 "  group \"root\"" > n
 "  node \"0644\"" > n
 "  variables( " (p ":config_var => node[:configs][:config_var]") " 	)" > n
 "end" > n
 q)

;; chef-mode/user
(user "user \"" (p "random") "\" do" n
 "  action :create" > n
 "  comment \"" (p "Random User") "\"" > n
 "  uid " (p "1000") > n
 "  gid \"" (p "users") "\"" > n
 "  home \"" (p "/home/random") "\"" > n
 "  shell \"" (p "/bin/zsh") "\"" > n
 "  password \"" (p "$1$JJsvHslV$szsCjVEroftprNn4JHtDi.") "\"" > n
 "  supports :manage_home =>manage_home true" > n
 "end" > n
 q)


clojure-mode cider-repl-mode

;; clojure-mode/bench
(bench "(dotimes [_ 5 ]" n
 "  (time (dotimes [i 1000000]" > n
 "  " > q n
 "  )))" >)

;; clojure-mode/bp
(bp "(swank.core/break)")

;; clojure-mode/com
(com "(defrecord " p " [" p "]" n
 "  component/Lifecycle" > n
 "  (start [this] " p ")" > n
 "  (stop [this] " p "))" q >)

;; clojure-mode/cond
(cond "(cond" n
 "  (" p ") " p > n
 "  (" p ") " p > n
 "  :else " p ")" q >)

;; clojure-mode/condp
(condp "(condp " (p "pred") " " (p "expr") n
 "  " > q ")")

;; clojure-mode/def
(def "(def " q ")" n
 )

;; clojure-mode/defm
(defm "(defmacro " p n
 "  \"" p "\"" > n
 "  [" p "]" > n
 "  " > q ")" n
 
 :ann "defmacro")

;; clojure-mode/defn
(defn "(defn " p n
 "  \"" p "\"" > n
 "  [" p "]" > n
 "  " > q ")" n
 )

;; clojure-mode/defr
(defr "(defrecord" n
 "  " > p n
 "  [" p "]" > n
 "  " > q ")" n
 
 :ann "defrecord")

;; clojure-mode/deft
(deft "(deftype" n
 "  " > p n
 "  [" p "]" > n
 "  " > q ")"
 :ann "deftype")

;; clojure-mode/doseq
(doseq "(doseq [" p " " p "]" n
 "  " > p ")" n
 q)

;; clojure-mode/fdef
(fdef "(s/fdef " p n
 "        :args (s/cat " p ")" > n
 "        :ret " q ")" >)

;; clojure-mode/fn
(fn "(fn [" p "]" n
 "  " > q ")" n
 )

;; clojure-mode/for
(for "(for [" p " " p "]" n
 "  " > p ")" n
 q)

;; clojure-mode/if
(if "(if " p n
 "  " > p n
 "  " > p ")" n
 q n
 )

;; clojure-mode/ifl
(ifl "(if-let [" p " " p "]" n
 "  " > p ")" n
 q)

;; clojure-mode/import
(import "(:import (" p "))")

;; clojure-mode/is
(is "(is (= " p " " p "))" n
 q)

;; clojure-mode/let
(let "(let [" p " " p "]" n
 "  " > p ")" n
 q n
 )

;; clojure-mode/letfn
(letfn "(letfn [(" p " [" p "]" n
 "          " > p ")]" n
 "        " > p ")" n
 q)

;; clojure-mode/main
(main "(defn -main [& args]" n
 "  " > p ")")

;; clojure-mode/map
(map "(map " p " " p ")" q)

;; clojure-mode/map.lambda
;; WARNING: Duplicate key ‘map’ renamed to ‘map-2’
(map-2 "(map #(" p ") " p ")"
 :ann "map lambda")

;; clojure-mode/mdoc
(mdoc "^{:doc \"" p "\"}" q)

;; clojure-mode/ns
(ns "(ns " (list 'l (format "%s" (or (cl-flet ((try-src-prefix (path src-pfx) (let ((parts (split-string path src-pfx))) (if (= 2 (length parts)) (cl-second parts) nil)))) (let* ((p (buffer-file-name)) (p2 (cl-first (cl-remove-if-not (lambda (x) x) (mapcar (lambda (pfx) (try-src-prefix p pfx)) '("/src/cljs/" "/src/clj/" "/src/" "/test/"))))) (p3 (file-name-sans-extension p2)) (p4 (mapconcat (lambda (x) x) (split-string p3 "/") "."))) (replace-regexp-in-string "_" "-" p4))) ""))) ")" n
 )

;; clojure-mode/opts
(opts "{:keys [" p "]" n
 " :or {" p "}" > n
 " :as " p "}" >)

;; clojure-mode/pr
(pr "(prn " p ")" n
 q)

;; clojure-mode/print
(print "(println " p ")" n
 q)

;; clojure-mode/reduce
;; WARNING: Nested fields flattened into field 1
(reduce "(reduce " (p "(fn [p n] )") " " p ")" n
 q)

;; clojure-mode/require
(require "(:require [" p " :as " p "])")

;; clojure-mode/sdef
;; WARNING: $0 is not the last field; kept as a plain field
(sdef "(s/def ::" p " " p ")")

;; clojure-mode/skeys
;; WARNING: $0 is not the last field; kept as a plain field
(skeys "(s/keys :" p " " p ")")

;; clojure-mode/test
(test "(deftest " p n
 "  (is (= " p "))" > n
 "  " > q ")" n
 )

;; clojure-mode/testing
(testing "(testing \"" p "\")" n
 q)

;; clojure-mode/thread-first
(-> "(-> " p ")" q)

;; clojure-mode/thread-last
(->> "(->> " p ")" q)

;; clojure-mode/try
(try "(try" n
 p n
 "(catch " (p "Exception") " e" > n>
 p "))")

;; clojure-mode/use
(use "(:use [" p " :refer [" p "]])")

;; clojure-mode/when
(when "(when " p n
 "      " > p ")" n
 q n
 )

;; clojure-mode/whenl
(whenl "(when-let [" p " " p "]" n
 "  " > p ")" n
 "  " > q)


cmake-mode

;; cmake-mode/add_executable
(exe "add_executable(" (s f1) " " (p "main.cpp") ")" n
 "target_link_libraries(" f1 " " p ")" q >
 :ann "add_executable")

;; cmake-mode/add_library
(lib "add_library(" (s f1) " " (p "class.cpp") ")" n
 "target_link_libraries(" f1 " " p ")" q >
 :ann "add_library")

;; cmake-mode/cmake_minimum_required
(min "cmake_minimum_required(VERSION " (p "2.6") ")" q
 :ann "cmake_minimum_required")

;; cmake-mode/foreach
(for "foreach(" (p "var" f1) " ${" (p "array") "})" n
 "        " > p n
 "endforeach(" f1 ")" q >
 :ann "foreach")

;; cmake-mode/function
(fun "function(" (p "name" f1) ")" n
 "         " > p n
 "endfunction(" f1 ")" q >
 :ann "function")

;; cmake-mode/if
(if "if(" (p "cond" f1) ")" n
 "   " > p n
 "endif(" f1 ")" q >)

;; cmake-mode/ifelse
;; WARNING: Duplicate key ‘if’ renamed to ‘if-2’
(if-2 "if(" (p "cond" f1) ")" n
 "        " > p n
 "else(" (p "cond") ")" > n
 "        " > n
 "endif(" f1 ")" q >
 :ann "ifelse")

;; cmake-mode/include
(inc "include(" q ")" n
 
 :ann "include")

;; cmake-mode/macro
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(macro "macro(" (p "name" f1) (p " args" f2) ")" n
 "        " > f2 n
 "endmacro(" f1 ")" q >)

;; cmake-mode/message
(msg "message(" (p "STATUS ") "\"" q "\")"
 :ann "message")

;; cmake-mode/option
(opt "option(" (p "OPT") " \"" (p "docstring") "\" " (p "value") ")" q
 :ann "option")

;; cmake-mode/project
(proj "project(" q ")"
 :ann "project")

;; cmake-mode/set
(set "set(" (p "var") " " (p "value") ")" q)


conf-desktop-mode

;; conf-desktop-mode/desk
(desk "[Desktop Entry]" n
 "Encoding=" (p "UTF-8") > n
 "Version=" (p "1.0") > n
 "Type=" (p "Application") > n
 "NoDisplay=" (p "false") > n
 "Terminal=" (p "false") > n
 "Exec=" p > n
 "Name=" p > n
 "Icon=" p > n
 "Comment=" p > n
 "Categories=" p > n
 q
 :ann "current-date")


conf-unix-mode

;; conf-unix-mode/section
(sec "Section \"" (p "Device") "\"" n
 "        " > q n
 "EndSection" >
 :ann "section")


coq-mode

;; coq-mode/definitions/definition.yasnippet
(Def "Definition " p " (" p " : " p ") : " p " :=" n
 "  " > q "." n
 
 :ann "Definition")

;; coq-mode/definitions/fixpoint-with.yasnippet
;; WARNING: Fields visited in buffer order (1 2 3 4 9 5 6 7 8) instead of numeric order
(Fixpw "Fixpoint " p " (" p " : " p ") : " p " :=" n
 "  " > p n
 "with " p " (" p " : " p ") : " p " :=" > n
 "  " > q "."
 :ann "Fixpoint-with")

;; coq-mode/definitions/fixpoint.yasnippet
(Fixp "Fixpoint " p " (" p " : " p ") : " p " :=" n
 "  " > q "."
 :ann "Fixpoint")

;; coq-mode/definitions/fun.yasnippet
(fun "fun (" p " : " p " => " q ")" n
 )

;; coq-mode/definitions/inductive.yasnippet
(Ind "Inductive " p " : " p " :=" n
 "| " q > n
 "." > n
 
 :ann "Inductive")

;; coq-mode/lookup/check.yasnippet
(C "Check " p "." n
 q n
 
 :ann "Check")

;; coq-mode/lookup/locate.yasnippet
(L "Locate \"" p "\"." n
 q n
 
 :ann "Locate")

;; coq-mode/lookup/print.yasnippet
(P "Print " p "." n
 q n
 
 :ann "Print")

;; coq-mode/lookup/search.yasnippet
(S "Search " p "." n
 q n
 
 :ann "Search")

;; coq-mode/lookup/searchabout.yasnippet
(SA "SearchAbout " p "." n
 q n
 
 :ann "SearchAbout")

;; coq-mode/lookup/searchpattern.yasnippet
(SP "SearchPattern (" p ")." n
 q n
 
 :ann "SearchPattern")

;; coq-mode/misc/forall.yasnippet
(fa "forall (" p " : " p "), " q n
 
 :ann "forall")

;; coq-mode/misc/if.yasnippet
(if "if " p " then " p " else " q n
 )

;; coq-mode/misc/infix.yasnippet
(Inf "Infix \"" p "\" := " p " (at level " p ", " p " associativity)." n
 q n
 
 :ann "Infix")

;; coq-mode/misc/match.yasnippet
(match "match " p " with" n
 "  | " q " =>" > n
 "end" >)

;; coq-mode/misc/notation.yasnippet
(Nota "Notation \"" p "\" := (" p ") (at level " p ", " p " associativity)." n
 q n
 
 :ann "Notation")

;; coq-mode/misc/require.yasnippet
;; WARNING: Field transformation of field 1 is only evaluated on expansion
(Req `(p ,(format "%s" (or (dlet ((yas-text "")) (coq-insert-requires)) ""))) n
 q
 :ann "Require")

;; coq-mode/propositions/axiom.yasnippet
(Axi (p (current-column) col noinsert) "Axiom " p " :" n
 (ignore (indent-to col)) "  " q "." n
 
 :ann "Axiom")

;; coq-mode/propositions/conjecture.yasnippet
(Conj "Conjecture " p " : " q "." n
 
 :ann "Conjecture")

;; coq-mode/propositions/corollary.yasnippet
(Cor (p (current-column) col noinsert) "Corollary " p " :" n
 (ignore (indent-to col)) "  " p "." n
 (ignore (indent-to col)) "Proof." n
 (ignore (indent-to col)) "  " q n
 (ignore (indent-to col)) "Qed." n
 
 :ann "Corollary")

;; coq-mode/propositions/example.yasnippet
(Exp (p (current-column) col noinsert) "Example " p " :" n
 (ignore (indent-to col)) "  " p "." n
 (ignore (indent-to col)) "Proof." n
 (ignore (indent-to col)) "  " q n
 (ignore (indent-to col)) "Qed." n
 
 :ann "Example")

;; coq-mode/propositions/fact.yasnippet
(Fact (p (current-column) col noinsert) "Fact " p " :" n
 (ignore (indent-to col)) "  " p "." n
 (ignore (indent-to col)) "Proof." n
 (ignore (indent-to col)) "  " q n
 (ignore (indent-to col)) "Qed." n
 )

;; coq-mode/propositions/hypotheses.yasnippet
(Hypos "Hypotheses " p " : " q "." n
 
 :ann "Hypotheses")

;; coq-mode/propositions/hypothesis.yasnippet
(Hypo "Hypothesis " p " : " q "." n
 
 :ann "Hypothesis")

;; coq-mode/propositions/instance.yasnippet
(Ins (p (current-column) col noinsert) "Instance " p " :" n
 (ignore (indent-to col)) "  " p "." n
 (ignore (indent-to col)) "Proof." n
 (ignore (indent-to col)) "  " q n
 (ignore (indent-to col)) "Qed." n
 
 :ann "Instance")

;; coq-mode/propositions/lemma.yasnippet
(Lem (p (current-column) col noinsert) "Lemma " p " :" n
 (ignore (indent-to col)) "  " p "." n
 (ignore (indent-to col)) "Proof." n
 (ignore (indent-to col)) "  " q n
 (ignore (indent-to col)) "Qed." n
 
 :ann "Lemma")

;; coq-mode/propositions/parameter.yasnippet
(Param "Parameter " p " : " q "." n
 
 :ann "Parameters")

;; coq-mode/propositions/proposition.yasnippet
(Pro (p (current-column) col noinsert) "Proposition " p " :" n
 (ignore (indent-to col)) "  " p "." n
 (ignore (indent-to col)) "Proof." n
 (ignore (indent-to col)) "  " q n
 (ignore (indent-to col)) "Qed." n
 
 :ann "Proposition")

;; coq-mode/propositions/remark.yasnippet
(Rem (p (current-column) col noinsert) "Remark " p " :" n
 (ignore (indent-to col)) "  " p "." n
 (ignore (indent-to col)) "Proof." n
 (ignore (indent-to col)) "  " q n
 (ignore (indent-to col)) "Qed." n
 
 :ann "Remark")

;; coq-mode/propositions/theorem.yasnippet
(The (p (current-column) col noinsert) "Theorem " p " :" n
 (ignore (indent-to col)) "  " p "." n
 (ignore (indent-to col)) "Proof." n
 (ignore (indent-to col)) "  " q n
 (ignore (indent-to col)) "Qed."
 :ann "Theorem")

;; coq-mode/propositions/variable.yasnippet
(Var "Variable " p " : " q "." n
 
 :ann "Variable")

;; coq-mode/propositions/variables.yasnippet
(Vars "Variables " p " : " q "." n
 
 :ann "Variables")

;; coq-mode/tactics/case.yasnippet
(case "case " (p "n" f1) " as [ | " f1 "' ]." q n
 )

;; coq-mode/tactics/destruct.yasnippet
(des "destruct " p " as [ " q " ]."
 :ann "destruct")

;; coq-mode/tactics/induction.yasnippet
(ind "induction " (p "n" f1) " as [ | " f1 "' IH_" f1 "' ]." q
 :ann "induction")

;; coq-mode/tactics/rename.yasnippet
(rename "rename " p " into " p "." n
 q)

;; coq-mode/tactics/rewrite-left.yasnippet
(rwl "rewrite <- " q "." n
 
 :ann "rewrite-left")

;; coq-mode/tactics/rewrite-right.yasnippet
(rwr "rewrite -> " q "." n
 
 :ann "rewrite-right")

;; coq-mode/tactics/rewrite.yasnippet
(rw "rewrite " q "." n
 
 :ann "rewrite")


cpp-omnet-mode

;; cpp-omnet-mode/EV
(ev "EV << \"" (p "string") "\"" q ";"
 :ann "EV")

;; cpp-omnet-mode/emit_signal
(emit "emit(" (p "signal_id") ", " (p "long") ");" q
 :ann "emit_signal")

;; cpp-omnet-mode/intuniform
(intuni "intuniform(" (p "0") ", " (p "1") ")" q
 :ann "intuniform")

;; cpp-omnet-mode/math
(math "#include <cmath>")

;; cpp-omnet-mode/nan
(nan "isnan(" (p "x") ")" q)

;; cpp-omnet-mode/omnet
(omnet "#include <omnetpp.h>")

;; cpp-omnet-mode/parameter_omnetpp
(par (p "var") " = par(\"" (p "par") "\");" q
 :ann "parameter_omnetpp")

;; cpp-omnet-mode/scheduleAt
(sched "scheduleAt(simTime()+" (p "1.0") ", " (p "event") ");" q
 :ann "scheduleAt")

;; cpp-omnet-mode/uniform
(uni "uniform(" (p "0") ", " (p "1") ")" q
 :ann "uniform")


crystal-mode

;; crystal-mode/any
(any "any? { |" (p "e") "| " q " }"
 :ann "any? { |...| ... }")

;; crystal-mode/case
(case "case " (p "object") n
 "when " (p "condition") > n
 "  " > q n
 "end" >
 :ann "case ... end")

;; crystal-mode/cls
(cls "class " `(p ,(format "%s" (or (let ((fn (capitalize (file-name-nondirectory (file-name-sans-extension (or (buffer-file-name) (buffer-name (current-buffer)))))))) (replace-regexp-in-string "_" "" fn t t)) ""))) n
 "  " > q n
 "end" >
 :ann "class ... end")

;; crystal-mode/def
;; WARNING: Nested fields flattened into field 2
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(def "def " (p "method") (p "(args)") n
 "    " > q n
 "end" >
 :ann "def ... end")

;; crystal-mode/ea
(ea "each { |" (p "e") "| " q " }"
 :ann "each { |...| ... }")

;; crystal-mode/eac
(eac "each_cons(" (p "2") ") { |" (p "group") "| " q " }"
 :ann "each_cons(...) { |...| ... }")

;; crystal-mode/eai
(eai "each_index { |" (p "i") "| " q " }"
 :ann "each_index { |i| ... }")

;; crystal-mode/eawi
(eawi "each_with_index { |" (p "e") ", " (p "i") "| " q " }"
 :ann "each_with_index { |e, i| ... }")

;; crystal-mode/el
(el "else" (list 'l (format "%s" (or (indent-for-tab-command) ""))) n
 "  " > p
 :ann "else ...")

;; crystal-mode/esi
(esi "elsif" (list 'l (format "%s" (or (indent-for-tab-command) ""))) " " (p "condition") n
 "  " > p
 :ann "elsif ...")

;; crystal-mode/for
(for "for " (p "el") " in " (p "collection") n
 "    " > q n
 "end" >)

;; crystal-mode/forin
(forin "for " (p "element") " in " (p "collection") n
 "  " > q n
 "end" >
 :ann "for ... in ...; ... end")

;; crystal-mode/if
(if "if " (p "condition") n
 "  " > q n
 "end" >
 :ann "if ... end")

;; crystal-mode/ife
(ife "if " (p "condition") n
 "  " > p n
 "else" > n
 "  " > p n
 "end" q >
 :ann "if ... else ... end")

;; crystal-mode/inc
(inc "include " (p "Module") n
 q n
 
 :ann "include Module")

;; crystal-mode/init
(init "def initialize(" (p "args") ")" n
 "    " > q n
 "end" >)

;; crystal-mode/map
(map "map { |" (p "e") "| " q " }"
 :ann "map { |...| ... }")

;; crystal-mode/mod
(mod "module " `(p ,(format "%s" (or (let ((fn (capitalize (file-name-nondirectory (file-name-sans-extension (or (buffer-file-name) (buffer-name (current-buffer)))))))) (while (string-match "_" fn) (setq fn (replace-match "" nil nil fn))) fn) ""))) n
 "  " > q n
 "end" >
 :ann "module ... end")

;; crystal-mode/red
(red "reduce(" (p "0") ") { |" (p "accumulator") ", " (p "element") "| " q " }"
 :ann "reduce(...) { |...| ... }")

;; crystal-mode/reject
(reject "reject { |" (p "element") "| " q " }"
 :ann "reject { |...| ... }")

;; crystal-mode/req
(require "require '" q "'"
 :ann "require \"...\"")

;; crystal-mode/select
(select "select { |" (p "element") "| " q " }"
 :ann "select { |...| ... }")

;; crystal-mode/upt
(upt "upto(" (p "n") ") { |" (p "i") "|" n
 "  " > q n
 "}" >
 :ann "upto(...) { |n| ... }")

;; crystal-mode/when
(when "when " (p "condition") n
 "  " > q n
 "end" >
 :ann "when ... end")

;; crystal-mode/while
(while "while " (p "condition") n
 "  " > q n
 "end" >
 :ann "while ... end")

;; crystal-mode/zip
(zip "zip(" (p "enums") ") { |" (p "row") "| " q " }"
 :ann "zip(...) { |...| ... }")


csharp-mode csharp-ts-mode

;; csharp-mode/attrib
;; WARNING: Fields visited in buffer order (3 1 2) instead of numeric order
(attrib "/// <summary>" n
 "/// " p > n
 "/// </summary>" > n
 "private " p " " p ";" q >
 :ann "private attribute ....;")

;; csharp-mode/attrib.1
;; WARNING: Duplicate key ‘attrib’ renamed to ‘attrib-2’
;; WARNING: Fields visited in buffer order (3 1 2 4 5) instead of numeric order
(attrib-2 "/// <summary>" n
 "/// " p > n
 "/// </summary>" > n
 "private " (s f1) " " (s f2) ";" > n
 n
 "/// <summary>" > n
 "/// " p > n
 "/// </summary>" > n
 "/// <value>" p "</value>" > n
 "public " f1 " " f2 > n
 "{" > n
 "    get {" > n
 "        return this." f2 ";" > n
 "    }" > n
 "    set {" > n
 "        this." f2 " = value;" > n
 "    }" > n
 "}" q >
 :ann "private attribute ....; public property ... ... { ... }")

;; csharp-mode/attrib.2
;; WARNING: Duplicate key ‘attrib’ renamed to ‘attrib-3’
;; WARNING: Fields visited in buffer order (3 1 2) instead of numeric order
(attrib-3 "/// <summary>" n
 "/// " f3 > n
 "/// </summary>" > n
 "private " f1 " " (or (if (> (length f2) 0) (format "_%s%s" (downcase (substring f2 0 1)) (substring f2 1 (length f2))) "") f2) ";" > n
 n
 "/// <summary>" > n
 "/// " (p "Description" f3) > n
 "/// </summary>" > n
 "/// <value><c>" f1 "</c></value>" > n
 "public " (p "Type" f1) " " (p "Name" f2) > n
 "{" > n
 "    get {" > n
 "        return this." (or (if (> (length f2) 0) (format "_%s%s" (downcase (substring f2 0 1)) (substring f2 1 (length f2))) "") f2) ";" > n
 "    }" > n
 "    set {" > n
 "        this." (or (if (> (length f2) 0) (format "_%s%s" (downcase (substring f2 0 1)) (substring f2 1 (length f2))) "") f2) " = value;" > n
 "    }" > n
 "}" q >
 :ann "private _attribute ....; public Property ... ... { ... }")

;; csharp-mode/class
;; WARNING: Fields visited in buffer order (5 1 3 2 4) instead of numeric order
(class (p "public") " class " (p "Name" f1) n
 "{" > n
 "    #region Ctor & Destructor" > n
 "    /// <summary>" > n
 "    /// " (p "Standard Constructor") > n
 "    /// </summary>" > n
 "    public " f1 "(" p ")" > n
 "    {" > n
 "    }" > n
 n
 "    /// <summary>" > n
 "    /// " (p "Default Destructor") > n
 "    /// </summary>" > n
 "    public ~" f1 "()" > n
 "    {" > n
 "    }" > n
 "    #endregion" > n
 "}" q >
 :ann "class ... { ... }")

;; csharp-mode/comment
(comment "/// <summary>" n
 "/// " p > n
 "/// </summary>" q >
 :ann "/// <summary> ... </summary>")

;; csharp-mode/comment.1
;; WARNING: Duplicate key ‘comment’ renamed to ‘comment-2’
(comment-2 "/// <param name=\"" p "\">" p "</param>" q
 :ann "/// <param name=\"...\"> ... </param>")

;; csharp-mode/comment.2
;; WARNING: Duplicate key ‘comment’ renamed to ‘comment-3’
(comment-3
 :ann "/// <returns>$1</returns>")

;; csharp-mode/comment.3
;; WARNING: Duplicate key ‘comment’ renamed to ‘comment-4’
(comment-4 "/// <exception cref=\"" p "\">" p "</exception>" q
 :ann "/// <exception cref=\"...\"> ... </exception>")

;; csharp-mode/fore
(fore "foreach (" (p "var") " " (p "item") " in " (p "list") ")" n
 "{" > n
 "    " > q n
 "}" >
 :ann "foreach { ... }")

;; csharp-mode/main
(main "static void Main(string[] args)" n
 "{" > n
 "    " > q n
 "}" > n
 )

;; csharp-mode/method
;; WARNING: Fields visited in buffer order (5 1 2 3 4) instead of numeric order
(method "/// <summary>" n
 "/// " (p "Description") > n
 "/// </summary>" (or (if (string= (upcase f2) "VOID") "" (format "%s%s%s" "\n/// <returns><c>" f2 "</c></returns>")) f2) > n>
 (p "public") " " (p "void" f2) " " (p "MethodName") "(" p ")" n
 "{" > n
 q n
 "}" >
 :ann "public void Method { ... }")

;; csharp-mode/namespace
(namespace "namespace " p n
 "{" > n
 q n
 "}" >
 :ann "namespace .. { ... }")

;; csharp-mode/prop
;; WARNING: Fields visited in buffer order (5 6 1 2 3 4) instead of numeric order
(prop "/// <summary>" n
 "/// " p > n
 "/// </summary>" > n
 "/// <value>" p "</value>" > n>
 p p p n
 "{" > n
 "    get {" > n
 "        return this." (s f4) ";" > n
 "    }" > n
 "    set {" > n
 "        this." f4 " = value;" > n
 "    }" > n
 "}" q >
 :ann "property ... ... { ... }")

;; csharp-mode/read
(cr "Console.Read(" (p "Read") ");" q
 :ann "Console.Read(...);")

;; csharp-mode/readline
(crl "Console.ReadLine(" (p "Read") ");" n
 q
 :ann "Console.ReadLine(...);")

;; csharp-mode/region
(region "#region " p n
 q n
 "#endregion" >
 :ann "#region ... #endregion")

;; csharp-mode/trycatch
;; WARNING: $0 is not the last field; kept as a plain field
(tc "try" n
 "{" > n
 "	" > p n
 "}" > n
 "catch (" (p "Exception") " ex)" > n
 "{" > n
 "	" > (p "Console.WriteLine(ex.ToString());") n
 "}" >
 :ann "try{...} catch (ex) {...}")

;; csharp-mode/using
(using "using " p ";" q
 :ann "using ...;")

;; csharp-mode/using.1
;; WARNING: Duplicate key ‘using’ renamed to ‘using-2’
(using-2 "using System;"
 :ann "using System;")

;; csharp-mode/using.2
;; WARNING: Duplicate key ‘using’ renamed to ‘using-3’
(using-3 "using System." p ";" q
 :ann "using System....;")

;; csharp-mode/write
(cw "Console.Write(" (p "Write") ");" q
 :ann "Console.Write(...);")

;; csharp-mode/writeline
(cwl "Console.WriteLine(" (p "Write") ");" q
 :ann "Console.WriteLine(...);")


css-mode css-ts-mode

;; css-mode/bg
(bg "background-color: #" (p "DDD") ";" q
 :ann "background-color: ...")

;; css-mode/bg.1
(bg.1 "background-image: url(" p ");" q
 :ann "background-image: ...")

;; css-mode/bor
(bor "border: " (p "1px") " " (p "solid") " #" (p "999") ";" q
 :ann "border size style color")

;; css-mode/cl
(cl "clear: " p ";" q
 :ann "clear: ...")

;; css-mode/disp.block
(disp.block "display: block;"
 :ann "display: block")

;; css-mode/disp.inline
(disp.inline "display: inline;"
 :ann "display: inline")

;; css-mode/disp.none
(disp.none "display: none;"
 :ann "display: none")

;; css-mode/ff
(ff "font-family: " p ";" q
 :ann "font-family: ...")

;; css-mode/fs
(fs "font-size: " (p "12px") ";" q
 :ann "font-size: ...")

;; css-mode/mar.bottom
(mar.bottom "margin-bottom: " p ";" q
 :ann "margin-bottom: ...")

;; css-mode/mar.left
(mar.left "margin-left: " p ";" q
 :ann "margin-left: ...")

;; css-mode/mar.mar
(mar.mar "margin: " p ";" q
 :ann "margin: ...")

;; css-mode/mar.margin
(mar.margin "margin: " (p "top") " " (p "right") " " (p "bottom") " " (p "left") ";" q
 :ann "margin top right bottom left")

;; css-mode/mar.right
(mar.right "margin-right: " p ";" q
 :ann "margin-right: ...")

;; css-mode/mar.top
(mar.top "margin-top: " p ";" q
 :ann "margin-top: ...")

;; css-mode/pad.bottom
(pad.bottom "padding-bottom: " p ";" q
 :ann "padding-bottom: ...")

;; css-mode/pad.left
(pad.left "padding-left: " p ";" q
 :ann "padding-left: ...")

;; css-mode/pad.pad
(pad.pad "padding: " p ";" q
 :ann "padding: ...")

;; css-mode/pad.padding
(pad.padding "padding: " (p "top") " " (p "right") " " (p "bottom") " " (p "left") ";" q
 :ann "padding: top right bottom left")

;; css-mode/pad.right
(pad.right "padding-right: " p ";" q
 :ann "padding-right: ...")

;; css-mode/pad.top
(pad.top "padding-top: " p ";" q
 :ann "padding-top: ...")

;; css-mode/v
(-v (s f1) ": " (s f2) ";" n
 "-webkit-" f1 ": " f2 ";" > n
 "-moz-" f1 ": " f2 ";" > n
 "-ms-" f1 ": " f2 ";" > n
 "-o-" f1 ": " f2 ";" q >
 :ann "-vendor-prefix")


d-mode

;; d-mode/class
(class "/++" n
 " + " f1 > n
 " +/" > n
 "class " (p "ClassName" f1) > n
 "{" > n
 "	this(" p ")" > n
 "	{" > n
 "      " > q n
 "	}" > n
 "}" > n
 
 :ann "class … { … }")

;; d-mode/debug
(debug "debug" n
 "{" > n
 "	" > q n
 "}" > n
 )

;; d-mode/debugm
(debugm "debugm (" (p "module") ")" n
 "{" > n
 "	" > q n
 "}" >)

;; d-mode/enum
(enum "enum " (p "name") n
 "{" > n
 "	" > q n
 "}" >
 :ann "enum … { … }")

;; d-mode/fe
(fe "foreach (" (p "e") " ; " p ")" n
 "{" > n
 "	" > q n
 "}" > n
 
 :ann "foreach (e ; …) { … }")

;; d-mode/fer
(fer "foreach_reverse (" (p "e") " ; " p ")" n
 "{" > n
 "	" > q n
 "}" > n
 
 :ann "foreach_reverse (e ; …) { … }")

;; d-mode/if
(if "if (" p ")" n
 "{" > n
 "  " > p n
 "}" > n
 q
 :ann "if …")

;; d-mode/ife
(ife "if (" p ")" n
 "{" > n
 "  " > p n
 "}" > n
 "else" > n
 "{" > n
 "	" > p n
 "}" > n
 q
 :ann "if … else")

;; d-mode/im
(import "import " (p "module") ";" n
 q
 :ann "import …")

;; d-mode/main
(main "int main(string[] args)" n
 "{" > n
 "	" > q n
 "}" > n
 
 :ann "int main(string[] args) { … }")

;; d-mode/me
(method "/++" n
 " + " f2 > n
 " +/" > n>
 (p "void") " " (p "method" f2) "(" (p "args") ")" n
 "{" > n
 "	" > q n
 "}" > n
 
 :ann "method … { … }")

;; d-mode/r
(return "return " q ";" n
 
 :ann "return …")

;; d-mode/struct
(struct "/++" n
 " + " f1 > n
 " +/" > n
 "struct " (p "StructName" f1) > n
 "{" > n
 "	" > q n
 "}" > n
 
 :ann "struct … { … }")

;; d-mode/tc
;; WARNING: $0 is not the last field; kept as a plain field
(tc "try" n
 "{" > n
 "  " > p n
 "}" > n
 "catch (" (p "Exception") " e)" > n
 "{" > n
 "  " > p n
 "}" > n
 
 :ann "try … catch …")

;; d-mode/tcf
;; WARNING: $0 is not the last field; kept as a plain field
(tcf "try" n
 "{" > n
 "  " > p n
 "}" > n
 "catch (" (p "Exception") " e)" > n
 "{" > n
 "  " > p n
 "}" > n
 "finally" > n
 "{" > n
 "  " > p n
 "}" > n
 
 :ann "try … catch … finally")

;; d-mode/tf
;; WARNING: $0 is not the last field; kept as a plain field
(tryf "try" n
 "{" > n
 "  " > p n
 "}" > n
 "finally" > n
 "{" > n
 "  " > p n
 "}" > n
 
 :ann "try … finally …")

;; d-mode/unit
(unittest "@(\"" (p "test name") "\") unittest" n
 "{" > n
 "	" > q n
 "}" >
 :ann "unittest { … }")

;; d-mode/version
(version "version (" (p (completing-read "Choose: " (flatten-tree (list "unittest" "DigitalMars" "GNU" "LDC" "SDC" "Windows" "linux" "OSX" "FreeBSD" "OpenBSD" "NetBSD" "DragonFlyBSD" "BSD" "Posix" "Android" "X64" "X86_64" "ARM" "PPC" "LittleEndian" "BigEndian" "D_BetterC" "D_Coverage" "D_Ddoc" "D_InlineAsm_X86" "D_InlineAsm_X86_64" "D_LP64" "D_X32" "D_HardFloat" "D_SoftFloat" "D_PIC" "D_SIMD" "D_AVX" "D_AVX2" "D_Version2" "D_NoBoundsChecks" "D_ObjectiveC" "Core" "Std" "assert" "none" "all")))) ")" n
 "{" > n
 "  " > q n
 "}" > n
 
 :ann "version (ident) { … }")

;; d-mode/while
(while "while (" (p "condition") ")" n
 "{" > n
 "  " > q n
 "}" >
 :ann "while (…) { … }")


dart-mode

;; dart-mode/aclass
(acls "abstract class " (p "Name") " {" n
 "  " > q n
 "}" >
 :ann "aclass")

;; dart-mode/builtvalue
(blt "abstract class " (p "Name" f1) " implements Built<" f1 ", " f1 "Builder> {" n
 "  factory " f1 "([void Function(" f1 "Builder) updates]) = _$" f1 ";" > n
 "  " > f1 "._();" n
 n
 "  " > q n
 "}" >
 :ann "built_value")

;; dart-mode/class
(cls "class " (p "Name") " {" n
 "  " > q n
 "}" >
 :ann "class")

;; dart-mode/didchangedependencies
(dcd "@override" n
 "void didChangeDependencies() {" > n
 "  super.didChangeDependencies();" > n
 "  " > q n
 "}" >
 :ann "didChangeDependencies")

;; dart-mode/dispose
(dis "@override" n
 "void dispose() {" > n
 "  super.dispose();" > n
 "  " > q n
 "}" >
 :ann "dispose")

;; dart-mode/ext
(ext "extends " (p "Name"))

;; dart-mode/for
(for (p (current-column) col noinsert) "for(var i = " (p "0") "; i " (p "< 10") "; i" (p "++") ") {" n
 (ignore (indent-to col)) "  " q n
 (ignore (indent-to col)) "}")

;; dart-mode/fori
(fori (p (current-column) col noinsert) "for(var " (p "obj") " in " (p "collection") ") {" n
 (ignore (indent-to col)) "  " q n
 (ignore (indent-to col)) "}")

;; dart-mode/func
(fun (p "Type") " " (p "Name") "(" p ") {" n
 "  " > q n
 "}" >)

;; dart-mode/funca
(afun "Future<" (p "Type") "> " (p "Name") "(" p ") async {" n
 "  " > q n
 "}" >
 :ann "funca")

;; dart-mode/getset
(getset (p "Type" f1) " _" (p "Name" f2) ";" n>
 f1 " get " f2 " => _" f2 ";" n
 "set " f2 "(" f1 " " f2 ") => _" f2 " = " f2 ";" q >)

;; dart-mode/getter
(get (p "Type" f1) " _" (p "Name" f2) ";" n>
 f1 " get " f2 " => _" f2 ";" q
 :ann "getter")

;; dart-mode/impl
(impl "implements " (p "Name"))

;; dart-mode/import
(imp "import '" (p "Library") "';" q
 :ann "import")

;; dart-mode/initstate
(is "@override" n
 "void initState() {" > n
 "  super.initState();" > n
 "  " > q n
 "}" >
 :ann "initState")

;; dart-mode/main
(main "main(List<String> args) {" n
 "  " > q n
 "}" >)

;; dart-mode/part
(part "part of " (p "Part"))

;; dart-mode/setter
(set (p "Type" f1) " _" (p "Name" f2) ";" n
 "set " f2 "(" f1 " " f2 ") => _" f2 " = " f2 ";" q >
 :ann "setter")

;; dart-mode/statefulwidget
(sfw "class " (p "Name" f1) " extends StatefulWidget {" n
 "  @override" > n
 "  " > f1 "State createState() => " f1 "State();" n
 "}" > n
 n
 "class " f1 "State extends State<" f1 "> {" > n
 "  @override" > n
 "  Widget build(BuildContext context) {" > n
 "    return Container(" q ");" > n
 "  }" > n
 "}" >
 :ann "StatefulWidget")

;; dart-mode/statelesswidget
(slw "class " (p "Name") " extends StatelessWidget {" n
 "  @override" > n
 "  Widget build(BuildContext context) {" > n
 "    return Container(" q ");" > n
 "  }" > n
 "}" >
 :ann "StatelessWidget")


dix-mode :when (progn (backward-char 1) (dix-transfer-enclosing-allows "call-macro"))

;; dix-mode/call-macro
(m "<call-macro n=\"" p "\"><with-param pos=\"" p "\"/>" q "</call-macro>" n
 
 :ann "<call-macro>")


dix-mode :when (progn (backward-char 1) (dix-transfer-enclosing-allows "choose"))

;; dix-mode/choose
(c "<choose><when>" n
 "  <test><equal>" q "</equal></test>" > n
 n
 "</when></choose>" > n
 
 :ann "<choose><when><test> elements")


dix-mode :when (progn (backward-char 1) (dix-transfer-enclosing-allows "clip"))

;; dix-mode/clip
;; WARNING: Duplicate key ‘c’ renamed to ‘c-2’
(c-2 "<clip pos=\"" p "\" " (p "side=\"tl\"") " part=\"" p "\"/>" q
 :ann "<clip> elements")


dix-mode :when (dix-enclosing-is-mono-section)

;; dix-mode/e-in-mono-section
;; WARNING: Field transformation of field 3 is only evaluated on expansion
(<e "<e lm=\"" `(p ,(format "%s" (or (dix-yas-prev-lemma) "")) f1) "\"> <i>" (or (dlet ((yas-text f1)) (dix-yas-lm-to-i)) f1) "</i> <par n=\"" `(p ,(format "%s" (or (dlet ((yas-text "")) (dix-yas-choose-pdname)) ""))) "\"/></e>" n
 q
 :ann "<e> in monodix section")


dix-mode :when (not (dix-enclosing-is-mono-section))

;; dix-mode/e-in-pardef
;; WARNING: Duplicate key ‘<e’ renamed to ‘<e-2’
;; WARNING: A field directly follows a mirror; it may move when the mirror changes
(<e-2 "<e> <p><l>" (s f1) "</l>  <r>" f1 q "</r></p> </e>" n
 
 :ann "<e> in pardefs")


dix-mode :when (progn (backward-char 1) (dix-transfer-enclosing-allows "let"))

;; dix-mode/let
(l "<let>" q "</let>"
 :ann "<let> elements")


dix-mode :when (progn (backward-char 1) (dix-transfer-enclosing-allows "lit"))

;; dix-mode/lit
;; WARNING: Duplicate key ‘l’ renamed to ‘l-2’
(l-2 "<lit v=\"" p "\"/>" q
 :ann "<lit> elements")


dix-mode :when (progn (backward-char 1) (dix-transfer-enclosing-allows "lit-tag"))

;; dix-mode/lit-tag
(lt "<lit-tag v=\"" p "\"/>" q
 :ann "<lit-tag> elements")


dix-mode :when (progn (backward-char 1) (dix-transfer-enclosing-allows "otherwise"))

;; dix-mode/otherwise
(o "<otherwise>" q "</otherwise>"
 :ann "<otherwise> elements")


dix-mode :when (progn (backward-char 1) (equal (dix-enclosing-elt 'noerror) "e"))

;; dix-mode/p
;; WARNING: A field directly follows a mirror; it may move when the mirror changes
(p "<p><l>" (s f1) "</l> <r>" f1 q "</r></p>"
 :ann "<p> element")


dix-mode :when (progn (backward-char 2) (equal (dix-enclosing-elt 'noerror) "e"))

;; dix-mode/par
;; WARNING: Field transformation of field 1 is only evaluated on expansion
(<p "<par n=\"" `(p ,(format "%s" (or (dlet ((yas-text ":`")) (dix-yas-message-pardef (completing-read "Choose: " (flatten-tree (list (dix-pardef-suggest-for (dix-lemma-at-point))))))) ""))) "\"/>" q
 :ann "<par> element")


dix-mode :when (progn (backward-char 2) (not (equal (dix-enclosing-elt 'noerror) "e")))

;; dix-mode/pardef
;; WARNING: Duplicate key ‘<p’ renamed to ‘<p-2’
;; WARNING: A field directly follows a mirror; it may move when the mirror changes
(<p-2 "<pardef n=\"" (p "corp/us__n" f1) "\">" n
 "  <e>       <p><l>" (or (dix-yas-pdname-to-suffix f1) f1) "</l>      <r>" (or (dix-yas-pdname-to-suffix f1) f1) (or (dix-yas-pdname-to-pos f1) f1) (s f2) "</r></p></e>" > n
 "  <e>       <p><l>" (or (dix-yas-pdname-to-suffix f1) f1) q "</l>      <r>" (or (dix-yas-pdname-to-suffix f1) f1) (or (dix-yas-pdname-to-pos f1) f1) f2 "</r></p></e>" > n
 "  <e>       <p><l>" (or (dix-yas-pdname-to-suffix f1) f1) "</l>      <r>" (or (dix-yas-pdname-to-suffix f1) f1) (or (dix-yas-pdname-to-pos f1) f1) f2 "</r></p></e>" > n
 "  <e>       <p><l>" (or (dix-yas-pdname-to-suffix f1) f1) "</l>      <r>" (or (dix-yas-pdname-to-suffix f1) f1) (or (dix-yas-pdname-to-pos f1) f1) f2 "</r></p></e>" > n
 "</pardef>" >
 :ann "<pardef> element")


dix-mode :when (progn (backward-char 2) (member (dix-enclosing-elt 'noerror) '("l" "r" "i" "g")))

;; dix-mode/s
(<s "<s n=\"" p "\"/>" q
 :ann "<s> element")


dix-mode :when (progn (backward-char 2) (equal (dix-enclosing-elt 'noerror) "sdefs"))

;; dix-mode/sdef
;; WARNING: Duplicate key ‘<s’ renamed to ‘<s-2’
(<s-2 "<sdef n=\"" p "\"  c=\"" p "\"/>" n
 q
 :ann "<sdef> element")


dix-mode :when (progn (backward-char 2) (and (not (dix-enclosing-is-mono-section)) (not (member (dix-enclosing-elt 'noerror) '("l" "r" "i" "g" "sdefs")))))

;; dix-mode/section
;; WARNING: Duplicate key ‘<s’ renamed to ‘<s-3’
(<s-3 "<section id=\"" (p "main") "\" type=\"" (p (completing-read "Choose: " (flatten-tree (list '("standard" "inconditional" "postblank" "preblank"))))) "\">" n
 q n
 "</section>" > n
 
 :ann "<section> element")


dix-mode :when (progn (backward-char 1) (dix-transfer-enclosing-allows "var"))

;; dix-mode/var
(v "<var n=\"" p "\"/>" q
 :ann "<var> elements")


dix-mode :when (progn (backward-char 1) (dix-transfer-enclosing-allows "when"))

;; dix-mode/when
(w "<when>" q "</when>"
 :ann "<when> elements")


dix-mode :when (progn (backward-char 1) (dix-transfer-enclosing-allows "with-param"))

;; dix-mode/with-param
;; WARNING: Duplicate key ‘w’ renamed to ‘w-2’
(w-2 "<with-param pos=\"" p "\"/>" q
 :ann "<with-param>")


dockerfile-mode

;; dockerfile-mode/dockerize
(dockerize (p (current-column) col noinsert) "ENV DOCKERIZE_VERSION " (p "v0.6.1") n
 n
 (ignore (indent-to col)) (list 'l (format "%s" (or (pcase (completing-read "Choose: " (flatten-tree (list "ubuntu" "alpine"))) ("ubuntu" (concat "RUN wget https://github.com/jwilder/dockerize/releases/download/${DOCKERIZE_VERSION}/dockerize-linux-amd64-${DOCKERIZE_VERSION}.tar.gz && \\\n" "    tar -C /usr/local/bin -xzvf dockerize-linux-amd64-${DOCKERIZE_VERSION}.tar.gz && \\\n" "    rm dockerize-linux-amd64-${DOCKERIZE_VERSION}.tar.gz")) ("alpine" (concat "RUN apk add --no-cache openssl && \\\n" "    wget https://github.com/jwilder/dockerize/releases/download/${DOCKERIZE_VERSION}/dockerize-alpine-linux-amd64-${DOCKERIZE_VERSION}.tar.gz && \\\n" "    tar -C /usr/local/bin -xzvf dockerize-alpine-linux-amd64-${DOCKERIZE_VERSION}.tar.gz && \\\n" "    rm dockerize-alpine-linux-amd64-${DOCKERIZE_VERSION}.tar.gz"))) ""))))


easycrypt-mode

;; easycrypt-mode/lookup/locate
(L "locate " p "." n
 q
 :ann "locate")

;; easycrypt-mode/lookup/print
(P "print " p "." n
 q
 :ann "print")

;; easycrypt-mode/lookup/search
(S "search " p "." q
 :ann "search")

;; easycrypt-mode/misc/abbrev
(abbrev "abbrev " p " = " p "." q)

;; easycrypt-mode/misc/and1
(and1 p " /\\ " p)

;; easycrypt-mode/misc/and2
(and2 p " && " p)

;; easycrypt-mode/misc/comment
(comment "(* " p " *)" q)

;; easycrypt-mode/misc/forall
(forall "forall (" p ": " p "), " p)

;; easycrypt-mode/misc/from
(from "from " p " require import " p "." n
 q)

;; easycrypt-mode/misc/from-jasmin
(from-jasmin "from Jasmin require import " p "." n
 q)

;; easycrypt-mode/misc/fun
(fun "fun (" p ") => " p)

;; easycrypt-mode/misc/ge0
(ge0 "0 <= " p)

;; easycrypt-mode/misc/geq0
;; WARNING: Duplicate key ‘ge0’ renamed to ‘ge0-2’
(ge0-2 "0 <= " p)

;; easycrypt-mode/misc/hint-simplify
(hint "hint simplify " p "." q
 :ann "hint-simplify")

;; easycrypt-mode/misc/if
(if "if " p " then " p " else " p)

;; easycrypt-mode/misc/import
(import "import " p "." n
 q)

;; easycrypt-mode/misc/let-in
(let-in "let " p " in " p)

;; easycrypt-mode/misc/list
(list "[" p " ; " p "]" q)

;; easycrypt-mode/misc/new-comment
(new-comment "(* " p " *)" q)

;; easycrypt-mode/misc/or1
(or1 p " \\/ " p)

;; easycrypt-mode/misc/or2
(or2 p " || " p)

;; easycrypt-mode/misc/param
(param "(" p " : " p ")" q)

;; easycrypt-mode/misc/pred
(pred "pred " p " = " p "." n
 q)

;; easycrypt-mode/misc/probability
(probability "Pr[" p "]" q)

;; easycrypt-mode/misc/probability-mem
(probability-memory "Pr[" p " @ &" p " : " p "]" q)

;; easycrypt-mode/misc/range
(range p " <= " p " <= " p)

;; easycrypt-mode/misc/range-exclusive
(range-exclusive p " < " p " < " p)

;; easycrypt-mode/misc/range-exclusive-inclusive
(range-exclusive-inclusive p " < " p " <= " p)

;; easycrypt-mode/misc/range-inclusive
(range-inclusive p " <= " p " <= " p)

;; easycrypt-mode/misc/range-inclusive-exclusive
(range-inclusive-exclusive p " <= " p " < " p)

;; easycrypt-mode/misc/require-import
(require-import "require import " p "." n
 q)

;; easycrypt-mode/modules/module
(module (p (current-column) col noinsert) "module " p " = {" n
 n
 (ignore (indent-to col)) "}." q)

;; easycrypt-mode/modules/module-instance
(module-instance "module " p " = " p "(" p ")." q)

;; easycrypt-mode/modules/module-type
(module-type (p (current-column) col noinsert) "module type " p " = {" n
 n
 (ignore (indent-to col)) "}." q)

;; easycrypt-mode/modules/module-with-params
(module-with-params "module " p " (" p ": " p ") : " p " {" n
 n
 "}." q >)

;; easycrypt-mode/modules/module-with-params-return
(module-with-params-return (p (current-column) col noinsert) "module " p " (" p ": " p ") : " p " {" n
 n
 (ignore (indent-to col)) "}." q)

;; easycrypt-mode/modules/module-with-return
(module-with-return (p (current-column) col noinsert) "module " p " : " p " = {" n
 n
 (ignore (indent-to col)) "}." q)

;; easycrypt-mode/procedures/assign
(assign p " <- " p ";" q)

;; easycrypt-mode/procedures/decrement-counter
(decrement-counter (s f1) " <- " f1 " - 1;" q)

;; easycrypt-mode/procedures/if-else-proc
(if-else-proc (p (current-column) col noinsert) "if (" p "){" n
 n
 (ignore (indent-to col)) "    } else {" n
 n
 (ignore (indent-to col)) "}" q)

;; easycrypt-mode/procedures/if-proc
;; WARNING: Duplicate key ‘if’ renamed to ‘if-2’
(if-2 (p (current-column) col noinsert) "if (" p ") {" n
 n
 (ignore (indent-to col)) "}" q)

;; easycrypt-mode/procedures/increment-counter
(increment-counter (s f1) " <- " f1 " + 1;" q)

;; easycrypt-mode/procedures/init-var
(init-var p " <- witness;" q)

;; easycrypt-mode/procedures/new-var
(new-var "var " p " : " p ";" q)

;; easycrypt-mode/procedures/proc
(proc (p (current-column) col noinsert) "proc " p "(" p " : " p ") : " p " = {" n
 n
 (ignore (indent-to col)) "    return " p ";" n
 (ignore (indent-to col)) "}" q)

;; easycrypt-mode/procedures/proc-abstract
(proc-abstract "proc " p "(" p ": " p ") : " p)

;; easycrypt-mode/procedures/proc-call
(proc-call p " <@ " p ";" q)

;; easycrypt-mode/procedures/proc-no-return
(proc-no-return (p (current-column) col noinsert) "proc " p " (" p ": " p ") = {" n
 n
 (ignore (indent-to col)) "}" q)

;; easycrypt-mode/procedures/sample
(sample p " <$ " p ";" q)

;; easycrypt-mode/procedures/while-proc
(while-proc (p (current-column) col noinsert) "while (" p ")" n
 (ignore (indent-to col)) "{" n
 n
 (ignore (indent-to col)) "}" q)

;; easycrypt-mode/propositions/axiom
(axiom "axiom " p " : " p "." n
 q)

;; easycrypt-mode/propositions/axiomatized
(axiomatized "axiomatized by " p "." q)

;; easycrypt-mode/propositions/declare
(declare "declare " p n
 q)

;; easycrypt-mode/propositions/equiv
(equiv (p (current-column) col noinsert) "equiv " p " : " p " ~ " p " :" n
 (ignore (indent-to col)) "    " p n
 (ignore (indent-to col)) "    ==>" n
 (ignore (indent-to col)) "    " p " ." n
 (ignore (indent-to col)) "proof." n
 n
 (ignore (indent-to col)) "qed." n
 q)

;; easycrypt-mode/propositions/hoare
(hoare (p (current-column) col noinsert) "hoare " p " : " p " :" n
 (ignore (indent-to col)) "      " p n
 (ignore (indent-to col)) "      ==>" n
 (ignore (indent-to col)) "      " p "." n
 (ignore (indent-to col)) "proof." n
 n
 (ignore (indent-to col)) "qed." n
 q)

;; easycrypt-mode/propositions/lemma
(lemma (p (current-column) col noinsert) "lemma " p " :" n
 (ignore (indent-to col)) "  " p "." n
 (ignore (indent-to col)) "proof." n
 n
 (ignore (indent-to col)) "qed." n
 q)

;; easycrypt-mode/propositions/lemma_equality
(lemma-equality (p (current-column) col noinsert) "lemma  " p " :" n
 (ignore (indent-to col)) "  (" p ") = (" p ")." n
 (ignore (indent-to col)) "proof." n
 p n
 (ignore (indent-to col)) "qed." q)

;; easycrypt-mode/propositions/lemma_equiv
(lemma-equiv (p (current-column) col noinsert) "lemma " p " :" n
 (ignore (indent-to col)) "  equiv [" n
 (ignore (indent-to col)) "      " p " ~ " p n
 (ignore (indent-to col)) "      " p n
 (ignore (indent-to col)) "      ==>" n
 (ignore (indent-to col)) "      " p n
 (ignore (indent-to col)) "  ]." n
 (ignore (indent-to col)) "proof." n
 n
 (ignore (indent-to col)) "qed." n
 q)

;; easycrypt-mode/propositions/lemma_hoare
(lemma-hoare (p (current-column) col noinsert) "lemma " p " :" n
 (ignore (indent-to col)) "  hoare [" n
 (ignore (indent-to col)) "      " p n
 (ignore (indent-to col)) "      ==>" n
 (ignore (indent-to col)) "      " p n
 (ignore (indent-to col)) "  ]." n
 (ignore (indent-to col)) "proof." n
 n
 (ignore (indent-to col)) "qed." n
 q)

;; easycrypt-mode/propositions/lemma_islossless
(lemma-islossless (p (current-column) col noinsert) "lemma " p " : islossless" n
 (ignore (indent-to col)) "  " p "." n
 (ignore (indent-to col)) "proof." n
 n
 (ignore (indent-to col)) "qed." n
 q)

;; easycrypt-mode/propositions/lemma_phoare
(lemma-phoare (p (current-column) col noinsert) "lemma " p " :" n
 (ignore (indent-to col)) "  phoare [" n
 (ignore (indent-to col)) "      " p n
 (ignore (indent-to col)) "      ==>" n
 (ignore (indent-to col)) "      " p n
 (ignore (indent-to col)) "  ] = 1%r." n
 (ignore (indent-to col)) "proof." n
 n
 (ignore (indent-to col)) "qed." n
 q)

;; easycrypt-mode/propositions/local
(local "local " p)

;; easycrypt-mode/propositions/op-as
(op-as "op " p " : {" p " | " p "} as " p "." q)

;; easycrypt-mode/propositions/op-axiomatixed
(op-axiomatized "op " p " = " p " axiomatized by " p "." q)

;; easycrypt-mode/propositions/op-barebones
(op-barebones "op " p " = " p "." q)

;; easycrypt-mode/propositions/op-no-return
(op-no-return "op " p " (" p ": " p ") = " p "." q)

;; easycrypt-mode/propositions/phoare
(phoare (p (current-column) col noinsert) "phoare :" n
 (ignore (indent-to col)) "  [   " p n
 (ignore (indent-to col)) "      ==>" n
 (ignore (indent-to col)) "      " p n
 (ignore (indent-to col)) "  ] = 1%r." n
 (ignore (indent-to col)) "proof." n
 n
 (ignore (indent-to col)) "qed." n
 q)

;; easycrypt-mode/propositions/small-lemma
(small-lemma "lemma " p " : " p " by " p "." q)

;; easycrypt-mode/tactics/alias
(alias "alias " p " " p " = " p)

;; easycrypt-mode/tactics/alias-with
(alias-with "alias " p " with " p)

;; easycrypt-mode/tactics/alias1
(alias1 "alias{1} " p " " p " = " p)

;; easycrypt-mode/tactics/alias1-with
(alias-with1 "alias{1} " p " with " p)

;; easycrypt-mode/tactics/alias2
(alias2 "alias{2} " p " " p " = " p)

;; easycrypt-mode/tactics/alias2-with
(alias-with2 "alias{2} " p " with " p)

;; easycrypt-mode/tactics/aliaswith
(aliaswith "alias " p " with " p)

;; easycrypt-mode/tactics/aliaswith1
(aliaswith1 "alias{1} " p " with " p)

;; easycrypt-mode/tactics/aliaswith2
(aliaswith2 "alias{2} " p " with " p)

;; easycrypt-mode/tactics/apply-args
(apply-args "apply (" p " " p ")." q)

;; easycrypt-mode/tactics/apply-in
(apply-in "apply " p " in " p)

;; easycrypt-mode/tactics/apply-proof-term
(apply-proof-term "apply /" p)

;; easycrypt-mode/tactics/async-while
(async-while (p (current-column) col noinsert) "async while" n
 (ignore (indent-to col)) "     [ (fun r => " p "), (" (s f2) ") ]" n
 (ignore (indent-to col)) "     [ (fun r => " p "), (" f2 ") ]" n
 (ignore (indent-to col)) "        (" p ") (" p ")" n
 (ignore (indent-to col)) "     :" n
 (ignore (indent-to col)) "     (" p ")." q)

;; easycrypt-mode/tactics/auto
(auto "auto => />." n
 )

;; easycrypt-mode/tactics/auto.yasnippet
;; WARNING: Duplicate key ‘auto’ renamed to ‘auto-2’
(auto-2 "auto => />." n
 )

;; easycrypt-mode/tactics/bad-smt
(bad-smt "smt.")

;; easycrypt-mode/tactics/byequiv
(byequiv "byequiv (_ : " p " ==> " p ")" q)

;; easycrypt-mode/tactics/byphoare
(nyphoare "byphoare (_ : " p " ==> " p ")" q
 :ann "byphoare")

;; easycrypt-mode/tactics/bypr
(bypr "bypr (" p "){1} (" p "){2}" q)

;; easycrypt-mode/tactics/call
(call "call (_ : " p " ==> " p ")" q)

;; easycrypt-mode/tactics/call-invar
(call-invar "call (_: " p ")" q)

;; easycrypt-mode/tactics/call-invar2
(call-invar2 "call (_: " p ", " p ")" q)

;; easycrypt-mode/tactics/call-invar3
(call-invar3 "call (_: " p ", " p ", " p ")" q)

;; easycrypt-mode/tactics/call-true
(call-true "call (_: true)")

;; easycrypt-mode/tactics/call-true-true
(call-true-true "call(_: true ==> true).")

;; easycrypt-mode/tactics/call1
(call1 "call{1} (_ : " p " ==> " p ")" q)

;; easycrypt-mode/tactics/call1-invar
(call1-invar "call{1} (" p ")" q)

;; easycrypt-mode/tactics/call1-true
(call1-true "call{1} (_ : true)")

;; easycrypt-mode/tactics/call1-true-true
(call1-true-true "call{1} (_ : true ==> true)")

;; easycrypt-mode/tactics/call2
(call2 "call{2} (_ : " p " ==> " p ")" q)

;; easycrypt-mode/tactics/call2-invar
(call2-invar "call{2} (" p ")" q)

;; easycrypt-mode/tactics/call2-true
(call2-true "call{2} (_ : true)")

;; easycrypt-mode/tactics/call2-true-true
(call2-true-true "call{2} (_ : true ==> true)")

;; easycrypt-mode/tactics/case
(case "case: (" p "{1})" q)

;; easycrypt-mode/tactics/case-move
(case-move "case: (" p "{1}) => " p)

;; easycrypt-mode/tactics/cfold
(cfold "cfold " p)

;; easycrypt-mode/tactics/cfold-from-to
(cfold-from-to "cfold " p " ! " p)

;; easycrypt-mode/tactics/cfold-from-to1
(cfold-from-to1 "cfold{1} " p " ! " p)

;; easycrypt-mode/tactics/cfold-from-to2
(cfold-from-to2 "cfold{2} " p " ! " p)

;; easycrypt-mode/tactics/cfold1
(cfold1 "cfold{1} " p)

;; easycrypt-mode/tactics/cfold2
(cfold2 "cfold{2} " p)

;; easycrypt-mode/tactics/cfold_from_to
(cfold_from_to "cfold " p " ! " p)

;; easycrypt-mode/tactics/cfold_from_to1
(cfold_from_to1 "cfold{1} " p " ! " p)

;; easycrypt-mode/tactics/cfold_from_to2
(cfold_from_to2 "cfold{2} " p " ! " p)

;; easycrypt-mode/tactics/clear
(clear "clear " p "." q)

;; easycrypt-mode/tactics/conseq
(conseq "conseq (_ : " p " ==> " p ")" q)

;; easycrypt-mode/tactics/conseq-invar
(conseq-invar "conseq (_ : " p ")" q)

;; easycrypt-mode/tactics/conseq-true
(conseq-true "conseq (_: _ ==> true)")

;; easycrypt-mode/tactics/conseq-true-true
(conseq-true-true "conseq (_: true ==> true)")

;; easycrypt-mode/tactics/conseq1
(conseq1 "conseq{1} (_ : " p " ==> " p ")" q)

;; easycrypt-mode/tactics/conseq1-invar
(conseq1-invar "conseq{1} (" p ")" q)

;; easycrypt-mode/tactics/conseq1-true
(conseq1-true "conseq{1} (_ : true)")

;; easycrypt-mode/tactics/conseq1-true-true
(conseq1-true-true "conseq{1} (_ : true ==> true)")

;; easycrypt-mode/tactics/conseq2
(conseq2 "conseq{2} (_ : " p " ==> " p ")" q)

;; easycrypt-mode/tactics/conseq2-invar
(conseq2-invar "conseq{2} (" p ")" q)

;; easycrypt-mode/tactics/conseq2-true
(conseq2-true "conseq{2} (_ : true)")

;; easycrypt-mode/tactics/conseq2-true-true
(conseq2-true-true "conseq{2} (_ : true ==> true)")

;; easycrypt-mode/tactics/do-n-times
(do-n-times "do " p "! (" p ")." q)

;; easycrypt-mode/tactics/do-split
(dosplit (p (current-column) col noinsert) "do split." n
 (ignore (indent-to col)) "    + " p)

;; easycrypt-mode/tactics/dosplit
;; WARNING: Duplicate key ‘dosplit’ renamed to ‘dosplit-2’
(dosplit-2 "do split." n
 "    + " p >)

;; easycrypt-mode/tactics/ecall
(ecall "ecall (" p ")." q)

;; easycrypt-mode/tactics/ecall-wp
(ecall-wp "ecall (" p "); wp." q)

;; easycrypt-mode/tactics/elim-proof-term
(elim-proof-term "elim /" p)

;; easycrypt-mode/tactics/elim-var
(elim-var "elim: " p)

;; easycrypt-mode/tactics/exists
(exists "exists (" p ")" q)

;; easycrypt-mode/tactics/exists-astreix1
(exists-astreix1 "exists* (" p "){1}, (" p ")" q)

;; easycrypt-mode/tactics/exists-astrix
(exists-astreix "exists* (" p ")" q
 :ann "exists-astrix")

;; easycrypt-mode/tactics/first-last
(first-last p "; first last" q)

;; easycrypt-mode/tactics/first-n-last
(## p "; first " p " last" q
 :ann "first-n-last")

;; easycrypt-mode/tactics/firstn
(firstn p "; first " p " " p)

;; easycrypt-mode/tactics/fission
(fission "fission " p "!" p " @ " p ", " p)

;; easycrypt-mode/tactics/fission1
(fission1 "fission{1} " p "!" p " @ " p ", " p)

;; easycrypt-mode/tactics/fission2
(fission2 "fission{2} " p "!" p " @ " p ", " p)

;; easycrypt-mode/tactics/fusion
(fusion "fusion " p "!" p " @ " p ", " p)

;; easycrypt-mode/tactics/fusion1
(fusion1 "fusion{1} " p "!" p " @ " p ", " p)

;; easycrypt-mode/tactics/fusion2
(fusion2 "fusion{2} " p "!" p " @ " p ", " p)

;; easycrypt-mode/tactics/have-define
(have-define "have := " p "." q)

;; easycrypt-mode/tactics/have-introduce
(have-introduce "have " p ": " p "." q)

;; easycrypt-mode/tactics/have-write-left
(have-write-left "have <-: " p "." q)

;; easycrypt-mode/tactics/have-write-left-eq
(have-write-left-eq "have <-: (" p ") = (" p ")." q)

;; easycrypt-mode/tactics/have-write-right
(have-write-right "have ->: " p "." q)

;; easycrypt-mode/tactics/have-write-right-eq
(have-wrtie-right-eq "have ->: (" p ") = (" p ")." q
 :ann "have-write-right-eq")

;; easycrypt-mode/tactics/if
;; WARNING: Duplicate key ‘if’ renamed to ‘if-3’
(if-3 "if (" p ") {" n
 n
 "}" q >)

;; easycrypt-mode/tactics/if1
(if1 "if{1}")

;; easycrypt-mode/tactics/if2
(if2 "if{2}")

;; easycrypt-mode/tactics/implies
(implies p " => " p)

;; easycrypt-mode/tactics/implies-brackets
(implies-brackets "(" p " => " p ")" q)

;; easycrypt-mode/tactics/inline
(inline "inline {" p "} (" p ")." q)

;; easycrypt-mode/tactics/inline1
(inline1 "inline{1} (" p ")." q)

;; easycrypt-mode/tactics/inline2
(inline2 "inline{2} (" p ")." q)

;; easycrypt-mode/tactics/last
(last p "; last " p)

;; easycrypt-mode/tactics/last-first
(last-first p "; last first" q)

;; easycrypt-mode/tactics/last-n-first
(last-n-first p "; last " p " first" q)

;; easycrypt-mode/tactics/lastn
(lastn "τ1; last " p " " p)

;; easycrypt-mode/tactics/move-1
(move-1 "move => &1 " p)

;; easycrypt-mode/tactics/move-1m2
(move-1m2 "move => &1 &m &2 " p)

;; easycrypt-mode/tactics/move-2
(move-2 "move => &2 " p)

;; easycrypt-mode/tactics/move-from-assumption
(move-from-assumption "move : " p "." q)

;; easycrypt-mode/tactics/move-hr
(move-hr "move => &hr " p)

;; easycrypt-mode/tactics/move-lemma-args
(move-lemma-args "move: (" p " " p ")" q)

;; easycrypt-mode/tactics/move-to-assumption
(move-to-assumptions "move => " p
 :ann "move-to-assumption")

;; easycrypt-mode/tactics/pose
(pose "pose " p " := " p "." q)

;; easycrypt-mode/tactics/proc-simplify
(proc-simplify "proc => //=.")

;; easycrypt-mode/tactics/proc-up-to-bad
(proc-up-to-bad "proc " p " (" p ")" q)

;; easycrypt-mode/tactics/proc-up-to-bad2
(proc-up-to-bad2 "proc " p " (" p ") (" p ")" q)

;; easycrypt-mode/tactics/rcondf1
(rcondf1 "rcondf{1} " p)

;; easycrypt-mode/tactics/rcondf2
(rcondf2 "rcondf{2} " p)

;; easycrypt-mode/tactics/rcondt-rl
(rcondt-prog "rcondt {" p "} " p
 :ann "rcondt-equiv")

;; easycrypt-mode/tactics/rcondt1
(rcondt1 "rcondt{1} " p)

;; easycrypt-mode/tactics/rcondt2
(rcondt2 "rcondt{2} " p)

;; easycrypt-mode/tactics/repeat-from-to
(repeat-from-to "[" p ".." p "]!" p)

;; easycrypt-mode/tactics/rewrite
(rewrite "rewrite " p)

;; easycrypt-mode/tactics/rewrite-crush
(rewrite-crush "rewrite " p " => />." q)

;; easycrypt-mode/tactics/rewrite-expand-list
(rewrite-expand-list "rewrite " p " !/mkseq -iotaredE => />." q)

;; easycrypt-mode/tactics/rewrite-fun-backwards
(rewrite-lemma-backwards-args "rewrite -(" p " " p ")" q)

;; easycrypt-mode/tactics/rewrite-in
(rewrite-in "rewrite " p " in " p)

;; easycrypt-mode/tactics/rewrite-lemma-forwards-args
(rewrite-lemma-forward-args "rewrite (" p " " p ")." q
 :ann "rewrite-lemma-forwards-args")

;; easycrypt-mode/tactics/rewrite-proof-term-in
(rewrite-proof-term-in "rewrite /" p " in " p)

;; easycrypt-mode/tactics/rnd-f
(rnd-f "rnd (" p ")" q)

;; easycrypt-mode/tactics/rnd-fg
(rnd-fg "rnd (" p ") (" p ")" q)

;; easycrypt-mode/tactics/rnd1
(rnd1 "rnd{1}")

;; easycrypt-mode/tactics/rnd2
(rnd2 "rnd{2}")

;; easycrypt-mode/tactics/seq
(seq "seq " p " " p " : (" p ")" q)

;; easycrypt-mode/tactics/sim
(sim "sim (" p ") / (" p ") : (" p ")" q)

;; easycrypt-mode/tactics/smt
(smt "smt().")

;; easycrypt-mode/tactics/splitwhile
(splitwhile "splitwhile " p " : (" p ")" q)

;; easycrypt-mode/tactics/splitwhile1
(splitwhile1 "splitwhile{1} " p " : (" p ")" q)

;; easycrypt-mode/tactics/splitwhile2
(splitwhile2 "splitwhile{2} " p " : (" p ")" q)

;; easycrypt-mode/tactics/transitivity
(transitivity (p (current-column) col noinsert) "transitivity " p n
 (ignore (indent-to col)) "    (" p " ==> " p ")" n
 (ignore (indent-to col)) "    (" p " ==> " p ")." q)

;; easycrypt-mode/tactics/transitivity-code
(transitivity-code (p (current-column) col noinsert) "transitivity{" p "} {" (s f2) ";}" n
 (ignore (indent-to col)) "      (" f2 " ==> " p ")" n
 (ignore (indent-to col)) "      (" p " ==> " p ")." q)

;; easycrypt-mode/tactics/unroll-equiv
(unroll-prog "unroll {" p "} " p
 :ann "unroll-equiv")

;; easycrypt-mode/tactics/unroll-while
(unroll "unroll for{" p "} ^while" q
 :ann "unroll-while")

;; easycrypt-mode/tactics/while
(while (p (current-column) col noinsert) "while(" n
 n
 (ignore (indent-to col)) ").")

;; easycrypt-mode/tactics/while-true
(while-true "while true " p)

;; easycrypt-mode/tactics/while1
(while1 "while{1} (" p ")" q)

;; easycrypt-mode/tactics/while2
;; WARNING: Duplicate key ‘’ renamed to ‘-2’
(\-2 "while{2} (" p ")" q
 :ann "while2")

;; easycrypt-mode/theories/abstract-theory
(abstract-theory (p (current-column) col noinsert) "abstract theory " (s f1) "." n
 n
 n
 (ignore (indent-to col)) "end " f1 "." q)

;; easycrypt-mode/theories/clone-as
(clone-as "clone " p " as " p "." q)

;; easycrypt-mode/theories/clone-as-with
(clone-as-with "clone " p " as " p " with" q)

;; easycrypt-mode/theories/clone-import
(clone-import "clone import " p " as " p " with" q)

;; easycrypt-mode/theories/clone-include
(clone-include "clone include " p)

;; easycrypt-mode/theories/clone-no-with
(clone-no-with "clone " p " as " p "." q)

;; easycrypt-mode/theories/rename-as
(rename-as "rename \"" p "\" as \"" p "\"" q)

;; easycrypt-mode/theories/section
(section (p (current-column) col noinsert) "section." n
 n
 (ignore (indent-to col)) "end section.")

;; easycrypt-mode/theories/theory
(theory (p (current-column) col noinsert) "theory " (s f1) "." n
 n
 n
 (ignore (indent-to col)) "end " f1 "." q)

;; easycrypt-mode/theories/type
(type "type " p " = " p "." q)

;; easycrypt-mode/theories/with
(with "with " p " => " p)


elixir-mode elixir-ts-mode

;; elixir-mode/after
(after "after " (p "500") " ->" n
 "  " > q)

;; elixir-mode/call
(call "GenServer.call(" (p "__MODULE__") ", " q ")")

;; elixir-mode/case
(case "case " p " do" n
 "  " > q n
 "end" >)

;; elixir-mode/cast
(cast "GenServer.cast(" (p "__MODULE__") ", " q ")")

;; elixir-mode/cond
(cond "cond do" n
 "  " > q n
 "end" >)

;; elixir-mode/def
;; WARNING: Nested fields flattened into field 2
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(def "def " (p "function") (p "(args)") " do" n
 "  " > q n
 "end" >)

;; elixir-mode/defmacro
(defmacro "defmacro " p " do" n
 "  " > q n
 "end" >)

;; elixir-mode/defmacrop
(defmacrop "defmacrop " p " do" n
 "  " > q n
 "end" >)

;; elixir-mode/defmodule
(defmodule "defmodule " p " do" n
 "  " > q n
 "end" >)

;; elixir-mode/defmodule_filename
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(dm "defmodule " `(p ,(concat (capitalize (file-name-nondirectory (directory-file-name (file-name-directory buffer-file-name)))) ".")) `(p ,(mapconcat 'capitalize (split-string (file-name-base) "_") "")) " do" n
 "  " > q n
 "end" >
 :ann "defmodule XXX end")

;; elixir-mode/defp
(defp "defp " p " do" n
 "  " > q n
 "end" >)

;; elixir-mode/do
(do "do" n
 "  " > q n
 "end" >)

;; elixir-mode/doc
(doc "@doc \"\"\"" n
 q n
 "\"\"\"" >)

;; elixir-mode/fn
;; WARNING: A field directly follows a mirror; it may move when the mirror changes
(fn "fn " (p "x" f1) " -> " f1 q " end")

;; elixir-mode/for
;; WARNING: Fields visited in buffer order (2 1) instead of numeric order
;; WARNING: A field directly follows a mirror; it may move when the mirror changes
(for "for " (p "x" f2) " <- " (p "enum") " do" n
 "  " > f2 q n
 "end" >)

;; elixir-mode/function
;; WARNING: Field transformation of field 3 is only evaluated on expansion
(dfun "def " p "(" p ")" `(p ,(format "%s" (or (when (and nil nil) (concat " when " "")) ""))) " do" n
 "    " > q n
 "end" >
 :ann "function")

;; elixir-mode/function-one-line
;; WARNING: Field transformation of field 3 is only evaluated on expansion
(df "def " p "(" p ")" `(p ,(format "%s" (or (when (and nil nil) (concat " when " "")) ""))) ", do: " q
 :ann "function-one-line")

;; elixir-mode/hcall
(hcall "def handle_call(" p ", _from, " (p "state") ") do" n
 "  " > q n
 "end" >)

;; elixir-mode/hcast
(hcast "def handle_cast(" p ", " (p "state") ") do" n
 "  " > q n
 "end" >)

;; elixir-mode/hinfo
(hinfo "def handle_info(" p ", " (p "state") ") do" n
 "  " > q n
 "end" >)

;; elixir-mode/if
(if "if " (p "condition") " do" n
 "  " > q n
 "end" >)

;; elixir-mode/ife
(ife "if " (p "condition") " do" n
 "  " > p n
 "else" > n
 "  " > p n
 "end" q >
 :ann "if-else")

;; elixir-mode/inspect
(i "IO.inspect(" (s f1) ", label: \"" f1 "\")" q
 :ann "Inspect value")

;; elixir-mode/io
(io "IO.puts(\"" p "\")" q)

;; elixir-mode/iop
(iop "IO.puts(\"" (s f1) " #{inspect " f1 "}\")" q)

;; elixir-mode/mdoc
(mdoc "@moduledoc \"\"\"" n
 q n
 "\"\"\"" >
 :ann "moduledoc")

;; elixir-mode/pry
(pry "require IEx; IEx.pry")

;; elixir-mode/receive
(rcv "receive do" n
 "  " > q n
 "end" >
 :ann "receive")

;; elixir-mode/test
(test "test \"" p "\" do" n
 "  " > q n
 "end" >)

;; elixir-mode/unless
(unless "unless " (p "condition") " do" n
 "  " > q n
 "end" >)

;; elixir-mode/wt
(wt "with " (p "{:ok, tuple") " <- function(arg)}, do" n
 "  " > q n
 "end" >)

;; elixir-mode/wte
(wte "with " (p "{:ok, tuple") " <- function(arg)}, do" n
 "  " > p n
 "else" > n
 "  " > q n
 "end" >)


emacs-lisp-mode

;; emacs-lisp-mode/add-hook
;; WARNING: Nested fields flattened into field 2
(ah "(add-hook '" (p "name") "-hook " (p "'function") ")" q n
 
 :ann "add-hook")

;; emacs-lisp-mode/and
(a "(and " q ")"
 :ann "and")

;; emacs-lisp-mode/aref
(aref "(aref " (p "array") " " (p "index") ")" n
 q)

;; emacs-lisp-mode/aset
(aset "(aset " (p "array") " " (p "index") " " (p "element") ")" q)

;; emacs-lisp-mode/assq
(assq "(assq " (p "key") " " (p "list") ")" q)

;; emacs-lisp-mode/autoload
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(autoload "(autoload " (p "function") " \"" (p "filename") "\"" (p " \"docstring\"") (p " interactive") (p " type") ")" q)

;; emacs-lisp-mode/backward-char
(bc "(backward-char " q ")"
 :ann "backward-char")

;; emacs-lisp-mode/beginning-of-line
(bol "(beginning-of-line)"
 :ann "beginning-of-line")

;; emacs-lisp-mode/bounds-of-thing-at-point
(botap "(bounds-of-thing-at-point " q ")"
 :ann "bounds-of-thing-at-point")

;; emacs-lisp-mode/buffer-file-name
(bfn "(buffer-file-name)"
 :ann "buffer-file-name")

;; emacs-lisp-mode/buffer-modified-p
(bmp "(buffer-modified-p " q ")"
 :ann "buffer-modified-p")

;; emacs-lisp-mode/buffer-substring
(bs "(buffer-substring " (p "start") " " (p "end") ")" q
 :ann "buffer-substring")

;; emacs-lisp-mode/buffer-substring-no-properties
(bsnp "(buffer-substring-no-properties " (p "start") " " (p "end") ")" q
 :ann "buffer-substring-no-properties")

;; emacs-lisp-mode/cond
(cond "(cond" n
 " (" (p "condition") " " (p "body") ")" q ")" >)

;; emacs-lisp-mode/condition-case
(cc "(condition-case " q ")"
 :ann "condition-case")

;; emacs-lisp-mode/const
;; WARNING: Nested fields flattened into field 3
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(const "(defconst " (p "name") " " (p "value") (p " \"docstring\"") ")" q
 :ann "defconst")

;; emacs-lisp-mode/copy-directory
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(cd "(copy-directory " (p "directory") " {2:target}" (p " keep-time") (p " parents") ")" q
 :ann "copy-directory")

;; emacs-lisp-mode/copy-file
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(cf "(copy-file " (p "filename") " " (p "newname") (p " ok-if-already-exists") (p " keep-time") "{5: preserve-uid-gid})" q
 :ann "copy-file")

;; emacs-lisp-mode/current-buffer
(cb "(current-buffer)"
 :ann "current-buffer")

;; emacs-lisp-mode/custom-autoload
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(ca "(custom-autoload " (p "symbol") " " (p "load") (p " noset") ")" q
 :ann "custom-autoload")

;; emacs-lisp-mode/defalias
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(defalias "(defalias '" (p "symbol") " '" (p "alias") (p " \"docstring\"") ")" q)

;; emacs-lisp-mode/defcustom
(defcustom "(defcustom " (p "symbol") " " (p "standard") " \"" (p "docstring") "\"" (p " args") ")" q)

;; emacs-lisp-mode/define-key
(dk "(define-key " (p "mode") "-map (kbd \"" (p "key") "\") " q ")"
 :ann "define-key")

;; emacs-lisp-mode/defun
;; WARNING: Nested fields flattened into field 4
(def "(defun " (p "fun") " (" (p "args") ")" n
 "  \"" (p "docstring") "\"" > n
 "  " > (p "(interactive \"P\")") n
 "  " > q ")"
 :ann "defun")

;; emacs-lisp-mode/defvar
(defvar "(defvar " (p "symbol") " " (p "initvalue") " \"" (p "docstring") "\")" q)

;; emacs-lisp-mode/delete-char
(dc "(delete-char " q ")"
 :ann "delete-char")

;; emacs-lisp-mode/delete-directory
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(dd "(delete-directory " (p "directory") (p " recursive") ")" n
 q
 :ann "delete-directory")

;; emacs-lisp-mode/delete-file
(df "(delete-file " q ")"
 :ann "delete-file")

;; emacs-lisp-mode/delete-region
(dr "(delete-region " q ")"
 :ann "delete-region")

;; emacs-lisp-mode/directory-files
;; WARNING: Duplicate key ‘df’ renamed to ‘df-2’
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(df-2 "(directory-files " (p "directory") (p " full") (p " match") (p " nosort") ")" q
 :ann "directory-files")

;; emacs-lisp-mode/dired.process_marked
(dired.process_marked ";; idiom for processing a list of files in dired's marked files" n
 n
 ";; suppose myProcessFile is your function that takes a file path" > n
 ";; and do some processing on the file" > n
 n
 "(defun dired-myProcessFile ()" > n
 "  \"apply myProcessFile function to marked files in dired.\"" > n
 "  (interactive)" > n
 "  (require 'dired)" > n
 "  (mapc 'myProcessFile (dired-get-marked-files))" > n
 ")" > n
 n
 ";; to use it, type M-x dired-myProcessFile" >
 :ann "process marked files in dired")

;; emacs-lisp-mode/end-of-line
(eol "(end-of-line)"
 :ann "end-of-line")

;; emacs-lisp-mode/error
(error "(error \"" (p "message") "\"" (p " format-args") ")" q)

;; emacs-lisp-mode/ert-deftest
(edt "(ert-deftest " (p "test-name") " ()" n
 "  " > q ")" n
 
 :ann "ert-deftest")

;; emacs-lisp-mode/expand-file-name
(efn "(expand-file-name " q ")"
 :ann "expand-file-name")

;; emacs-lisp-mode/fboundp
(fboundp "(fboundp '" q ")")

;; emacs-lisp-mode/file-name-directory
(fnd "(file-name-directory " q ")"
 :ann "file-name-directory")

;; emacs-lisp-mode/file-name-extension
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(fne "(file-name-extension " (p "filename") (p " period") ")" q
 :ann "file-name-extension")

;; emacs-lisp-mode/file-name-nondirectory
(fnn "(file-name-nondirectory " q ")"
 :ann "file-name-nondirectory")

;; emacs-lisp-mode/file-name-sans-extension
(fnse "(file-name-sans-extension " q ")"
 :ann "file-name-sans-extension")

;; emacs-lisp-mode/file-relative-name
(frn "(file-relative-name " q ")"
 :ann "file-relative-name")

;; emacs-lisp-mode/file.process
(file.process "(defun doThisFile (fpath)" n
 "  \"Process the file at path FPATH ...\"" > n
 "  (let ()" > n
 "    ;; create temp buffer without undo record or font lock. (more efficient)" > n
 "    ;; first space in temp buff name is necessary" > n
 "    (set-buffer (get-buffer-create \" myTemp\"))" > n
 "    (insert-file-contents fpath nil nil nil t)" > n
 n
 "    ;; process it ..." > n
 "    ;; (goto-char 0) ; move to begining of file's content (in case it was open)" > n
 "    ;; ... do something here" > n
 "    ;; (write-file fpath) ;; write back to the file" > n
 n
 "    (kill-buffer \" myTemp\")))" >
 :ann "a function that process a file")

;; emacs-lisp-mode/file.read-lines
(file.read-lines "(defun read-lines (filePath)" n
 "  \"Return a list of lines in FILEPATH.\"" > n
 "  (with-temp-buffer" > n
 "    (insert-file-contents filePath)" > n
 "    (split-string" > n
 "     (buffer-string) \"\\n\" t)))" > n
 n
 ";; process all lines" > n
 "(mapc" > n
 " (lambda (aLine)" > n
 "   (message aLine) ; do your stuff here" > n
 "   )" > n
 " (read-lines \"inputFilePath\")" > n
 ")" >
 :ann "read lines of a file")

;; emacs-lisp-mode/find-file
(ff "(find-file " q ")"
 :ann "find-file")

;; emacs-lisp-mode/format
(f "(format \"" (p "message") "\" " (p "format-args") ")" q
 :ann "format")

;; emacs-lisp-mode/forward-char
(fc "(forward-char " q ")"
 :ann "forward-char")

;; emacs-lisp-mode/forward-line
(fl "(forward-line " q ")"
 :ann "forward-line")

;; emacs-lisp-mode/get
(get "(get " (p "symbol") " {2:propname})" q)

;; emacs-lisp-mode/global-set-key
(gsk "(global-set-key (kbd \"" (p "key") "\") " q ")"
 :ann "global-set-key")

;; emacs-lisp-mode/goto-char
(gc "(goto-char " q ")"
 :ann "goto-char")

;; emacs-lisp-mode/grabstring
(grabstring "(setq " q " (buffer-substring-no-properties myStartPos myEndPos))"
 :ann "grab buffer substring")

;; emacs-lisp-mode/grabthing
(grabthing "(setq " q " (thing-at-point 'symbol))"
 :ann "grab word under cursor")

;; emacs-lisp-mode/header
;; WARNING: Multi-line default of field 9 is not re-indented
(header ";;; " (p "name" f1) ".el --- " (p "summary") " -*- lexical-binding: t -*-" n
 n
 ";; Author: " `(p ,(user-full-name)) > n
 ";; Maintainer: " `(p ,(user-full-name)) > n
 ";; Version: " (p "version") > n
 ";; Package-Requires: (" (p "dependencies") ")" > n
 ";; Homepage: " (p "homepage") > n
 ";; Keywords: " (p "keywords") > n
 n>
 (p "\n;; This file is not part of GNU Emacs\n\n;; This program is free software: you can redistribute it and/or modify\n;; it under the terms of the GNU General Public License as published by\n;; the Free Software Foundation, either version 3 of the License, or\n;; (at your option) any later version.\n\n;; This program is distributed in the hope that it will be useful,\n;; but WITHOUT ANY WARRANTY; without even the implied warranty of\n;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n;; GNU General Public License for more details.\n\n;; You should have received a copy of the GNU General Public License\n;; along with this program.  If not, see <https://www.gnu.org/licenses/>.\n") n
 n
 ";;; Commentary:" > n
 n
 ";; " (p "commentary") > n
 n
 ";;; Code:" > n
 n>
 (p "(message \"Hello World!\")") n
 n
 "(provide '" f1 ")" > n
 n
 ";;; " f1 ".el ends here" q >
 :ann "package header")

;; emacs-lisp-mode/insert
(i "(insert " q ")"
 :ann "insert")

;; emacs-lisp-mode/insert-file-contents
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(ifc "(insert-file-contents " (p "filename") (p " visit") (p " beg") (p " end") (p " replace") ")" q
 :ann "insert-file-contents")

;; emacs-lisp-mode/interactive
;; WARNING: Nested fields flattened into field 1
(interactive "(interactive" (p " \"P\"") ")" q)

;; emacs-lisp-mode/kbd
(kbd "(kbd \"" q "\")")

;; emacs-lisp-mode/kill-buffer
(kb "(kill-buffer " q ")"
 :ann "kill-buffer")

;; emacs-lisp-mode/lambda
;; WARNING: Nested fields flattened into field 2
;; WARNING: $0 directly follows a field; Tempel exits at the template end
(lam "(lambda (" p ") " (p "(interactive \"\") ") ")" n
 
 :ann "lambda")

;; emacs-lisp-mode/let
(l "(let" (p "*") " (" (p "args") ")" n
 "  " > q ")"
 :ann "let")

;; emacs-lisp-mode/line-beginning-position
(lbp "(line-beginning-position)"
 :ann "line-beginning-position")

;; emacs-lisp-mode/line-end-position
(lep "(line-end-position)"
 :ann "line-end-position")

;; emacs-lisp-mode/looking-at
(la "(looking-at " q ")"
 :ann "looking-at")

;; emacs-lisp-mode/make-directory
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(md "(make-directory " (p "directory") (p " parents") ")" q
 :ann "make-directory")

;; emacs-lisp-mode/make-local-variable
(mlv "(make-local-variable " q ")"
 :ann "make-local-variable")

;; emacs-lisp-mode/mapc
(mapc "(mapc " (p "function") " " q ")")

;; emacs-lisp-mode/match-beginning
(mb "(match-beginning " q ")"
 :ann "match-beginning")

;; emacs-lisp-mode/match-end
(me "(match-end " q ")"
 :ann "match-end")

;; emacs-lisp-mode/match-string
(ms "(match-string " q ")"
 :ann "match-string")

;; emacs-lisp-mode/memq
(memq "(memq " (p "element") " " (p "list") ")" q)

;; emacs-lisp-mode/message
(m "(message \"" (p "message") "\"" (p " format-args") ")" q
 :ann "message")

;; emacs-lisp-mode/minor_mode
(minor "(defvar " (p "mode" f1) "-modeline-indicator \" " (p "indicator") "\"" n
 "  \"call (" f1 "-install-mode) again if this is changed\")" > n
 n
 "(defvar " f1 "-mode nil)" > n
 "(make-variable-buffer-local '" f1 "-mode)" > n
 "(put '" f1 "-mode 'permanent-local t)" > n
 n
 "(defun " f1 "-mode (&optional arg)" > n
 "  \"" q "\"" > n
 "  (interactive \"P\")" > n
 "  (setq " f1 "-mode" > n
 "        (if (null arg) (not " f1 "-mode)" > n
 "          (> (prefix-numeric-value arg) 0)))" > n
 "  (force-mode-line-update))" > n
 n
 "(provide '" f1 "-mode)" >
 :ann "minor_mode")

;; emacs-lisp-mode/not
(n "(not " q ")"
 :ann "not")

;; emacs-lisp-mode/nth
(nth "(nth " (p "index") " " (p "list") ")" q)

;; emacs-lisp-mode/number-to-string
(nts "(number-to-string " q ")"
 :ann "number-to-string")

;; emacs-lisp-mode/or
(o "(or " q ")"
 :ann "or")

;; emacs-lisp-mode/point
(p "(point)"
 :ann "point")

;; emacs-lisp-mode/point-max
(point-max "(point-max)")

;; emacs-lisp-mode/point-min
(pm "(point-min)"
 :ann "point-min")

;; emacs-lisp-mode/put
(put "(put " (p "symbol") " " (p "propname") " " (p "value") ")" q)

;; emacs-lisp-mode/re-search-backward
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(rsb "(re-search-backward " (p "regexp") (p " bound") (p " noerror") (p " count") ")" q
 :ann "re-search-backward")

;; emacs-lisp-mode/re-search-forward
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(rsf "(re-search-forward " (p "regexp") (p " bound") (p " noerror") (p " count") ")" q
 :ann "re-search-forward")

;; emacs-lisp-mode/region-active-p
(rap "(region-active-p)"
 :ann "region-active-p")

;; emacs-lisp-mode/region-beginning
(rb "(region-beginning)"
 :ann "region-beginning")

;; emacs-lisp-mode/region-end
(re "(region-end)"
 :ann "region-end")

;; emacs-lisp-mode/rename-file
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(rf "(rename-file " (p "file") " " (p "newname") (p " ok-if-already-exists") ")" q
 :ann "rename-file")

;; emacs-lisp-mode/replace-regexp
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(rr "(replace-regexp " (p "regexp") (p " delimited") (p " start") (p " end") ")" q
 :ann "replace-regexp")

;; emacs-lisp-mode/replace-regexp-in-string
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(rris "(replace-regexp-in-string " (p "regexp") " " (p "rep") " " (p "string") (p " fixedcase") (p " literal") (p " subexp") (p " start") ")" q
 :ann "replace-regexp-in-string")

;; emacs-lisp-mode/save-buffer
(sb "(save-buffer " q ")"
 :ann "save-buffer")

;; emacs-lisp-mode/save-excursion
(se "(save-excursion " q ")"
 :ann "save-excursion")

;; emacs-lisp-mode/search-backward
;; WARNING: Duplicate key ‘sb’ renamed to ‘sb-2’
;; WARNING: Nested fields flattened into field 2
(sb-2 "(search-backward \"" p "\"" (p " bound noerror count") ")" q n
 
 :ann "search-backward")

;; emacs-lisp-mode/search-backward-regexp
;; WARNING: Nested fields flattened into field 2
(sbr "(search-backward-regexp \"" p "\"" (p " bound noerror count") ")" q n
 
 :ann "search-backward-regexp")

;; emacs-lisp-mode/search-forward
;; WARNING: Nested fields flattened into field 2
(sf "(search-forward \"" p "\"" (p " bound noerror count") ")" q n
 
 :ann "search-forward")

;; emacs-lisp-mode/search-forward-regexp
;; WARNING: Nested fields flattened into field 2
(sfr "(search-forward-regexp \"" p "\"" (p " bound noerror count") ")" q n
 
 :ann "search-forward-regexp")

;; emacs-lisp-mode/set-buffer
;; WARNING: Duplicate key ‘sb’ renamed to ‘sb-3’
(sb-3 "(set-buffer " q ")"
 :ann "set-buffer")

;; emacs-lisp-mode/set-file-modes
(sfm "(set-file-modes " (p "filename") " " q ")"
 :ann "set-file-modes")

;; emacs-lisp-mode/set-mark
(sm "(set-mark " q ")"
 :ann "set-mark")

;; emacs-lisp-mode/setq
(s "(setq " q ")"
 :ann "setq")

;; emacs-lisp-mode/should
(sh "(should " q ")"
 :ann "should")

;; emacs-lisp-mode/skip-chars-backward
(scb "(skip-chars-backward \"" (p "string") "\"" (p " lim") ")" q
 :ann "skip-chars-backward")

;; emacs-lisp-mode/skip-chars-forward
(scf "(skip-chars-forward \"" (p "string") "\"" (p " lim") ")" q
 :ann "skip-chars-forward")

;; emacs-lisp-mode/split-string
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(ss "(split-string " (p "string") (p " separators") (p " omit-nulls") ")" q
 :ann "split-string")

;; emacs-lisp-mode/string-match
;; WARNING: Duplicate key ‘sm’ renamed to ‘sm-2’
(sm-2 "(string-match \"" (p "regexp") "\" \"" (p "string") "\"" (p " start") ")" q
 :ann "string-match")

;; emacs-lisp-mode/string-to-number
(stn "(string-to-number " q ")"
 :ann "string-to-number")

;; emacs-lisp-mode/string=
(string= "(string= " q ")")

;; emacs-lisp-mode/substring
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(substring "(substring " (p "string") " " (p "from") (p " to") ")" q)

;; emacs-lisp-mode/thing-at-point
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(tap "(thing-at-point " (p "thing") (p " no-properties") ")" q
 :ann "thing-at-point")

;; emacs-lisp-mode/use-package
(up "(use-package " (p "package-name") n
 ":ensure t" q ")" >
 :ann "use-package")

;; emacs-lisp-mode/use-package-binding
(upb ":bind (\"" (p "binding") "\" . " (p "function-name") ")" q
 :ann "use-package binding")

;; emacs-lisp-mode/vector
(v "(vector " q ")"
 :ann "vector")

;; emacs-lisp-mode/when
(w "(when " q ")"
 :ann "when")

;; emacs-lisp-mode/widget-get
(wg "(widget-get " q ")"
 :ann "widget-get")

;; emacs-lisp-mode/with-current-buffer
(wcb "(with-current-buffer " q ")"
 :ann "with-current-buffer")

;; emacs-lisp-mode/word-or-region
(word-or-region ";; example of a command that works on current word or text selection" n
 "(defun down-case-word-or-region ()" > n
 "  \"Lower case the current word or text selection.\"" > n
 "(interactive)" > n
 "(let (pos1 pos2 meat)" > n
 "  (if (and transient-mark-mode mark-active)" > n
 "      (setq pos1 (region-beginning)" > n
 "            pos2 (region-end))" > n
 "    (setq pos1 (car (bounds-of-thing-at-point 'symbol))" > n
 "          pos2 (cdr (bounds-of-thing-at-point 'symbol))))" > n
 n
 "  ; now, pos1 and pos2 are the starting and ending positions" > n
 "  ; of the current word, or current text selection if exists" > n
 n
 "  ;; put your code here." > n
 "  " > q n
 "  ;; Some example of things you might want to do" > n
 "  (downcase-region pos1 pos2) ; example of a func that takes region as args" > n
 "  (setq meat (buffer-substring-no-properties pos1 pos2)) ; grab the text." > n
 "  (delete-region pos1 pos2) ; get rid of it" > n
 "  (insert \"newText\") ; insert your new text" > n
 n
 "  )" > n
 ")" >
 :ann "Command that works on region or word")

;; emacs-lisp-mode/word_regexp
(< "\"\\_<" (p "word") "\\_>\"" q
 :ann "word_regexp")

;; emacs-lisp-mode/yes-or-no-p
(yonp "(yes-or-no-p \"" (p "prompt") " \")" q
 :ann "yes-or-no-p")


erc-mode

;; erc-mode/blist
(b "blist" n
 
 :ann "blist")

;; erc-mode/help
(h "help " q
 :ann "help")


erlang-mode

;; erlang-mode/after
(after "after" n
 "    " > p "-> " q
 :ann "after ... ->")

;; erlang-mode/begin
(begin "begin" n
 "    " > q n
 "end" >
 :ann "begin ... end")

;; erlang-mode/beh
(beh "-behaviour(" (p "gen_server") ")." n
 q
 :ann "-behaviour(...).")

;; erlang-mode/case
(case "case " p " of" n
 "    " > q n
 "end" >
 :ann "case ... of ... end")

;; erlang-mode/compile
(compile "-compile([" (p "export_all") "])." n
 q
 :ann "-compile(...).")

;; erlang-mode/def
(def "-define(" p "," p ")." n
 q
 :ann "-define(...,...).")

;; erlang-mode/exp
(exp "-export([" (p "start/0") "])." n
 q
 :ann "-export([]).")

;; erlang-mode/fun
(fun "fun (" p ") -> " q " end"
 :ann "fun (...) -> ... end")

;; erlang-mode/if
(if "if" n
 "    " > p "-> " p ";" n
 "    true -> " q > n
 "end" >
 :ann "if ... -> ... ; true -> ... end")

;; erlang-mode/ifdef
(ifdef "-ifdef(" p ")." n
 q n
 "-endif." >
 :ann "-ifdef(...). ... -endif.")

;; erlang-mode/ifndef
(ifndef "-ifndef(" p ")." n
 q n
 "-endif." >
 :ann "-ifndef(...). ... -endif.")

;; erlang-mode/imp
(imp "-import(" (p "lists") ", [" (p "map/2, sum/1") "])." n
 q
 :ann "-import([]).")

;; erlang-mode/inc
(inc "-include(\"" p "\")." n
 q
 :ann "-include(\"...\").")

;; erlang-mode/inc.lib
(inc.lib "-include_lib(\"" p "\")." n
 q
 :ann "-include_lib(\"...\").")

;; erlang-mode/loop
(loop (p "loop" f1) "(" (s f2) ") ->" n
 "    receive" > n
 "	" > (p "_") " ->" n
 "	    " > f1 "(" f2 ")" n
 "    end." > n
 q
 :ann "loop(...) -> receive _ -> loop(...) end.")

;; erlang-mode/mod
(mod "-module(" `(p ,(file-name-nondirectory (file-name-sans-extension (or (buffer-file-name) (buffer-name))))) ")." n
 q
 :ann "-module().")

;; erlang-mode/rcv
(rcv "receive" n
 "    " > p "-> " q n
 "end" >
 :ann "receive ... -> ... end")

;; erlang-mode/rcv.after
(rcv.after "receive" n
 "after" > n
 "    " > p "-> " q n
 "end" >
 :ann "receive after ... -> ... end")

;; erlang-mode/rec
(rec "-record(" p ",{" p "})." n
 q
 :ann "-record(...,{...}).")

;; erlang-mode/try
(try "try " p " of" n
 "    " > q n
 "catch" > n
 "after" > n
 "end" >
 :ann "try ... of ... catch after end")

;; erlang-mode/undef
(undef "-undef(" p ")." n
 q
 :ann "-undef(...).")


f90-mode

;; f90-mode/bd
(bd "block data " q
 :ann "block data")

;; f90-mode/c
(c "continue " q
 :ann "continue")

;; f90-mode/ch
(ch "character " q
 :ann "character")

;; f90-mode/cx
(cx "complex " q
 :ann "complex")

;; f90-mode/dc
(dc "double complex " q
 :ann "double complex")

;; f90-mode/do
(do "do " p n
 "  " > q n
 "end do" > n
 
 :ann "do ... end do ...")

;; f90-mode/dp
(dp "double precision " q
 :ann "double precision")

;; f90-mode/forall
(forall "forall (" p ")" n
 "  " > q n
 "end forall" >
 :ann "forall ... end forall ...")

;; f90-mode/function
(function "function " (p "name" f1) " (" (p "arg" f2) ")" n
 "  " > (p "real") " :: " f1 n
 "  " > (p "real, intent(in)") " :: " f2 n
 n
 "  " > q n
 n
 "end function " f1 > n
 
 :ann "function ... end function ...")

;; f90-mode/if
(if "if ( " (p "condition") " ) then" n
 "   " > q n
 "end if" >
 :ann "if then end if")

;; f90-mode/in
(in "implicit none"
 :ann "implicit none")

;; f90-mode/inc
(inc "include " q
 :ann "include")

;; f90-mode/intr
(intr "intrinsic " q
 :ann "intrinsic")

;; f90-mode/l
(l "logical " q
 :ann "logical")

;; f90-mode/module
(module "module " (p "name" f1) n
 n
 "contains" > n
 n
 "  " > q n
 n
 "end module " f1 > n
 
 :ann "module ... end module ...")

;; f90-mode/pa
(pa "parameter " q
 :ann "parameter")

;; f90-mode/program
(program "program " (p "name" f1) n
 "  " > (p "implicit none") n
 n
 "  " > q n
 n
 "end program " f1 > n
 
 :ann "program .. end program")

;; f90-mode/puref
(pure\ func "pure function " (p "name" f1) " (" (p "arg" f2) ")" n
 "  " > (p "real") " :: " f1 n
 "  " > (p "real, intent(in)") " :: " f2 n
 n
 "  " > q n
 n
 "end function " f1 > n
 
 :ann "pure function ... end function ...")

;; f90-mode/pures
;; WARNING: Nested fields flattened into field 3
(pure\ subroutine "pure subroutine " (p "name" f1) " (" (p "arg" f2) ")" n
 "  " > (p "real, intent(in) :: arg") n
 n
 "  " > q n
 "end subroutine " f1 > n
 
 :ann "pure subroutine .. end subroutine")

;; f90-mode/re
(re "read (" (p "*") "," (p "*") ") " q
 :ann "read (*,*)")

;; f90-mode/subroutine
;; WARNING: Nested fields flattened into field 3
(subroutine "subroutine " (p "name" f1) " (" (p "arg" f2) ")" n
 "  " > (p "real, intent(in) :: arg") n
 n
 "  " > q n
 "end subroutine " f1 > n
 
 :ann "subroutine .. end subroutine")

;; f90-mode/until
;; WARNING: $0 is not the last field; kept as a plain field
(until "do" n
 "  " > p n
 "  if (" (p "condition") ") exit" > n
 "end do" > n
 
 :ann "until ... end until ...")

;; f90-mode/where
(where "where (" (p "condition") ")" n
 "  " > q n
 "end where" > n
 
 :ann "where ... end where ...")

;; f90-mode/while
(while "do while (" (p "condition") ")" n
 "  " > q n
 "end do" > n
 
 :ann "while ... end while ...")

;; f90-mode/wr
(wr "write (" (p "*") "," (p "*") ") " q
 :ann "write (*,*)")


faust-mode

;; faust-mode/button
(bu "button(\"" p "\")" q
 :ann "button")

;; faust-mode/case
(ca "case {" n
 "  " > p n
 "  };" > n
 q
 :ann "case")

;; faust-mode/checkbox
(ch "checkbox(\"" p "\")" q
 :ann "checkbox")

;; faust-mode/component
(co "component(\"" p ".dsp\")" q
 :ann "component")

;; faust-mode/declare
(de "declare " (p "key") " \"" (p "value") "\";" n
 q
 :ann "declare")

;; faust-mode/declareauthor
(da "declare author \"" p "\";" n
 q
 :ann "declare author")

;; faust-mode/declarelicense
(dl "declare license \"" (p (completing-read "Choose: " (flatten-tree (list '("AGPL-3.0-only" "AGPL-3.0-or-later" "Apache-2.0" "BSD-2-Clause" "BSD-3-Clause" "CC-BY-4.0" "CC0-1.0" "EPL-2.0" "GPL-2.0-only" "GPL-2.0-or-later" "GPL-3.0-only" "GPL-3.0-or-later" "ISC" "LGPL-2.1-only" "LGPL-2.1-or-later" "LGPL-3.0-only" "LGPL-3.0-or-later" "MIT" "MPL-2.0" "Unlicense"))))) "\";" n
 q
 :ann "declare license")

;; faust-mode/declarename
(dn "declare name \"" p "\";" n
 q
 :ann "declare name")

;; faust-mode/declareversion
(dv "declare version \"" (p "0.1") "\";" n
 q
 :ann "declare version")

;; faust-mode/hbargraph
(hb "hbargraph(\"" (p "name") "\", " (p "min") ", " (p "max") ")" q
 :ann "hbargraph")

;; faust-mode/header
;; WARNING: Nested fields flattened into field 6
;; WARNING: Nested fields flattened into field 7
(he "declare name \"" p "\";" n
 "declare version \"" (p "0.1") "\";" > n
 "declare author \"" (s f3) "\";" > n
 "declare license \"" (p (completing-read "Choose: " (flatten-tree (list '("AGPL-3.0-only" "AGPL-3.0-or-later" "Apache-2.0" "BSD-2-Clause" "BSD-3-Clause" "CC-BY-4.0" "CC0-1.0" "EPL-2.0" "GPL-2.0-only" "GPL-2.0-or-later" "GPL-3.0-only" "GPL-3.0-or-later" "ISC" "LGPL-2.1-only" "LGPL-2.1-or-later" "LGPL-3.0-only" "LGPL-3.0-or-later" "MIT" "MPL-2.0" "Unlicense"))))) "\";" > n
 "declare copyright \"" (p "starting year") " - " `(p ,(format-time-string "%Y")) ", " p "\";" > n
 q n
 
 :ann "header")

;; faust-mode/hgroup
(hg "hgroup(\"" (p "name") "\", " (p "expression") ")" q
 :ann "hgroup")

;; faust-mode/hslider
(hs "hslider(\"" (p "name") "\", " (p "default") ", " (p "min") ", " (p "max") ", " (p "step") ")" q
 :ann "hslider")

;; faust-mode/import
(im "import(\"" p ".lib\");" q
 :ann "import")

;; faust-mode/nentry
(ne "nentry(\"" (p "name") "\", " (p "default") ", " (p "min") ", " (p "max") ", " (p "step") ")" q
 :ann "nentry")

;; faust-mode/par
(pa "par(" (p "i") ", " (p "Nr") ", " (p "expression") ")" q
 :ann "par")

;; faust-mode/process
(pr "process = " p ";" n
 q
 :ann "process")

;; faust-mode/processx
(px "process(" (p "x" f1) ") = " (p "expression") "(" f1 ");" q
 :ann "processx")

;; faust-mode/prod
(mu "prod(" (p "i") ", " (p "Nr") ", " (p "expression") ")" q
 :ann "prod")

;; faust-mode/rule
(ru "  (" (p "pattern") ") => " (p "expression") ";" n
 q
 :ann "rule")

;; faust-mode/seq
(se "seq(" (p "i") ", " (p "Nr") ", " (p "expression") ")" q
 :ann "seq")

;; faust-mode/sum
(su "sum(" (p "i") ", " (p "Nr") ", " (p "expression") ")" q
 :ann "sum")

;; faust-mode/tgroup
(tg "tgroup(\"" (p "name") "\", " (p "expression") ")" q
 :ann "tgroup")

;; faust-mode/vbargraph
(vb "vbargraph(\"" (p "name") "\", " (p "min") ", " (p "max") ")" q
 :ann "vbargraph")

;; faust-mode/vgroup
(vg "vgroup(\"" (p "name") "\", " (p "expression") ")" q
 :ann "vgroup")

;; faust-mode/vslider
(vs "vslider(\"" (p "name") "\", " (p "default") ", " (p "min") ", " (p "max") ", " (p "step") ")" q
 :ann "vslider")

;; faust-mode/with
(wi "with {" n
 "  " > (p "expression") n
 "};" > n
 q
 :ann "with")


fish-mode

;; fish-mode/bang
(! "#!/usr/bin/env fish" n
 q n
 
 :ann "bang")

;; fish-mode/block
(block "begin" n
 "    " > q n
 "end" > n
 
 :ann "begin ... end")

;; fish-mode/bp
(bp "breakpoint" n
 q
 :ann "breakpoint")

;; fish-mode/for
(for "for " (p "var") " in " (p "stuff") n
 "    " > q n
 "end" > n
 
 :ann "for loop")

;; fish-mode/function
(function "function " (p "name") n
 "    " > q n
 "end" > n
 )

;; fish-mode/if
(if "if " (p "[ -f file ]") n
 "    " > (p "do") n
 "end" > n
 q
 :ann "if ... end")

;; fish-mode/ife
(ife "if " (p "cond") n
 "    " > (p "stuff") n
 "else" > n
 "    " > (p "other") n
 "end" > n
 q n
 
 :ann "if ... ... else ... end")

;; fish-mode/sw
(sw "switch " (p "condition") n
 "    case " (p "*") > n
 "         " > q n
 "end" > n
 
 :ann "switch")

;; fish-mode/while
(while "while " (p "cond") n
 "    " > q n
 "end" > n
 
 :ann "while loop")


fundamental-mode conf-unix-mode cperl-mode git-commit-mode nesc-mode perl-mode text-mode

;; fundamental-mode/current-date
(dd (list 'l (format-time-string "%Y-%m-%d"))
 :ann "current-date")

;; fundamental-mode/current-date-and-time
(dt (list 'l (format-time-string "%Y-%m-%dT%H:%M:%S%:z")) n
 
 :ann "current-date-and-time")

;; fundamental-mode/mode-line
(-*- "# -*- mode:" (p "conf") "; -*-" q
 :ann "Mode line")


gdscript-mode

;; gdscript-mode/class
(cls "class " (p "class") ":" n
 "    " > q n
 
 :ann "class")

;; gdscript-mode/class_name
(clsn "class_name " (p "Item") " = " (p "\"res://icons/item.png\"")
 :ann "class_name")

;; gdscript-mode/const
(const "const " (p "name") " = " q)

;; gdscript-mode/enum
(enum "enum " p " {" p "}" q)

;; gdscript-mode/for
(for "for " (p "var") " in " (p "collection") ":" n
 "    " > q "pass"
 :ann "for ... in ... : ...")

;; gdscript-mode/func
(f "func " (p "func_name") " (" (p "param") "):" n
 "    " > q "pass"
 :ann "func")

;; gdscript-mode/if
(if "if " (p "condition") ":" n
 "    " > q "pass")

;; gdscript-mode/ife
(ife "if " (p "condition") ":" n
 "    " > (p "pass") n
 "else:" > n
 "    " > (p "pass"))

;; gdscript-mode/match
(match "match " (p "expression") ":" n
 "    " > (p "pattern") ":" n
 "        " > (p "pass"))

;; gdscript-mode/onready
(onr "onready var " (p "name") " = " q
 :ann "onready")

;; gdscript-mode/print
(p "print(" q ")"
 :ann "print")

;; gdscript-mode/return
(r "return " q
 :ann "return")

;; gdscript-mode/setget
(vsg "var " (p "variable") " = " (p "value") " setget " (p "setterfunc") ", " (p "getterfunc")
 :ann "setget")

;; gdscript-mode/static_func
(sf "static func " (p "func_name") " (" (p "param") "):" n
 "    " > q "pass"
 :ann "static_func")

;; gdscript-mode/var
(var "var " (p "name") " = " q)

;; gdscript-mode/while
(wh "while " (p "True") ":" n
 "    " > q "pass"
 :ann "while")


git-commit-mode

;; git-commit-mode/fixes
(fix "fixes #" (p "100")
 :ann "fixes")

;; git-commit-mode/references
(ref "references #" (p "100")
 :ann "references")

;; git-commit-mode/type
(type (p (completing-read "Choose: " (flatten-tree (list '("fix" "hotfix" "add" "update" "change" "clean" "disable" "remove" "upgrade" "revert"))))) " " q)


go-mode go-ts-mode

;; go-mode/benchmark
(bench "func Benchmark" p "(b *testing.B) {" n
 "	for i := 0; i < b.N; i++ {" > n
 "		" > q n
 "	}" > n
 "}" > n
 
 :ann "benchmark")

;; go-mode/const
(const "const " (p "name type") " = " (p "val") n
 q)

;; go-mode/const(
;; WARNING: Duplicate key ‘const’ renamed to ‘const-2’
(const-2 "const (" n
 "  " > (p "name type") " = " (p "val") n
 ")" q >
 :ann "const (...)")

;; go-mode/dd
(dd "fmt.Printf(\"%+v\\n\", " p ") // output for debug" n
 q
 :ann "debug fmt.Printf")

;; go-mode/default
(def "default:" n
 q
 :ann "default")

;; go-mode/else
(el " else {" n
 "  " > q n
 "}" >
 :ann "else")

;; go-mode/error
(err "if err != nil {" n
 "	" > q n
 "}" >
 :ann "error")

;; go-mode/example
(example "func Example" p "() {" n
 "	" > q n
 "}" > n
 )

;; go-mode/for
(for "for " p " {" n
 "  " > q n
 "}" > n
 )

;; go-mode/forrange
;; WARNING: Fields visited in buffer order (3 2 1) instead of numeric order
(range "for " (p "key") ", " (p "value") " := range " (p "target") " {" n
 "	" > q n
 "}" >
 :ann "for range")

;; go-mode/func
;; WARNING: Fields visited in buffer order (4 1 2 3) instead of numeric order
(func "// " f1 " " (p "...") n
 "func " (p "name" f1) "(" (p "args") ") " p " {" > n
 "  " > q n
 "}" >)

;; go-mode/if
(if "if " p " {" n
 "  " > q n
 "}" >)

;; go-mode/iferr
(iferr "if err != " p " {" n
 "  " > q n
 "}" > n
 
 :ann "if error")

;; go-mode/import
(imp "import \"" p "\"" n
 q
 :ann "import")

;; go-mode/import(
;; WARNING: Duplicate key ‘imp’ renamed to ‘imp-2’
(imp-2 "import (" n
 "  \"" q "\"" > n
 ")" > n
 
 :ann "import (...)")

;; go-mode/init
(init "func init() {" n
 "  " > q n
 "}" >
 :ann "func init()")

;; go-mode/lambda
(lambda "func(" (p "args") ") " p " {" n
 "  " > q n
 "}()" >
 :ann "lambda func")

;; go-mode/map
(map "map[" (p "type") "]" (p "type") n
 q)

;; go-mode/method
;; WARNING: Fields visited in buffer order (5 1 2 3 4) instead of numeric order
(mthd "// " f2 " " (p "...") n
 "func (" (p "recv") ") " (p "name" f2) "(" (p "args") ") " p " {" > n
 "  " > q n
 "}" >
 :ann "method")

;; go-mode/parallel_benchmark
(parbench "func Benchmark" p "(b *testing.B) {" n
 "	b.RunParallel(func(pb *testing.PB) {" > n
 "		for pb.Next() {" > n
 "			" > q n
 "		}" > n
 "	})" > n
 "}" > n
 
 :ann "parallel_benchmark")

;; go-mode/printf
(pr "fmt.Printf(\"" (p "%s") "\\n\", " (p "args") ")" n
 q
 :ann "fmt.Printf(...)")

;; go-mode/select
(sel "select {" n
 "case " p ":" > n
 "  " > q n
 "}" >
 :ann "select")

;; go-mode/switch
(sw "switch " p " {" n
 "case " p ":" > n
 "  " > q n
 "}" >
 :ann "switch")

;; go-mode/test
(at "func Test" p "(t *testing.T) {" n
 "	" > q n
 "}" > n
 
 :ann "test")

;; go-mode/testmain
(testmain "func TestMain(m *testing.M) {" n
 "	setup()" > n
 "	ret := m.Run()" > n
 "	if ret == 0 {" > n
 "		teardown()" > n
 "	}" > n
 "	os.Exit(ret)" > n
 "}" > n
 n
 "func setup() {" > n
 "	" > p n
 "}" > n
 n
 "func teardown() {" > n
 "	" > p n
 "}" q >)

;; go-mode/type
(type "type " p " " p " {" n
 "  " > q n
 "}" >)

;; go-mode/var
(var "var " (p "name") " " (p "type") n
 q)

;; go-mode/var(
;; WARNING: Duplicate key ‘var’ renamed to ‘var-2’
(var-2 "var (" n
 "  " > (p "name") " " (p "type") n
 ")" q >
 :ann "var (...)")


go-mode go-ts-mode :when (progn (forward-line 0) (not (and (eq (point-min) (point)) (looking-at-p "package"))))

;; go-mode/main
(main "func main() {" n
 "  " > q n
 "}" >
 :ann "func main()")


groovy-mode

;; groovy-mode/class
(class "class " (p "Class") " {" n
 "      " > q n
 "}" >)

;; groovy-mode/def
(def "def " (p "method") "(" (p "args") ") {" n
 "    " > q n
 "}" >)

;; groovy-mode/dict
;; WARNING: $0 directly follows a field; Tempel exits at the template end
(dict (p "dict") " = [" (p "key") " : " (p "value") "]")

;; groovy-mode/for
(for "for (" (p "var") " in " (p "iter") ") {" n
 "    " > q n
 "}" > n
 )

;; groovy-mode/println
(pr "println " (p "\"string\"") n
 q
 :ann "println")

;; groovy-mode/times
(times (p "10") ".times {" n
 "              " > q n
 "}                ." >)


haskell-mode haskell-ts-mode

;; haskell-mode/case
;; WARNING: $0 is not the last field; kept as a plain field
(case (p (current-column) col noinsert) "case " (p "var") " of" n
 (ignore (indent-to col)) "     " (p "cond") " -> " (p "value") n
 (ignore (indent-to col)) "     " p n
 (ignore (indent-to col)) "     otherwise -> " (p "other"))

;; haskell-mode/data
(da "data " (p "Type") " = " p
 :ann "data")

;; haskell-mode/doc
(d "{-" n
 "  " > q n
 "-}" >
 :ann "doc")

;; haskell-mode/efix
(efix "(error \"FIXME: " q "\")"
 :ann "fixme dummy")

;; haskell-mode/function
(fun (p (current-column) col noinsert) (p "function-name" f1) " :: " (p "type") n
 (ignore (indent-to col)) f1 " " (p "arguments") " " q n
 )

;; haskell-mode/functione
(fune (p "function-name" f1) " :: " (p "type") n>
 f1 " = " q "error \"Not implemented: " f1 "\"")

;; haskell-mode/import
;; WARNING: Nested fields flattened into field 2
;; WARNING: Nested fields flattened into field 4
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(import "import" (p " qualified") " " (p "Module(symbols)") (p " as alias"))

;; haskell-mode/instance
;; WARNING: Nested fields flattened into field 1
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(ins "instance " (p "(Show a) => ") (p "Ord") " " (p "DataType") " where" n
 q n
 
 :ann "instance")

;; haskell-mode/main
(main (p (current-column) col noinsert) "main :: IO ()" n
 (ignore (indent-to col)) "main = do " q
 :ann "main function")

;; haskell-mode/module
(mod "module " (p "Module") " where" n
 q
 :ann "module")

;; haskell-mode/new class
(class (p (current-column) col noinsert) "class " (p "Name") " where" n
 (ignore (indent-to col)) "      " q
 :ann "new class")

;; haskell-mode/pragma
(prag "{-# " (p "PRAGMA") " #-}" q
 :ann "pragma")

;; haskell-mode/print
(pr "print " q
 :ann "print")


html-mode html-ts-mode web-mode

;; html-mode/dd
(dd "<dd>" p "</dd>" q
 :ann "<dd> ... </dd>")

;; html-mode/dl
(dl "<dl>" n
 "    " > q n
 "</dl>" >
 :ann "<dl> ... </dl>")

;; html-mode/doctype
(doctype "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">"
 :ann "Doctype HTML 4.01 Strict")

;; html-mode/doctype.html5
(doctype.html5 "<!DOCTYPE html>"
 :ann "Doctype HTML 5")

;; html-mode/doctype.xhtml1
(doctype.xhtml1 "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">"
 :ann "DocType XHTML 1.0 frameset")

;; html-mode/doctype.xhtml1_1
(doctype.xhtml1_1 "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"
 :ann "DocType XHTML 1.1")

;; html-mode/doctype.xhtml1_strict
(doctype.xhtml1_strict "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
 :ann "DocType XHTML 1.0 Strict")

;; html-mode/doctype.xhtml1_transitional
(doctype.xhtml1_transitional "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
 :ann "DocType XHTML 1.0 Transitional")

;; html-mode/dt
(dt "<dt>" p "</dt>" q
 :ann "<dt> ... </dt>")

;; html-mode/form
(form "<form method=\"" p "\" id=\"" p "\" action=\"" p "\">" n
 "  " > q n
 "</form>" >)

;; html-mode/html
(html "<html>" n
 "  " > q n
 "</html>" >
 :ann "<html>...</html>")

;; html-mode/html.xmlns
(html.xmlns "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"" (p "en") "\" lang=\"" (p "en") "\">" n
 "  " > q n
 "</html>" >
 :ann "<html xmlns=\"...\">...</html>")

;; html-mode/link.import
(link.import "<link rel=\"import\" href=\"" (p "url") "\" />" q
 :ann "<link rel=\"import\" />")

;; html-mode/link.stylesheet
(link.stylesheet "<link rel=\"" (p "stylesheet") "\" href=\"" (p "url") "\" type=\"" (p "text/css") "\" media=\"" (p "screen") "\" />" q
 :ann "<link stylesheet=\"...\" />")

;; html-mode/link.stylesheet-ie
(link.stylesheet-ie "<!--[if IE]>" n
 "<link rel=\"" (p "stylesheet") "\" href=\"" (p "url") "\" type=\"" (p "text/css") "\" media=\"" (p "screen") "\" />" > n
 "<![endif]-->" q >
 :ann "<!--[if IE]><link stylesheet=\"...\" /><![endif]-->")

;; html-mode/mailto
(mailto "<a href=\"mailto:" p "@" p "\">" q "</a>"
 :ann "<a href=\"mailto:...@...\">...</a>")

;; html-mode/meta
(meta "<meta name=\"" (p "generator") "\" content=\"" (p "content") "\" />" q
 :ann "<meta name=\"...\" content=\"...\" />")

;; html-mode/meta.http-equiv
(meta.http-equiv "<meta http-equiv=\"" (p "Content-Type") "\" content=\"" (p "text/html; charset=UTF-8") "\" />" q
 :ann "<meta http-equiv=\"...\" content=\"...\" />")

;; html-mode/script.javascript
(script "<script type=\"text/javascript\">" n
 "  " > q n
 "</script>" >)

;; html-mode/script.javascript-src
;; WARNING: Duplicate key ‘script’ renamed to ‘script-2’
(script-2 "<script type=\"text/javascript\" src=\"" p "\"></script>" q
 :ann "script src")

;; html-mode/textarea
(textarea "<textarea name=\"" p "\" id=\"" p "\" rows=\"" p "\" cols=\"" p "\" tabindex=\"" p "\"></textarea>" q
 :ann "<textarea ...></textarea>")

;; html-mode/th
(th "<th" p ">" p "</th>" q
 :ann "<th>...</th>")


hy-mode

;; hy-mode/class
;; WARNING: Multi-line default of field 3 is not re-indented
;; WARNING: Nested fields flattened into field 3
(class "(defclass " (p "ClassName") " [" (p "BaseClass") "]" n>
 (p "  (defn --init-- [arg]\n    (setv ))") ")" q)

;; hy-mode/cond
;; WARNING: Nested fields flattened into field 5
;; WARNING: Unnumbered field visited before numbered fields
(cond "(cond" n
 "  [" (p "condition") " " (p "result:2") "]" > n
 "  [" (p "condition") " " (p "result:4") "]" > n
 "  " > (p "[True ]") ")" q)

;; hy-mode/def
(def "(defn " (p "function-name") " [" (p "arg") "]" n
 "  " > q ")")

;; hy-mode/defm
;; WARNING: Nested fields flattened into field 3
(defm "(defmacro " (p "macro-name") " [" (p "arg") "]" n
 "  " > (p "`()") ")" q)

;; hy-mode/do
(do "(do " q ")")

;; hy-mode/for
(for "(for [" (p "item items") "] " q ")")

;; hy-mode/if
(if "(if " (p "condition") n
 "  " > (p "result-true") n
 "  " > (p "result-false") ")" q)

;; hy-mode/ifn
(ifn "(if-not " (p "condition") n
 "  " > (p "result-false") n
 "  " > (p "result-true") ")" q)

;; hy-mode/imp
(import "(import [" (p "module-name") " [" (p "exposing") "]])" q)

;; hy-mode/let
(let "(let [" (p "arg") "]" n
 "  " > q ")")

;; hy-mode/main
(main "(defmain [&rest " (p "args") "]" n
 "  " > q ")")

;; hy-mode/req
(req "(req [" (p "module-name") " [" (p "exposing") "]])" q)

;; hy-mode/s
(s "(setv " q ")")

;; hy-mode/unless
(unless "(unless " (p "condition") n
 "  " > q ")")

;; hy-mode/when
(when "(when " (p "condition") n
 "  " > q ")")


java-mode groovy-mode java-ts-mode

;; java-mode/apr_assert
(apr_assert "if (Globals.useAssertions) {" n
 "   " > (p "assert ..") ";" n
 "}" > n
 q)

;; java-mode/assert
(as "assert " (p "expression") ";" q
 :ann "assert")

;; java-mode/assertEquals
(ae "Assert.assertEquals(" p ", " p ");" n
 q
 :ann "assertEquals")

;; java-mode/cls
(cls "class " (p "Class") " {" n
 "      " > q n
 "}" >)

;; java-mode/constructor
(c "public " `(p ,(file-name-base (or (buffer-file-name) (buffer-name)))) "(" p ") {" n
 "       " > q n
 "}" >
 :ann "constructor")

;; java-mode/define test method
(dt "@Test" n
 "public void test" (p "Name") "() throws Exception {" > n
 "    " > q n
 "}" > n
 
 :ann "define test method")

;; java-mode/doc
(/* "/**" n
 " * " (p "documentation") > n
 " */" q >
 :ann "doc")

;; java-mode/equals
(eq "public boolean equals(" (p "Class") " other) {" n
 "       " > q n
 "}" >
 :ann "equals")

;; java-mode/file_class
(file "public class " `(p ,(file-name-base (or (buffer-file-name) (buffer-name)))) " {" n
 "  " > q n
 "}" > n
 
 :ann "file_class")

;; java-mode/for
(for "for (" (p "int i = 0") "; " (p "i < N") "; " (p "i++") ") {" n
 "    " > q n
 "}" >)

;; java-mode/fori
(fori "for (" (p "Object el") " : " (p "iterator") ") {" n
 "    " > q n
 "}" > n
 )

;; java-mode/getter
(g "public " (p "int") " get" (p "Field") "() {" n
 "       return " (p "field") ";" > n
 "}" q >
 :ann "getter")

;; java-mode/if
(if "if (" (p "condition") ") {" n
 "   " > q n
 "}" >)

;; java-mode/ife
(ife "if (" (p "cond") ") {" n
 "    " > p n
 "}" > n
 "else {" > n
 "     " > p n
 "}" q >)

;; java-mode/import
(imp "import " (p "System.") ";" n
 q
 :ann "import")

;; java-mode/iterator
(iterator "public Iterator<" (p "type") "> iterator() {" n
 "       " > q n
 "}" > n
 )

;; java-mode/javadoc
(doc "/**" n
 " * " q > n
 " *" > n
 " */" >
 :ann "javadoc")

;; java-mode/lambda
(\\ "(" (p "args") ") -> " (p "expression") ";" q
 :ann "lambda")

;; java-mode/main
(main "public static void main(String[] args) {" n
 "       " > q n
 "}" >)

;; java-mode/main_class
(main_class "class " (list 'l (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))) n
 "{" > n
 "public static void main(String[] args)" > n
 "{" > n
 " " > q n
 "}" > n
 "}" > n
 )

;; java-mode/method
(m (p (completing-read "Choose: " (flatten-tree (list '("public" "private" "protected" ""))))) " " (p "void") " " (p "name") "(" (p "args") ") {" n
 "            " > q n
 "}" >
 :ann "method")

;; java-mode/new
(new (p "Type") " " (p "obj") " = new " (p "Constr") "(" (p "args") ");" n
 q)

;; java-mode/override
(o "@Override" n
 q
 :ann "override")

;; java-mode/param
(param "@param " (p "parameter") " " q)

;; java-mode/printf
(printf "System.out.printf(\"" q "%n\");")

;; java-mode/println
(pr "System.out.println(\"" (p "text") "\");" n
 q
 :ann "println")

;; java-mode/return
(r "return " q
 :ann "return")

;; java-mode/test
(test "@Test" n
 "public void test_" (p "Case") "() {" > n
 "       " > q n
 "}" >)

;; java-mode/testClass
(tc "import junit.framework.*;" n
 "import junit.textui.*;" > n
 n
 "public class Test" (p "Class") " extends TestCase {" > n
 "       protected void setUp() {" > n
 "                 " > q n
 "       }" > n
 "}" >
 :ann "testClass")

;; java-mode/this
(\. "this." (s f1) " = " f1 ";" n
 q
 :ann "this")

;; java-mode/toString
(toStr "public String toString() {" n
 "       " > q n
 "}" >
 :ann "toString")

;; java-mode/try
;; WARNING: Multi-line default of field 2 is not re-indented
;; WARNING: $0 is not the last field; kept as a plain field
(try "try {" n
 "    " > p n
 "}" > n
 "catch (" (p "Throwable e") ") {" > n
 "      " > (p "System.out.println(\"Error \" + e.getMessage());\n      e.printStackTrace();") n
 "}" > n
 )

;; java-mode/value
(val "final " (p "int") " " (p "n") " = " q ";"
 :ann "value")


js-mode js-ts-mode js2-mode js3-mode rjsx-mode tsx-ts-mode typescript-mode typescript-ts-mode typescriptreact-mode web-mode

;; js-mode/al
(al "alert(" q ");"
 :ann "alert")

;; js-mode/anfn
(anfn "(" (p "params") ") => {" n
 "  " > p n
 "}" q >
 :ann "anonymousFunction")

;; js-mode/bnd
(bnd "this." (p "methodName" f1) " = this." f1 ".bind(this)" q
 :ann "bindThis")

;; js-mode/class
;; WARNING: Nested fields flattened into field 2
;; WARNING: Multi-line default of field 4 is not re-indented
;; WARNING: Nested fields flattened into field 4
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(class "class " (p "Class") (p " extends ParentClass") " {" n
 "  " > (p "constructor(arg) {\n    super(arg);\n    \n  }") n
 n
 "  " > q n
 "}" >)

;; js-mode/cmmb
(cmmb "/**" n
 "|--------------------------------------------------" > n
 "| " p > n
 "|--------------------------------------------------" > n
 "*/" q >
 :ann "Comment Big Block")

;; js-mode/com
(com "/*" n
 " * " q > n
 " */" >
 :ann "comment (/* ... */)")

;; js-mode/console/cas
(cas "console.assert(" (p "expression") ", " (p "object") ")" q
 :ann "console.assert")

;; js-mode/console/ccl
(ccl "console.clear()"
 :ann "console.clear")

;; js-mode/console/cco
(cco "console.count(" (p "label") ")" q
 :ann "console.count")

;; js-mode/console/cdi
(cdi "console.dir(" (p "object") ")" q
 :ann "console.dir")

;; js-mode/console/cer
(cer "console.error(" (p "object") ")" q
 :ann "console.error")

;; js-mode/console/cge
(cge "console.groupEnd()"
 :ann "console.groupEnd")

;; js-mode/console/cgr
(cgr "console.group(\"" (p "label") "\")" q
 :ann "console.group")

;; js-mode/console/cin
(cin "console.info(" (p "object") ")" q
 :ann "console.info")

;; js-mode/console/clg
(clg "console.log(" (p "object") ")" q
 :ann "console.log")

;; js-mode/console/clo
(clo "console.log('" f1 ": ', " (p "object" f1) ")" q
 :ann "console.log (formatted)")

;; js-mode/console/cta
(cta "console.table(" (p "object") ")" q
 :ann "console.table")

;; js-mode/console/cte
(cte "console.timeEnd('" (p "object") "')" q
 :ann "console.timeEnd")

;; js-mode/console/cwa
(cwa "console.warn(" (p "object") ")" q
 :ann "console.warn")

;; js-mode/const
(const "const " (p "name") " = " (p "initial") ";" q
 :ann "const declaration")

;; js-mode/dar
(dar "const [" (p "propertyName") "] = " (p "arrayToDestruct")
 :ann "destructingArray")

;; js-mode/debugger
(dbg "debugger;"
 :ann "debugger")

;; js-mode/dob
(dob "const {" (p "propertyName") "} = " (p "objectToDestruct")
 :ann "destructingObject")

;; js-mode/each
(each (p "collection") ".forEach(function (" (p "elem") ") {" n
 "  " > q n
 "});" >)

;; js-mode/edf
(edf "export default (" (p "params") ") => {" n
 "  " > q n
 "}" >
 :ann "exportDefaultFunction")

;; js-mode/el
(el "else {" n
 "  " > q n
 "}" >
 :ann "else")

;; js-mode/enf
(enf "export const " (p "functionName") " = (" (p "params") ") => {" n
 "  " > q n
 "}" >
 :ann "exportNamedFunction")

;; js-mode/error
(err "new Error(" (p "message") ");" q
 :ann "new error")

;; js-mode/exa
;; WARNING: Fields visited in buffer order (2 3 1) instead of numeric order
(exa "export { " (p "originalName") " as " (p "alias") " } from '" (p "module") "'" q
 :ann "exportAs")

;; js-mode/exd
;; WARNING: Fields visited in buffer order (2 1) instead of numeric order
(exd "export { " p " } from '" (p "module") "'" q
 :ann "exportDestructing")

;; js-mode/exp
(exp "export default " p
 :ann "exportDefault")

;; js-mode/fin
(fin "for(let " (p "item") " in " (p "object") ") {" n
 "  " > q n
 "}" >
 :ann "forIn")

;; js-mode/flow
(flow "/* @flow */"
 :ann "/* @flow */")

;; js-mode/fof
(fof "for(let " (p "item") " of " (p "object") ") {" n
 "  " > q n
 "}" >
 :ann "forOf")

;; js-mode/for
(for "for (let " (p "i" f1) " = " (p "0") "; " f1 " < " (p "collection") ".length; " f1 "++) {" n
 "  " > q n
 "}" >)

;; js-mode/fre
(fre (p "array") ".forEach(" (p "currentItem") " => {" n
 "  " > q n
 "})" >
 :ann "forEach")

;; js-mode/function
(f "function " (p "name") "(" (p "arg") ") {" n
 "         " > q n
 "}" >
 :ann "function")

;; js-mode/if
(if "if (" (p "condition") ") {" n
 "  " > q n
 "}" >)

;; js-mode/ima
;; WARNING: Fields visited in buffer order (2 3 1) instead of numeric order
(ima "import { " (p "originalName") " as " (p "alias") " } from '" (p "module") "'" q
 :ann "importAs")

;; js-mode/imd
;; WARNING: Fields visited in buffer order (2 1) instead of numeric order
(imd "import { " p " } from '" (p "module") "'" q
 :ann "importDestructing")

;; js-mode/ime
;; WARNING: Fields visited in buffer order (2 1) instead of numeric order
(ime "import * as " (p "alias") " from '" (p "module") "'" q
 :ann "importEverything")

;; js-mode/imn
(imn "import '" (p "module") "'" q
 :ann "importNoModuleName")

;; js-mode/imp
;; WARNING: Fields visited in buffer order (2 1) instead of numeric order
(imp "import " (p "moduleName") " from '" (p "module") "'" q
 :ann "import")

;; js-mode/init
;; WARNING: Nested fields flattened into field 2
(init "constructor(" (p "arg") ") {" n
 "  " > (p "super(arg);") n
 "  " > q n
 "}" >
 :ann "constructor")

;; js-mode/let
(let "let " (p "name") " = " (p "initial") ";" q
 :ann "let declaration")

;; js-mode/met
(met (p "name") "(" (p "arg") ") {" n
 "  " > q n
 "}" >
 :ann "method")

;; js-mode/metb
(metb (p "methodName") " = (" (p "params") ") => {" n
 "  " > q n
 "}" >
 :ann "bound method")

;; js-mode/multiline-comment
(/** "/**" n
 " * " q > n
 " */" >
 :ann "multiline-comment")

;; js-mode/nfn
(nfn "const " (p "name") " = (" (p "params") ") => {" n
 "  " > p n
 "}" q >
 :ann "namedFunction")

;; js-mode/pge
(pge "get " (p "propertyName") "() {" n
 "  return this." (p "propertyName") > n
 "}" q >
 :ann "propertyGet")

;; js-mode/prom
(prom "return new Promise((resolve, reject) => {" n
 "  " > p n
 "})" q >
 :ann "promise")

;; js-mode/pse
(pse "set " (p "propertyName") "(" (p "value") ") {" n
 "  " > q n
 "}" >
 :ann "propertyset")

;; js-mode/sti
(sti "setInterval(() => {" n
 "  " > p n
 "}, " q "intervalInms)" >
 :ann "setInterval")

;; js-mode/sto
;; WARNING: Fields visited in buffer order (2 1) instead of numeric order
(sto "setTimeout(() => {" n
 "  " > p n
 "}, " (p "delayInms") ")" q >
 :ann "setTimeOut")

;; js-mode/switch
(sw "switch (" (p "condition") ") {" n
 "  case " (p "expression") ":" > n
 "    " > q n
 "    break;" > n
 "  default:" > n
 "}" >
 :ann "switch")

;; js-mode/try-catch
;; WARNING: Multi-line default of field 3 is not re-indented
;; WARNING: Nested fields flattened into field 3
(try "try {" n
 "  " > p n
 "} catch (err) {" > n
 "  " > p n
 "}" (p " finally {\n  \n") "}" q >
 :ann "try...catch statement")


js-mode js-ts-mode js2-mode js3-mode rjsx-mode tsx-ts-mode typescript-mode typescript-ts-mode typescriptreact-mode web-mode :when (= (js2-node-type (js2-node-at-point)) js2-COMMENT)

;; js-mode/param-comment
(*@p "* @param {" (p "type") "} " (p "comment") "." q
 :ann "param-comment")

;; js-mode/return-comment
(*@r "* @return {" (p "type") "}" q
 :ann "return-comment")

;; js-mode/type-multiline-comment
(*ty "* @type {" (p "type") "}" q
 :ann "type-multiline-comment")


js-mode js-ts-mode js2-mode js3-mode rjsx-mode tsx-ts-mode typescript-mode typescript-ts-mode typescriptreact-mode web-mode :when (not (= (js2-node-type (js2-node-at-point)) js2-COMMENT))

;; js-mode/type-inline-comment
(@ty "/** @type {" (p "type") "} */" q
 :ann "type-inline-comment")


julia-mode julia-ts-mode

;; julia-mode/atype
;; WARNING: Nested fields flattened into field 1
(atype "abstract type " (p "type <: supertype") " end" q
 :ann "abstract type ... end")

;; julia-mode/begin
(begin "begin" n
 "    " > q n
 "end" >
 :ann "begin ... end")

;; julia-mode/do
(do "do " (p "x") n
 "    " > q n
 "end" >
 :ann "do ... ... end")

;; julia-mode/for
;; WARNING: Field transformation of field 2 is only evaluated on expansion
;; WARNING: Requires YASnippet function ‘yas-julia-iteration-keyword-choice’
(for "for " (p "i") " " `(p ,(format "%s" (or (dlet ((yas-text "")) (yas-julia-iteration-keyword-choice)) ""))) " " (p "1:n") n
 "    " > q n
 "end" >
 :ann "for ... ... end")

;; julia-mode/fun
(fun "function " (p "fun") "(" (p "args") ")" n
 "    " > q n
 "end" >
 :ann "function(...) ... end")

;; julia-mode/if
(if "if " (p "cond") n
 "    " > q n
 "end" >
 :ann "if ... ... end")

;; julia-mode/ife
(ife "if " (p "cond") n
 "    " > (p "true") n
 "else" > n
 "    " > (p "false") n
 "end" > n
 q
 :ann "if ... ... else ... end")

;; julia-mode/let
(let "let " (p "x = 0") n
 "    " > q n
 "end" >
 :ann "let ... ... end")

;; julia-mode/macro
(macro "macro " (p "macro") "(" (p "args") ")" n
 "    " > q n
 "end" >
 :ann "macro(...) ... end")

;; julia-mode/module
(module "module " (p "name") n
 q n
 "end" >
 :ann "module ... ... end")

;; julia-mode/mutstr
(mutstr "mutable struct " (p "name") n
 "    " > q n
 "end" >
 :ann "mutable struct ... end")

;; julia-mode/ptype
;; WARNING: Nested fields flattened into field 1
(ptype "primitive type " (p "type <: supertype") " " (p "bits") " end" q n
 
 :ann "primitive type ... end")

;; julia-mode/quote
(quote "quote" n
 "    " > (p "expr") n
 "end" > n
 q
 :ann "quote ... end")

;; julia-mode/struct
(struct "struct " (p "name") n
 "    " > q n
 "end" >
 :ann "struct ... end")

;; julia-mode/try
(try "try" n
 "    " > (p "expr") n
 "catch " (p "error") > n
 "    " > (p "e_expr") n
 "end" > n
 q n
 
 :ann "try ... catch ... end")

;; julia-mode/tryf
;; WARNING: Duplicate key ‘try’ renamed to ‘try-2’
(try-2 "try" n
 "    " > (p "expr") n
 "catch " (p "error") > n
 "    " > (p "e_expr") n
 "finally" > n
 "    " > (p "f_expr") n
 "end" > n
 q n
 
 :ann "try ... catch ... finally ... end")

;; julia-mode/using
;; WARNING: Nested fields flattened into field 1
;; WARNING: $0 directly follows a field; Tempel exits at the template end
(using "using " (p "package:") n
 
 :ann "using ...")

;; julia-mode/while
(while "while " (p "cond") n
 "    " > q n
 "end" >
 :ann "while ... ... end")


kotlin-mode

;; kotlin-mode/anonymous
(object "object: " p " {" n
 "    " > q n
 "}" >
 :ann "Anonymous class")

;; kotlin-mode/closure
(closure "{ " p " -> " q " }"
 :ann "Closure (function without name)")

;; kotlin-mode/exfun
(exfun "fun " p "." p "(" p "): " p " {" n
 "    " > q n
 "}" >
 :ann "Extension function")

;; kotlin-mode/exval
(exval "val " p "." p ": " p n
 "    get() {" > n
 "        " > q n
 "    }" >
 :ann "Extension read-only property")

;; kotlin-mode/exvar
(exvar "var " p "." p ": " p n
 "    get() {" > n
 "        " > q n
 "    }" > n
 "    set(value) {" > n
 n
 "    }" >
 :ann "Extension read-write property")

;; kotlin-mode/fun
(fun0 "fun " p "(" p "): " p " {" n
 "    " > q n
 "}" >
 :ann "Function with no parameters")

;; kotlin-mode/ifn
(ifn "if (" p " == null) {" n
 "   " > q n
 "}" >
 :ann "Inserts 'if null' expression")

;; kotlin-mode/inn
(inn "if (" p " != null) {" n
 "   " > q n
 "}" >
 :ann "Inserts 'if not null' expression")

;; kotlin-mode/interface
(interface "interface " p " {" n
 "    " > q n
 "}" >
 :ann "Interface")

;; kotlin-mode/iter
(iter "for (" p " in " p ") {" n
 "    " > q n
 "}" >
 :ann "Iterate over elements of iterable (for-in) loop")

;; kotlin-mode/main
(main "fun main(args: Array<String>) {" n
 "    " > q n
 "}" >
 :ann "main() function")

;; kotlin-mode/psvm
(psvm "fun main(args: Array<string>) {" n
 "    " > q n
 "}" >)

;; kotlin-mode/serr
(serr "System.err.println(" q ")" n
 
 :ann "Prints a string to System.err")

;; kotlin-mode/singleton
(singleton "object " p " {" n
 "    " > q n
 "}" >
 :ann "Singleton")

;; kotlin-mode/sout
(sout "println(" q ")"
 :ann "Prints a string to System.out")

;; kotlin-mode/soutv
;; WARNING: Nested fields flattened into field 
(soutv "println(\" = " p "\")" q
 :ann "Prints a value to System.out")

;; kotlin-mode/void
(void "fun " p "(" p ") {" n
 "    " > q n
 "}" >
 :ann "Function returning nothing")


latex-mode

;; latex-mode/acronym
(ac "\\newacronym{" (p "label" f1) "}{" (upcase f1) "}{" (p "Name") "}" q
 :ann "acronym")

;; latex-mode/alertblock
(al "\\begin{alertblock}{" p "}" n
 "        " > q n
 "\\end{alertblock}" >
 :ann "alertblock")

;; latex-mode/alg
(alg "\\begin{algorithmic}" n
 q n
 "\\end{algorithmic}" > n
 )

;; latex-mode/align
;; WARNING: $0 is not the last field; kept as a plain field
(ali "\\begin{align}" n
 "\\label{" p "}" > n
 p n
 "\\end{align}" > n
 
 :ann "align")

;; latex-mode/article
(article "\\documentclass[" (p "options") "]{article}" n
 n
 "\\author{" p "}" > n
 n
 "\\begin{document}" > n
 q n
 "\\end{document}" > n
 
 :ann "full template of article class")

;; latex-mode/begin
(begin "\\begin{" (p "environment" f1) "}" n
 q n
 "\\end{" f1 "}" >)

;; latex-mode/bigcap
(bigcap "\\bigcap" (p "\\limits") "_{" p "}^{" p "}" q)

;; latex-mode/bigcup
(bigcup "\\bigcup" (p "\\limits") "_{" p "}^{" p "}" q)

;; latex-mode/binom
(binom "\\binom{" (p "n") "}{" (p "k") "}" q)

;; latex-mode/block
(bl "\\begin{block}{" p "}" n
 "        " > q n
 "\\end{block}" >
 :ann "block")

;; latex-mode/capgls
(G "\\Gls{" (p "label") "}" q
 :ann "Gls")

;; latex-mode/caption
(ca "\\caption{" q "}"
 :ann "caption")

;; latex-mode/cite
(c "\\cite{" p "} " q
 :ann "cite")

;; latex-mode/code
(code "\\begin{lstlisting}" n
 q n
 "\\end{lstlisting}" >)

;; latex-mode/columns
;; WARNING: $0 is not the last field; kept as a plain field
(cols "\\begin{columns}" n
 "  \\begin{column}{." (p "5") "\\textwidth}" > n
 "  " > p n
 "  \\end{column}" > n
 n
 "  \\begin{column}{." (p "5") "\\textwidth}" > n
 n
 "  \\end{column}" > n
 "\\end{columns}" >
 :ann "columns")

;; latex-mode/corollary
;; WARNING: Fields visited in buffer order (2 1) instead of numeric order
(corr "\\begin{cor}[" (p "title") "]" n
 "  \\label{cor:" (p "label") "}" > n
 "  " > q n
 "\\end{cor}" > n
 
 :ann "corollary")

;; latex-mode/description
(desc "\\begin{description}" n
 "\\item[" p "] " q > n
 "\\end{description}" >
 :ann "description")

;; latex-mode/documentclass
(doc "\\documentclass[" (p "options") "]{" p "}" n
 q n
 
 :ann "documentclass")

;; latex-mode/emph
(e "\\emph{" p "}" q
 :ann "emph")

;; latex-mode/enumerate
(enum "\\begin{enumerate}" n
 "\\item " q > n
 "\\end{enumerate}" >
 :ann "enumerate")

;; latex-mode/equation
;; WARNING: $0 is not the last field; kept as a plain field
(eq "\\begin{equation}" n
 "\\label{" p "}" > n
 p n
 "\\end{equation}" > n
 
 :ann "equation")

;; latex-mode/figure
(fig "\\begin{figure}[ht]" n
 "  \\centering" > n
 "  \\includegraphics[" (p "options") "]{figures/" (p "path.pdf") "}" > n
 "  \\caption{\\label{fig:" (p "label") "} " q "}" > n
 "\\end{figure}" > n
 
 :ann "figure")

;; latex-mode/frac
(frac "\\frac{" (p "numerator") "}{" (p "denominator") "}" q)

;; latex-mode/frame
;; WARNING: Nested fields flattened into field 1
;; WARNING: Nested fields flattened into field 3
(fr "\\begin{frame}" (p "[]") n
 "        " > (p "\\frametitle{") "}" n
 "        " > q n
 "\\end{frame}" >
 :ann "frame")

;; latex-mode/german-quotes
(gqq "\\glqq{}" q "\\grqq{}"
 :ann "German quotes")

;; latex-mode/german-quotes-single
(gq "\\glq{}" q "\\grq{}"
 :ann "German single quotes")

;; latex-mode/gls
(g "\\gls{" (p "label") "}" q
 :ann "gls")

;; latex-mode/glspl
(gp "\\glspl{" (p "label") "}" q
 :ann "glspl")

;; latex-mode/if
(if "\\IF {$" (p "cond") "$}" n
 "    " > q n
 "\\ELSE" > n
 "\\ENDIF" > n
 )

;; latex-mode/includegraphics
;; WARNING: Nested fields flattened into field 1
(ig "\\includegraphics" (p "[]") "{" q "}"
 :ann "includegraphics")

;; latex-mode/int
(int "\\int_{" p "}^{" p "}" q)

;; latex-mode/item
(- "\\item " q
 :ann "item")

;; latex-mode/itemize
(it "\\begin{itemize}" n
 "\\item " q > n
 "\\end{itemize}" >
 :ann "itemize")

;; latex-mode/label
(lab "\\label{" q "}"
 :ann "label")

;; latex-mode/labelcref
(lref "\\labelcref{" q "}"
 :ann "labelcref")

;; latex-mode/left-right
(lr "\\left( " q " \\right)"
 :ann "left and right")

;; latex-mode/lemma
;; WARNING: Fields visited in buffer order (2 1) instead of numeric order
(lem "\\begin{lem}[" (p "title") "]" n
 "  \\label{lem:" (p "label") "}" > n
 "  " > q n
 "\\end{lem}" > n
 
 :ann "lemma")

;; latex-mode/listing
(lst "\\begin{lstlisting}[float,label=lst:" (p "label") ",caption=nextHopInfo: " (p "caption") "]" n
 q n
 "\\end{lstlisting}" >
 :ann "listing")

;; latex-mode/moderncv
(moderncv "\\documentclass[" (p "options") "]{moderncv}" n
 n
 "\\firstname{" p "}" > n
 "\\familyname{" p "}" > n
 n
 "\\moderncvstyle{" (p "casual") "}" > n
 "\\moderncvcolor{" (p "blue") "}" > n
 n
 "\\begin{document}" > n
 q n
 "\\end{document}" > n
 
 :ann "full template of moderncv class")

;; latex-mode/moderncv-cvcomputer
(cvcomp "\\cvcomputer{" (p "category") "}{" (p "programs") "}{" (p "category" f3) "}{" f3 "}" n
 q n
 
 :ann "moderncv-cvcomputer")

;; latex-mode/moderncv-cventry
(cventry "\\cventry{" (p "year") "}{" (p "job") "}{" (p "employer") "}{" (p "city") "}{" (p "description") "}" n
 q n
 
 :ann "moderncv-cventry")

;; latex-mode/moderncv-cvlanguage
(cvlang "\\cvlanguage{" (p "language") "}{" (p "skill-level") "}{" (p "comment") "}" n
 q n
 
 :ann "moderncv-cvlanguage")

;; latex-mode/moderncv-cvline
(cvline "\\cvline{" (p "hobby") "}{" (p "Description") "}" n
 q n
 
 :ann "moderncv-cvline")

;; latex-mode/moderncv-cvlistdoubleitem
(cvditem "\\cvlistdoubleitem{" (p "item") "}{" (p "item") "}" n
 q n
 
 :ann "moderncv-cvlistdoubleitem")

;; latex-mode/moderncv-cvlistitem
(cvitem "\\cvlistitem{" (p "item") "}" n
 q n
 
 :ann "moderncv-cvlistitem")

;; latex-mode/movie
(movie "\\begin{center}" n
 "\\includemovie[" > n
 "  label=test," > n
 "  controls=false," > n
 "  text={\\includegraphics[width=4in]{" (p "image.pdf") "}}" > n
 "]{4in}{4in}{" (p "video file") "}" > n
 n
 "\\movieref[rate=3]{test}{Play Fast}" > n
 "\\movieref[rate=1]{test}{Play Normal Speed}" > n
 "\\movieref[rate=0.2]{test}{Play Slow}" > n
 "\\movieref[resume]{test}{Pause/Resume}" > n
 q)

;; latex-mode/newcommand
;; WARNING: Nested fields flattened into field 2
(cmd "\\newcommand{\\" (p "name") "}" (p "[0]") "{" q "}"
 :ann "newcommand")

;; latex-mode/newglossaryentry
;; WARNING: Nested fields flattened into field 3
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(gl "\\newglossaryentry{" (p "AC") "}{name=" (p "Andrea Crotti") (p ", description=description") "}" q
 :ann "newglossaryentry")

;; latex-mode/note
(no "\\note{" q "}"
 :ann "note")

;; latex-mode/prod
(prod "\\prod_{" p "}^{" p "}" q)

;; latex-mode/python
(py "\\lstset{language=python}" n
 "\\begin[language=python]{lstlisting}" > n
 q n
 "\\end{lstlisting}" >
 :ann "python")

;; latex-mode/question
(q "\\question{" q "}"
 :ann "question")

;; latex-mode/section
(sec "\\section{" (p "name") "}" n
 "\\label{sec:" (p "label") "}" > n
 n
 q
 :ann "section")

;; latex-mode/subf
(sf "\\subfigure[" (p "caption") "]{" n
 "  \\label{fig:" (p "label") "}" > n
 "  \\includegraphics[width=." (p "3") "\\textwidth]{" (p "path") "}}" > n
 q
 :ann "subf")

;; latex-mode/subfigure
(subfig "\\begin{figure}[ht]" n
 "  \\centering" > n
 "  \\subfigure[" p "]" > n
 "  {\\label{fig:" (p "label") "}" > n
 "    \\includegraphics[width=." (p "5") "\\textwidth]{" (p "path") "}}" > n
 n
 "  \\caption{" (p "caption") "}" > n
 "\\label{fig:" (p "label") "}" > n
 "\\end{figure}" > n
 q
 :ann "subfigure")

;; latex-mode/subsec
(sub "\\subsection{" (p "name") "}" n
 "\\label{subsec:" (p "label") "}" > n
 n
 q
 :ann "subsec")

;; latex-mode/sum
(sum "\\sum_{" p "}^{" p "}" q)

;; latex-mode/textbf
(b "\\textbf{" p "}" q
 :ann "textbf")

;; latex-mode/theorem
;; WARNING: Fields visited in buffer order (2 1) instead of numeric order
(thm "\\begin{thm}[" (p "title") "]" n
 "  \\label{thm:" (p "label") "}" > n
 "  " > q n
 "\\end{thm}" > n
 
 :ann "theorem")

;; latex-mode/usepackage
(pkg "\\usepackage{" q "}"
 :ann "usepackage")


ledger-mode

;; ledger-mode/price
(pr "P " (list 'l (format-time-string "%Y-%m-%d")) " " (p "BTC") " " (p "35000") " " (p "EUR") n
 q
 :ann "ledger price")

;; ledger-mode/transaction
(tr (list 'l (format-time-string "%Y-%m-%d")) " " (p "Description") n
 "    " > p (p "Amount") " " (p "EUR") n
 "    " > p n
 q
 :ann "ledger transaction")


lisp-interaction-mode

;; lisp-interaction-mode/defun
(defun "(defun " (p "fun") " (" (p "args") ")" n
 "       " > q n
 ")" >)


lisp-mode

;; lisp-mode/class
;; WARNING: Fields visited in buffer order (1 2 4 3) instead of numeric order
(cls "(defclass " (p "name") " (" (p "inherits") ")" n
 "   (" (p "slot") ")" > n
 "   (:documentation \"" (p "doc") "\"))" > n
 q n
 
 :ann "class")

;; lisp-mode/comment
(/* "#|" (p "type the comment here") "|#" n
 q n
 
 :ann "comment")

;; lisp-mode/cond
(cond "(cond (" (p "case1") " (" (p "do-this") "))" n
 "      (" (p "case2") " (" (p "do-this") "))" > n
 "      (t " (p "default") "))" > n
 q n
 )

;; lisp-mode/defpackage
;; WARNING: Unnumbered field visited before numbered fields
(defp "(defpackage #:" (p "name") n
 "   (:nicknames #:" (p "nick") ")" > n
 "   (:use #:cl #:closer-mop #:" (p "package") ")" > n
 "   (:shadow :" (p "4.symbol") ")" > n
 "   (:shadowing-import-from #:" (p "package") " #:" (p "symbol") ")" > n
 "   (:export :" q "))" > n
 
 :ann "defpackage")

;; lisp-mode/do
(do "(do ((" (p "var1") " " (p "init-form") " " (p "step-form") ")" n
 "     (" (p "var2") " " (p "init-form") " " (p "step-form") "))" > n
 "    (" (p "condition") " " (p "return-value") ")" > n
 "    (" (p "body") "))" > n
 q n
 )

;; lisp-mode/for
(for "(dotimes (" (p "var") " " (p "count-form") ")" n
 "         " > (p "body") ")" n
 q n
 )

;; lisp-mode/foreach
(foreach "(dolist (" (p "var") " " (p "list-form") ")" n
 "        " > (p "body") ")" n
 q n
 )

;; lisp-mode/format
(print "(format t \"~& " q " ~%\")" n
 
 :ann "format")

;; lisp-mode/if
(if "(when (" (p "condition") ")" n
 "      (" (p "then-do-this") "))" > n
 q n
 )

;; lisp-mode/ifelse
(ifelse "(if (" (p "condition") ")" n
 "    (" (p "then") ")" > n
 "    (" (p "else") "))" > n
 q n
 
 :ann "ifelse (...) (...) (...) ...")

;; lisp-mode/ifnot
(ifnot "(unless (" (p "condition") ")" n
 "        (" (p "then-do-this") "))" > n
 q n
 
 :ann "ifnot (...) (...)  ...")

;; lisp-mode/slot
;; WARNING: Fields visited in buffer order (1 3 2) instead of numeric order
(slot "(" (p "name" f1) " :initarg :" (or (let ((str f1)) (save-match-data (if (string-match "[^: ]*" str) (match-string-no-properties 0 str) str))) f1) n
 "           :initform (error \":" (or (let ((str f1)) (save-match-data (if (string-match "[^: ]*" str) (match-string-no-properties 0 str) str))) f1) " must be specified\")" > n
 "           ;; :accessor " (or (let ((str f1)) (save-match-data (if (string-match "[^: ]*" str) (match-string-no-properties 0 str) str))) f1) > n
 "           :reader " (or (let ((str f1)) (save-match-data (if (string-match "[^: ]*" str) (match-string-no-properties 0 str) str))) f1) "-changed" > n
 "           :writer set-" (or (let ((str f1)) (save-match-data (if (string-match "[^: ]*" str) (match-string-no-properties 0 str) str))) f1) > n
 "           :type" > n
 "           :allocation " (p ":class :instance") > n
 "           :documentation \"" (p "about-slot") "\")" > n
 q n
 )

;; lisp-mode/typecast
(typecast "(coerce " (p "object") " " (p "type") ")" n
 q n
 
 :ann "cast")


lua-mode

;; lua-mode/do
(do "do" n
 "	" > q n
 "end" > n
 )

;; lua-mode/eif
(eif "if " p " then" n
 "	" > p n
 "elseif " p " then" > n
 "	" > q n
 "end" > n
 
 :ann "if elseif")

;; lua-mode/eife
(eife "if " p " then" n
 "	" > p n
 "elseif " p " then" > n
 "	" > p n
 "else" > n
 "	" > q n
 "end" > n
 
 :ann "if elseif else")

;; lua-mode/fun
(fun "function (" p ")" n
 "	" > q n
 "end" > n
 )

;; lua-mode/if
(if "if " p " then" n
 "	" > q n
 "end" > n
 )

;; lua-mode/ife
(ife "if " p " then" n
 "	" > p n
 "else" > n
 "	" > q n
 "end" > n
 
 :ann "if else")

;; lua-mode/ipairs
(ipairs "for " (p "idx") ", " (p "val") " in ipairs(" (p "table_name") ") do" n
 "	" > q n
 "end" > n
 
 :ann "for in ipairs")

;; lua-mode/pairs
(pairs "for " (p "name") ", " (p "val") " in pairs(" (p "table_name") ") do" n
 "	" > q n
 "end" > n
 
 :ann "for in pairs")

;; lua-mode/repeat
;; WARNING: $0 is not the last field; kept as a plain field
(repeat "repeat" n
 "	" > p n
 "until " p > n
 
 :ann "repeat util")

;; lua-mode/require
(require "local " p " = require \"" p "\"" n
 q)

;; lua-mode/while
(while "while " p " do" n
 "	" > q n
 "end" > n
 )


m4-mode

;; m4-mode/def
(def "define(`" (p "macro") "',`" (p "subst") "')." n
 q)


makefile-automake-mode

;; makefile-automake-mode/noinst_HEADERS
(noinst "noinst_HEADERS = " q
 :ann "noinst_HEADERS")


makefile-bsdmake-mode

;; makefile-bsdmake-mode/PHONY
(phony ".PHONY: " q
 :ann "PHONY")

;; makefile-bsdmake-mode/echo
(echo "@echo " (p "\"message to output\"") n
 q)

;; makefile-bsdmake-mode/gen
;; WARNING: $0 is not the last field; kept as a plain field
(gen "all: " (p "targets") n
 n
 p n
 n
 "clean:" > n
 "        " > (p "clean actions") n
 )

;; makefile-bsdmake-mode/if
(if "@if [ " (p "cond") " ]" n
 "    then " q > n
 "fi" > n
 )

;; makefile-bsdmake-mode/var
($ "$(" (p "VAR") ")" q
 :ann "var")


makefile-gmake-mode

;; makefile-gmake-mode/abspath
;; WARNING: Nested fields flattened into field 1
(abs "$(abspath " (p "$(paths)") ")" q
 :ann "abspath")

;; makefile-gmake-mode/addprefix
;; WARNING: Nested fields flattened into field 1
;; WARNING: Nested fields flattened into field 3
(ap "$(addprefix " (p "$(dir)/") "," (p "$(items)") ")" q
 :ann "addprefix")

;; makefile-gmake-mode/addsuffix
;; WARNING: Nested fields flattened into field 2
(as "$(addsuffix " (p ".suffix") "," (p "$(items)") ")" q
 :ann "addsuffix")

;; makefile-gmake-mode/dir
;; WARNING: Nested fields flattened into field 1
(d "$(dir " (p "$(paths)") ")" q
 :ann "dir")

;; makefile-gmake-mode/make
(make "$(MAKE) --directory=" (p "$@") n
 q)

;; makefile-gmake-mode/notdir
;; WARNING: Nested fields flattened into field 1
(nd "$(notdir " (p "$(paths)") ")" q
 :ann "notdir")

;; makefile-gmake-mode/patsubst
(ps "$(patsubst " (p "from") "," (p "to") "," (p "src") ")" q
 :ann "patsubst")

;; makefile-gmake-mode/phony
(ph ".PHONY = " q
 :ann "phony")

;; makefile-gmake-mode/shell
(sh "$(shell " (p "command") ")" q
 :ann "shell")

;; makefile-gmake-mode/special
;; WARNING: Field transformation of field 1 is only evaluated on expansion
(\. "." `(p ,(upcase "PHONY")) ": " q
 :ann "special targets")

;; makefile-gmake-mode/template
;; WARNING: Field transformation of field 1 is only evaluated on expansion
;; WARNING: Nested fields flattened into field 3
;; WARNING: $0 is not the last field; kept as a plain field
(template "define " `(p ,(upcase "PROGRAM") f1) "_template" n
 p n
 "endef" > n
 n
 "$(foreach " `(p ,"" f2) ",$(" (p "PROGRAMS") "),$(eval $(call " f1 "_template,$(" f2 "))))" >)

;; makefile-gmake-mode/wildcard
(wl "$(wildcard " q ")"
 :ann "wildcard")


makefile-mode

;; makefile-mode/all
(all "all:" n
 "        " > q)

;; makefile-mode/clean
;; WARNING: Nested fields flattened into field 1
(cl (p (current-column) col noinsert) "clean:" n
 (ignore (indent-to col)) "	" (p "rm -r $(OUTDIR)") n
 q n
 
 :ann "clean")


malabar-mode

;; malabar-mode/variable
(var (p "int") " " (p "n") " = " q ";"
 :ann "variable")


markdown-mode markdown-ts-mode

;; markdown-mode/back-quote
(\` "`" (p "Code") "` " q
 :ann "Inline Code")

;; markdown-mode/code
(code "```" p n
 q n
 "```" > n
 
 :ann "Code block")

;; markdown-mode/emphasis
(_ "_" (p "Text") "_ " q
 :ann "Emphasis")

;; markdown-mode/h1.1
(h1 "# " (p "Header 1") " #" n
 n
 q
 :ann "Header 1 (#)")

;; markdown-mode/h1.2
;; WARNING: Duplicate key ‘h1’ renamed to ‘h1-2’
(h1-2 (p "Header 1" f1) n>
 (make-string (string-width f1) 61) n
 n
 q
 :ann "Header 1 (=)")

;; markdown-mode/h2.1
(h2 "## " (p "Header 1") " ##" n
 n
 q
 :ann "Header 2 (##)")

;; markdown-mode/h2.2
;; WARNING: Duplicate key ‘h2’ renamed to ‘h2-2’
(h2-2 (p "Header 2" f1) n>
 (make-string (string-width f1) 45) n
 n
 q
 :ann "Header 2 (-)")

;; markdown-mode/h3
(h3 "### " (p "Header 3") " ###" n
 n
 q
 :ann "Header 3")

;; markdown-mode/h4
(h4 "#### " (p "Header 4") " ####" n
 n
 q
 :ann "Header 4")

;; markdown-mode/h5
(h5 "##### " (p "Header 5") " #####" n
 n
 q
 :ann "Header 5")

;; markdown-mode/h6
(h6 "###### " (p "Header 6") " ######" n
 n
 q
 :ann "Header 6")

;; markdown-mode/highlight
(highlight "{% highlight " (p "language") " %}" n>
 q "content" n
 "{% endhighlight %}" >
 :ann "Highlight")

;; markdown-mode/hr.1
(hr "----------" n
 n
 q
 :ann "Horizontal Rule (-)")

;; markdown-mode/hr.2
;; WARNING: Duplicate key ‘hr’ renamed to ‘hr-2’
(hr-2 "*******" n
 n
 q
 :ann "Horizontal Rule (*)")

;; markdown-mode/hyphen
(- "- " (p "Text") n
 "- " q > n
 
 :ann "Unordered List -")

;; markdown-mode/img
(img "![" (p "Alt Text") "](" (p "URL") " " p ") " q
 :ann "Image")

;; markdown-mode/link
(link "[" (p "Link Text") "](" (p "URL") " " p ") " q
 :ann "Link")

;; markdown-mode/ordered-list
(ol (p "1" f1) ". " (p "Text") n>
 (number-to-string (1+ (string-to-number f1))) ". " q
 :ann "Ordered List")

;; markdown-mode/plus
(+ "+ " (p "Text") n
 "+" q >
 :ann "Unordered List +")

;; markdown-mode/rimg
(rimg "![" (p "Alt Text") "][" p "] " q
 :ann "Referenced Image")

;; markdown-mode/rlb
(rlb "[" (p "Reference") "]: " (p "URL") " " p n
 q
 :ann "Reference Label")

;; markdown-mode/rlink
(rlink "[" (p "Link Text") "][" p "] " q
 :ann "Reference Link")

;; markdown-mode/strong-emphasis
(__ "**" (p "Text") "** " q
 :ann "Strong emphasis")

;; markdown-mode/utf8
(utf8 "<meta http-equiv='Content-Type' content='text/html; charset=utf-8' />" n
 q
 :ann "UTF-8 encoding")


nasm-mode

;; nasm-mode/function_doxygen_doc
(doxy ";;" n
 ";; @brief      " (p "function description") > n
 ";;" > n
 ";; @details    " (p "detailed description") > n
 ";;" > n
 ";; @param      " (p "param") > n
 ";;" > n
 ";; @return     " (p "return") > n
 ";;" q >
 :ann "Assembly Function Doc")


ned-mode

;; ned-mode/chan
(chan "channel Channel extends " (p "ned.DelayChannel") " {" n
 "        " > q n
 "}" > n
 )

;; ned-mode/connections
(conn "connections" (p " allowunconnected") ":" n
 "                " > q
 :ann "connections")

;; ned-mode/for
(for "for " (p "i") "=" (p "0") ".." (p "sizeof(port)-1") " {" n
 "    " > q n
 "}" >)

;; ned-mode/import
(imp "import ned." (p "Package") ";" q
 :ann "import")

;; ned-mode/network
(net "network " (p "Name") n
 "{" > n
 "        submodules:" > n
 "           " > p n
 "        connections:" > n
 "           " > p n
 "}" q >
 :ann "network")

;; ned-mode/simple
;; WARNING: Nested fields flattened into field 2
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(simple "simple " (p "Component") (p " extends Component") n
 "{" > n
 "    " > q n
 "}" >)

;; ned-mode/submodules
(sub "submodules:" n
 "   " > q
 :ann "submodules")


nesc-mode

;; nesc-mode/TOSSIM
(tossim "#ifndef TOSSIM" n
 "        " > q n
 "#endif" >
 :ann "TOSSIM")

;; nesc-mode/command
(command "command " (p "void") " " (p "naMe") "(" p ") {" n
 n
 "}" q >)

;; nesc-mode/dbg
;; WARNING: Nested fields flattened into field 3
(dbg "dbg(\"" (p "Module") "\", \"" (p "message") "\"" (p ", var list") ");" q)

;; nesc-mode/event
(event "event " (p "void") " " (p "On.Event") "(" p ") {" n
 "      " > q n
 "}" >)

;; nesc-mode/ifdef
(ifdef "#ifdef " (p "Macro") n
 " " > p n>
 (p "#else") n
 " " > p n
 "#endif" q >)

;; nesc-mode/interface
(int "interface " (p "Interface") " {" n
 "          " > q n
 "}" >
 :ann "interface")

;; nesc-mode/module
;; WARNING: Nested fields flattened into field 2
(mod "module " (p "Module") " {" n
 "       " > (p "uses interface Packet") ";" n
 "       " > q n
 "}" >
 :ann "module")

;; nesc-mode/nx
(nx "nx_uint" (p "8") "_t " (p "var") ";" n
 q)

;; nesc-mode/provides
(provides "provides interface " (p "Interface") ";" q)

;; nesc-mode/sim
(sim "#ifdef TOSSIM" n
 "       " > q n
 "#endif" >)

;; nesc-mode/uint8_t
(u8 "uint8_t " (p "var") ";" n
 q
 :ann "uint8_t")

;; nesc-mode/uses
;; WARNING: Nested fields flattened into field 2
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(uses "uses interface " (p "Interface") (p " as alias") ";" n
 q)


nim-mode

;; nim-mode/array
(array "array[" (p "length") ", " (p "type") "]" q)

;; nim-mode/block
(block "block " (p "name") ":" n
 "  " > q)

;; nim-mode/case
(case "case " (p "value") n
 q)

;; nim-mode/for
(for "for " (p "index") " in " (p "sequence") ":" n
 "  " > q)

;; nim-mode/function
(func "func " (p "name") "(" (p "arguments") "): " (p "return type") " =" n
 "  " > q)

;; nim-mode/if
(if "if " (p "expression") ":" n
 "  " > q)

;; nim-mode/import
(import "import " (p "module"))

;; nim-mode/import from
(from "from " (p "module") " import " (p "field"))

;; nim-mode/iterator
(iterator "iterator " (p "name") "(" (p "arguments") "): " (p "return type"))

;; nim-mode/macro
(macro "macro " (p "name") "(" (p "arguments") "): " (p "return type") " =" n
 "  " > q)

;; nim-mode/method
(method "method " (p "name") "(" (p "arguments") "): " (p "return type") " =" n
 "  " > q)

;; nim-mode/of
(of "of " (p "value") ":" n
 "  " > q)

;; nim-mode/pragma
(pr "{." (p "name") ".}" q)

;; nim-mode/procedure
(proc "proc " (p "name") "(" (p "arguments") "): " (p "return type") " =" n
 "  " > q)

;; nim-mode/sequence
(seq "seq[" (p "type") "]" q)

;; nim-mode/template
(template "template " (p "name") "(" (p "arguments") "): " (p "return type") " =" n
 "  " > q)

;; nim-mode/try-except
;; WARNING: $0 is not the last field; kept as a plain field
(try "try:" n
 "  " > p n
 "except " (p "exception") ":" > n
 "  " >)

;; nim-mode/while
(while "while " (p "expression") ":" n
 "  " > q)


nix-mode nix-ts-mode

;; nix-mode/buildPhase
(bp "buildPhase= ''" n
 "  runHook preBuild" > n
 p n
 "  runHook postBuild" > n
 "'';" > n
 q
 :ann "buildPhase")

;; nix-mode/callPackage
(ca "  " (s f1) " = callPackage ../" p "/" f1 " { };" n
 q
 :ann "callPackage")

;; nix-mode/checkPhase
(ch "checkPhase= ''" n
 "  runHook preCheck" > n
 p n
 "  runHook postCheck" > n
 "'';" > n
 q
 :ann "checkPhase")

;; nix-mode/configurePhase
(cp "configurePhase= ''" n
 "  runHook preConfigure" > n
 p n
 "  runHook postConfigure" > n
 "'';" > n
 q
 :ann "configurePhase")

;; nix-mode/distPhase
(dp "distPhase= ''" n
 "  runHook preDist" > n
 p n
 "  runHook postDist" > n
 "'';" > n
 q
 :ann "distPhase")

;; nix-mode/doCheck
(dc "doCheck = " (p (completing-read "Choose: " (flatten-tree (list '("stdenv.buildPlatform.canExecute stdenv.hostPlatform;" "false;"))))) n
 q
 :ann "doCheck")

;; nix-mode/fixupPhase
(fp "fixupPhase= ''" n
 "  runHook preFixup" > n
 p n
 "  runHook postFixup" > n
 "'';" > n
 q
 :ann "fixPhase")

;; nix-mode/installCheckPhase
(ic "installCheckPhase= ''" n
 "  runHook preInstallCheck" > n
 p n
 "  runHook postInstallCheck" > n
 "'';" > n
 q
 :ann "installCheckPhase")

;; nix-mode/installPhase
;; WARNING: Multi-line default of field 1 is not re-indented
(ip "installPhase= ''" n
 "  runHook preInstall" > n>
 (p "    mkdir -p $out/bin\n    for f in $(find . -executable -type f);\n    do\n      cp $f $out/bin/\n    done") n
 "  runHook postInstall" > n
 "'';" > n
 q
 :ann "installPhase")

;; nix-mode/package_github
;; WARNING: Nested fields flattened into field 9
(pg "{ lib" n
 ", stdenv" > n
 ", fetchFromGitHub" > n
 ", " (s f1) > n
 ", " (s f2) > n
 "}:" > n
 n
 "stdenv.mkDerivation rec {" > n
 "  pname = \"" (s f3) "\";" > n
 "  version = \"" p "\";" > n
 n
 "  src = fetchFromGitHub {" > n
 "    owner = \"" (s f5) "\";" > n
 "    repo = " (p "pname") ";" > n
 "    rev = \"" (p "v${version}") "\";" > n
 "    sha256 = \"" (p "${lib.fakeSha256}") "\";" > n
 "  };" > n
 n
 "  nativeBuildInputs = [ " (replace-regexp-in-string "  *" " " (subst-char-in-string 44 32 f1)) " ];" > n
 "  buildInputs = [" > n
 "    " > (replace-regexp-in-string "  *" " " (subst-char-in-string 44 32 f2)) n
 "  ];" > n
 n
 "  meta = with lib; {" > n
 "    homepage = \"https://" (p "github.com//") "\";" > n
 "    description = \"" p "\";" > n
 "    longDescription = ''" > n
 "      " > p n
 "    '';" > n
 "    license = licenses." (p (completing-read "Choose: " (flatten-tree (list '("agpl3" "asl20" "bsd1" "bsd2" "bsd3" "free" "gpl2" "gpl2Only" "gpl2Plus" "gpl3" "gpl3Only" "gpl3Plus" "isc" "lgpl21Only" "lgpl21Plus" "lgpl2Only" "lgpl2Plus" "lgpl3" "lgpl3Only" "mit" "mpl20" "ofl" "unfree")))) f12) ";" > n
 "    maintainers = with maintainers; [ " f12 " ];" > n
 "    platforms = platforms." (p (completing-read "Choose: " (flatten-tree (list '("all" "allBut" "arm" "cygwin" "darwin" "freebsd" "gnu" "i686" "illumos" "linux" "mesaPlatforms" "mips" "netbsd" "none" "openbsd" "unix" "x86"))))) ";" > n
 "  };" > n
 "}" > n
 q
 :ann "package github")

;; nix-mode/package_url
(pu "{ lib" n
 ", stdenv" > n
 ", fetchurl" > n
 (s f1) n
 (s f2) n
 "}:" > n
 n
 "stdenv.mkDerivation rec {" > n
 "  pname = \"" p "\";" > n
 "  version = \"" p "\";" > n
 n
 "  src = fetchurl {" > n
 "    url = \"" p "\";" > n
 "    sha256 = \"" (p "${lib.fakeSha256}") "\";" > n
 "  };" > n
 n
 "  nativeBuildInputs = [ " (replace-regexp-in-string "  *" " " (subst-char-in-string 44 32 f1)) " ];" > n
 "  buildInputs = [" > n
 "    " > (replace-regexp-in-string "  *" " " (subst-char-in-string 44 32 f2)) n
 "  ];" > n
 n
 "  meta = with lib; {" > n
 "    homepage = \"https://" p "\";" > n
 "    description = \"" p "\";" > n
 "    longDescription = ''" > n
 "      " > p n
 "    '';" > n
 "    license = licenses." (p (completing-read "Choose: " (flatten-tree (list '("agpl3" "asl20" "bsd1" "bsd2" "bsd3" "free" "gpl2" "gpl2Only" "gpl2Plus" "gpl3" "gpl3Only" "gpl3Plus" "isc" "lgpl21Only" "lgpl21Plus" "lgpl2Only" "lgpl2Plus" "lgpl3" "lgpl3Only" "mit" "mpl20" "ofl" "unfree"))))) ";" > n
 "    maintainers = with maintainers; [ " p " ];" > n
 "    platforms = platforms." (p (completing-read "Choose: " (flatten-tree (list '("all" "allBut" "arm" "cygwin" "darwin" "freebsd" "gnu" "i686" "illumos" "linux" "mesaPlatforms" "mips" "netbsd" "none" "openbsd" "unix" "x86"))))) ";" > n
 "  };" > n
 "}" > n
 q
 :ann "package url")

;; nix-mode/passthru_update
;; WARNING: Duplicate key ‘pu’ renamed to ‘pu-2’
(pu-2 "passthru.updateScript = nix-update-script { };" n
 
 :ann "passthru_update")

;; nix-mode/patchPhase
(pp "patchPhase = ''" n
 "  runHook prePatch" > n
 "  " > p n
 "  runHook postPatch" > n
 "'';" > n
 q
 :ann "patchPhase")

;; nix-mode/phases
(ph "   phases=\"" (p "$prePhases unpackPhase patchPhase $preConfigurePhases configurePhase $preBuildPhases buildPhase checkPhase $preInstallPhases installPhase fixupPhase installCheckPhase $preDistPhases distPhase $postPhases") "\";" n
 q
 :ann "phases")

;; nix-mode/pkgs_mkshell
;; WARNING: Fields visited in buffer order (1 3 2 4) instead of numeric order
(pms "pkgs." (p (completing-read "Choose: " (flatten-tree (list '("mkShell" "mkShellNoCC"))))) " {" n
 "  inputsFrom = [ " p " ];" > n
 n
 "  packages = [ " p " ];" > n
 n
 "  shellHook = ''" > n
 "  " > p n
 "  '';" > n
 "}" q >
 :ann "pkgs_mkshell")

;; nix-mode/unpackPhase
(up "unpackPhase = ''" n
 "  runHook preUnpack" > n
 "  " > p n
 "  runHook postUnpack" > n
 "'';" > n
 q n
 
 :ann "unpackPhase")


nsis-mode

;; nsis-mode/define
(def "!define " (p "CONSTANT") " " (p "value")
 :ann "define")

;; nsis-mode/function
(fun "Function " (p "Name") n
 "         " > q n
 "FunctionEnd" >
 :ann "function")

;; nsis-mode/if
;; WARNING: Unnumbered field visited before numbered fields
;; WARNING: $0 is not the last field; kept as a plain field
(if (p "IF") " " (p "cond") n
 "      " > p n>
 (p "ElseIf") " " (p "else_cond") n
 n>
 (p "EndIf"))

;; nsis-mode/include
(inc "!include \"" (p "Library.nsh") "\"" q
 :ann "include")

;; nsis-mode/insert_macro
(im "!insertmacro " (p "Name") " " (p "\"args\"") n
 q
 :ann "insert_macro")

;; nsis-mode/instdir
($ "$INSTDIR"
 :ann "instdir")

;; nsis-mode/macro
(macro "!macro " (p "Name") " UN" n
 q n
 n
 "!macroend" >)

;; nsis-mode/message
(msg "MessageBox MB_OK \"" (p "hello") "\"" q
 :ann "message")

;; nsis-mode/outdir
;; WARNING: Duplicate key ‘$’ renamed to ‘$-2’
($-2 "$OUTDIR"
 :ann "outdir")

;; nsis-mode/outfile
(out "outFile \"" (p "setup") ".exe\"" q
 :ann "outfile")

;; nsis-mode/section
(sec "Section \"" (p "Program") "\"" n
 "        " > q n
 "SectionEnd" >
 :ann "section")


nxml-mode html-mode html-ts-mode web-mode

;; nxml-mode/body
(body "<body" p ">" n
 "  " > q n
 "</body>" >
 :ann "<body>...</body>")

;; nxml-mode/br
(br "<br />"
 :ann "<br />")

;; nxml-mode/doctype
(doctype "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"
 :ann "DocType XHTML 1.1")

;; nxml-mode/doctype_xhtml1_strict
(doctype_xhtml1_strict "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
 :ann "DocType XHTML 1.0 Strict")

;; nxml-mode/doctype_xhtml1_transitional
(doctype_xhtml1_transitional "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
 :ann "DocType XHTML 1.0 Transitional")

;; nxml-mode/form
(form "<form method=\"" p "\" action=\"" p "\">" n
 "  " > q n
 "</form>" >)

;; nxml-mode/href
(a "<a href=\"" p "\">" p "</a>" q
 :ann "<a href=\"...\">...</a>")

;; nxml-mode/html
(html "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"" (p "en") "\" lang=\"" (p "en") "\">" n
 "  " > q n
 "</html>" >
 :ann "<html xmlns=\"...\">...</html>")

;; nxml-mode/img
(img "<img src=\"" p "\" alt=\"" p "\" />" q
 :ann "<img src=\"...\" alt=\"...\" />")

;; nxml-mode/input
(input "<input type=\"" p "\" name=\"" p "\" value=\"" p "\" />" q
 :ann "<input ... />")

;; nxml-mode/link
(link "<link rel=\"" (p "stylesheet") "\" href=\"" (p "url") "\" type=\"" (p "text/css") "\" media=\"" (p "screen") "\" />" q
 :ann "<link stylesheet=\"...\" />")

;; nxml-mode/meta
(meta "<meta name=\"" (p "generator") "\" content=\"" (p "content") "\" />" q
 :ann "<meta name=\"...\" content=\"...\" />")

;; nxml-mode/name
(name "<a name=\"" p "\"></a>" q
 :ann "<a name=\"...\"></a>")

;; nxml-mode/quote
(quote "<blockquote>" n
 "  " > p n
 "</blockquote>" q >
 :ann "<blockquote>...</blockquote>")

;; nxml-mode/style
(style "<style type=\"text/css\" media=\"" (p "screen") "\">" n
 "  " > q n
 "</style>" >
 :ann "<style type=\"text/css\" media=\"...\">...</style>")

;; nxml-mode/tag
(t "<" (p "tag" f1) ">" p "</" f1 ">" q
 :ann "<tag>...</tag>")

;; nxml-mode/tag_closing
;; WARNING: Duplicate key ‘t’ renamed to ‘t-2’
(t-2 "<" p " " p " />" q
 :ann "<tag.../>")

;; nxml-mode/tag_newline
(tn "<" (p "tag" f1) ">" n
 "  " > p n
 "</" f1 ">" q >
 :ann "<tag> \\n...\\n</tag>")


octave-mode

;; octave-mode/for
(for "for " (p "var") " = " (p "expr") n
 "    " > q n
 "endfor" >)

;; octave-mode/function
(fun "function " (p "return_val") " = " (p "fname") "(" (p "args") ")" n
 "          " > q n
 "endfunction" >
 :ann "function")

;; octave-mode/if
;; WARNING: Multi-line default of field 2 is not re-indented
;; WARNING: Nested fields flattened into field 2
;; WARNING: $0 is not the last field; kept as a plain field
(if "if " (p "cond") n
 "   " > p n>
 (p "else\n        other") n
 "endif" >)


odin-mode

;; odin-mode/case
;; WARNING: Field transformation of field 1 is only evaluated on expansion
(case "case " `(p ,(format "%s" (or (indent-according-to-mode) ""))) ":" n
 "	" > q)

;; odin-mode/dfri
(dfri "defer if " p " {" n>
 q n
 "}" >
 :ann "defer if")

;; odin-mode/distinct
(distinct (p "name") " :: distinct {" n>
 q n
 "}" >)

;; odin-mode/enum
(enum (p "name") " :: enum {" n>
 q n
 "}" >)

;; odin-mode/fd
(fd "for " p "; " p "; " p " do " q " "
 :ann "for do")

;; odin-mode/for
(for "for " p "; " p "; " p " {" n>
 q n
 "}" >)

;; odin-mode/ford
;; WARNING: Field transformation of field 1 is only evaluated on expansion
;; WARNING: Requires YASnippet function ‘yas-skip-and-clear-field’
(ford `(p ,(format "%s" (or (dlet ((yas-text "Directive(TAB to skip)")) (if (and (not nil) nil) (yas-skip-and-clear-field) nil)) ""))) " for " (p " ") " {" n>
 q n
 "}" >
 :ann "for (with directive)")

;; odin-mode/fori
(fori "for " p " in " p " {" n>
 q n
 "}" >
 :ann "for in")

;; odin-mode/if
(if "if " p " {" n>
 q n
 "}" >)

;; odin-mode/ife
;; WARNING: Field transformation of field 1 is only evaluated on expansion
;; WARNING: Requires YASnippet function ‘yas-skip-and-clear-field’
(ife "if " `(p ,(format "%s" (or (dlet ((yas-text " ")) (if (and (not nil) nil) (yas-skip-and-clear-field) nil)) ""))) "; " (p " ") " {" n
 "	" > p n
 "} else " q >
 :ann "if else")

;; odin-mode/ifz
(ifz "if " p "; " p " {" n>
 q n
 "}" >
 :ann "if (with initialization)")

;; odin-mode/proc
(pr (p "name") " :: proc(" p ") -> " p " {" n>
 q n
 "}" >
 :ann "proc")

;; odin-mode/struct
(struct (p "name") " :: struct {" n>
 q n
 "}" >)

;; odin-mode/sw
(sw "switch " p ";" p " {" n>
 q n
 "}" > n
 
 :ann "switch")

;; odin-mode/swp
(swp "#partial switch " p ";" p " {" n>
 q n
 "}" > n
 
 :ann "partial switch")

;; odin-mode/union
(union (p "name") " :: union {" n>
 q n
 "}" >)

;; odin-mode/when
(when "when " p " {" n>
 q n
 "}" >)

;; odin-mode/whene
;; WARNING: Field transformation of field 2 is only evaluated on expansion
(whene "when " p " {" n>
 `(p ,(format "%s" (or (indent-according-to-mode) ""))) n
 "} else {" > n>
 q n
 "}" >
 :ann "when else")


org-mode

;; org-mode/author
(<au "#+author: " q
 :ann "author")

;; org-mode/center
(<c "#+begin_center" n
 q n
 "#+end_center" >
 :ann "center")

;; org-mode/date
(<da "#+date: " (p "year") ":" (p "month") ":" (p "day")
 :ann "date")

;; org-mode/description
(desc "#+description: " q
 :ann "description")

;; org-mode/dot
;; WARNING: $0 is not the last field; kept as a plain field
(dot_ "#+begin_src dot :file " (p "file") " :cmdline -t" (p "pdf") " :exports none :results silent" n
 p n
 "#+end_src" > n
 "[[file:" (p "path") "]]" >
 :ann "dot")

;; org-mode/elisp
(elisp_ "#+begin_src emacs-lisp :tangle yes" n
 q n
 "#+end_src" >
 :ann "elisp")

;; org-mode/emacs-lisp
(emacs-lisp_ "#+begin_src emacs-lisp :tangle yes" n
 q n
 "#+end_src" >
 :ann "emacs-lisp")

;; org-mode/email
(<em "#+email: " q
 :ann "email")

;; org-mode/embedded
;; WARNING: Nested fields flattened into field 2
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(emb_ "src_" (p "lang") (p "[where]") "{" (p "code") "}" q
 :ann "embedded")

;; org-mode/entry
(entry_ "#+begin_html" n
 "---" > n
 "layout: " (p "default") > n
 "title: " (p "title") > n
 "---" > n
 "#+end_html" > n
 q
 :ann "entry")

;; org-mode/exampleblock
(<e "#+begin_example" n
 q n
 "#+end_example" >
 :ann "example")

;; org-mode/export
(<ex "#+begin_export " (p "type") n
 q n
 "#+end_export" >
 :ann "export")

;; org-mode/figure
(fig_ "#+caption: " (p "caption") n
 "#+attr_latex: " (p "scale=0.75") > n
 "#+label: fig:" (p "label") >
 :ann "figure")

;; org-mode/html
(<ht "#+html:" p
 :ann "html")

;; org-mode/image
(<im "#+caption: " (p "caption of the image") n
 "[[file:" (p "image_path") "]]" q >
 :ann "image")

;; org-mode/img
(img_ "<img src=\"" p "\" alt=\"" p "\" align=\"" (p "left") "\" title=\"" (p "image title") "\" class=\"img\" " p "/>" q
 :ann "img")

;; org-mode/include
(<i "#+include: " q
 :ann "include")

;; org-mode/ipython
;; WARNING: Field transformation of field 2 is only evaluated on expansion
(ipy_ "#+begin_src ipython :session " (p "session01") " :file " `(p ,(concat (make-temp-name "./ipython-") ".png")) " :exports " (p "both") n
 q n
 "#+end_src" >
 :ann "ipython")

;; org-mode/keywords
(<ke "#+keywords: " q
 :ann "keywords")

;; org-mode/language
(<lan "#+language: " (p "en")
 :ann "language")

;; org-mode/link
(<li "[[" (p "link") "][" (p "description") "]]" n
 q
 :ann "link")

;; org-mode/matrix
(matrix_ "\\left (" n
 "\\begin{array}{" (p "ccc") "}" > n>
 (p "v1 & v2") " \\" n
 q n
 "\\end{array}" > n
 "\\right )" >
 :ann "matrix")

;; org-mode/options
(<op "#+options: h:" (p "1") " num:" (p "t||nil") " toc:" (p "t||nil")
 :ann "options")

;; org-mode/python
(py_ "#+begin_src python" n
 q n
 "#+end_src" >
 :ann "python")

;; org-mode/quote
(<q "#+begin_quote" n
 q n
 "#+end_quote" >
 :ann "quote")

;; org-mode/rv_background
(<rsb ":properties:" n
 ":reveal_background: " (p " #123456") > n
 ":end:" q >
 :ann "reveal_single_colored_background")

;; org-mode/rv_image_background
(<rib "    :properties:" n
 "    :reveal_background: " (p " path of the image") > n
 "    :reveal_background_trans: " (p " default||cube||page||concave||zoom||linear||fade||none||slide") > n
 "    :end:" q >
 :ann "reveal_image_background")

;; org-mode/setup
(set "#+setupfile: " q
 :ann "setup")

;; org-mode/src
(<src "#+begin_src " p n
 "  " > q n
 "#+end_src" > n
 
 :ann "src")

;; org-mode/style
(<st "#+style: <link rel=\"stylesheet\" type=\"text/css\" href=\"" p "\" />" q
 :ann "style")

;; org-mode/table
(<ta "#+caption: " (p " caption of the table") n
 "|" (p "column 1") " | " (p " column 2") " |" > n
 "|--------------+----------------|" > n
 q
 :ann "table")

;; org-mode/title
(<ti "#+title: " q
 :ann "title")

;; org-mode/uml
(uml "#+begin_uml" n
 p n
 "#+end_uml" q >)

;; org-mode/uuid
(uuid_ (list 'l (format "%s" (or (org-id-uuid) "")))
 :ann "generate uuid")

;; org-mode/verse
(<v "#+begin_verse" n
 q n
 "#+end_verse" >
 :ann "verse")

;; org-mode/video
(<vi "[[" (p "link to the video") "][file:" (p "link of the image") "]" n
 q
 :ann "video")


perl-mode cperl-mode

;; perl-mode/bang
(! "#!/usr/bin/env perl" n
 q n
 
 :ann "bang")

;; perl-mode/enc
(enc "binmode STDOUT, ':encoding(UTF-8)';" n
 "binmode STDERR, ':encoding(UTF-8)';" > n
 q n
 
 :ann "encoding")

;; perl-mode/eval
(eval "eval {" n
 "    " > (p "# do something risky...") n
 "};" > n
 "if ($@) {" > n
 "    " > (p "# handle failure...") n
 "}" q >
 :ann "eval { ... } if ($@) { ... }")

;; perl-mode/for
(for "for (my $" (p "var" f1) " = 0; $" f1 " < " (p "expression") "; $" f1 "++) {" n
 "    " > (p "# body...") n
 "}" q >
 :ann "for (...) { ... }")

;; perl-mode/fore
(fore "foreach my $" (p "x") " (@" (p "array") ") {" n
 "    " > (p "# body...") n
 "}" q >
 :ann "foreach ... { ... }")

;; perl-mode/if
(if "if (" p ") {" n
 "    " > q n
 "}" >
 :ann "if (...) { ... }")

;; perl-mode/ife
(ife "if (" p ") {" n
 "    " > p n
 "} else {" > n
 "    " > p n
 "}" q >
 :ann "if (...) { ... } else { ... }")

;; perl-mode/ifee
(ifee "if (" p ") {" n
 "	" > (p "# body...") n
 "} elsif (" p ") {" > n
 "	" > (p "# elsif...") n
 "} else {" > n
 "	" > (p "# else...") n
 "}" q >
 :ann "if, elsif, else ...")

;; perl-mode/strict
(strict "use warnings;" n
 "use strict;" > n
 q n
 )

;; perl-mode/sub
(sub "sub " (p "function_name") " {" n
 "    " > q n
 "}" >
 :ann "sub ... { ... }")

;; perl-mode/unless
(unless "unless (" p ") {" n
 "    " > q n
 "}" >
 :ann "unless (...) { ... }")

;; perl-mode/while
(while "while (" p ") {" n
 "    " > q n
 "}" >
 :ann "while (...) { ... }")

;; perl-mode/xfore
(xfore (p "expression") " foreach @" (p "array") ";" q
 :ann "... foreach ...")

;; perl-mode/xif
(xif (p "expression") " if " (p "condition")
 :ann "... if ...")

;; perl-mode/xunless
(xunless (p "expression") " unless " (p "condition")
 :ann "... unless ...")

;; perl-mode/xwhile
(xwhile (p "expression") " while " (p "condition") ";" q
 :ann "... while ...")


php-mode php-ts-mode

;; php-mode/__call
(__call "public function __call($name, $args)" n
 "{" > n
 "    " > q n
 "}" >
 :ann "__call()")

;; php-mode/__callStatic
(__callStatic "public function __callStatic($name, $args)" n
 "{" > n
 "    " > q n
 "}" >
 :ann "__callStatic()")

;; php-mode/catch
;; WARNING: Multi-line default of field 3 is not re-indented
(catch "catch (" (p "\\PDOException") " " (p "$e") ") {" n
 "    " > (p "\n    throw $e;") n
 "}" q >)

;; php-mode/cls
;; WARNING: Requires YASnippet function ‘yas-php-get-class-name-by-file-name’
(cls "class " `(p ,(format "%s" (or (yas-php-get-class-name-by-file-name) ""))) n
 "{" > n
 "  " > q n
 "}" >
 :ann "class")

;; php-mode/clse
;; WARNING: Requires YASnippet function ‘yas-php-get-class-name-by-file-name’
(clse "class " `(p ,(format "%s" (or (yas-php-get-class-name-by-file-name) ""))) " extends " (p "ParentClass") n
 "{" > n
 "    " > q n
 "}" >
 :ann "class extends")

;; php-mode/clsi
;; WARNING: Requires YASnippet function ‘yas-php-get-class-name-by-file-name’
(clsi "class " `(p ,(format "%s" (or (yas-php-get-class-name-by-file-name) ""))) " implements " (p "interface") n
 "{" > n
 "    " > q n
 "}" >
 :ann "class implements")

;; php-mode/const
(const "const " (p "NAME") " = " (p "value") ";" q)

;; php-mode/define
(define "define('" (p "NAME") "', " (p "value") ");" q)

;; php-mode/doc-comment-multiline
(/** "/**" n
 " * " (p "Summary") > n
 " *" > n
 " *" q > n
 " */" >
 :ann "PHPDoc (multi-line)")

;; php-mode/doc-comment-oneline
(* "/** " (p "Summary") " */" n
 q
 :ann "Doc comment (one-line)")

;; php-mode/else
(else "else {" n
 "    " > q n
 "}" >)

;; php-mode/elseif
(elseif "elseif (" (p "condition") ") {" n
 "    " > q n
 "}" >)

;; php-mode/enum
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(enum "enum " (p "Name") (p ": type") " {" n
 "    case: " (p "One") ";" > n
 "    case: " (p "Two") ";" > n
 q n
 "}" > n
 )

;; php-mode/fn
(fn "function (" (p "$arg, ...") ") " (p "use ($var) ") "{" n
 "    " > q n
 "}" (list 'l (format "%s" (or (if (eq (point) (line-end-position)) ";" "") ""))) >
 :ann "function expr (closure)")

;; php-mode/foreach
(foreach "foreach (" (p "$expression") " as $" (p "value") ") {" n
 "    " > q n
 "}" >)

;; php-mode/foreach_value
(foreach=> "foreach (" (p "$expression") " as $" (p "key") " => $" (p "value") ") {" n
 "    " > q n
 "}" >
 :ann "foreach (array as $key => $value)")

;; php-mode/function
;; WARNING: Fields visited in buffer order (4 1 2) instead of numeric order
(function (p "public ") "function " (p "name") "(" (p "$arg, ...") ")" n
 "{" > n
 "    " > q n
 "}" >
 :ann "function statement")

;; php-mode/function-return-type
;; WARNING: Fields visited in buffer order (4 1 2 3) instead of numeric order
(function: (p "public ") "function " (p "name") "(" (p "$arg, ...") "): " (p "type") n
 "{" > n
 "    " > q n
 "}" >
 :ann "function :type")

;; php-mode/get
(get "public function get" (upcase-initials f1) "()" n
 "{" > n
 "    return $this->" (s f1) ";" > n
 "}" q >)

;; php-mode/if
(if "if (" (p "condition") ") {" n
 "    " > q n
 "}" >)

;; php-mode/interface
;; WARNING: Requires YASnippet function ‘yas-php-get-class-name-by-file-name’
(interface "interface " `(p ,(format "%s" (or (yas-php-get-class-name-by-file-name) ""))) n
 "{" > n
 "    " > q n
 "}" >)

;; php-mode/license-doc
;; WARNING: $0 directly follows a field; Tempel exits at the template end
(*license "* @license " (p "https://www.gnu.org/licenses/gpl-3.0.html") " " (p "GPL-3.0") "\n*"
 :ann "PHPDoc @license")

;; php-mode/license-doc-apache
(*license-apache "* @license https://www.apache.org/licenses/LICENSE-2.0 Apache-2.0" q "\n*"
 :ann "PHPDoc @license Apache License")

;; php-mode/license-doc-gplv2
(*license-gplv2 "* @license https://www.gnu.org/licenses/old-licenses/gpl-2.0.html GPL-2.0" q "\n*"
 :ann "PHPDoc @license GPLv2")

;; php-mode/license-doc-mit
(*license-mit "* @license https://opensource.org/licenses/MIT MIT" q "\n*"
 :ann "PHPDoc @license MIT")

;; php-mode/license-doc-mpl
(*license-mpl "* @license https://www.mozilla.org/en-US/MPL/2.0/ MPL-2.0" q "\n*"
 :ann "PHPDoc @license MPL")

;; php-mode/match
(match "match (" (p "$var") ") {" n
 "    " > (p "$cond") " => " (p "$return") "," n
 q n
 "};" > n
 )

;; php-mode/param-doc
;; WARNING: $0 directly follows a field; Tempel exits at the template end
(*param "* @param " (p "type") " " (p "$variable") " " (p "description") "\n*"
 :ann "PHPDoc @param")

;; php-mode/property-doc
;; WARNING: $0 directly follows a field; Tempel exits at the template end
(*property "* @property " (p "type") " " (p "$variable") " " (p "description") "\n*"
 :ann "PHPDoc @property")

;; php-mode/psysh
(psysh "eval(\\Psy\\sh());")

;; php-mode/return-doc
(*return "* @return " (p "type") " " (p "description")
 :ann "PHPDoc @return")

;; php-mode/set
(set "public function set" (upcase-initials f1) "($" (s f1) ")" n
 "{" > n
 "    $this->" f1 " = $" f1 ";" > n
 "}" q >)

;; php-mode/stdout
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(stdout "fwrite(STDOUT, " (p "$var") (p " . PHP_EOL") ");" q
 :ann "fwrite(STDOUT)")

;; php-mode/strict-types
(strict_types "declare(strict_types=1);" n
 
 :ann "declare(strict_types=1)")

;; php-mode/switch
(switch "switch (" (p "condition") ") {" n
 "    case " (p "expr") ":" > n
 "        " > q n
 "        break;" > n
 "    default:" > n
 "}" >)

;; php-mode/trait
;; WARNING: Requires YASnippet function ‘yas-php-get-class-name-by-file-name’
(trait "trait " `(p ,(format "%s" (or (yas-php-get-class-name-by-file-name) ""))) n
 "{" > n
 "    " > q n
 "}" >)

;; php-mode/try
;; WARNING: Multi-line default of field 3 is not re-indented
;; WARNING: $0 is not the last field; kept as a plain field
(try "try {" n
 "    " > p n
 "} catch (" (p "\\PDOException") " " (p "$e") ") {" > n
 "    " > (p "\n    throw $e;") n
 "}" >)

;; php-mode/var-doc
;; WARNING: $0 directly follows a field; Tempel exits at the template end
(*var "* @var " (p "type") "\n*"
 :ann "PHPDoc: var")

;; php-mode/var-oneline
(@var "/** @var " (p "type") " */" n
 q
 :ann "PHPDoc: @var (one-line)")

;; php-mode/vd
(vd "var_dump(" (p "$var") ");" q
 :ann "var_dump()")

;; php-mode/vde
(vde "var_dump(" (p "$var") ");exit(1);" q
 :ann "var_dump() exit")

;; php-mode/ve
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(ve "var_export(" (p "$var") (p ", true") ");" q
 :ann "var_export()")


powershell-mode

;; powershell-mode/cmdletbinding
(cmd "[cmdletbinding()]" n
 "Param (" > n
 "  " > q n
 ")" >
 :ann "cmdletbinding")

;; powershell-mode/comment-based-help
(cbh "<#" n
 ".SYNOPSIS" > n>
 (p "Brief description") n
 n
 ".DESCRIPTION" > n>
 (p "Longer description") n
 n
 ".PARAMETER Foobar" > n>
 (p "Descriptions of parameter Foobar") n
 n
 ".EXAMPLE" > n>
 (p "Actual example") n
 n
 ".NOTES" > n>
 (p "Additional notes") n
 "#>" > n
 q
 :ann "comment-based-help")

;; powershell-mode/function
(fun "Function " (p "Verb-Noun") " {" n
 q n
 "}" > n
 
 :ann "function")

;; powershell-mode/parameter
(par "[Parameter(Mandatory=$true, ValueFromPipelineByPropertyName=$true)][string]$" (p "Foobar")
 :ann "parameter")


prog-mode apples-mode applescript-mode cider-repl-mode clojure-mode elixir-mode elixir-ts-mode ned-mode nsis-mode

;; prog-mode/comment
;; WARNING: Requires YASnippet function ‘yas-trimmed-comment-start’
;; WARNING: Requires YASnippet function ‘yas-trimmed-comment-end’
(co (list 'l (format "%s" (or (yas-trimmed-comment-start) ""))) " " (p "comment") (list 'l (format "%s" (or (unless (eq (length comment-end) 0) (concat " " (yas-trimmed-comment-end))) ""))) q
 :ann "comment")

;; prog-mode/commentblock
;; WARNING: Requires YASnippet function ‘yas-trimmed-comment-start’
;; WARNING: Requires YASnippet function ‘yas-trimmed-comment-end’
;; WARNING: A field directly follows a mirror; it may move when the mirror changes
(cob (or (let* ((col (current-column)) (str "") (lastcom (substring (yas-trimmed-comment-start) -1)) (start (yas-trimmed-comment-start)) (end (yas-trimmed-comment-end)) (over (- (+ (string-width f1) (length start) (length end) col) 77))) (while (< (length str) (+ (- 79 (length start) (length end) col) (if (> over 0) over 0))) (setq str (concat str lastcom))) (concat start str end)) f1) n>
 (or (let* ((col (current-column)) (str "") (start (yas-trimmed-comment-start)) (start (if (string= start ";") ";;" start)) (end (yas-trimmed-comment-end))) (while (< (length str) (ffloor (/ (- 78.0 (+ col (length start) (string-width f1) (length end))) 2.0))) (setq str (concat str " "))) (concat start str)) f1) " " (p "comment" f1) " " (or (let* ((col (current-column)) (str "") (start (yas-trimmed-comment-start)) (start (if (string= start ";") ";;" start)) (end (yas-trimmed-comment-end))) (while (< (length str) (- 79 (length end) (if (eq (mod (+ (string-width f1) (length start) (length end)) 2) 1) (- col 1) col))) (setq str (concat str " "))) (concat str end)) f1) n>
 (or (let* ((col (current-column)) (str "") (lastcom (substring (yas-trimmed-comment-start) -1)) (start (yas-trimmed-comment-start)) (end (yas-trimmed-comment-end)) (over (- (+ (string-width f1) (length start) (length end) col) 77))) (while (< (length str) (+ (- 79 (length start) (length end) col) (if (> over 0) over 0))) (setq str (concat str lastcom))) (concat start str end)) f1) q
 :ann "commentblock")

;; prog-mode/commentline
;; WARNING: Requires YASnippet function ‘yas-trimmed-comment-start’
;; WARNING: Requires YASnippet function ‘yas-trimmed-comment-end’
;; WARNING: A field directly follows a mirror; it may move when the mirror changes
(col (list 'l (format "%s" (or (yas-trimmed-comment-start) ""))) " " (p "comment" f1) " " (or (dlet ((yas-text f1)) (let* ((start (yas-trimmed-comment-start)) (lastcom (aref start (1- (length start)))) (end (yas-trimmed-comment-end)) (endpadlen (- 79 (+ (current-column) (length end))))) (concat (make-string (max endpadlen 0) lastcom) end))) f1) q
 :ann "commentline")

;; prog-mode/spdxcopyright
;; WARNING: Requires YASnippet function ‘yas-trimmed-add-comment’
(spc (list 'l (format "%s" (or (yas-trimmed-add-comment) ""))) " SPDX-FileCopyrightText: " q (list 'l (format "%s" (or comment-end "")))
 :ann "spdxcopyright")

;; prog-mode/spdxlicense
;; WARNING: Requires YASnippet function ‘yas-trimmed-add-comment’
(spl (list 'l (format "%s" (or (yas-trimmed-add-comment) ""))) " SPDX-License-Identifier: " q (list 'l (format "%s" (or comment-end "")))
 :ann "spdxlicense")

;; prog-mode/todo
(t (list 'l (format "%s" (or comment-start ""))) "TODO: " q (list 'l (format "%s" (or comment-end "")))
 :ann "todo")

;; prog-mode/xxx
(x (list 'l (format "%s" (or comment-start ""))) "XXX: " q (list 'l (format "%s" (or comment-end "")))
 :ann "xxx")


prog-mode apples-mode applescript-mode cider-repl-mode clojure-mode elixir-mode elixir-ts-mode ned-mode nsis-mode :when (not (member major-mode '(sh-mode bash-ts-mode)))

;; prog-mode/fixme
(fi (list 'l (format "%s" (or comment-start ""))) "FIXME: " q (list 'l (format "%s" (or comment-end "")))
 :ann "fixme")


protobuf-mode

;; protobuf-mode/message
(msg "message " (p "Name") " {" n
 "        " > q n
 "}" >
 :ann "message")

;; protobuf-mode/syntax
(syn "syntax = \"" (p "proto3") "\";" n
 q
 :ann "syntax")


python-mode

;; python-mode/__abs__
(_abs "def __abs__(self):" n
 "    return " q > n
 
 :ann "__abs__")

;; python-mode/__add__
(_add "def __add__(self, other):" n
 "    return " q > n
 
 :ann "__add__")

;; python-mode/__aenter__
(_aenter "async def __aenter__(self):" n
 "    " > q n
 "    return self" > n
 
 :ann "__aenter__")

;; python-mode/__aexit__
(_aexit "async def __aexit__(self, exc_type, exc_value, traceback):" n
 "    " > q n
 
 :ann "__aexit__")

;; python-mode/__aiter__
(_aiter "def __aiter__(self):" n
 "    return " q > n
 
 :ann "__aiter__")

;; python-mode/__and__
(_and "def __and__(self, other):" n
 "    return " q > n
 
 :ann "__and__")

;; python-mode/__anext__
(_anext "async def __anext__(self):" n
 "    return " q > n
 
 :ann "__anext__")

;; python-mode/__await__
(_await "def __await__(self):" n
 "    " > q n
 
 :ann "__await__")

;; python-mode/__bool__
(_bool "def __bool__(self):" n
 "    return " q > n
 
 :ann "__bool__")

;; python-mode/__bytes__
(_bytes "def __bytes__(self):" n
 "    return " q > n
 
 :ann "__bytes__")

;; python-mode/__call__
(_call "def __call__(self, " (p "*args") "):" n
 "    return " q > n
 
 :ann "__call__")

;; python-mode/__ceil__
(_ceil "def __ceil__(self):" n
 "    return " q > n
 
 :ann "__ceil__")

;; python-mode/__class_getitem__
(_class_getitem "def __class_getitem__(cls, key):" n
 "    return " q > n
 
 :ann "__class_getitem__")

;; python-mode/__cmp__
(_cmp "def __cmp__(self, other):" n
 "    return " q > n
 
 :ann "__cmp__")

;; python-mode/__complex__
(_complex "def __complex__(self):" n
 "    return " q > n
 
 :ann "__complex__")

;; python-mode/__contains__
(_contains "def __contains__(self, item):" n
 "    return " q > n
 
 :ann "__contains__")

;; python-mode/__del__
(_del "def __del__(self):" n
 "    " > q n
 
 :ann "__del__")

;; python-mode/__delattr__
(_delattr "def __delattr__(self, name):" n
 "    " > q n
 
 :ann "__delattr__")

;; python-mode/__delete__
(_delete "def __delete__(self, instance):" n
 "    " > q n
 
 :ann "__delete__")

;; python-mode/__delitem__
(_delitem "def __delitem__(self, key):" n
 "    " > q n
 
 :ann "__delitem__")

;; python-mode/__dir__
(_dir "def __dir__(self):" n
 "    return " q > n
 
 :ann "__dir__")

;; python-mode/__div__
(_div "def __div__(self, other):" n
 "    return " q > n
 
 :ann "__div__")

;; python-mode/__divmod__
(_divmod "def __divmod__(self, other):" n
 "    return " q > n
 
 :ann "__divmod__")

;; python-mode/__enter__
(_enter "def __enter__(self):" n
 "    " > q n
 n
 "    return self" > n
 
 :ann "__enter__")

;; python-mode/__eq__
(_eq "def __eq__(self, other):" n
 "    return " q > n
 
 :ann "__eq__")

;; python-mode/__exit__
(_exit "def __exit__(self, exc_type, exc_value, traceback):" n
 "    " > q n
 
 :ann "__exit__")

;; python-mode/__float__
(_float "def __float__(self):" n
 "    return " q > n
 
 :ann "__float__")

;; python-mode/__floor__
(_floor "def __floor__(self):" n
 "    return " q > n
 
 :ann "__floor__")

;; python-mode/__floordiv__
(_floordiv "def __floordiv__(self, other):" n
 "    return " q > n
 
 :ann "__floordiv__")

;; python-mode/__format__
(_format "def __format__(self, format_spec):" n
 "    return " q > n
 
 :ann "__format__")

;; python-mode/__ge__
(_ge "def __ge__(self, other):" n
 "    return " q > n
 
 :ann "__ge__")

;; python-mode/__get__
(_get "def __get__(self, instance, owner=None):" n
 "    return " q > n
 
 :ann "__get__")

;; python-mode/__getattr__
(_getattr "def __getattr__(self, name):" n
 "    return " q > n
 
 :ann "__getattr__")

;; python-mode/__getattribute__
(_getattribute "def __getattribute__(self, name):" n
 "    return " q > n
 
 :ann "__getattribute__")

;; python-mode/__getitem__
(_getitem "def __getitem__(self, key):" n
 "    return " q > n
 
 :ann "__getitem__")

;; python-mode/__gt__
(_gt "def __gt__(self, other):" n
 "    return " q > n
 
 :ann "__gt__")

;; python-mode/__hash__
(_hash "def __hash__(self):" n
 "    return " q > n
 
 :ann "__hash__")

;; python-mode/__iadd__
(_iadd "def __iadd__(self, other):" n
 "    return " q > n
 
 :ann "__iadd__")

;; python-mode/__iand__
(_iand "def __iand__(self, other):" n
 "    return " q > n
 
 :ann "__iand__")

;; python-mode/__idiv__
(_idiv "def __idiv__(self, other):" n
 "    return " q > n
 
 :ann "__idiv__")

;; python-mode/__ifloordiv__
(_ifloordiv "def __ifloordiv__(self, other):" n
 "    return " q > n
 
 :ann "__ifloordiv__")

;; python-mode/__ilshift__
(_ilshift "def __ilshift__(self, other):" n
 "    return " q > n
 
 :ann "__ilshift__")

;; python-mode/__imatmul__
(_imatmul "def __imatmul__(self, other):" n
 "    return " q > n
 
 :ann "__imatmul__")

;; python-mode/__imod__
(_imod "def __imod__(self, other):" n
 "    return " q > n
 
 :ann "__imod__")

;; python-mode/__imul__
(_imul "def __imul__(self, other):" n
 "    return " q > n
 
 :ann "__imul__")

;; python-mode/__index__
(_index "def __index__(self):" n
 "    return " q > n
 
 :ann "__index__")

;; python-mode/__init__
;; WARNING: Multi-line default of field 2 is not re-indented
;; WARNING: Nested fields flattened into field 2
;; WARNING: $0 directly follows a field; Tempel exits at the template end
(_init "def __init__(self" (p ", args") "):" n
 "    " > (p "\"docstring\"\n    ") n
 
 :ann "__init__")

;; python-mode/__init_subclass__
(_init_subclass "def __init_subclass__(cls, /" (p ", param") ", **kwargs):" n
 "    super().__init_subclass__(**kwargs)" > n
 "    " > q n
 
 :ann "__init_subclass__")

;; python-mode/__instancecheck__
(_instancecheck "def __instancecheck__(self, instance):" n
 "    return " q > n
 
 :ann "__instancecheck__")

;; python-mode/__int__
(_int "def __int__(self):" n
 "    " > q n
 
 :ann "__int__")

;; python-mode/__invert__
(_invert "def __invert__(self):" n
 "    return " q > n
 
 :ann "__invert__")

;; python-mode/__ior__
(_ior "def __ior__(self, other):" n
 "    return " q > n
 
 :ann "__ior__")

;; python-mode/__ipow__
(_ipow "def __ipow__(self, other):" n
 "    return " q > n
 
 :ann "__ipow__")

;; python-mode/__irshift__
(_irshift "def __irshift__(self, other):" n
 "    return " q > n
 
 :ann "__irshift__")

;; python-mode/__isub__
(_isub "def __isub__(self, other):" n
 "    return " q > n
 
 :ann "__isub__")

;; python-mode/__iter__
(_iter "def __iter__(self):" n
 "    " > q n
 
 :ann "__iter__")

;; python-mode/__itruediv__
(_itruediv "def __itruediv__(self, other):" n
 "    return " q > n
 
 :ann "__itruediv__")

;; python-mode/__ixor__
(_ixor "def __ixor__(self, other):" n
 "    return " q > n
 
 :ann "__ixor__")

;; python-mode/__le__
(_le "def __le__(self, other):" n
 "    return " q > n
 
 :ann "__le__")

;; python-mode/__len__
(_len "def __len__(self):" n
 "    return " q > n
 
 :ann "__len__")

;; python-mode/__length_hint__
(_length_hint "def __length_hint__(self):" n
 "    return " q > n
 
 :ann "__length_hint__")

;; python-mode/__lshift__
(_lshift "def __lshift__(self, other):" n
 "    return " q > n
 
 :ann "__lshift__")

;; python-mode/__lt__
(_lt "def __lt__(self, other):" n
 "    return " q > n
 
 :ann "__lt__")

;; python-mode/__matmul__
(_matmul "def __matmul__(self, other):" n
 "    return " q > n
 
 :ann "__matmul__")

;; python-mode/__missing__
(_missing "def __missing__(self, key):" n
 "    return " q > n
 
 :ann "__missing__")

;; python-mode/__mod__
(_mod "def __mod__(self, other):" n
 "    return " q > n
 
 :ann "__mod__")

;; python-mode/__mul__
(_mul "def __mul__(self, other):" n
 "    return " q > n
 
 :ann "__mul__")

;; python-mode/__ne__
(_ne "def __ne__(self, other):" n
 "    return " q > n
 
 :ann "__ne__")

;; python-mode/__neg__
(_neg "def __neg__(self):" n
 "    return " q > n
 
 :ann "__neg__")

;; python-mode/__new__
(_new "def __new__(mcs, name, bases, dct):" n
 "    " > q n
 "    return type.__new__(mcs, name, bases, dct)" > n
 
 :ann "__new__")

;; python-mode/__next__
(_next "def __next__(self):" n
 "    " > q n
 
 :ann "__next__")

;; python-mode/__or__
(_or "def __or__(self, other):" n
 "    return " q > n
 
 :ann "__or__")

;; python-mode/__pos__
(_pos "def __pos__(self):" n
 "    return " q > n
 
 :ann "__pos__")

;; python-mode/__pow__
(_pow "def __pow__(self, other, modulo=None):" n
 "    return " q > n
 
 :ann "__pow__")

;; python-mode/__prepare__
(_prepare "def __prepare__(name, bases, **kwds):" n
 "    return " q "{}" > n
 
 :ann "__prepare__")

;; python-mode/__radd__
(_radd "def __radd__(self, other):" n
 "    return " q > n
 
 :ann "__radd__")

;; python-mode/__rand__
(_rand "def __rand__(self, other):" n
 "    return " q > n
 
 :ann "__rand__")

;; python-mode/__rdivmod__
(_rdivmod "def __rdivmod__(self, other):" n
 "    return " q > n
 
 :ann "__rdivmod__")

;; python-mode/__repr__
(_repr "def __repr__(self):" n
 "    return " q > n
 
 :ann "__repr__")

;; python-mode/__reversed__
(_reversed "def __reversed__(self):" n
 "    return " q > n
 
 :ann "__reversed__")

;; python-mode/__rfloordiv__
(_rfloordiv "def __rfloordiv__(self, other):" n
 "    return " q > n
 
 :ann "__rfloordiv__")

;; python-mode/__rlshift__
(_rlshift "def __rlshift__(self, other):" n
 "    return " q > n
 
 :ann "__rlshift__")

;; python-mode/__rmatmul__
(_rmatmul "def __rmatmul__(self, other):" n
 "    return " q > n
 
 :ann "__rmatmul__")

;; python-mode/__rmod__
(_rmod "def __rmod__(self, other):" n
 "    return " q > n
 
 :ann "__rmod__")

;; python-mode/__rmul__
(_rmul "def __rmul__(self, other):" n
 "    return " q > n
 
 :ann "__rmul__")

;; python-mode/__ror__
(_ror "def __ror__(self, other):" n
 "    return " q > n
 
 :ann "__ror__")

;; python-mode/__round__
(_round "def __round__(self, ndigits=None):" n
 "    return " q > n
 
 :ann "__round__")

;; python-mode/__rpow__
(_rpow "def __rpow__(self, other):" n
 "    return " q > n
 
 :ann "__rpow__")

;; python-mode/__rrshift__
(_rrshift "def __rrshift__(self, other):" n
 "    return " q > n
 
 :ann "__rrshift__")

;; python-mode/__rshift__
(_rshift "def __rshift__(self, other):" n
 "    return " q > n
 
 :ann "__rshift__")

;; python-mode/__rsub__
(_rsub "def __rsub__(self, other):" n
 "    return " q > n
 
 :ann "__rsub__")

;; python-mode/__rtruediv__
(_rtruediv "def __rtruediv__(self, other):" n
 "    return " q > n
 
 :ann "__rtruediv__")

;; python-mode/__rxor__
(_rxor "def __rxor__(self, other):" n
 "    return " q > n
 
 :ann "__rxor__")

;; python-mode/__set__
(_set "def __set__(self, instance, value):" n
 "    " > q n
 
 :ann "__set__")

;; python-mode/__set_name__
(_set_name "def __set_name__(self, owner, name):" n
 "    " > q n
 
 :ann "__set_name__")

;; python-mode/__setattr__
(_setattr "def __setattr__(self, name, value):" n
 "    " > q n
 
 :ann "__setattr__")

;; python-mode/__setitem__
(_setitem "def __setitem__(self, key, value):" n
 "    " > q n
 
 :ann "__setitem__")

;; python-mode/__slots__
(_slots "__slots__ = (" p ")" n
 q n
 
 :ann "__slots__")

;; python-mode/__str__
(_str "def __str__(self):" n
 "    return " q > n
 
 :ann "__str__")

;; python-mode/__sub__
(_sub "def __sub__(self, other):" n
 "    return " q > n
 
 :ann "__sub__")

;; python-mode/__subclasscheck__
(_subclasscheck "def __subclasscheck__(self, instance):" n
 "    return " q > n
 
 :ann "__subclasscheck__")

;; python-mode/__truediv__
(_truediv "def __truediv__(self, other):" n
 "    return " q > n
 
 :ann "__truediv__")

;; python-mode/__trunc__
(_trunc "def __trunc__(self):" n
 "    return " q > n
 
 :ann "__trunc__")

;; python-mode/__xor__
(_xor "def __xor__(self, other):" n
 "    return " q > n
 
 :ann "__xor__")

;; python-mode/all
(all (p (current-column) col noinsert) "__all__ = [" n
 (ignore (indent-to col)) "    " q n
 (ignore (indent-to col)) "]" n
 )

;; python-mode/arg
(arg "parser.add_argument('-" p "', '--" p "'," n
 "                    " > q ")" n
 )

;; python-mode/arg_positional
(posarg "parser.add_argument('" (p "varname") "', " q ")" n
 
 :ann "arg_positional")

;; python-mode/ase
(ase "self.assertEqual(" (p "expected") ", " (p "actual") ")" n
 q
 :ann "Assert Equal")

;; python-mode/asne
(asne "self.assertNotEqual(" (p "expected") ", " (p "actual") ")" n
 q
 :ann "Assert Not Equal")

;; python-mode/asr
(asr "with self.assertRaises(" (p "Exception") "):" n
 "    " > q n
 
 :ann "Assert Raises")

;; python-mode/assert
(ass "assert " q n
 
 :ann "assert")

;; python-mode/assertEqual
(ae "self.assertEqual(" p ", " p ")" n
 q
 :ann "assertEqual")

;; python-mode/assertFalse
(af "self.assertFalse(" q ")" n
 
 :ann "assertFalse")

;; python-mode/assertIn
(ai "self.assertIn(" (p "member") ", " (p "container") ")" n
 q
 :ann "assertIn")

;; python-mode/assertNotEqual
(ane "self.assertNotEqual(" p ", " p ")" n
 q
 :ann "assertNotEqual")

;; python-mode/assertNotIn
(an "self.assertNotIn(" (p "member") ", " (p "container") ")" n
 q
 :ann "assetNotIn")

;; python-mode/assertRaises
(ar "self.assertRaises(" (p "Exception") ", " (p "fun") ")" n
 q
 :ann "assertRaises")

;; python-mode/assertRaises.with
(arw "with self.assertRaises(" (p "Exception") "):" n
 "    " > q n
 
 :ann "assertRaisesWith")

;; python-mode/assertTrue
(at "self.assertTrue(" q ")" n
 
 :ann "assertTrue")

;; python-mode/bang
(\#! "#!/usr/bin/env python" n
 )

;; python-mode/celery_pdb
(cdb "from celery.contrib import rdb; rdb.set_trace()" n
 
 :ann "celery pdb")

;; python-mode/class_doxygen_doc
(doxy_class "\"\"\"" n
 "@brief      " (p "class description") > n
 n
 "@details    " (p "detailed description") > n
 "\"\"\"" > n
 q
 :ann "Class Doxygen Doc")

;; python-mode/classmethod
(cm "@classmethod" n
 "def " (p "meth") "(cls, " p "):" > n
 "    " > q n
 
 :ann "classmethod")

;; python-mode/cls
(cls "class " (p "class") ":" n
 "    " > q n
 
 :ann "class")

;; python-mode/dataclass
(dc "@dataclass" n
 "class " (p "class") ":" > n
 "    " > q n
 
 :ann "dataclass")

;; python-mode/dec
(dec "def " (p "decorator" f1) "(func):" n
 "    " > p n
 "    def _" f1 "(*args, **kwargs):" > n
 "        " > p n
 "        ret = func(*args, **kwargs)" > n
 "        " > p n
 "        return ret" > n
 n
 "    return _" f1 > n
 q)

;; python-mode/def
(def "def " (p "methodname") "(self, " (p "arg") "):" n
 "    " > (p "pass") n
 q
 :ann "def method(self, ...):")

;; python-mode/deftest
(dt "def test_" (p "long_name") "(self):" n
 "    " > q n
 
 :ann "deftest")

;; python-mode/django_test_class
(dtcs "class " (p "Model") "Test(TestCase):" n
 "    " > q n
 
 :ann "django_test_class")

;; python-mode/doc
(d "\"\"\"" q n
 "\"\"\"" > n
 
 :ann "doc")

;; python-mode/doctest
(doctest ">>> " (p "function calls") n>
 (p "desired output") n
 q n
 )

;; python-mode/embed
(embed "from IPython import embed; embed()" n
 )

;; python-mode/enum
(en "class " (p "class") "(Enum):" n
 "    " > q n
 
 :ann "enum")

;; python-mode/env
(env "#!/usr/bin/env python" n
 
 :ann "#!/usr/bin/env python")

;; python-mode/for
(for "for " (p "var") " in " (p "collection") ":" n
 "    " > q n
 
 :ann "for ... in ... : ...")

;; python-mode/from
(from "from " (p "module") " import " (p "symbol") n
 q n
 
 :ann "from MOD import SYM")

;; python-mode/function
(f "def " (p "fun") "(" (p "args") "):" n
 "    " > q n
 
 :ann "function")

;; python-mode/function_docstring
(fd "def " (p "name") "(" (s f2) "):" n
 " \"\"\"" p > n
 " " > (or (dlet ((yas-text f2)) (python-args-to-docstring)) f2) n
 " \"\"\"" > n
 " " > q n
 
 :ann "function_docstring")

;; python-mode/function_docstring_numpy
(fdn "def " (p "name") "(" (s f2) "):" n
 "    \"\"\"" p > n
 n>
 (or (dlet ((yas-text f2)) (python-args-to-docstring-numpy)) f2) n
 "    \"\"\"" > n
 "    " > q n
 
 :ann "function_docstring_numpy")

;; python-mode/function_doxygen_doc
(doxy_func "\"\"\"" n
 "@brief      " (p "function description") > n
 n
 "@details    " (p "detailed description") > n
 n
 "@param      " (p "param") > n
 n
 "@return     " (p "return type") > n
 "\"\"\"" > n
 q
 :ann "Function Doxygen Doc")

;; python-mode/ic.py
(ic "from icecream import ic" n
 "ic(" p ")" > n
 q)

;; python-mode/if
(if "if " (p "cond") ":" n
 "    " > q n
 )

;; python-mode/ife
(ife (p (current-column) col noinsert) "if " p ":" n
 (ignore (indent-to col)) "    " p n
 (ignore (indent-to col)) "else:" n
 (ignore (indent-to col)) "    " q n
 )

;; python-mode/ifmain
(ifm "if __name__ == '__main__':" n
 "    " > (p "main()") n
 q
 :ann "ifmain")

;; python-mode/import
;; WARNING: Nested fields flattened into field 2
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(imp "import " (p "lib") (p " as alias") n
 q n
 
 :ann "import")

;; python-mode/init_docstring
(id "def __init__(self" (s f1) "):" n
 "    \"\"\"" p > n
 "    " > (or (dlet ((yas-text f1)) (python-args-to-docstring)) f1) n
 "    \"\"\"" > n
 "    " > q n
 
 :ann "init_docstring")

;; python-mode/init_docstring_numpy
(idn "def __init__(self" (s f1) "):" n
 "    \"\"\"" p > n
 "    " > (or (dlet ((yas-text f1)) (python-args-to-docstring-numpy)) f1) n
 "    \"\"\"" > n
 "    " > q n
 
 :ann "init_docstring_numpy")

;; python-mode/interact
(int "import code; code.interact(local=locals())" n
 
 :ann "interact")

;; python-mode/ipdb
(ipdb "import ipdb; ipdb.set_trace()" n
 
 :ann "ipdb trace")

;; python-mode/lambda
(lam "lambda " (p "x") ": " q n
 
 :ann "lambda")

;; python-mode/list
(li "[" (p "el" f1) " for " f1 " in " (p "list") "]" n
 q n
 
 :ann "list")

;; python-mode/logger_name
(ln "logger = logging.getLogger(" (p "__name__") ")" n
 q
 :ann "logger_name")

;; python-mode/logging
(log "logger = logging.getLogger(\"" (p "name") "\")" n
 "logger.setLevel(logging." (p "level") ")" > n
 q
 :ann "logging")

;; python-mode/main
(main "def main():" n
 "    " > q n
 )

;; python-mode/method
;; WARNING: Nested fields flattened into field 2
(m "def " (p "method") "(self" (p ", ") "):" n
 "    " > q n
 
 :ann "method")

;; python-mode/method_docstring
(md "def " (p "name") "(self" (s f2) "):" n
 "    \"\"\"" p > n
 "    " > (or (dlet ((yas-text f2)) (python-args-to-docstring)) f2) n
 "    \"\"\"" > n
 "    " > q n
 
 :ann "method_docstring")

;; python-mode/method_docstring_numpy
(mdn "def " (p "name") "(self" (s f2) "):" n
 "    \"\"\"" p > n
 "    " > (or (dlet ((yas-text f2)) (python-args-to-docstring-numpy)) f2) n
 "    \"\"\"" > n
 "    " > q n
 
 :ann "method_docstring_numpy")

;; python-mode/not_impl
(not_impl "raise NotImplementedError" n
 )

;; python-mode/np
(np "import numpy as np" n
 q n
 )

;; python-mode/parse_args
(pargs "def parse_arguments():" n
 "    parser = argparse.ArgumentParser(description='" p "')" > n
 "    " > q n
 "    return parser.parse_args()" > n
 
 :ann "parse_args")

;; python-mode/parser
(pars "parser = argparse.ArgumentParser(description='" p "')" n
 q n
 
 :ann "parser")

;; python-mode/pass
(ps "pass" n
 
 :ann "pass")

;; python-mode/pdb
(pdb "breakpoint()" n
 
 :ann "pdb trace")

;; python-mode/pl
(plt "import matplotlib.pyplot as plt" n
 q n
 
 :ann "Import pyplot")

;; python-mode/print
(p "print(" q ")" n
 
 :ann "print")

;; python-mode/prop
(prop (p (current-column) col noinsert) "def " (p "foo" f1) "():" n
 (ignore (indent-to col)) "    doc = \"\"\"" (p "Doc string") "\"\"\"" n
 (ignore (indent-to col)) "    def fget(self):" n
 (ignore (indent-to col)) "        return self._" f1 n
 n
 (ignore (indent-to col)) "    def fset(self, value):" n
 (ignore (indent-to col)) "        self._" f1 " = value" n
 n
 (ignore (indent-to col)) "    def fdel(self):" n
 (ignore (indent-to col)) "        del self._" f1 n
 (ignore (indent-to col)) "    return locals()" n
 (ignore (indent-to col)) f1 " = property(**" f1 "())" n
 n
 q n
 )

;; python-mode/pudb
(pudb "import pudb; pudb.set_trace()" n
 
 :ann "pudb trace")

;; python-mode/reg
(reg (p "regexp") " = re.compile(r\"" (p "expr") "\")" n
 q n
 )

;; python-mode/return
(r "return " q n
 
 :ann "return")

;; python-mode/scls
(scls "class " (p "class") "(" (p "super-class") "):" n
 "    " > q n
 
 :ann "subclass")

;; python-mode/script
(script "#!/usr/bin/env python" n
 n
 "def main():" > n
 "    pass" > n
 n
 "if __name__ == '__main__':" > n
 "    main()" > n
 )

;; python-mode/self
(\. "self." q n
 
 :ann "self")

;; python-mode/self_without_dot
(s "self" n
 
 :ann "self_without_dot")

;; python-mode/selfassign
(sn "self." (s f1) " = " f1 n
 q
 :ann "selfassign")

;; python-mode/setdef
(setdef (p "var") ".setdefault(" (p "key") ", []).append(" (p "value") ")" n
 q)

;; python-mode/setup
(setup "from setuptools import setup" n
 n
 "package = '" (p "name") "'" > n
 "version = '" (p "0.1") "'" > n
 n
 "setup(name=package," > n
 "      version=version," > n
 "      description=\"" (p "description") "\"," > n
 "      url='" (p "url") "'" q ")" > n
 )

;; python-mode/size
(size "sys.getsizeof(" q ")" n
 )

;; python-mode/static
(sm "@staticmethod" n
 "def " (p "func") "(" q "):" > n
 
 :ann "static")

;; python-mode/test_class
(tcs "class Test" (p "toTest") "(" (p "unittest.TestCase") "):" n
 "    " > q n
 
 :ann "test_class")

;; python-mode/test_file
;; WARNING: Nested fields flattened into field 1
(tf "import unittest" n>
 (p "from test_file import *") n
 n
 q n
 n
 "if __name__ == '__main__':" > n
 "    unittest.main()" > n
 
 :ann "test_file")

;; python-mode/try
;; WARNING: $0 is not the last field; kept as a plain field
(try (p (current-column) col noinsert) "try:" n
 (ignore (indent-to col)) "    " p n
 (ignore (indent-to col)) "except " (p "Exception") ":" n
 (ignore (indent-to col)) "    " p n
 )

;; python-mode/tryelse
;; WARNING: $0 is not the last field; kept as a plain field
(tryelse (p (current-column) col noinsert) "try:" n
 (ignore (indent-to col)) "    " p n
 (ignore (indent-to col)) "except " p ":" n
 (ignore (indent-to col)) "    " p n
 (ignore (indent-to col)) "else:" n
 (ignore (indent-to col)) "    " p n
 )

;; python-mode/utf8
(utf8 "# -*- coding: utf-8 -*-" n
 
 :ann "utf-8 encoding")

;; python-mode/uv-script
(uv "#!/usr/bin/uv run" n
 "# /// script" > n
 "# dependencies = [" > n
 "# " p > n
 "# ]" > n
 "# ///" > n
 q
 :ann "uv-script")

;; python-mode/while
(wh "while " (p "True") ":" n
 "    " > q n
 
 :ann "while")

;; python-mode/with
;; WARNING: Nested fields flattened into field 2
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(with "with " (p "expr") (p " as alias") ":" n
 "    " > q n
 )

;; python-mode/with-open
;; WARNING: Nested fields flattened into field 2
;; WARNING: Nested fields flattened into field 4
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(wo "with open(" (p "\"filename\"") (p ", encoding=\"utf-8\"") (p ", mode=\"w\"") ") as " (p "myfile") ":" n
 "    " > q
 :ann "with-open")


racket-mode

;; racket-mode/case
(case "(case " (p "expression") " [" (p "datum") " " q "])"
 :ann "(case ... [... ...]...)")

;; racket-mode/caselambda
(case-lambda "(case-lambda [" (p "arguments") " " q "])"
 :ann "(case-lambda [... ...]...)")

;; racket-mode/cond
(cond "(cond [" (p "predicate") " " q "])"
 :ann "(cond [... ...]...)")

;; racket-mode/define
;; WARNING: Nested fields flattened into field 1
(define "(define " (p "(name arguments)") " " q ")"
 :ann "(define ... ...)")

;; racket-mode/define-syntax-rule
(define-syntax-rule "(define-syntax-rule (" (p "id") " " (p "pattern") ") " q ")"
 :ann "(define-syntax-rule (... ...) ...)")

;; racket-mode/do
(do "(do ([" (p "name") " " (p "init") " " (p "step") "]" p ")" n
 "    (" (p "stop-predicate") " " (p "finish") ")" > n
 "  " > q ")"
 :ann "(do ([... ... ...]...) (... ...) ...)")

;; racket-mode/for
(for "(for" p " (" (p "for-clause") ") " q ")"
 :ann "(for... (...) ...)")

;; racket-mode/if
;; WARNING: Nested fields flattened into field 1
(if "(if " (p "(predicate)") n
 "    " > q ")"
 :ann "(if ... ... ...)")

;; racket-mode/lambda
;; WARNING: Nested fields flattened into field 1
(lambda "(lambda " (p "(arguments)") " " q ")"
 :ann "(lambda (...) ...)")

;; racket-mode/let
(let "(let" p " ([" (p "name") " " (p "expression") "]" p ") " q ")"
 :ann "(let... ([... ...]...) ...)")

;; racket-mode/match
(match "(match " (p "expression") " [" (p "clause") " " q "])"
 :ann "(match ... [... ...]...)")

;; racket-mode/module
(module "(module " (p "id") " " (p "module-path") n
 "  " > q ")"
 :ann "(module ... ... ...)")

;; racket-mode/module-plus
(module+ "(module+ " (p "id") n
 "  " > q ")"
 :ann "(module+ ... ...)")

;; racket-mode/module-star
(module* "(module* " (p "id") " " (p "module-path") n
 "  " > q ")"
 :ann "(module* ... ... ...)")

;; racket-mode/syntax-rules
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(syntax-rules "(syntax-rules (" (p "literal-id") p ")" n
 "  [(" (p "id") " " (p "pattern") ") " (p "template") "]" p ")" q >
 :ann "(syntax-rules (...) [(... ...) ...]...)")

;; racket-mode/unless
;; WARNING: Nested fields flattened into field 1
(unless "(unless " (p "(predicate)") " " q ")"
 :ann "(unless ...)")

;; racket-mode/when
;; WARNING: Nested fields flattened into field 1
(when "(when " (p "(predicate)") " " q ")"
 :ann "(when ...)")


raku-mode

;; raku-mode/elsif
(ifi "if " (p "condition") " {" n
 "  " > p n
 "} elsif " (p "condition") " {" > n
 "  " > p n
 "} else {" > n
 "  " > p n
 "}" > n
 q
 :ann "elsif")

;; raku-mode/for
(for "for " (p "lista") " {" n
 "  " > q n
 "}" > n
 )

;; raku-mode/for-guard
;; WARNING: $0 is not the last field; kept as a plain field
(forg p " for " (p "list") n
 
 :ann "for-guard")

;; raku-mode/forv
(forv "for " (p "list") " -> " (p "i") " {" n
 "  " > q n
 "}" > n
 
 :ann "for variable")

;; raku-mode/forvc
(forvc "for " (p "list") " -> " (p "i") " is copy {" n
 "  " > q n
 "}" > n
 n
 
 :ann "for variable is copy")

;; raku-mode/forw
(forw "for " (p "list") " -> " (p "i") " is rw {" n
 "  " > q n
 "}" > n
 
 :ann "for variable is rw")

;; raku-mode/given
(given "given " (p "variable") " {" n
 "  when " (p "condition") " { " (p "block") " }" > n
 "  when " (p "condition") " { " (p "block") " }" > n
 "  default { " (p "default-block") " }" > n
 "}" q >)

;; raku-mode/if
(if "if " (p "condition") " {" n
 "  " > q n
 "}" >)

;; raku-mode/if-else
(ife "if " (p "condition") " {" n
 "  " > p n
 "} else {" > n
 "  " > p n
 "}" q >
 :ann "if else")

;; raku-mode/ifguard
;; WARNING: $0 is not the last field; kept as a plain field
(ifg p " if " (p "condition") ";"
 :ann "if guard")

;; raku-mode/main
;; WARNING: Nested fields flattened into field 2
(main "sub MAIN" (p "(args)") " {" n
 "  " > q n
 "}" > n
 )

;; raku-mode/multi
;; WARNING: Nested fields flattened into field 2
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(multi "multi " (p "subrutine") (p "(args)") " {" n
 "  " > q n
 "}" > n
 )

;; raku-mode/multi-line-comment
(co "#`(" q ")"
 :ann "multi-line and in-the-middle comments")

;; raku-mode/pod
(pod "=begin " (p "comment" f1) n
 " " > n
 q n
 " " > n
 "=end " f1 >
 :ann "Pod")

;; raku-mode/subrutine
;; WARNING: Nested fields flattened into field 2
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(sub "sub " (p "subrutine") (p "(args)") " {" n
 "  " > q n
 "}" > n
 
 :ann "Subrutine")

;; raku-mode/subset
(subset "subset " (p "name") " of " (p "(Type)") " where { $_ " q "}")

;; raku-mode/ternary-operator
(tern (p "condition") " ?? " (p "true") " !! " (p "false") ";" q
 :ann "Ternary Operator")

;; raku-mode/unless
(un "unless " (p "condition") " {" n
 "  " > q n
 "}" >
 :ann "unless")

;; raku-mode/unless-guard
;; WARNING: $0 is not the last field; kept as a plain field
(ung p " unless " (p "condition") ";"
 :ann "unless guard")

;; raku-mode/until
(until "until " (p "condition") " {" n
 n
 "}" q >)

;; raku-mode/untilguard
;; WARNING: $0 is not the last field; kept as a plain field
(untilg p " until " (p "condition") ";")

;; raku-mode/while
(while "while " (p "condition") " {" n
 "  " > q n
 "}" > n
 )

;; raku-mode/whileguard
;; WARNING: $0 is not the last field; kept as a plain field
(whileg p " while " (p "condition") ";")


reason-mode

;; reason-mode/component
(component "module " (p "Component") " = {" n
 "  [@react.component]" > n
 "  let make = (" (p "parameters") ") => {" > n
 "    " > q n
 "  };" > n
 "};" >)

;; reason-mode/for
(for "for (" (p "i") " in " (p "iFirst") " to " (p "iLast") ") {" n
 "  " > q n
 "};" >)

;; reason-mode/function
(func "(" (p "paramters") ") => " q n
 
 :ann "function")

;; reason-mode/functor
(functor "module " (p "Functor") " = (" (p "Module") ": " (p "ModuleType") ") => {" n
 "  " > q n
 "};" >)

;; reason-mode/if
(if "if (" (p "cond") ") {" n
 "  " > q n
 "}" >)

;; reason-mode/ifelse
;; WARNING: Duplicate key ‘if’ renamed to ‘if-2’
(if-2 "if (" (p "cond") ") {" n
 "  " > p n
 "} else {" > n
 "  " > q n
 "}" >
 :ann "ifelse")

;; reason-mode/let
(let "let " (p "var") " = " (p "e") ";" n
 q)

;; reason-mode/match_case
(| "| " (p "Case") " => " q
 :ann "match case")

;; reason-mode/module
(module "module " (p "M") " = {" n
 "  " > q n
 "};" >)

;; reason-mode/switch
(switch "switch (" (p "to_match") ") {" n
 "| " (p "matching") " => " q > n
 "}" >)

;; reason-mode/while
(while "while (" (p "cond") ") {" n
 "  " > q n
 "};" >)


rjsx-mode

;; rjsx-mode/GraphQL/expgql
(expgql "export default compose(" n
 "  graphql(" (p "queryOrMutation" f1) ", { name: " (p "name") " })," > n
 ")(" f1 ")" q >
 :ann "exportGraphQL")

;; rjsx-mode/GraphQL/graphql
(graphql "import { compose, graphql } from 'react-apollo'"
 :ann "graphQLForComponent")

;; rjsx-mode/Jest/desc
(desc "describe('" p "', () => {" n
 "  " > q n
 "})" >
 :ann "describeBlock")

;; rjsx-mode/Jest/snrtest
;; WARNING: Nested fields flattened into field 1
(snrtest "import 'react-native'" n
 "import React from 'react'" > n
 "import renderer from 'react-test-renderer'" > n
 "import { Provider } from 'react-redux'" > n
 n
 "import store from 'src/store'" > n
 "import " (p "TM_FILENAME_BASE" f1) " from '../" f1 "'" > n
 n
 "describe('<" f1 " />', () => {" > n
 "  const defaultProps = {}" > n
 "  const wrapper = renderer.create(" > n
 "    <Provider store={store}>" > n
 "      <" f1 " {...defaultProps} />" > n
 "    </Provider>," > n
 "  )" > n
 n
 "  test('render', () => {" > n
 "    expect(wrapper).toMatchSnapshot()" > n
 "  })" > n
 "})" q >
 :ann "setupReactNativeTestWithRedux")

;; rjsx-mode/Jest/sntest
;; WARNING: Nested fields flattened into field 1
(sntest "import 'react-native'" n
 "import React from 'react'" > n
 "import renderer from 'react-test-renderer'" > n
 n
 "import " (p "TM_FILENAME_BASE" f1) " from '../" f1 "'" > n
 n
 "describe('<" f1 " />', () => {" > n
 "  const defaultProps = {}" > n
 "  const wrapper = renderer.create(<" f1 " {...defaultProps} />)" > n
 n
 "  test('render', () => {" > n
 "    expect(wrapper).toMatchSnapshot()" > n
 "  })" > n
 "})" q >
 :ann "setupReactNativeTest")

;; rjsx-mode/Jest/srtest
;; WARNING: Nested fields flattened into field 1
(srtest "import React from 'react'" n
 "import renderer from 'react-test-renderer'" > n
 "import { Provider } from 'react-redux'" > n
 n
 "import store from 'src/store'" > n
 "import { " (p "TM_FILENAME_BASE" f1) " } from '../" f1 "'" > n
 n
 "describe('<" f1 " />', () => {" > n
 "  const defaultProps = {}" > n
 "  const wrapper = renderer.create(" > n
 "    <Provider store={store}>" > n
 "     <" f1 " {...defaultProps} />" > n
 "    </Provider>," > n
 "  )" > n
 n
 "  test('render', () => {" > n
 "    expect(wrapper).toMatchSnapshot()" > n
 "  })" > n
 "})" q >
 :ann "setupReactComponentTestWithRedux")

;; rjsx-mode/Jest/stest
;; WARNING: Nested fields flattened into field 1
(stest "import React from 'react'" n
 "import renderer from 'react-test-renderer'" > n
 n
 "import { " (p "TM_FILENAME_BASE" f1) " } from '../" f1 "'" > n
 n
 "describe('<" f1 " />', () => {" > n
 "  const defaultProps = {}" > n
 "  const wrapper = renderer.create(<" f1 " {...defaultProps} />)" > n
 n
 "  test('render', () => {" > n
 "    expect(wrapper).toMatchSnapshot()" > n
 "  })" > n
 "})" q >
 :ann "setupTest")

;; rjsx-mode/Jest/test
(test "test('should " p "', () => {" n
 "  " > q n
 "})" >
 :ann "testBlock")

;; rjsx-mode/Jest/tit
(tit "it('should " p "', () => {" n
 "  " > q n
 "})" >
 :ann "itBlock")

;; rjsx-mode/React/cdm
(cdm "componentDidMount() {" n
 "  " > q n
 "}" >
 :ann "componentDidMount")

;; rjsx-mode/React/cdup
(cdup "componentDidUpdate(prevProps, prevState, snapshot) {" n
 "  " > q n
 "}" >
 :ann "componentDidUpdate")

;; rjsx-mode/React/cp
(cp "const { " p " } = this.props" q
 :ann "destructProps")

;; rjsx-mode/React/cref
(cref "this." (p "refName") "Ref = React.createRef()" q
 :ann "createRef")

;; rjsx-mode/React/cs
(cs "const { " p " } = this.state" q
 :ann "destructState")

;; rjsx-mode/React/cwun
(cwun "componentWillUnmount() {" n
 "  " > q n
 "}" >
 :ann "componentWillUnmount")

;; rjsx-mode/React/est
(est "state = {" n
 "  " > p n
 "}" q >
 :ann "emptyState")

;; rjsx-mode/React/fref
(fref "const ref = React.createRef()"
 :ann "forwardRef")

;; rjsx-mode/React/gdsfp
(gdsfp "static getDerivedStateFromProps(nextProps, prevState) {" n
 "  " > q n
 "}" >
 :ann "getDerivedStateFromProps")

;; rjsx-mode/React/gsbu
(gsbu "getSnapshotBeforeUpdate(prevProps, prevState) {" n
 "  " > q n
 "}" >
 :ann "getSnapshotBeforeUpdate")

;; rjsx-mode/React/hoc
(hoc "import React from 'react'" n
 "import PropTypes from 'prop-types'" > n
 n
 "export default (WrappedComponent) => {" > n
 "  const hocComponent = ({ ...props }) => <WrappedComponent {...props} />" > n
 n
 "  hocComponent.propTypes = {" > n
 "  }" > n
 n
 "  return hocComponent" > n
 "}" >
 :ann "hocComponent")

;; rjsx-mode/React/impt
(impt "import PropTypes from 'prop-types'"
 :ann "import PropTypes")

;; rjsx-mode/React/imr
(imr "import React from 'react'"
 :ann "import React")

;; rjsx-mode/React/imrc
(imrc "import React, { Component } from 'react'"
 :ann "import React, { Component }")

;; rjsx-mode/React/imrcp
(imrcp "import React, { Component } from 'react'" n
 "import PropTypes from 'prop-types'" >
 :ann "import React, { Component } & PropTypes")

;; rjsx-mode/React/imrd
(imrd "import ReactDOM from 'react-dom'"
 :ann "import ReactDOM")

;; rjsx-mode/React/imrm
(imrm "import React, { memo } from 'react'"
 :ann "import React, { memo }")

;; rjsx-mode/React/imrmp
(imrmp "import React, { memo } from 'react'" n
 "import PropTypes from 'prop-types'" >
 :ann "import React, { memo } & PropTypes")

;; rjsx-mode/React/imrpc
(imrpc "import React, { PureComponent } from 'react'"
 :ann "import React, { PureComponent }")

;; rjsx-mode/React/imrpcp
(imrpcp "import React, { PureComponent } from 'react'" n
 "import PropTypes from 'prop-types'" >
 :ann "import React, { PureComponent } & PropTypes")

;; rjsx-mode/React/imrr
(imrr "import { BrowserRouter as Router, Route, Link } from 'react-router-dom'"
 :ann "import React Router")

;; rjsx-mode/React/props
(props "this.props." q
 :ann "componentProps")

;; rjsx-mode/React/pta
(pta "PropTypes.array,"
 :ann "propTypeArray")

;; rjsx-mode/React/ptany
(ptany "PropTypes.any,"
 :ann "propTypeAny")

;; rjsx-mode/React/ptao
(ptao "PropTypes.arrayOf(" q "),"
 :ann "propTypeArrayOf")

;; rjsx-mode/React/ptaor
(ptaor "PropTypes.arrayOf(" q ").isRequired,"
 :ann "propTypeArrayOfRequired")

;; rjsx-mode/React/ptar
(ptar "PropTypes.array.isRequired,"
 :ann "propTypeArrayRequired")

;; rjsx-mode/React/ptb
(ptb "PropTypes.bool,"
 :ann "propTypeBool")

;; rjsx-mode/React/ptbr
(ptbr "PropTypes.bool.isRequired,"
 :ann "propTypeBoolRequired")

;; rjsx-mode/React/pte
(pte "PropTypes.oneOf(['" q "']),"
 :ann "propTypeEnum")

;; rjsx-mode/React/ptel
(ptel "PropTypes.element,"
 :ann "propTypeElement")

;; rjsx-mode/React/ptelr
(ptelr "PropTypes.element.isRequired,"
 :ann "propTypeElementRequired")

;; rjsx-mode/React/pter
(pter "PropTypes.oneOf(['" q "']).isRequired,"
 :ann "propTypeEnumRequired")

;; rjsx-mode/React/ptet
(ptet "PropTypes.oneOfType([" n
 "  " > q n
 "])," >
 :ann "propTypeOneOfType")

;; rjsx-mode/React/ptetr
(ptetr "PropTypes.oneOfType([" n
 "  " > q n
 "]).isRequired," >
 :ann "propTypeOneOfTypeRequired")

;; rjsx-mode/React/ptf
(ptf "PropTypes.func,"
 :ann "propTypeFunc")

;; rjsx-mode/React/ptfr
(ptfr "PropTypes.func.isRequired,"
 :ann "propTypeFuncRequired")

;; rjsx-mode/React/pti
(pti "PropTypes.instanceOf(" q "),"
 :ann "propTypeInstanceOf")

;; rjsx-mode/React/ptir
(ptir "PropTypes.instanceOf(" q ").isRequired,"
 :ann "propTypeInstanceOfRequired")

;; rjsx-mode/React/ptn
(ptn "PropTypes.number,"
 :ann "propTypeNumber")

;; rjsx-mode/React/ptnd
(ptnd "PropTypes.node,"
 :ann "propTypeNode")

;; rjsx-mode/React/ptndr
(ptndr "PropTypes.node.isRequired,"
 :ann "propTypeNodeRequired")

;; rjsx-mode/React/ptnr
(ptnr "PropTypes.number.isRequired,"
 :ann "propTypeNumberRequired")

;; rjsx-mode/React/pto
(pto "PropTypes.object,"
 :ann "propTypeObject")

;; rjsx-mode/React/ptoo
(ptoo "PropTypes.objectOf(" q "),"
 :ann "propTypeObjectOf")

;; rjsx-mode/React/ptoor
(ptoor "PropTypes.objectOf(" q ").isRequired,"
 :ann "propTypeObjectOfRequired")

;; rjsx-mode/React/ptor
(ptor "PropTypes.object.isRequired,"
 :ann "propTypeObjectRequired")

;; rjsx-mode/React/pts
(pts "PropTypes.string,"
 :ann "propTypeString")

;; rjsx-mode/React/ptsh
(ptsh "PropTypes.shape({" n
 "  " > q n
 "})," >
 :ann "propTypeShape")

;; rjsx-mode/React/ptshr
(ptshr "PropTypes.shape({" n
 "  " > q n
 "}).isRequired," >
 :ann "propTypeShapeRequired")

;; rjsx-mode/React/ptsr
(ptsr "PropTypes.string.isRequired,"
 :ann "propTypeStringRequired")

;; rjsx-mode/React/ptypes
(ptypes "static propTypes = {" n
 q n
 "}" >
 :ann "staticPropTpyes")

;; rjsx-mode/React/rafc
;; WARNING: Requires YASnippet function ‘yas-jsx-get-class-name-by-file-name’
(rafc "import React from 'react'" n
 n
 "const " `(p ,(format "%s" (or (yas-jsx-get-class-name-by-file-name) "")) f1) " = (props) => {" > n
 "  return (" > n
 "    <div>" > n
 "      " > q n
 "    </div>" > n
 "  )" > n
 "}" > n
 n
 "export default " f1 >
 :ann "reactArrowFunctionComponent")

;; rjsx-mode/React/rafce
;; WARNING: Requires YASnippet function ‘yas-jsx-get-class-name-by-file-name’
(rafce "import React from 'react'" n
 n
 "const " `(p ,(format "%s" (or (yas-jsx-get-class-name-by-file-name) "")) f1) " = (props) => {" > n
 "  return (" > n
 "    <div>" > n
 "      " > q n
 "    </div>" > n
 "  )" > n
 "}" > n
 n
 "export default " f1 >
 :ann "reactArrowFunctionExportComponent")

;; rjsx-mode/React/rafcp
;; WARNING: Requires YASnippet function ‘yas-jsx-get-class-name-by-file-name’
(rafcp "import React from 'react'" n
 "import PropTypes from 'prop-types'" > n
 n
 "const " `(p ,(format "%s" (or (yas-jsx-get-class-name-by-file-name) "")) f1) " = (props) => {" > n
 "  return (" > n
 "    <div>" > n
 "      " > q n
 "    </div>" > n
 "  )" > n
 "}" > n
 n>
 f1 ".propTypes = {" n
 n
 "}" > n
 n
 "export default " f1 >
 :ann "reactArrowFunctionComponentWithPropTypes")

;; rjsx-mode/React/rcc
;; WARNING: Requires YASnippet function ‘yas-jsx-get-class-name-by-file-name’
(rcc "import React, { Component } from 'react'" n
 n
 "export default class " `(p ,(format "%s" (or (yas-jsx-get-class-name-by-file-name) ""))) " extends Component {" > n
 "  render() {" > n
 "    return (" > n
 "      <div>" > n
 "        " > q n
 "      </div>" > n
 "    )" > n
 "  }" > n
 "}" >
 :ann "reactClassCompoment")

;; rjsx-mode/React/rccp
;; WARNING: Requires YASnippet function ‘yas-jsx-get-class-name-by-file-name’
(rccp "import React, { Component } from 'react'" n
 "import PropTypes from 'prop-types'" > n
 n
 "export default class " `(p ,(format "%s" (or (yas-jsx-get-class-name-by-file-name) ""))) " extends Component {" > n
 "  static propTypes = {" > n
 "    " > (p "prop") ": " (p "PropTypes") n
 "  }" > n
 n
 "  render() {" > n
 "    return (" > n
 "      <div>" > n
 "        " > q n
 "      </div>" > n
 "    )" > n
 "  }" > n
 "}" >
 :ann "reactClassCompomentPropTypes")

;; rjsx-mode/React/rce
;; WARNING: Requires YASnippet function ‘yas-jsx-get-class-name-by-file-name’
(rce "import React, { Component } from 'react'" n
 n
 "export class " `(p ,(format "%s" (or (yas-jsx-get-class-name-by-file-name) "")) f1) " extends Component {" > n
 "  render() {" > n
 "    return (" > n
 "      <div>" > n
 "        " > q n
 "      </div>" > n
 "    )" > n
 "  }" > n
 "}" > n
 n
 "export default " f1 >
 :ann "reactClassExportComponent")

;; rjsx-mode/React/rcep
;; WARNING: Requires YASnippet function ‘yas-jsx-get-class-name-by-file-name’
(rcep "import React, { Component } from 'react'" n
 "import PropTypes from 'prop-types'" > n
 n
 "export class " `(p ,(format "%s" (or (yas-jsx-get-class-name-by-file-name) "")) f1) " extends Component {" > n
 "  static propTypes = {" > n
 n
 "  }" > n
 n
 "  render() {" > n
 "    return (" > n
 "      <div>" > n
 "        " > q n
 "      </div>" > n
 "    )" > n
 "  }" > n
 "}" > n
 n
 "export default " f1 >
 :ann "reactClassExportComponentWithPropTypes")

;; rjsx-mode/React/rconst
(rconst "constructor(props) {" n
 "  super(props)" > n
 n
 "  this.state = {" > n
 "     " > q n
 "  }" > n
 "}" >
 :ann "classConstructor")

;; rjsx-mode/React/rcontext
(rcontext "const " (p "contextName") " = React.createContext()" q
 :ann "createContext")

;; rjsx-mode/React/ren
(ren "render() {" n
 "  return (" > n
 "    <div>" > n
 "      " > q n
 "    </div>" > n
 "  )" > n
 "}" >
 :ann "componentRender")

;; rjsx-mode/React/rmc
(rmc "import React, { memo } from 'react'" n
 n
 "export default memo((props) => (" > n
 "  <div>" > n
 "    " > q n
 "  </div>" > n
 "))" >
 :ann "reactFunctionMemoComponent")

;; rjsx-mode/React/rmcp
;; WARNING: Requires YASnippet function ‘yas-jsx-get-class-name-by-file-name’
(rmcp "import React, { memo } from 'react'" n
 "import PropTypes from 'prop-types'" > n
 n
 "const " `(p ,(format "%s" (or (yas-jsx-get-class-name-by-file-name) "")) f1) " = memo((props) => (" > n
 "  <div>" > n
 "    " > q n
 "  </div>" > n
 "))" > n
 n>
 f1 ".propTypes = {" n
 n
 "}" > n
 n
 "export default " f1 >
 :ann "reactFunctionMemoComponentWithPropTypes")

;; rjsx-mode/React/rpc
;; WARNING: Requires YASnippet function ‘yas-jsx-get-class-name-by-file-name’
(rpc "import React, { PureComponent } from 'react'" n
 n
 "export default class " `(p ,(format "%s" (or (yas-jsx-get-class-name-by-file-name) ""))) " extends PureComponent {" > n
 "  render() {" > n
 "    return (" > n
 "      <div>" > n
 "        " > q n
 "      </div>" > n
 "    )" > n
 "  }" > n
 "}" >
 :ann "reactClassPureComponent")

;; rjsx-mode/React/rpce
;; WARNING: Requires YASnippet function ‘yas-jsx-get-class-name-by-file-name’
(rpce "import React, { PureComponent } from 'react'" n
 n
 "export class " `(p ,(format "%s" (or (yas-jsx-get-class-name-by-file-name) "")) f1) " extends PureComponent {" > n
 "  render() {" > n
 "    return (" > n
 "      <div>" > n
 "        " > q n
 "      </div>" > n
 "    )" > n
 "  }" > n
 "}" > n
 n
 "export default " f1 >
 :ann "reactClassExportPureComponent")

;; rjsx-mode/React/rpcp
;; WARNING: Requires YASnippet function ‘yas-jsx-get-class-name-by-file-name’
(rpcp "import React, { PureComponent } from 'react'" n
 "import PropTypes from 'prop-types'" > n
 n
 "export default class " `(p ,(format "%s" (or (yas-jsx-get-class-name-by-file-name) ""))) " extends PureComponent {" > n
 "  static propTypes = {" > n
 n
 "  }" > n
 n
 "  render() {" > n
 "    return (" > n
 "      <div>" > n
 "        " > q n
 "      </div>" > n
 "    )" > n
 "  }" > n
 "}" >
 :ann "reactClassPureComponentWithPropTypes")

;; rjsx-mode/React/scu
(scu "shouldComponentUpdate(nextProps, nextState) {" n
 "  " > q n
 "}" >
 :ann "shouldComponentUpdate")

;; rjsx-mode/React/ssf
(ssf "this.setState((state, props) => { return { " q " }})"
 :ann "componentSetStateFunc")

;; rjsx-mode/React/sst
(sst "this.setState({" q "})"
 :ann "componentSetStateObject")

;; rjsx-mode/React/state
(state "this.state." q
 :ann "componentState")

;; rjsx-mode/React-Native/imrn
(imrn "import { " (p "moduleName") " } from 'react-native'" q
 :ann "reactNativeImport")

;; rjsx-mode/React-Native/rnc
;; WARNING: Requires YASnippet function ‘yas-jsx-get-class-name-by-file-name’
(rnc "import React, { Component } from 'react'" n
 "import { Text, View } from 'react-native'" > n
 n
 "export default class " `(p ,(format "%s" (or (yas-jsx-get-class-name-by-file-name) ""))) " extends Component {" > n
 "  render() {" > n
 "    return (" > n
 "      <View>" > n
 "        <Text>" (p "textInComponent") "</Text>" > n
 "      </View>" > n
 "    )" > n
 "  }" > n
 "}" q >
 :ann "reactNativeComponent")

;; rjsx-mode/React-Native/rnce
;; WARNING: Requires YASnippet function ‘yas-jsx-get-class-name-by-file-name’
(rnce "import React, { Component } from 'react'" n
 "import { Text, View } from 'react-native'" > n
 n
 "export class " `(p ,(format "%s" (or (yas-jsx-get-class-name-by-file-name) "")) f1) " extends Component {" > n
 "  render() {" > n
 "    return (" > n
 "      <View>" > n
 "        <Text>" (p "textInComponent") "</Text>" > n
 "      </View>" > n
 "    )" > n
 "  }" > n
 "}" > n
 n
 "export default " f1 >
 :ann "reactNativeComponentExport")

;; rjsx-mode/React-Native/rncs
;; WARNING: Requires YASnippet function ‘yas-jsx-get-class-name-by-file-name’
(rncs "import React, { Component } from 'react'" n
 "import { Text, StyleSheet, View } from 'react-native'" > n
 n
 "export default class " `(p ,(format "%s" (or (yas-jsx-get-class-name-by-file-name) ""))) " extends Component {" > n
 "  render() {" > n
 "    return (" > n
 "      <View>" > n
 "        <Text>" (p "textInComponent") "</Text>" > n
 "      </View>" > n
 "    )" > n
 "  }" > n
 "}" > n
 n
 "const styles = StyleSheet.create({})" q >
 :ann "reactNativeComponentWithStyles")

;; rjsx-mode/React-Native/rnpc
;; WARNING: Requires YASnippet function ‘yas-jsx-get-class-name-by-file-name’
(rnpc "import React, { PureComponent } from 'react'" n
 "import { Text, View } from 'react-native'" > n
 n
 "export default class " `(p ,(format "%s" (or (yas-jsx-get-class-name-by-file-name) ""))) " extends PureComponent {" > n
 "  render() {" > n
 "    return (" > n
 "      <View>" > n
 "        <Text>" (p "textInComponent") "</Text>" > n
 "      </View>" > n
 "    )" > n
 "  }" > n
 "}" q >
 :ann "reactNativePureComponent")

;; rjsx-mode/React-Native/rnpce
;; WARNING: Requires YASnippet function ‘yas-jsx-get-class-name-by-file-name’
(rnpce "import React, { PureComponent } from 'react'" n
 "import { Text, View } from 'react-native'" > n
 n
 "export class " `(p ,(format "%s" (or (yas-jsx-get-class-name-by-file-name) "")) f1) " extends PureComponent {" > n
 "  render() {" > n
 "    return (" > n
 "      <View>" > n
 "        <Text>" (p "textInComponent") "</Text>" > n
 "      </View>" > n
 "    )" > n
 "  }" > n
 "}" > n
 n
 "export default " f1 >
 :ann "reactNativePureComponentExport")

;; rjsx-mode/React-Native/rnstyle
(rnstyle "const styles = StyleSheet.create({" n
 "  " > (p "style") n
 "})" q >
 :ann "reactNativeStyles")

;; rjsx-mode/Redux/hocredux
(hocredux "import React from 'react'" n
 "import PropTypes from 'prop-types'" > n
 "import { connect } from 'react-redux'" > n
 n
 "export const " (p "hocComponentName" f1) " = (WrappedComponent) => {" > n
 "  const hocComponent = (props) => <WrappedComponent {...props} />" > n
 n
 "  hocComponent.propTypes = {" > n
 "  }" > n
 n
 "  return hocComponent" > n
 "}" > n
 n
 "const mapStateToProps = (state, ownProps) => ({" > n
 n
 "})" > n
 n
 "const mapDispatchToProps = (dispatch, ownProps) => ({" > n
 n
 "})" > n
 n
 "export default WrapperComponent => connect(mapStateToProps, mapDispatchToProps)(" f1 "(WrapperComponent))" q >
 :ann "hocComponentWithRedux")

;; rjsx-mode/Redux/rcredux
;; WARNING: Requires YASnippet function ‘yas-jsx-get-class-name-by-file-name’
(rcredux "import React, { Component } from 'react'" n
 "import PropTypes from 'prop-types'" > n
 "import { connect } from 'react-redux'" > n
 n
 "export class " `(p ,(format "%s" (or (yas-jsx-get-class-name-by-file-name) "")) f1) " extends Component {" > n
 "  static propTypes = {" > n
 "    " > (p "prop") ": " (p "PropTypes") n
 "  }" > n
 n
 "  render() {" > n
 "    return (" > n
 "      <div>" > n
 "        " > q n
 "      </div>" > n
 "    )" > n
 "  }" > n
 "}" > n
 n
 "const mapStateToProps = (state, ownProps) => ({" > n
 "  " > n
 "})" > n
 n
 "const mapDispatchToProps = (dispatch, ownProps) => ({" > n
 "  " > n
 "})" > n
 n
 "export default connect(mapStateToProps, mapDispatchToProps)(" f1 ")" >
 :ann "reactClassCompomentRedux")

;; rjsx-mode/Redux/redux
(redux "import { connect } from 'react-redux'"
 :ann "import redux statement")

;; rjsx-mode/Redux/reduxmap
(reduxmap "const mapStateToProps = (state, ownProps) => ({" n
 n
 "})" > n
 n
 "const mapDispatchToProps = (dispatch, ownProps) => ({" > n
 n
 "})" > n
 n
 "const mergeProps = (stateProps, dispatchProps, ownProps) => ({" > n
 "  ...ownProps," > n
 "  ...dispatchProps," > n
 "  ...stateProps," > n
 "})" >
 :ann "mappingToProps")

;; rjsx-mode/Redux/rncredux
;; WARNING: Requires YASnippet function ‘yas-jsx-get-class-name-by-file-name’
(rncredux "import React, { Component } from 'react'" n
 "import { View, Text } from 'react-native'" > n
 "import PropTypes from 'prop-types'" > n
 "import { connect } from 'react-redux'" > n
 n
 "export class " `(p ,(format "%s" (or (yas-jsx-get-class-name-by-file-name) "")) f1) " extends Component {" > n
 "  static propTypes = {" > n
 "    " > (p "prop" f2) ": " (p "PropTypes") n
 "  }" > n
 n
 "  render() {" > n
 "    return (" > n
 "      <View>" > n
 "        <Text>" f2 "</Text>" > n
 "      </View>" > n
 "    )" > n
 "  }" > n
 "}" > n
 n
 "const mapStateToProps = (state, ownProps) => ({" > n
 n
 "})" > n
 n
 "const mapDispatchToProps = (dispatch, ownProps) => ({" > n
 n
 "})" > n
 n
 "export default connect(mapStateToProps, mapDispatchToProps)(" f1 ")" q >
 :ann "reactNativeClassComponentRedux")

;; rjsx-mode/Redux/rxaction
;; WARNING: Requires YASnippet function ‘yas-snake-case’
(rxaction "export const " (p "actionName" f1) " = (payload) => ({" n
 "  type: " (or ((yas-snake-case f1)) f1) "," > n
 "  payload" > n
 "})" q >
 :ann "reduxAction")

;; rjsx-mode/Redux/rxconst
(rxconst "export const " (p "constantName" f1) " = '" f1 "'" q
 :ann "reduxConst")

;; rjsx-mode/Redux/rxreducer
(rxreducer "const initialState = {" n
 n
 "}" > n
 n
 "export default (state = initialState, { type, payload }) => {" > n
 "  switch (type) {" > n
 "    case " (p "typeName") ":" > n
 "      return { ...state, ...payload }" > n
 n
 "    default:" > n
 "      return state" > n
 "  }" > n
 "}" q >
 :ann "reduxReducer")

;; rjsx-mode/Redux/rxselect
(rxselect "import { createSelector } from 'reselect'" n
 n
 "export const " (p "selectorName") " = state => state." (p "selector") >
 :ann "reduxSelector")


rst-mode

;; rst-mode/autoclass
;; WARNING: Nested fields flattened into field 1
;; WARNING: $0 is not the last field; kept as a plain field
(auto ".. autoclass:: " p n
 "   " > (p ":members: members")
 :ann "autoclass")

;; rst-mode/autofunction
;; WARNING: Duplicate key ‘auto’ renamed to ‘auto-2’
(auto-2 ".. autofunction:: " q
 :ann "autofunction")

;; rst-mode/automodule
;; WARNING: Duplicate key ‘auto’ renamed to ‘auto-3’
(auto-3 ".. automodule:: " (p "module_name")
 :ann "automodule")

;; rst-mode/chapter
(chap (p "Chapter" f1) n>
 (make-string (string-width f1) 61) n
 n
 q n
 
 :ann "Chapter title")

;; rst-mode/class
(cls ":class:`" q "`"
 :ann "class")

;; rst-mode/code
(code ".. code:: " (p "python"))

;; rst-mode/digraph
(graph ".. digraph:: " p n
 n
 "   " > q
 :ann "digraph")

;; rst-mode/function
(fun ":function:`" q "`"
 :ann "function")

;; rst-mode/graph
;; WARNING: Duplicate key ‘graph’ renamed to ‘graph-2’
(graph-2 ".. graph:: " p n
 n
 "   " > q)

;; rst-mode/graphviz
;; WARNING: Duplicate key ‘graph’ renamed to ‘graph-3’
(graph-3 ".. graphviz::" n
 n
 "   " > q
 :ann "graphviz")

;; rst-mode/image
(img ".. image:: " (p "path") n
 "   :height: " (p "100") > n
 "   :width: " (p "200") > n
 "   :alt: " (p "description") > n
 n
 q
 :ann "image")

;; rst-mode/inheritance
(inh ".. inheritance-diagram:: " q
 :ann "inheritance")

;; rst-mode/literal_include
(inc ".. literalinclude:: " (p "path")
 :ann "literatal include")

;; rst-mode/meta
(: ":" (p "var") ": " q
 :ann "meta")

;; rst-mode/module
(mod ":mod: `" q "`"
 :ann "module")

;; rst-mode/parsed_literal
(src ".. parsed-literal::" n
 "   " > q
 :ann "parsed_literal")

;; rst-mode/pause
(pause ".. rst-class:: build")

;; rst-mode/section
(sec (p "Section" f1) n>
 (make-string (string-width f1) 45) n
 n
 q n
 
 :ann "Section title")

;; rst-mode/term
(term ":term:`" q "`")

;; rst-mode/title
(tit (make-string (string-width f1) 61) n>
 (p "Title" f1) n>
 (make-string (string-width f1) 61) n
 n
 q n
 
 :ann "Document title")

;; rst-mode/url
(url ".. _" (p "description") ": " q)

;; rst-mode/verbatim
(| "| " q n
 "|" >
 :ann "verbatim")

;; rst-mode/warning
(warn ".. warning:" n
 "   " > q
 :ann "warning")


ruby-mode enh-ruby-mode ruby-ts-mode

;; ruby-mode/#
(\# "# => "
 :ann "# =>")

;; ruby-mode/=b
(=b "=begin rdoc" n
 "  " > q n
 "=end" >
 :ann "=begin rdoc ... =end")

;; ruby-mode/Comp
(Comp "include Comparable" n
 n
 "def <=> other" > n
 "  " > q n
 "end" >
 :ann "include Comparable; def <=> ... end")

;; ruby-mode/Enum
(Enum "include Enumerable" n
 n
 "def each" (p "(&block)") > n
 "  " > q n
 "end" > n
 
 :ann "include Enumerable")

;; ruby-mode/GLOB
($ "$" (p "GLOBAL") " = " q
 :ann "GLOB")

;; ruby-mode/all
(all "all? { |" (p "e") "| " q " }"
 :ann "all? { |...| ... }")

;; ruby-mode/am
(am "alias_method :" (p "new_name") ", :" (p "old_name")
 :ann "alias_method new, old")

;; ruby-mode/any
(any "any? { |" (p "e") "| " q " }"
 :ann "any? { |...| ... }")

;; ruby-mode/app
(app "if __FILE__ == $PROGRAM_NAME" n
 "  " > q n
 "end" >
 :ann "if __FILE__ == $PROGRAM_NAME ... end")

;; ruby-mode/attribute
(@ "@" (p "attr") " = " q
 :ann "attribute")

;; ruby-mode/bang
(\#! "#!/usr/bin/env ruby" n
 )

;; ruby-mode/bench
(bench "require \"benchmark\"" n
 n
 "TESTS = " (p "1_000") > n
 "Benchmark.bmbm do |x|" > n
 "  x.report(\"" (p "var") "\") {}" > n
 "end" > n
 q)

;; ruby-mode/bm
(bm "Benchmark.bmbm(" (p "10") ") do |x|" n
 "  " > q n
 "end" >
 :ann "Benchmark.bmbm(...) do ... end")

;; ruby-mode/case
(case "case " (p "object") n
 "when " (p "condition") > n
 "  " > q n
 "end" >
 :ann "case ... end")

;; ruby-mode/cla
(cla "class << " (p "self") n
 "  " > q n
 "end" >
 :ann "class << self ... end")

;; ruby-mode/cls
(cls "class " `(p ,(format "%s" (or (let ((fn (capitalize (file-name-nondirectory (file-name-sans-extension (or (buffer-file-name) (buffer-name (current-buffer)))))))) (replace-regexp-in-string "_" "" fn t t)) ""))) n
 "  " > q n
 "end" >
 :ann "class ... end")

;; ruby-mode/collect
(collect "collect { |" (p "e") "| " q " }"
 :ann "collect { |...| ... }")

;; ruby-mode/dee
(dee "Marshal.load(Marshal.dump(" q "))"
 :ann "deep_copy(...)")

;; ruby-mode/def
;; WARNING: Nested fields flattened into field 2
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(def "def " (p "method") (p "(args)") n
 "    " > q n
 "end" >
 :ann "def ... end")

;; ruby-mode/deli
(deli "delete_if { |" (p "e") "| " q " }"
 :ann "delete_if { |...| ... }")

;; ruby-mode/det
(det "detect { |" (p "e") "| " q " }"
 :ann "detect { |...| ... }")

;; ruby-mode/dow
;; WARNING: $0 is not the last field; kept as a plain field
(dow "downto(" p ") { |" (p "n") "|" n
 "  " > n
 "}" >
 :ann "downto(...) { |n| ... }")

;; ruby-mode/ea
(ea "each { |" (p "e") "| " q " }"
 :ann "each { |...| ... }")

;; ruby-mode/eac
(eac "each_cons(" (p "2") ") { |" (p "group") "| " q " }"
 :ann "each_cons(...) { |...| ... }")

;; ruby-mode/eai
(eai "each_index { |" (p "i") "| " q " }"
 :ann "each_index { |i| ... }")

;; ruby-mode/eav
(eav "each_value { |" (p "val") "| " q " }"
 :ann "each_value { |val| ... }")

;; ruby-mode/eawi
(eawi "each_with_index { |" (p "e") ", " (p "i") "| " q " }"
 :ann "each_with_index { |e, i| ... }")

;; ruby-mode/for
(for "for " (p "el") " in " (p "collection") n
 "    " > q n
 "end" >)

;; ruby-mode/forin
(forin "for " (p "element") " in " (p "collection") n
 "  " > q n
 "end" >
 :ann "for ... in ...; ... end")

;; ruby-mode/formula
(form "require 'formula'" n
 n
 "class " (p "Name") " <Formula" > n
 "  url '" (p "url") "'" > n
 "  homepage '" (p "home") "'" > n
 "  md5 '" (p "md5") "'" > n
 n
 "  def install" > n
 "    " > (p "system \"./configure\"") n
 "    " > q n
 "  end" > n
 "end" > n
 
 :ann "formula")

;; ruby-mode/if
(if "if " (p "condition") n
 "  " > q n
 "end" >
 :ann "if ... end")

;; ruby-mode/ife
(ife "if " (p "condition") n
 "  " > p n
 "else" > n
 "  " > p n
 "end" q >
 :ann "if ... else ... end")

;; ruby-mode/inc
(inc "include " (p "Module") n
 q n
 
 :ann "include Module")

;; ruby-mode/init
(init "def initialize(" (p "args") ")" n
 "    " > q n
 "end" >)

;; ruby-mode/inject
(inject "inject(" (p "0") ") { |" (p "injection") ", " (p "element") "| " q " }"
 :ann "inject(...) { |...| ... }")

;; ruby-mode/map
(map "map { |" (p "e") "| " q " }"
 :ann "map { |...| ... }")

;; ruby-mode/mm
(mm "def method_missing(method, *args)" n
 "  " > q n
 "end" >
 :ann "def method_missing ... end")

;; ruby-mode/mod
(mod "module " `(p ,(format "%s" (or (let ((fn (capitalize (file-name-nondirectory (file-name-sans-extension (or (buffer-file-name) (buffer-name (current-buffer)))))))) (while (string-match "_" fn) (setq fn (replace-match "" nil nil fn))) fn) ""))) n
 "  " > q n
 "end" >
 :ann "module ... end")

;; ruby-mode/proc
(proc "Proc.new { |" (p "argument") "| " q " }"
 :ann "Proc.new { |...| ... }")

;; ruby-mode/pry
(pry "require 'pry'; binding.pry" n
 
 :ann "binding.pry")

;; ruby-mode/r
(r "attr_reader :"
 :ann "attr_reader ...")

;; ruby-mode/rb
(rb "#!/usr/bin/ruby -wU"
 :ann "/usr/bin/ruby -wU")

;; ruby-mode/red
(red "reduce(" (p "0") ") { |" (p "accumulator") ", " (p "element") "| " q " }"
 :ann "reduce(...) { |...| ... }")

;; ruby-mode/reject
(reject "reject { |" (p "element") "| " q " }"
 :ann "reject { |...| ... }")

;; ruby-mode/rel
(rel "require_relative '" q "'"
 :ann "require_relative")

;; ruby-mode/req
(req "require '" q "'"
 :ann "require \"...\"")

;; ruby-mode/rpry
(rpry "require 'pry-remote'; binding.remote_pry"
 :ann "binding.pry_remote")

;; ruby-mode/rw
(rw "attr_accessor :"
 :ann "attr_accessor ...")

;; ruby-mode/select
(select "select { |" (p "element") "| " q " }"
 :ann "select { |...| ... }")

;; ruby-mode/str
(s "#{" q "}"
 :ann "str")

;; ruby-mode/test class
(tc "class TC_" (p "Class") " < Test::Unit::TestCase" n
 "      " > q n
 "end" >
 :ann "test class")

;; ruby-mode/tim
(tim "times { |" (p "n") "| " q " }"
 :ann "times { |n| ... }")

;; ruby-mode/to_
(to_ "def to_s" n
 "    \"" (p "string") "\"" > n
 "end" > n
 q)

;; ruby-mode/tu
(tu "require 'test/unit'")

;; ruby-mode/unless
(unless "unless " (p "condition") n
 "  " > q n
 "end" >
 :ann "unless ... end")

;; ruby-mode/until
(until "until " (p "condition") n
 "  " > q n
 "end" >
 :ann "until ... end")

;; ruby-mode/upt
(upt "upto(" (p "n") ") { |" (p "i") "|" n
 "  " > q n
 "}" >
 :ann "upto(...) { |n| ... }")

;; ruby-mode/w
(w "attr_writer :"
 :ann "attr_writer ...")

;; ruby-mode/when
(when "when " (p "condition") n
 "  " > q n
 "end" >
 :ann "when ... end")

;; ruby-mode/while
(while "while " (p "condition") n
 "  " > q n
 "end" >
 :ann "while ... end")

;; ruby-mode/y
(y ":yields: " q
 :ann ":yields: arguments (rdoc)")

;; ruby-mode/zip
(zip "zip(" (p "enums") ") { |" (p "row") "| " q " }"
 :ann "zip(...) { |...| ... }")


rust-mode rust-ts-mode rustic-mode

;; rust-mode/afn
(afn "async fn " (p "name") "(" p ") {" n
 "    " > q n
 "}" >
 :ann "async fn name() { ... }")

;; rust-mode/afnr
(afnr "async fn " (p "name") "(" p ") -> " (p "Type") " {" n
 "    " > q n
 "}" >
 :ann "async fn name() -> Type { ... }")

;; rust-mode/afns
(afns "async fn " (p "name") "(" (p "&self") ") -> " (p "Type") "  {" n
 "    " > q n
 "}" >
 :ann "async fn name(&self) -> Type  { ... }")

;; rust-mode/afnw
(afnw "async fn " (p "name") "<" (p "T") ">(" (p "x: T") ") where " (p "T: Clone") " {" n
 "    " > q n
 "}" >
 :ann "async fn name<T>(x: T) where T: Clone { ... }")

;; rust-mode/allow
(allow "#[allow(" (p "lint") ")]" q
 :ann "#[allow(lint)]")

;; rust-mode/allow!
(allow! "#![allow(" (p "lint") ")]" q
 :ann "#![allow(lint)]")

;; rust-mode/assert
(ass "assert!(" (p "predicate") ");" q
 :ann "assert!(predicate);")

;; rust-mode/asserteq
(asseq "assert_eq!(" (p "expected") ", " (p "actual") ");" q
 :ann "assert_eq!(expected, actual);")

;; rust-mode/case
(case (p "pattern") " => " (p "expression") "," q
 :ann "pattern => expression,")

;; rust-mode/cfg
(cfg "#[cfg(" (p "option") ")]" q
 :ann "#[cfg(option)]")

;; rust-mode/cfg=
(cfg= "#[cfg(" (p "option") " = \"" (p "value") "\")]" q
 :ann "#[cfg(option = \"value\")]")

;; rust-mode/closure
(|| "|" (p "arguments") "| {" n
 "    " > q n
 "}" > n
 
 :ann "closure")

;; rust-mode/crate
(ec "extern crate " (p "name") ";" n
 q
 :ann "extern crate")

;; rust-mode/deny
(deny "#[deny(" (p "lint") ")]" q
 :ann "#[deny(lint)]")

;; rust-mode/deny!
(deny! "#![deny(" (p "lint") ")]" q
 :ann "#![deny(lint)]")

;; rust-mode/derive
(derive "#[derive(" (p "Trait") ")]" q
 :ann "#[derive(Trait)]")

;; rust-mode/display
(display "impl Display for " (p "Type") " {" n
 "    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {" > n
 "        write!(f, \"" q "\")" > n
 "    }" > n
 "}" > n
 
 :ann "impl Display for Type { fn fmt (...) }")

;; rust-mode/drop
(drop "impl Drop for " (p "Type") " {" n
 "    fn drop(&mut self) {" > n
 "        " > q n
 "    }" > n
 "}" > n
 
 :ann "impl Drop for Type { fn drop(...) }")

;; rust-mode/enum
(enum "enum " (p "Type") " {" n
 "    " > q n
 "}" > n
 
 :ann "enum Type { ... }")

;; rust-mode/eprint
(eprint "eprint!(\"" (p "{}") "\", " p ");" n
 q
 :ann "eprint!(\"{}\", value);")

;; rust-mode/eprintln
(eprintln "eprintln!(\"" (p "{}") "\", " p ");" n
 q
 :ann "eprintln!(\"{}\", value);")

;; rust-mode/fn
(fn "fn " (p "name") "(" p ") {" n
 "    " > q n
 "}" >
 :ann "fn name() { ... }")

;; rust-mode/fnr
(fnr "fn " (p "name") "(" p ") -> " (p "Type") " {" n
 "    " > q n
 "}" > n
 
 :ann "fn name() -> Type { ... }")

;; rust-mode/fns
(fns "fn " (p "name") "(" (p "&self") ") -> " (p "Type") "  {" n
 "    " > q n
 "}" >
 :ann "fn name(&self) -> Type  { ... }")

;; rust-mode/fnw
(fnw "fn " (p "name") "<" (p "T") ">(" (p "x: T") ") where " (p "T: Clone") " {" n
 "    " > q n
 "}" > n
 
 :ann "fn name<T>(x: T) where T: Clone { ... }")

;; rust-mode/for
(for "for " (p "var") " in " (p "iterable") " {" n
 "    " > q n
 "}" >
 :ann "for var in iterable { ... }")

;; rust-mode/from
(from "impl From<" (p "From" f1) "> for " (p "Type") " {" n
 "    fn from(source: " f1 ") -> Self {" > n
 "        " > q n
 "        Self { }" > n
 "    }" > n
 "}" > n
 
 :ann "impl From<From> for Type { fn from(...) }")

;; rust-mode/fromstr
(fromstr "impl FromStr for " (p "Type") " {" n
 "    type Err = " (p "Error") ";" > n
 n
 "    fn from_str(s: &str) -> Result<Self, Self::Err> {" > n
 "        Ok(Self{})" > n
 "    }" > n
 "}" > n
 q
 :ann "impl FromStr for Type { fn from_str(...) }")

;; rust-mode/if
(if "if " (p "expression") " {" n
 "    " > q n
 "}" >
 :ann "if expr { ... }")

;; rust-mode/ife
(ife "if " (p "expression") " {" n
 "    " > q n
 "} else {" > n
 "    " > n
 "}" > n
 
 :ann "if expression { ... } else { .. }")

;; rust-mode/ifl
(ifl "if let " (p "pattern") " = " (p "expression") " {" n
 "    " > q n
 "};" > n
 
 :ann "if let pattern = expression { ... };")

;; rust-mode/impl
(impl "impl " (p "Type") " {" n
 "    " > q n
 "}" > n
 
 :ann "impl Type { ... }")

;; rust-mode/implt
(implt "impl " (p "Trait") " for " (p "Type") " {" n
 "    " > q n
 "}" > n
 
 :ann "impl Trait for Type { ... }")

;; rust-mode/let
(let "let " (p "pattern") " = " (p "expression") ";" q
 :ann "let pattern = expression;")

;; rust-mode/letm
(letm "let mut " (p "pattern") " = " (p "expression") ";" q
 :ann "let mut pattern = expression;")

;; rust-mode/lett
(lett "let " (p "pattern") ": " (p "type") " = " (p "expression") ";" q
 :ann "let pattern: type = expression;")

;; rust-mode/lettm
(lettm "let mut " (p "pattern") ": " (p "type") " = " (p "expression") ";" q
 :ann "let mut pattern: type = expression;")

;; rust-mode/loop
(loop "loop {" n
 "    " > q n
 "}" > n
 
 :ann "loop { ... }")

;; rust-mode/macro
(macro "macro_rules! " (p "name") " {" n
 "    (" p ") => (" p ");" > n
 "}" > n
 q
 :ann "macro_rules! name { (..) => (..); }")

;; rust-mode/main
(main "fn main() {" n
 "    " > q n
 "}" > n
 
 :ann "fn main() { ... }")

;; rust-mode/match
(match "match " (p "expression") " {" n
 "    " > q n
 "}" > n
 
 :ann "match expression { ... }")

;; rust-mode/new
(new "pub fn new(" p ") -> " (p "Name" f2) " {" n
 "    " > f2 " { " p " }" n
 "}" > n
 q
 :ann "pub fn new() { ... }")

;; rust-mode/pafn
(pafn "pub async fn " (p "name") "(" p ") {" n
 "    " > q n
 "}" >
 :ann "pub async fn name() { ... }")

;; rust-mode/pafnr
(pafnr "pub async fn " (p "name") "(" p ") -> " (p "Type") " {" n
 "    " > q n
 "}" >
 :ann "pub async fn name() -> Type { ... }")

;; rust-mode/pafns
(pafns "pub async fn " (p "name") "(" (p "&self") ") -> " (p "Type") " {" n
 "    " > q n
 "}" >
 :ann "pub async fn name(&self) -> Type  { ... }")

;; rust-mode/pafnw
(pafnw "pub async fn " (p "name") "<" (p "T") ">(" (p "x: T") ") where " (p "T: Clone") " {" n
 "    " > q n
 "}" >
 :ann "pub async fn name<T>(x: T) where T: Clone { ... }")

;; rust-mode/pfn
(pfn "pub fn " (p "name") "(" p ") {" n
 "    " > q n
 "}" >
 :ann "pub fn name() { ... }")

;; rust-mode/pfnr
(pfnr "pub fn " (p "name") "(" p ") -> " (p "Type") " {" n
 "    " > q n
 "}" > n
 
 :ann "pub fn name() -> Type { ... }")

;; rust-mode/pfns
(pfns "pub fn " (p "name") "(" (p "&self") ") -> " (p "Type") " {" n
 "    " > q n
 "}" >
 :ann "pub fn name(&self) -> Type  { ... }")

;; rust-mode/pfnw
(pfnw "pub fn " (p "name") "<" (p "T") ">(" (p "x: T") ") where " (p "T: Clone") " {" n
 "    " > q n
 "}" > n
 
 :ann "pub fn name<T>(x: T) where T: Clone { ... }")

;; rust-mode/print
(print "print!(\"" (p "{}") "\", " p ");" q
 :ann "print!(\"{}\", value);")

;; rust-mode/println
(println "println!(\"" (p "{}") "\", " p ");" q
 :ann "println!(\"{}\", value);")

;; rust-mode/result
(result "Result<" (p "Type") ", " (p "failure::Error") ">" q
 :ann "Result<Type, failure::Error>")

;; rust-mode/spawn
(spawn "spawn(proc() {" n
 "    " > q n
 "});" > n
 
 :ann "spawn(proc() { ... });")

;; rust-mode/static
(static "static " (p "CONSTANT") ": " (p "Type") " = " (p "value") ";" q
 :ann "CONSTANT: Type = value;")

;; rust-mode/struct
(struct "struct " (p "TypeName") " {" n
 "    " > q n
 "}" > n
 
 :ann "struct TypeName { .. }")

;; rust-mode/test
(test "#[test]" n
 "fn " (p "test_name") "() {" > n
 "    " > q n
 "}" > n
 
 :ann "#[test] fn test_name() { .. }")

;; rust-mode/testmod
(testmod "#[cfg(test)]" n
 "mod " (p "tests") " {" > n
 "    use super::*;" > n
 n
 "    #[test]" > n
 "    fn " (p "test_name") "() {" > n
 "        " > q n
 "    }" > n
 "}" > n
 
 :ann "test module")

;; rust-mode/trait
(trait "trait " (p "Type") " {" n
 "    " > q n
 "}" > n
 
 :ann "trait Type { ... }")

;; rust-mode/type
(type "type " (p "TypeName") " = " (p "TypeName") ";" q
 :ann "type TypeName = TypeName;")

;; rust-mode/union
(union "union " (p "Type") " {" n
 "    " > q n
 "}" > n
 
 :ann "union Type { ... }")

;; rust-mode/warn
(warn "#[warn(" (p "lint") ")]" q
 :ann "#[warn(lint)]")

;; rust-mode/warn!
(warn! "#![warn(" (p "lint") ")]" q
 :ann "#![warn(lint)]")

;; rust-mode/while
(while "while " (p "expression") " {" n
 "    " > q n
 "}" > n
 
 :ann "while expression { ... }")

;; rust-mode/whilel
(whilel "while let " (p "pattern") " = " (p "expression") " {" n
 "    " > q n
 "}" > n
 
 :ann "while let pattern = expression { ... }")


scala-mode ensime-mode scala-ts-mode

;; scala-mode/app
(app "object " (p "name") " extends App {" n
 "  " > q n
 "}" >
 :ann "object name extends App")

;; scala-mode/case
(case "case " (p "_") " => " q
 :ann "case pattern =>")

;; scala-mode/cc
(cc "case class " (p "Name") "(" n
 "  " > (p "arg") ": " (p "Type") n
 ")" q >
 :ann "case class T(arg: A)")

;; scala-mode/co
(co "case object " (p "name") " " q
 :ann "case object T")

;; scala-mode/cons
(cons (p "element1") " :: " (p "element2") " " q
 :ann "element1 :: element2")

;; scala-mode/def
(def "def " (p "name") "(" (p "args") "): " (p "Unit") " = {" n
 "  " > q n
 "}" >
 :ann "def f(arg: T): R = {...}")

;; scala-mode/doc
(doc "/**" n
 " * " (p "description") > n
 " * " q > n
 " */" >
 :ann "/** ... */")

;; scala-mode/docfun
(docfun "/**" n
 " * " p > n
 " * " (or (let* ((indent (concat "\n * ")) (args (mapconcat '(lambda (x) (if (not (string= (nth 0 x) "")) (let ((par-type (mapcar 'string-trim (split-string (nth 0 x) ":")))) (concat "@param " (first par-type) indent "@tparam " (second par-type) indent)))) (mapcar '(lambda (x) (mapcar '(lambda (x) (replace-regexp-in-string "[[:blank:]]*$" "" (replace-regexp-in-string "^[[:blank:]]*" "" x))) x)) (mapcar '(lambda (x) (split-string x "=")) (split-string f3 ","))) indent))) (if (string= args "") (concat indent "@return: " indent "@rtype: " indent (make-string 3 34)) (mapconcat 'identity (list "" args) indent))) f3) > n
 " * @return " (or (and (not (equal f4 "")) f4) f4) > n
 " *" > n
 " **/" > n
 "def " (p "name") "(" (s f3) "): " (s f4) " = " q >
 :ann "docstring function")

;; scala-mode/for
(for "for {" n
 "  " > (p "x") " <- " (p "xs") n
 "} yield " (p "x") >
 :ann "for { x <- xs } yield")

;; scala-mode/if
(if "if (" (p "condition") ") {" n
 "  " > q n
 "}" >
 :ann "if (cond) { .. }")

;; scala-mode/ls
(ls "List(" (p "args") ", " (p "args") ") " q
 :ann "List(..)")

;; scala-mode/main
(main "def main(args: Array[String]) = {" n
 "  " > q n
 "}" >
 :ann "def main(args: Array[String]) = { ... }")

;; scala-mode/match
(match (p "cc") " match {" n
 "  case " (p "pattern") " => " q > n
 "}" >
 :ann "cc match { .. }")

;; scala-mode/ob
(ob "object " (p "name") " extends " (p "type") " " q
 :ann "object name extends T")

;; scala-mode/throw
(throw "throw new " (p "Exception") "(" (p "msg") ") " q
 :ann "throw new Exception")

;; scala-mode/try
;; WARNING: $0 is not the last field; kept as a plain field
(try "try {" n
 "  " > p n
 "} catch {" > n
 "  case e: " (p "Throwable") " =>" > n
 "    " > (p "// TODO: handle exception") n
 "}" >
 :ann "try { .. } catch { case e => ..}")

;; scala-mode/valueclass
(vc "case class " (p "Name") "(value: " (p "Type") ") extends AnyVal" n
 q
 :ann "value class")


sh-mode sh-ts-mode

;; sh-mode/args
(args "if [ $# -lt " (p "2") " ]" n
 "   then " q > n
 "fi" >)

;; sh-mode/bang
(! "#!/usr/bin/env bash" n
 q
 :ann "bang")

;; sh-mode/case
(case "case " (p "cond") " in" n
 "    " > (p "pattern") " )" n
 "        " > (p "stuff") n
 "        ;;" > n
 "    " > q n
 "esac" > n
 )

;; sh-mode/for loop
(for "for " (p "var") " in " (p "stuff") "; do" n
 "    " > q n
 "done" >
 :ann "for loop")

;; sh-mode/function
(f "function " (p "name") " {" n
 "         " > q n
 "}" >
 :ann "function")

;; sh-mode/if
(if "if " (p "[ -f file]") n
 "   then " (p "do") > n
 "fi" > n
 q)

;; sh-mode/ife
(ife "if " (p "cond") n
 "then " (p "stuff") > n
 "else " (p "other") > n
 "fi" > n
 q)

;; sh-mode/safe-bang
(s! "#!/usr/bin/env bash" n
 "set -euo pipefail" > n
 "IFS=$'\\n\\t'" > n
 n
 q
 :ann "safer bash settings for scripts")

;; sh-mode/script-dir
(script-dir "# See https://stackoverflow.com/a/246128/3561275" n
 "SOURCE=\"${BASH_SOURCE[0]}\"" > n
 "while [ -h \"$SOURCE\" ]; do # resolve $SOURCE until the file is no longer a symlink" > n
 "  DIR=\"$( cd -P \"$( dirname \"$SOURCE\" )\" >/dev/null 2>&1 && pwd )\"" > n
 "  SOURCE=\"$(readlink \"$SOURCE\")\"" > n
 "  [[ $SOURCE != /* ]] && SOURCE=\"$DIR/$SOURCE\" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located" > n
 "done" > n
 "DIR=\"$( cd -P \"$( dirname \"$SOURCE\" )\" >/dev/null 2>&1 && pwd )\"" > n
 n
 q
 :ann "the currently executing/sourced script's directory")

;; sh-mode/select
(select "select " (p "var") " in " (p "stuff") "; do" n
 "    " > q n
 "done" > n
 )

;; sh-mode/until
(until "until " (p "cond") "; do" n
 "    " > q n
 "done" >
 :ann "until loop")

;; sh-mode/while
(while "while " (p "cond") "; do" n
 "    " > q n
 "done" >
 :ann "while loop")


snippet-mode

;; snippet-mode/cont
(cont "# contributor: " (list 'l (format "%s" (or user-full-name ""))))

;; snippet-mode/elisp
(\` "`" q "`"
 :ann "elisp")

;; snippet-mode/field
;; WARNING: Nested fields flattened into field 1
;; WARNING: Nested fields flattened into field 4
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(field "${" (p "n:") p (p "$(lisp-fn)") "}" q
 :ann "${ ...  } field")

;; snippet-mode/group
(group "# group : " (p "group"))

;; snippet-mode/mirror
;; WARNING: Nested fields flattened into field 4
(mirror "${" (p "n") ":" (p "$(reflection-fn)") "}" q
 :ann "${n:$(...)} mirror")

;; snippet-mode/vars
;; WARNING: Multi-line default of field 2 is not re-indented
;; WARNING: Nested fields flattened into field 2
;; WARNING: Multi-line default of field 4 is not re-indented
;; WARNING: Nested fields flattened into field 4
;; WARNING: Multi-line default of field 6 is not re-indented
;; WARNING: Nested fields flattened into field 6
;; WARNING: Adjacent fields: text typed at their boundary may go to both
(vars "# name: " p (p "\n# key: trigger-key") (p "\n# keybinding : keybinding") (p "\n# expand-env : ()" f6) n
 "# contributor : " f6 > n
 "# key: vars" > n
 "# --" > n
 q
 :ann "Snippet header")


sql-mode

;; sql-mode/column
(column "	,	" (p "Name") "		" (p "Type") "			" (p "NOT NULL")
 :ann ", ColumnName ColumnType NOT NULL...")

;; sql-mode/constraint
(constraint "CONSTRAINT [" (p "PK_Name") "] PRIMARY KEY " (p "CLUSTERED") " ([" (p "ColumnName") "]) " q
 :ann "CONSTRAINT [..] PRIMARY KEY ...")

;; sql-mode/constraint.1
(constraint.1 "CONSTRAINT [" (p "FK_Name") "] FOREIGN KEY " (p "CLUSTERED") " ([" (p "ColumnName") "]) " q
 :ann "CONSTRAINT [..] FOREIGN KEY ...")

;; sql-mode/create
;; WARNING: $0 is not the last field; kept as a plain field
(create "CREATE TABLE [" (p "dbo") "].[" (p "TableName") "]" n
 "(" > n
 "		" > (p "Id" f3) "		" (p "INT IDENTITY(1,1)") "		" (p "NOT NULL") n
 p n
 "	CONSTRAINT [" (p "PK_") "] PRIMARY KEY " (p "CLUSTERED") " ([" f3 "])" > n
 ")" > n
 "GO" >
 :ann "create table ...")

;; sql-mode/create.1
(create.1 "CREATE PROCEDURE [" (p "dbo") "].[" (p "Name") "]" n
 "(" > n
 "		" > p p "= " (p "NULL") "		" (p "OUTPUT") n
 ")" > n
 "AS" > n
 "BEGIN" > n
 q n
 "END" > n
 "GO" >
 :ann "create procedure ...")

;; sql-mode/insert
(ins "INSERT INTO " (p "table-name") " (" (p "col1, col2") ") VALUES (" (p "'val1', 'val2'") ");" q
 :ann "insert")

;; sql-mode/references
(references "REFERENCES " (p "TableName") "([" (p "ColumnName") "])" q
 :ann "REFERENCES ...")


swift-mode swift-ts-mode

;; swift-mode/available
(available "@available(iOS " p ", *)" q)

;; swift-mode/checkversion
;; WARNING: $0 is not the last field; kept as a plain field
(checkversion "#if swift(>=3.0)" n
 "    " > p n
 "#elseif swift(>=2.2)" > n
 "    " > (s f2) n
 "#elseif swift(>=2.1)" > n
 "    " > f2 n
 "#endif" >)

;; swift-mode/dispatchafter
(dispatchAfter "DispatchQueue.main.asyncAfter(deadline: .now() + .seconds(" p ")) {" n
 "    " > q n
 "}" >)

;; swift-mode/dispatchasync
(dispatchAsync "DispatchQueue.global(qos: .default).async {" n
 "    " > p n
 "    DispatchQueue.main.async {" > n
 "        " > q n
 "    }" > n
 "}" >)

;; swift-mode/dispatchmain
(dispatchMain "DispatchQueue.main.async(execute: { () -> Void in" n
 "    " > q n
 "})" >)

;; swift-mode/documentdirectory
(documentDirectory "if let path =" n
 "    NSSearchPathForDirectoriesInDomains(" > n
 "        FileManager.SearchPathDirectory.documentDirectory," > n
 "        FileManager.SearchPathDomainMask.userDomainMask, true).first {" > n
 "    let documentsDirectoryURL = NSURL.fileURL(withPath: path)" > n
 "}" >)

;; swift-mode/forcase
(forcase "for case let " p " in " p " as " p " {" n
 "    " > q n
 "}" >)

;; swift-mode/fori
(fori "for " p " in " p " {" n
 "    " > q n
 "}" >)

;; swift-mode/func
(func "func " p "() -> " p " {" n
 "    " > q n
 "}" >)

;; swift-mode/if
(if "if " p " {" n
 "    " > q n
 "}" >
 :ann "if (...) { ... }")

;; swift-mode/initcoder
(initcoder "public required init?(coder aDecoder: NSCoder) {" n
 "  fatalError(\"init(coder:) has not been implemented\")" > n
 "}" >)

;; swift-mode/let
(let "let " p ": " p " = " q)

;; swift-mode/mark
(mark "// MARK: - " p)

;; swift-mode/prop
(prop-getset "var " p ": " p " {" n
 "    get {" (s f3) "}" > n
 "    set {" f3 " = value }" > n
 "}" q >)

;; swift-mode/sortarrayofstrings
(sortarrayofstrings p ".sorted { (a, b) -> Bool in" n
 "    let comparisonResult = a.compare(b," > n
 "                                     options: []," > n
 "                                     range: a.range(of: a)," > n
 "                                     locale: Locale.current)" > n
 "    return comparisonResult == .orderedSame" > n
 "}" q >)

;; swift-mode/trycatch
(trycatch "do {" n
 "    try " p > n
 "} catch " p " {" > n
 "    " > p n
 "} catch " p " {" > n
 "    " > p n
 "}" q >)

;; swift-mode/uialertcontroller
(uialertController "let alert =" n
 "    UIAlertController(title: " p "," > n
 "                    message: " p "," > n
 "             preferredStyle: .alert)" > n
 "	alert.addAction(UIAlertAction(title: \"\"," > n
 "                                  style: .default," > n
 "                                handler: { (action) in" > n
 "	alert.dismiss(animated: true, completion: {" > n
 "        " > q n
 "    })" > n
 "}))" > n
 n
 "self.presentViewController(alert, animated: true, completion: nil)" >)

;; swift-mode/uicollectionviewdatasource
(uicollectionViewDataSource "// MARK: UICollectionViewDataSource" n
 n
 "func numberOfSections(in collectionView: UICollectionView) -> Int {" > n
 "    return " p > n
 "}" > n
 n
 "func collectionView(_ collectionView: UICollectionView," > n
 "      numberOfItemsInSection section: Int) -> Int {" > n
 "    // TODO:- Required Method" > n
 "    return " p > n
 "}" > n
 n
 "func collectionView(_ collectionView: UICollectionView," > n
 "             cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {" > n
 "    let cell =" > n
 "        collectionView.dequeueReusableCell(withReuseIdentifier: " (s f3) ", for: indexPath)" > n
 "    configureCell(cell: cell, forItemAt: indexPath)" > n
 "    // TODO:- Required Method" > n
 "    return cell" > n
 "}" > n
 n
 "func configureCell(cell: UICollectionViewCell, forItemAt indexPath: IndexPath) {" > n
 n
 "}" > n
 n
 "func collectionView(_ collectionView: UICollectionView," > n
 "    viewForSupplementaryElementOfKind kind: String," > n
 "                              at indexPath: IndexPath) -> UICollectionReusableView {" > n
 "    let view = collectionView.dequeueReusableSupplementaryView(" > n
 "                   ofKind: UICollectionElementKindSectionHeader," > n
 "                   withReuseIdentifier: " f3 ", for: indexPath) as UIView" > n
 "    return view" > n
 "}" q >)

;; swift-mode/uicollectionviewdelegate
(uiCollectionViewDelegate "// MARK: UICollectionViewDelegate" n
 n
 "func collectionView(_ collectionView: UICollectionView," > n
 "           didSelectItemAt indexPath: IndexPath) {" > n
 n
 "}" > n
 n
 "func collectionView(_ collectionView: UICollectionView," > n
 "         didDeselectItemAt indexPath: IndexPath) {" > n
 n
 "}" >)

;; swift-mode/uitableviewdatasource
(uitableViewDataSource "// MARK: UITableViewDataSource" n
 n
 "func numberOfSections(in tableView: UITableView) -> Int {" > n
 "    return " p > n
 "}" > n
 n
 "func tableView(_ tableView: UITableView," > n
 "    numberOfRowsInSection section: Int) -> Int {" > n
 "    return " p > n
 "}" > n
 n
 "func tableView(_ tableView: UITableView," > n
 "    cellForRowAt indexPath: IndexPath) -> UITableViewCell {" > n
 "    let cell = tableView.dequeueReusableCell(withIdentifier: " p "," > n
 "                                                        for: indexPath)" > n
 "    configureCell(cell: cell, forRowAt: indexPath)" > n
 "    return cell" > n
 "}" > n
 n
 "func configureCell(cell: UITableViewCell, forRowAt indexPath: IndexPath) {" > n
 n
 "}" q >)

;; swift-mode/uitableviewdelegate
(uitableViewDelegate "// MARK: UITableViewDelegate" n
 n
 "func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {" > n
 n
 "}" > n
 n
 "func tableView(_ tableView: UITableView," > n
 "  heightForRowAt indexPath: IndexPath) -> CGFloat {" > n
 n
 "}" > n
 n
 "func tableView(_ tableView: UITableView," > n
 "    heightForHeaderInSection section: Int) -> CGFloat {" > n
 n
 "}" > n
 n
 "func tableView(_ tableView: UITableView," > n
 "    heightForFooterInSection section: Int) -> CGFloat {" > n
 n
 "}" > n
 n
 "func tableView(_ tableView: UITableView," > n
 "    viewForHeaderInSection section: Int) -> UIView? {" > n
 n
 "}" > n
 n
 "func tableView(_ tableView: UITableView," > n
 "    viewForFooterInSection section: Int) -> UIView? {" > n
 n
 "}" >
 :ann "uiTableViewDelegate")

;; swift-mode/uiviewcontrollerlifecycle
(uiViewControllerLifecycle "// MARK: UIViewController lifecycle" n
 n
 "override func viewDidLoad() {" > n
 "    super.viewDidLoad()" > n
 "}" > n
 n
 "override func viewWillAppear(_ animated: Bool) {" > n
 "    super.viewWillAppear(animated)" > n
 "}" > n
 n
 "override func viewDidAppear(_ animated: Bool) {" > n
 "    super.viewDidAppear(animated)" > n
 "}" > n
 n
 "override func viewWillDisappear(_ animated: Bool) {" > n
 "    super.viewWillDisappear(animated)" > n
 "}" > n
 n
 "override func viewDidDisappear(_ animated: Bool) {" > n
 "    super.viewDidDisappear(animated)" > n
 "}" >)

;; swift-mode/var
(var "var " p ": " p " = " q)

;; swift-mode/while
(while "while " p " {" n
 "      " > q n
 "}" >)


terraform-mode

;; terraform-mode/data
(data "data \"" (p "type") "\" \"" (p "name") "\" {" n
 "  " > q n
 "}" > n
 )

;; terraform-mode/google/app_engine_resources/google_app_engine_application
(goog_app_engine_application "resource \"google_app_engine_application\" \"" (p "name") "\" {" n
 "  project     = \"" (p "project_id") "\"" > n
 "  location_id = \"" (p "location_id") "\"" > n
 "}" > n
 n
 q
 :ann "google_app_engine_application")

;; terraform-mode/google/bigquery_resources/google_bigquery_dataset
(goog_bigquery_dataset "resource \"google_bigquery_dataset\" \"" (p "name") "\" {" n
 "  dataset_id = \"" (p "dataset_id") "\"" > n
 "}" > n
 n
 q
 :ann "google_bigquery_dataset")

;; terraform-mode/google/bigquery_resources/google_bigquery_table
(goog_bigquery_table "resource \"google_bigquery_table\" \"" (p "name") "\" {" n
 "  dataset_id = \"" (p "dataset_id") "\"" > n
 "  table_id   = \"" (p "table_id") "\"" > n
 "}" > n
 n
 q
 :ann "google_bigquery_table")

;; terraform-mode/google/bigtable_resources/google_bigtable_instance
(goog_bigtable_instance "resource \"google_bigtable_instance\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_bigtable_instance")

;; terraform-mode/google/bigtable_resources/google_bigtable_table
(goog_bigtable_table "resource \"google_bigtable_table\" \"" (p "name") "\" {" n
 "  name          = \"" (p "name") "\"" > n
 "  instance_name = \"" (p "instance_name") "\"" > n
 "}" > n
 n
 q
 :ann "google_bigtable_table")

;; terraform-mode/google/binary_authorization_resources/google_binary_authorization_attestor
(goog_binary_authorization_attestor "resource \"google_binary_authorization_attestor\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "  attestation_authority_note {" > n
 "    note_reference = \"" (p "note_name") "\"" > n
 "  }" > n
 "}" > n
 n
 q
 :ann "google_binary_authorization_attestor")

;; terraform-mode/google/binary_authorization_resources/google_binary_authorization_policy
(goog_binary_authorization_policy "resource \"google_binary_authorization_policy\" \"" (p "name") "\" {" n
 "  default_admission_rule {" > n
 "    evaluation_mode = \"" (p "evaluation_mode") "\"" > n
 "    enforcement_mode = \"" (p "enforcement_mode") "\"" > n
 "  }" > n
 "}" > n
 n
 q
 :ann "google_binary_authorization_policy")

;; terraform-mode/google/cloud_build_resources/google_cloudbuild_trigger
(goog_cloudbuild_trigger "resource \"google_cloudbuild_trigger\" \"build_trigger\" {" n
 "}" > n
 n
 
 :ann "google_cloudbuild_trigger")

;; terraform-mode/google/cloud_composer_resources/google_composer_environment
(goog_composer_environment "resource \"google_composer_environment\" \"" (p "name") "\" {" n
 "  name   = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_composer_environment")

;; terraform-mode/google/cloud_functions_resources/google_cloudfunctions_function
(goog_cloudfunctions_function "resource \"google_cloudfunctions_function\" \"" (p "name") "\" {" n
 "  name                  = \"" (p "name") "\"" > n
 "  source_archive_bucket = \"" (p "bucket_name") "\"" > n
 "  source_archive_object = \"" (p "obejct_name") "\"" > n
 "}" > n
 n
 q
 :ann "google_cloudfunctions_function")

;; terraform-mode/google/compute_engine_resources/google_compute_address
(goog_compute_address "resource \"google_compute_address\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_address")

;; terraform-mode/google/compute_engine_resources/google_compute_attached_disk
(goog_compute_attached_disk "resource \"google_compute_attached_disk\" \"" (p "name") "\" {" n
 "  disk     = \"" (p "name") "\"" > n
 "  instance = \"" (p "instance_name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_attached_disk")

;; terraform-mode/google/compute_engine_resources/google_compute_autoscaler
(goog_compute_autoscaler "resource \"google_compute_autoscaler\" \"" (p "name") "\" {" n
 "  name   = \"" (p "name") "\"" > n
 "  target = \"" (p "target") "\"" > n
 n
 "  autoscaling_policy = {" > n
 "    max_replicas = " (p "max_replicas") > n
 "    min_replicas = " (p "min_replicas") > n
 "  }" > n
 "}" > n
 n
 q
 :ann "google_compute_autoscaler")

;; terraform-mode/google/compute_engine_resources/google_compute_backend_bucket
(goog_compute_backend_bucket "resource \"google_compute_backend_bucket\" \"" (p "name") "\" {" n
 "  name        = \"" (p "name") "\"" > n
 "  bucket_name = \"" (p "bucket_name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_backend_bucket")

;; terraform-mode/google/compute_engine_resources/google_compute_backend_service
(goog_compute_backend_service "resource \"google_compute_backend_service\" \"" (p "name") "\" {" n
 "  name          = \"" (p "name") "\"" > n
 "  health_checks = [\"" (p "health_checks") "\"]" > n
 "}" > n
 n
 q
 :ann "google_compute_backend_service")

;; terraform-mode/google/compute_engine_resources/google_compute_disk
(goog_compute_disk "resource \"google_compute_disk\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_disk")

;; terraform-mode/google/compute_engine_resources/google_compute_firewall
(goog_compute_firewall "resource \"google_compute_firewall\" \"" (p "name") "\" {" n
 "  name    = \"" (p "name") "\"" > n
 "  network = \"" (p "network") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_firewall")

;; terraform-mode/google/compute_engine_resources/google_compute_forwarding_rule
(goog_compute_forwarding_rule "resource \"google_compute_forwarding_rule\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_forwarding_rule")

;; terraform-mode/google/compute_engine_resources/google_compute_global_address
(goog_compute_global_address "resource \"google_compute_global_address\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_global_address")

;; terraform-mode/google/compute_engine_resources/google_compute_global_forwarding_rule
(goog_compute_global_forwarding_rule "resource \"google_compute_global_forwarding_rule\" \"" (p "name") "\" {" n
 "  name       = \"" (p "name") "\"" > n
 "  target     = \"" (p "target") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_global_forwarding_rule")

;; terraform-mode/google/compute_engine_resources/google_compute_health_check
(goog_compute_health_check "resource \"google_compute_health_check\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_health_check")

;; terraform-mode/google/compute_engine_resources/google_compute_http_health_check
(goog_compute_http_health_check "resource \"google_compute_http_health_check\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_http_health_check")

;; terraform-mode/google/compute_engine_resources/google_compute_https_health_check
(goog_compute_https_health_check "resource \"google_compute_https_health_check\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_https_health_check")

;; terraform-mode/google/compute_engine_resources/google_compute_image
(goog_compute_image "resource \"google_compute_image\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_image")

;; terraform-mode/google/compute_engine_resources/google_compute_instance
(goog_compute_instance "resource \"google_compute_instance\" \"" (p "name") "\" {" n
 "  name         = \"" (p "instance_name") "\"" > n
 "  machine_type = \"" (p "machine_type") "\"" > n
 "  zone         = \"" (p "zone") "\"" > n
 n
 "  boot_disk {" > n
 "  }" > n
 n
 "  network_interface {" > n
 "  }" > n
 "}" > n
 n
 q
 :ann "google_compute_instance")

;; terraform-mode/google/compute_engine_resources/google_compute_instance_from_template
(goog_compute_instance_from_template "resource \"google_compute_instance_from_template\" \"" (p "name") "\" {" n
 "  name                     = \"" (p "name") "\"" > n
 "  source_instance_template = \"" (p "template") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_instance_from_template")

;; terraform-mode/google/compute_engine_resources/google_compute_instance_group
(goog_compute_instance_group "resource \"google_compute_instance_group\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "  zone = \"" (p "zone") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_instance_group")

;; terraform-mode/google/compute_engine_resources/google_compute_instance_group_manager
(goog_compute_instance_group_manager "resource \"google_compute_instance_group_manager\" \"" (p "name") "\" {" n
 "  name               = \"" (p "name") "\"" > n
 "  base_instance_name = \"" (p "base_name") "\"" > n
 "  zone               = \"" (p "zone") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_instance_group_manager")

;; terraform-mode/google/compute_engine_resources/google_compute_instance_template
(goog_compute_instance_template "resource \"google_compute_instance_template\" \"" (p "name") "\" {" n
 "  machine_type = \"" (p "machine_type") "\"" > n
 "  disk {" > n
 "    source_image = \"" (p "source_image") "\"" > n
 "  }" > n
 "}" > n
 n
 q
 :ann "google_compute_instance_template")

;; terraform-mode/google/compute_engine_resources/google_compute_interconnect_attachment
(goog_compute_interconnect_attachment "resource \"google_compute_interconnect_attachment\" \"" (p "name") "\" {" n
 "  name         = \"" (p "name") "\"" > n
 "  interconnect = \"" (p "interconnect") "\"" > n
 "  router       = \"" (p "router") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_interconnect_attachment")

;; terraform-mode/google/compute_engine_resources/google_compute_network
(goog_compute_network "resource \"google_compute_network\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_network")

;; terraform-mode/google/compute_engine_resources/google_compute_network_peering
(goog_compute_network_peering "resource \"google_compute_network_peering\" \"" (p "name") "\" {" n
 "  name         = \"" (p "name") "\"" > n
 "  network      = \"" (p "network") "\"" > n
 "  peer_network = \"" (p "peer_network") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_network_peering")

;; terraform-mode/google/compute_engine_resources/google_compute_project_metadata
(goog_compute_project_metadata "resource \"google_compute_project_metadata\" \"" (p "name") "\" {" n
 "  metadata {" > n
 "  }" > n
 "}" > n
 n
 q
 :ann "google_compute_project_metadata")

;; terraform-mode/google/compute_engine_resources/google_compute_project_metadata_item
(goog_compute_project_metadata_item "resource \"google_compute_project_metadata_item\" \"" (p "name") "\" {" n
 "  key   = \"" (p "key") "\"" > n
 "  value = \"" (p "value") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_project_metadata_item")

;; terraform-mode/google/compute_engine_resources/google_compute_region_autoscaler
(goog_compute_region_autoscaler "resource \"google_compute_region_autoscaler\" \"" (p "name") "\" {" n
 "  name   = \"" (p "name") "\"" > n
 "  target = \"" (p "target") "\"" > n
 n
 "  autoscaling_policy = {" > n
 "    max_replicas = " (p "max_replicas") > n
 "    min_replicas = " (p "min_replicas") > n
 "  }" > n
 "}" > n
 n
 q
 :ann "google_compute_region_autoscaler")

;; terraform-mode/google/compute_engine_resources/google_compute_region_backend_service
(goog_compute_region_backend_service "resource \"google_compute_region_backend_service\" \"" (p "name") "\" {" n
 "  name          = \"" (p "name") "\"" > n
 "  health_checks = [\"" (p "health_checks") "\"]" > n
 "}" > n
 n
 q
 :ann "google_compute_region_backend_service")

;; terraform-mode/google/compute_engine_resources/google_compute_region_disk
(goog_compute_region_disk "resource \"google_compute_region_disk\" \"" (p "name") "\" {" n
 "  name          = \"" (p "name") "\"" > n
 "  replica_zones = [\"" (p "replica_zones") "\"]" > n
 "}" > n
 n
 q
 :ann "google_compute_region_disk")

;; terraform-mode/google/compute_engine_resources/google_compute_region_instance_group_manager
(goog_compute_region_instance_group_manager "resource \"google_compute_region_instance_group_manager\" \"" (p "name") "\" {" n
 "  name               = \"" (p "name") "\"" > n
 "  base_instance_name = \"" (p "base_instance_name") "\"" > n
 "  region             = \"" (p "region") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_region_instance_group_manager")

;; terraform-mode/google/compute_engine_resources/google_compute_route
(goog_compute_route "resource \"google_compute_route\" \"" (p "name") "\" {" n
 "  name        = \"" (p "name") "\"" > n
 "  dest_range  = \"" (p "0.0.0.0/32") "\"" > n
 "  network     = \"" (p "network_name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_route")

;; terraform-mode/google/compute_engine_resources/google_compute_router
(goog_compute_router "resource \"google_compute_router\" \"" (p "name") "\" {" n
 "  name    = \"" (p "name") "\"" > n
 "  network = \"" (p "network") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_router")

;; terraform-mode/google/compute_engine_resources/google_compute_router_interface
(goog_compute_router_interface "resource \"google_compute_router_interface\" \"" (p "name") "\" {" n
 "  name       = \"" (p "name") "\"" > n
 "  router     = \"" (p "router") "\"" > n
 "  vpn_tunnel = \"" (p "vpn_tunnel") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_router_interface")

;; terraform-mode/google/compute_engine_resources/google_compute_router_nat
(goog_compute_router_nat "resource \"google_compute_router_nat\" \"" (p "name") "\" {" n
 "  name                               = \"" (p "name") "\"" > n
 "  router                             = \"" (p "router") "\"" > n
 "  region                             = \"" (p "region") "\"" > n
 "  nat_ip_allocate_option             = \"" (p "allocate_option") "\"" > n
 "  source_subnetwork_ip_ranges_to_nat = \"" (p "ranges") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_router_nat")

;; terraform-mode/google/compute_engine_resources/google_compute_router_peer
(goog_compute_router_peer "resource \"google_compute_router_peer\" \"" (p "name") "\" {" n
 "  name            = \"" (p "name") "\"" > n
 "  router          = \"" (p "router") "\"" > n
 "  peer_ip_address = \"" (p "ip_address") "\"" > n
 "  peer_asn        = " (p "ASN") > n
 "  interface       = \"" (p "interface") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_router_peer")

;; terraform-mode/google/compute_engine_resources/google_compute_security_policy
(goog_compute_security_policy "resource \"google_compute_security_policy\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_security_policy")

;; terraform-mode/google/compute_engine_resources/google_compute_shared_vpc_host_project
(goog_compute_shared_vpc_host_project "resource \"google_compute_shared_vpc_host_project\" \"" (p "name") "\" {" n
 "  project = \"" (p "project") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_shared_vpc_host_project")

;; terraform-mode/google/compute_engine_resources/google_compute_shared_vpc_service_project
(goog_compute_shared_vpc_service_project "resource \"google_compute_shared_vpc_service_project\" \"" (p "name") "\" {" n
 "  host_project    = \"" (p "host_project") "\"" > n
 "  service_project = \"" (p "service_project") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_shared_vpc_service_project")

;; terraform-mode/google/compute_engine_resources/google_compute_snapshot
(goog_compute_snapshot "resource \"google_compute_snapshot\" \"" (p "name") "\" {" n
 "  name        = \"" (p "name") "\"" > n
 "  source_disk = \"" (p "source_disk") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_snapshot")

;; terraform-mode/google/compute_engine_resources/google_compute_ssl_certificate
;; WARNING: Nested fields flattened into field 
(goog_compute_ssl_certificate "resource \"google_compute_ssl_certificate\" \"" (p "name") "\" {" n
 "  private_key = \"" (p "file('path')") "\"" > n
 "  certificate = \"" (p "file('path')") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_ssl_certificate")

;; terraform-mode/google/compute_engine_resources/google_compute_ssl_policy
(goog_compute_ssl_policy "resource \"google_compute_ssl_policy\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_ssl_policy")

;; terraform-mode/google/compute_engine_resources/google_compute_subnetwork
(goog_compute_subnetwork "resource \"google_compute_subnetwork\" \"" (p "name") "\" {" n
 "  name          = \"" (p "name") "\"" > n
 "  ip_cidr_range = \"" (p "0.0.0.0/32") "\"" > n
 "  network       = \"" (p "network") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_subnetwork")

;; terraform-mode/google/compute_engine_resources/google_compute_subnetwork_iam_binding
(goog_compute_subnetwork_iam_binding "resource \"google_compute_subnetwork_iam_binding\" \"" (p "name") "\" {" n
 "  subnetwork = \"" (p "subnet_id") "\"" > n
 "  role       = \"" (p "role") "\"" > n
 "  members    = [" > n
 "    \"" (p "user:jane@example.com") "\"," > n
 "  ]" > n
 "}" > n
 n
 q
 :ann "google_compute_subnetwork_iam_binding")

;; terraform-mode/google/compute_engine_resources/google_compute_subnetwork_iam_member
(goog_compute_subnetwork_iam_member "resource \"google_compute_subnetwork_iam_member\" \"" (p "name") "\" {" n
 "  subnetwork = \"" (p "subnet") "\"" > n
 "  role       = \"" (p "role") "\"" > n
 "  member     = \"" (p "member") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_subnetwork_iam_member")

;; terraform-mode/google/compute_engine_resources/google_compute_subnetwork_iam_policy
(goog_compute_subnetwork_iam_policy "resource \"google_compute_subnetwork_iam_policy\" \"" (p "name") "\" {" n
 "  subnetwork  = \"" (p "subnet") "\"" > n
 "  policy_data = \"" (p "policy_data") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_subnetwork_iam_policy")

;; terraform-mode/google/compute_engine_resources/google_compute_target_http_proxy
(goog_compute_target_http_proxy "resource \"google_compute_target_http_proxy\" \"" (p "name") "\" {" n
 "  name    = \"" (p "name") "\"" > n
 "  url_map = \"" (p "url_map") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_target_http_proxy")

;; terraform-mode/google/compute_engine_resources/google_compute_target_https_proxy
(goog_compute_target_https_proxy "resource \"google_compute_target_https_proxy\" \"" (p "name") "\" {" n
 "  name             = \"" (p "name") "\"" > n
 "  url_map          = \"" (p "url_map") "\"" > n
 "  ssl_certificates = [\"" (p "sll_cert") "\"]" > n
 "}" > n
 n
 q
 :ann "google_compute_target_https_proxy")

;; terraform-mode/google/compute_engine_resources/google_compute_target_pool
(goog_compute_target_pool "resource \"google_compute_target_pool\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_target_pool")

;; terraform-mode/google/compute_engine_resources/google_compute_target_ssl_proxy
(goog_compute_target_ssl_proxy "resource \"google_compute_target_ssl_proxy\" \"" (p "name") "\" {" n
 "  name             = \"" (p "name") "\"" > n
 "  backend_service  = \"" (p "backend_service") "\"" > n
 "  ssl_certificates = [\"" (p "ssl_cert") "\"]" > n
 "}" > n
 n
 q
 :ann "google_compute_target_ssl_proxy")

;; terraform-mode/google/compute_engine_resources/google_compute_target_tcp_proxy
(goog_compute_target_tcp_proxy "resource \"google_compute_target_tcp_proxy\" \"" (p "name") "\" {" n
 "  name            = \"" (p "name") "\"" > n
 "  backend_service = \"" (p "backend_service") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_target_tcp_proxy")

;; terraform-mode/google/compute_engine_resources/google_compute_url_map
(goog_compute_url_map "resource \"google_compute_url_map\" \"" (p "name") "\" {" n
 "  name            = \"" (p "name") "\"" > n
 "  default_service = \"" (p "default_service") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_url_map")

;; terraform-mode/google/compute_engine_resources/google_compute_vpn_gateway
(goog_compute_vpn_gateway "resource \"google_compute_vpn_gateway\" \"" (p "name") "\" {" n
 "  name    = \"" (p "name") "\"" > n
 "  network = \"" (p "network") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_vpn_gateway")

;; terraform-mode/google/compute_engine_resources/google_compute_vpn_tunnel
(goog_compute_vpn_tunnel "resource \"google_compute_vpn_tunnel\" \"" (p "name") "\" {" n
 "  name               = \"" (p "name") "\"" > n
 "  peer_ip            = \"" (p "0.0.0.0") "\"" > n
 "  shared_secret      = \"" (p "secret") "\"" > n
 "  target_vpn_gateway = \"" (p "target_vpn_gateway") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_vpn_tunnel")

;; terraform-mode/google/container_analysis_resources/google_container_analysis_note
(goog_container_analysis_note "resource \"google_container_analysis_note\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "  attestation_authority {" > n
 "    hint {" > n
 "      human_readable_name = \"" (p "human_readable_name") "\"" > n
 "    }" > n
 "  }" > n
 "}" > n
 n
 q
 :ann "google_container_analysis_note")

;; terraform-mode/google/container_engine_resources/google_container_cluster
(goog_container_cluster "resource \"google_container_cluster\" \"" (p "name") "\" {" n
 "  name = \"" (p "value") "\"" > n
 "}" > n
 n
 q
 :ann "google_container_cluster")

;; terraform-mode/google/container_engine_resources/google_container_node_pool
(goog_container_node_pool "resource \"google_container_node_pool\" \"" (p "name") "\" {" n
 "  cluster = \"" (p "value") "\"" > n
 "}" > n
 n
 q
 :ann "google_container_node_pool")

;; terraform-mode/google/data_sources/google_active_folder
(goog_active_folder "data \"google_active_folder\" \"" (p "name") "\" {" n
 "  display_name = \"" (p "name") "\"" > n
 "  parent       = \"" (p "parent") "\"" > n
 "}" > n
 n
 q
 :ann "google_active_folder")

;; terraform-mode/google/data_sources/google_billing_account
(goog_billing_account "data \"google_billing_account\" \"" (p "name") "\" {" n
 "}" > n
 n
 q
 :ann "google_billing_account")

;; terraform-mode/google/data_sources/google_client_config
(goog_client_config "data \"google_client_config\" \"" (p "name") "\" {}" n
 n
 q
 :ann "google_client_config")

;; terraform-mode/google/data_sources/google_cloudfunctions_function_data
(goog_cloudfunctions_function_data "data \"google_cloudfunctions_function\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_cloudfunctions_function_data")

;; terraform-mode/google/data_sources/google_compute_address_data
(goog_compute_address_data "data \"google_compute_address\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_address_data")

;; terraform-mode/google/data_sources/google_compute_backend_service_data
(goog_compute_backend_service_data "data \"google_compute_backend_service\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_backend_service_data")

;; terraform-mode/google/data_sources/google_compute_default_service_account
(goog_compute_default_service_account "data \"google_compute_default_service_account\" \"" (p "name") "\" { }" n
 n
 q
 :ann "google_compute_default_service_account")

;; terraform-mode/google/data_sources/google_compute_forwarding_rule_data
(goog_compute_forwarding_rule_data "data \"google_compute_forwarding_rule\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_forwarding_rule_data")

;; terraform-mode/google/data_sources/google_compute_global_address_data
(goog_compute_global_address_data "data \"google_compute_global_address\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_global_address_data")

;; terraform-mode/google/data_sources/google_compute_image_data
(goog_compute_image_data "data \"google_compute_image\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_image_data")

;; terraform-mode/google/data_sources/google_compute_instance_data
(goog_compute_instance_data "data \"google_compute_instance\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_instance_data")

;; terraform-mode/google/data_sources/google_compute_instance_group_data
(goog_compute_instance_group_data "data \"google_compute_instance_group\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_instance_group_data")

;; terraform-mode/google/data_sources/google_compute_lb_ip_ranges
(goog_compute_lb_ip_ranges "data \"google_compute_lb_ip_ranges\" \"" (p "name") "\" {}" n
 n
 q
 :ann "google_compute_lb_ip_ranges")

;; terraform-mode/google/data_sources/google_compute_network_data
(goog_compute_network_data "data \"google_compute_network\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_network_data")

;; terraform-mode/google/data_sources/google_compute_region_instance_group
(goog_compute_region_instance_group "data \"google_compute_region_instance_group\" \"" (p "name") "\" {" n
 "  name = \"" (p "instance_group_name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_region_instance_group")

;; terraform-mode/google/data_sources/google_compute_regions
(goog_compute_regions "data \"google_compute_regions\" \"" (p "name") "\" {}" n
 n
 q
 :ann "google_compute_regions")

;; terraform-mode/google/data_sources/google_compute_ssl_policy_data
(goog_compute_ssl_policy_data "data \"google_compute_ssl_policy\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_ssl_policy_data")

;; terraform-mode/google/data_sources/google_compute_subnetwork_data
(goog_compute_subnetwork_data "data \"google_compute_subnetwork\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_subnetwork_data")

;; terraform-mode/google/data_sources/google_compute_vpn_gateway_data
(goog_compute_vpn_gateway_data "data \"google_compute_vpn_gateway\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_compute_vpn_gateway_data")

;; terraform-mode/google/data_sources/google_compute_zones
(goog_compute_zones "data \"google_compute_zones\" \"" (p "name") "\" {}" n
 n
 q
 :ann "google_compute_zones")

;; terraform-mode/google/data_sources/google_container_cluster_data
(goog_container_cluster_data "data \"google_container_cluster\" \"" (p "name") "\" {" n
 "  name   = \"" (p "name") "\"" > n
 "  zone   = \"" (p "zone") "\"" > n
 "}" > n
 n
 q
 :ann "google_container_cluster_data")

;; terraform-mode/google/data_sources/google_container_engine_versions
(goog_container_engine_versions "data \"google_container_engine_versions\" \"" (p "name") "\" {}" n
 n
 q
 :ann "google_container_engine_versions")

;; terraform-mode/google/data_sources/google_container_registry_image
(goog_container_registry_image "data \"google_container_registry_image\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_container_registry_image")

;; terraform-mode/google/data_sources/google_container_registry_repository
(goog_container_registry_repository "data \"google_container_registry_repository\" \"" (p "name") "\" {}" n
 n
 q
 :ann "google_container_registry_repository")

;; terraform-mode/google/data_sources/google_dns_managed_zone_data
(goog_dns_managed_zone_data "data \"google_dns_managed_zone\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_dns_managed_zone_data")

;; terraform-mode/google/data_sources/google_folder_data
(goog_folder_data "data \"google_folder\" \"" (p "name") "\" {" n
 "  folder = \"" (p "folder") "\"" > n
 "}" > n
 n
 q
 :ann "google_folder_data")

;; terraform-mode/google/data_sources/google_iam_policy
(goog_iam_policy "data \"google_iam_policy\" \"" (p "name") "\" {" n
 "  binding {" > n
 "    role    = \"" (p "role") "\"" > n
 "    members = [" > n
 "      \"user:" (p "service_account") "\"," > n
 "    ]" > n
 "  }" > n
 "}" > n
 n
 q
 :ann "google_iam_policy")

;; terraform-mode/google/data_sources/google_iam_role
(goog_iam_role "data \"google_iam_role\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_iam_role")

;; terraform-mode/google/data_sources/google_kms_secret
(goog_kms_secret "data \"google_kms_secret\" \"" (p "name") "\" {" n
 "  crypto_key = \"" (p "crypto_key") "\"" > n
 "  ciphertext = \"" (p "ciphertext") "\"" > n
 "}" > n
 n
 q
 :ann "google_kms_secret")

;; terraform-mode/google/data_sources/google_netblock_ip_ranges
(goog_netblock_ip_ranges "data \"google_netblock_ip_ranges\" \"" (p "name") "\" {}" n
 n
 q
 :ann "google_netblock_ip_ranges")

;; terraform-mode/google/data_sources/google_organization
(goog_organization "data \"google_organization\" \"" (p "name") "\" {" n
 "}" > n
 n
 q
 :ann "google_organization")

;; terraform-mode/google/data_sources/google_project_data
(goog_project_data "data \"google_project\" \"project\" {}" n
 n
 
 :ann "google_project_data")

;; terraform-mode/google/data_sources/google_project_services_data
(goog_project_services_data "data \"google_project_services\" \"" (p "name") "\" {" n
 "  project = \"" (p "project_id") "\"" > n
 "}" > n
 n
 q
 :ann "google_project_services_data")

;; terraform-mode/google/data_sources/google_service_account_data
(goog_service_account_data "data \"google_service_account\" \"" (p "name") "\" {" n
 "  account_id = \"" (p "account_id") "\"" > n
 "}" > n
 n
 q
 :ann "google_service_account_data")

;; terraform-mode/google/data_sources/google_service_account_key_data
(goog_service_account_key_data "data \"google_service_account_key\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_service_account_key_data")

;; terraform-mode/google/data_sources/google_storage_object_signed_url
(goog_storage_object_signed_url "data \"google_storage_object_signed_url\" \"" (p "name") "\" {" n
 "  bucket = \"" (p "bucket") "\"" > n
 "  path   = \"" (p "path") "\"" > n
 "}" > n
 n
 q
 :ann "google_storage_object_signed_url")

;; terraform-mode/google/data_sources/google_storage_project_service_account
(goog_storage_project_service_account "data \"google_storage_project_service_account\" \"" (p "name") "\" {}" n
 n
 q
 :ann "google_storage_project_service_account")

;; terraform-mode/google/dataflow_resources/google_dataflow_job
(goog_dataflow_job "resource \"google_dataflow_job\" \"" (p "name") "\" {" n
 "  name              = \"" (p "name") "\"" > n
 "  template_gcs_path = \"" (p "gcs_patch") "\"" > n
 "  temp_gcs_location = \"" (p "gcs_location") "\"" > n
 "}" > n
 n
 q
 :ann "google_dataflow_job")

;; terraform-mode/google/dataproc_resources/google_dataproc_cluster
(goog_dataproc_cluster "resource \"google_dataproc_cluster\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_dataproc_cluster")

;; terraform-mode/google/dataproc_resources/google_dataproc_job
(goog_dataproc_job "resource \"google_dataproc_job\" \"" (p "name") "\" {" n
 "  placement {" > n
 "    cluster_name = \"" (p "cluster_name") "\"" > n
 "  }" > n
 "  " > (p "config_type") "_config {" n
 "    " > (p "arguments") n
 "  }" > n
 "}" > n
 n
 q
 :ann "google_dataproc_job")

;; terraform-mode/google/dns_resources/google_dns_managed_zone
(goog_dns_managed_zone "resource \"google_dns_managed_zone\" \"" (p "name") "\" {" n
 "  name     = \"" (p "name") "\"" > n
 "  dns_name = \"" (p "dns_name") "\"" > n
 "}" > n
 n
 q
 :ann "google_dns_managed_zone")

;; terraform-mode/google/dns_resources/google_dns_record_set
(goog_dns_record_set "resource \"google_dns_record_set\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "  type = \"" (p "type") "\"" > n
 "  ttl  = " (p "ttl") > n
 "  managed_zone = \"" (p "managed_zone") "\"" > n
 "  rrdatas = [\"" (p "rrdatas") "\"]" > n
 "}" > n
 n
 q
 :ann "google_dns_record_set")

;; terraform-mode/google/endpoints_resources/google_endpoints_service
(goog_endpoints_service "resource \"google_endpoints_service\" \"" (p "name") "\" {" n
 "  service_name = \"" (p "service_name") "\"" > n
 "}" > n
 n
 q
 :ann "google_endpoints_service")

;; terraform-mode/google/filestore_resources/google_filestore_instance
(goog_filestore_instance "resource \"google_filestore_instance\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "  zone = \"" (p "zone") "\"" > n
 "  tier = \"" (p "tier") "\"" > n
 n
 "  file_shares {" > n
 "    capacity_gb = " (p "capacity") > n
 "    name        = \"" (p "name") "\"" > n
 "  }" > n
 n
 "  networks {" > n
 "    network = \"" (p "network") "\"" > n
 "    modes   = [\"" (p "mode") "\"]" > n
 "  }" > n
 "}" > n
 n
 q
 :ann "google_filestore_instance")

;; terraform-mode/google/gcp_resources/google_billing_account_iam_binding
(goog_billing_account_iam_binding "resource \"google_billing_account_iam_binding\" \"" (p "name") "\" {" n
 "  billing_account_id = \"" (p "billing_account_id") "\"" > n
 "  role               = \"" (p "role") "\"" > n
 "  members            = [" > n
 "  \"user:" (p "user") "\"," > n
 "  ]" > n
 "}" > n
 n
 q
 :ann "google_billing_account_iam_binding")

;; terraform-mode/google/gcp_resources/google_billing_account_iam_member
(goog_billing_account_iam_member "resource \"google_billing_account_iam_member\" \"" (p "name") "\" {" n
 "  billing_account_id = \"" (p "billing_account_id") "\"" > n
 "  role               = \"" (p "role") "\"" > n
 "  member             = \"user:" (p "member") "\"" > n
 "}" > n
 n
 q
 :ann "google_billing_account_iam_member")

;; terraform-mode/google/gcp_resources/google_billing_account_iam_policy
(goog_billing_account_iam_policy "resource \"google_billing_account_iam_policy\" \"" (p "name") "\" {" n
 "  billing_account_id = \"" (p "billing_account_id") "\"" > n
 "  policy_data        = \"" (p "policy_data") "\"" > n
 "}" > n
 n
 q
 :ann "google_billing_account_iam_policy")

;; terraform-mode/google/gcp_resources/google_folder
(goog_folder "resource \"google_folder\" \"" (p "name") "\" {" n
 "  display_name = \"" (p "display_name") "\"" > n
 "  parent       = \"" (p "parent") "\"" > n
 "}" > n
 n
 q
 :ann "google_folder")

;; terraform-mode/google/gcp_resources/google_folder_iam_binding
(goog_folder_iam_binding "resource \"google_folder_iam_binding\" \"" (p "name") "\" {" n
 "  folder  = \"" (p "folder") "\"" > n
 "  role    = \"" (p "role") "\"" > n
 "  members = [" > n
 "    \"user:" (p "user") "\"," > n
 "  ]" > n
 "}" > n
 n
 q
 :ann "google_folder_iam_binding")

;; terraform-mode/google/gcp_resources/google_folder_iam_member
(goog_folder_iam_member "resource \"google_folder_iam_member\" \"" (p "name") "\" {" n
 "  folder = \"" (p "folder") "\"" > n
 "  role   = \"" (p "role") "\"" > n
 "  member = \"user:" (p "member") "\"" > n
 "}" > n
 n
 q
 :ann "google_folder_iam_member")

;; terraform-mode/google/gcp_resources/google_folder_iam_policy
(goog_folder_iam_policy "resource \"google_folder_iam_policy\" \"" (p "name") "\" {" n
 "  folder      = \"" (p "folder") "\"" > n
 "  policy_data = \"" (p "policy_data") "\"" > n
 "}" > n
 n
 q
 :ann "google_folder_iam_policy")

;; terraform-mode/google/gcp_resources/google_folder_organization_policy
(goog_folder_organization_policy "resource \"google_folder_organization_policy\" \"" (p "name") "\" {" n
 "  folder     = \"" (p "folder") "\"" > n
 "  constraint = \"" (p "constraint") "\"" > n
 "}" q >
 :ann "google_folder_organization_policy")

;; terraform-mode/google/gcp_resources/google_organization_iam_binding
(goog_organization_iam_binding "resource \"google_organization_iam_binding\" \"" (p "name") "\" {" n
 "  org_id  = \"" (p "org_id") "\"" > n
 "  role    = \"" (p "role") "\"" > n
 "  members = [" > n
 "    \"user:" (p "user") "\"," > n
 "  ]" > n
 "}" > n
 n
 q
 :ann "google_organization_iam_binding")

;; terraform-mode/google/gcp_resources/google_organization_iam_custom_role
(goog_organization_iam_custom_role "resource \"google_organization_iam_custom_role\" \"" (p "name") "\" {" n
 "  role_id     = \"" (p "role_id") "\"" > n
 "  org_id      = \"" (p "org_id") "\"" > n
 "  title       = \"" (p "title") "\"" > n
 "  permissions = [\"" (p "permission") "\"]" > n
 "}" > n
 n
 q
 :ann "google_organization_iam_custom_role")

;; terraform-mode/google/gcp_resources/google_organization_iam_member
(goog_organization_iam_member "resource \"google_organization_iam_member\" \"" (p "name") "\" {" n
 "  org_id  = \"" (p "org_id") "\"" > n
 "  role    = \"" (p "role") "\"" > n
 "  member  = \"user:" (p "user") "\"" > n
 "}" > n
 n
 q
 :ann "google_organization_iam_member")

;; terraform-mode/google/gcp_resources/google_organization_iam_policy
(goog_organization_iam_policy "resource \"google_organization_iam_policy\" \"" (p "name") "\" {" n
 "  org_id      = \"" (p "org_id") "\"" > n
 "  policy_data = \"" (p "policy_data") "\"" > n
 "}" > n
 n
 q
 :ann "google_organization_iam_policy")

;; terraform-mode/google/gcp_resources/google_organization_policy
(goog_organization_policy "resource \"google_organization_policy\" \"" (p "name") "\" {" n
 "  org_id     = \"" (p "org_id") "\"" > n
 "  constraint = \"" (p "constraint") "\"" > n
 "}" q >
 :ann "google_organization_policy")

;; terraform-mode/google/gcp_resources/google_project
(goog_project "resource \"google_project\" \"" (p "name") "\" {" n
 "  name       = \"" (p "name") "\"" > n
 "  project_id = \"" (p "project_id") "\"" > n
 "}" > n
 n
 q
 :ann "google_project")

;; terraform-mode/google/gcp_resources/google_project_iam_binding
(goog_project_iam_binding "resource \"google_project_iam_binding\" \"" (p "name") "\" {" n
 "  role    = \"" (p "role") "\"" > n
 "  members = [" > n
 "    \"user:" (p "user") "\"," > n
 "  ]" > n
 "}" > n
 n
 q
 :ann "google_project_iam_binding")

;; terraform-mode/google/gcp_resources/google_project_iam_custom_role
(goog_project_iam_custom_role "resource \"google_project_iam_custom_role\" \"" (p "name") "\" {" n
 "  role_id     = \"" (p "role") "\"" > n
 "  title       = \"" (p "title") "\"" > n
 "  permissions = [" > n
 "    \"" (p "permission") "\"," > n
 "  ]" > n
 "}" > n
 n
 q
 :ann "google_project_iam_custom_role")

;; terraform-mode/google/gcp_resources/google_project_iam_member
(goog_project_iam_member "resource \"google_project_iam_member\" \"" (p "name") "\" {" n
 "  role   = \"" (p "role") "\"" > n
 "  member = \"user:" (p "user") "\"" > n
 "}" > n
 n
 q
 :ann "google_project_iam_member")

;; terraform-mode/google/gcp_resources/google_project_iam_policy
(goog_project_iam_policy "resource \"google_project_iam_policy\" \"" (p "name") "\" {" n
 "  policy_data = \"" (p "policy_data") "\"" > n
 "}" > n
 n
 q
 :ann "google_project_iam_policy")

;; terraform-mode/google/gcp_resources/google_project_organization_policy
(goog_project_organization_policy "resource \"google_project_organization_policy\" \"" (p "name") "\" {" n
 "  project    = \"" (p "project") "\"" > n
 "  constraint = \"" (p "constraint") "\"" > n
 "}" > n
 n
 q
 :ann "google_project_organization_policy")

;; terraform-mode/google/gcp_resources/google_project_service
(goog_project_service "resource \"google_project_service\" \"" (p "name") "\" {" n
 "  service = \"" (p "service") "\"" > n
 "}" > n
 n
 q
 :ann "google_project_service")

;; terraform-mode/google/gcp_resources/google_project_services
(goog_project_services "resource \"google_project_services\" \"" (p "name") "\" {" n
 "  services = [\"" (p "service") "\"]" > n
 "}" > n
 n
 q
 :ann "google_project_services")

;; terraform-mode/google/gcp_resources/google_project_usage_export_bucket
(goog_project_usage_export_bucket "resource \"google_project_usage_export_bucket\" \"" (p "name") "\" {" n
 "  bucket_name = \"" (p "bucket_name") "\"" > n
 "}" > n
 n
 q
 :ann "google_project_usage_export_bucket")

;; terraform-mode/google/gcp_resources/google_resource_manager_lien
(goog_resource_manager_lien "resource \"google_resource_manager_lien\" \"" (p "name") "\" {" n
 "  parent       = \"" (p "parent") "\"" > n
 "  restrictions = [\"" (p "restriction") "\"]" > n
 "  origin       = \"" (p "origin") "\"" > n
 "  reason       = \"" (p "reason") "\"" > n
 "}" > n
 n
 q
 :ann "google_resource_manager_lien")

;; terraform-mode/google/gcp_resources/google_service_account
(goog_service_account "resource \"google_service_account\" \"" (p "name") "\" {" n
 "  account_id = \"" (p "value") "\"" > n
 "}" > n
 n
 q
 :ann "google_service_account")

;; terraform-mode/google/gcp_resources/google_service_account_iam_binding
(goog_service_account_iam_binding "resource \"google_service_account_iam_binding\" \"" (p "name") "\" {" n
 "  service_account_id = \"" (p "service_account_id") "\"" > n
 "  role               = \"" (p "role") "\"" > n
 "  members            = [" > n
 "    \"user:" (p "user") "\"," > n
 "  ]" > n
 "}" > n
 n
 q
 :ann "google_service_account_iam_binding")

;; terraform-mode/google/gcp_resources/google_service_account_iam_member
(goog_service_account_iam_member "resource \"google_service_account_iam_member\" \"" (p "name") "\" {" n
 "  service_account_id = \"" (p "service_account_id") "\"" > n
 "  role               = \"" (p "role") "\"" > n
 "  member             = \"user:" (p "member") "\"" > n
 "}" > n
 n
 q
 :ann "google_service_account_iam_member")

;; terraform-mode/google/gcp_resources/google_service_account_iam_policy
(goog_service_account_iam_policy "resource \"google_service_account_iam_policy\" \"" (p "name") "\" {" n
 "  service_account_id = \"" (p "service_account_id") "\"" > n
 "  policy_data        = \"" (p "policy_data") "\"" > n
 "}" > n
 n
 q
 :ann "google_service_account_iam_policy")

;; terraform-mode/google/gcp_resources/google_service_account_key
(goog_service_account_key "resource \"google_service_account_key\" \"" (p "name") "\" {" n
 "  service_account_id = \"" (p "service_account_id") "\"" > n
 "}" > n
 n
 q
 :ann "google_service_account_key")

;; terraform-mode/google/iot_core/google_cloudiot_registry
(goog_cloudiot_registry "resource \"google_cloudiot_registry\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_cloudiot_registry")

;; terraform-mode/google/key_management_service_resources/google_kms_crypto_key
(goog_kms_crypto_key "resource \"google_kms_crypto_key\" \"" (p "name") "\" {" n
 "  name     = \"" (p "name") "\"" > n
 "  key_ring = \"" (p "key_ring") "\"" > n
 "}" > n
 n
 q
 :ann "google_kms_crypto_key")

;; terraform-mode/google/key_management_service_resources/google_kms_crypto_key_iam_binding
(goog_kms_crypto_key_iam_binding "resource \"google_kms_crypto_key_iam_binding\" \"" (p "name") "\" {" n
 "  crypto_key_id = \"" (p "crypto_key_id") "\"" > n
 "  role          = \"" (p "role") "\"" > n
 "  members       = [" > n
 "    \"user:" (p "member") "\"," > n
 "  ]" > n
 "}" > n
 n
 q
 :ann "google_kms_crypto_key_iam_binding")

;; terraform-mode/google/key_management_service_resources/google_kms_crypto_key_iam_member
(goog_kms_crypto_key_iam_member "resource \"google_kms_crypto_key_iam_member\" \"" (p "name") "\" {" n
 "  crypto_key_id = \"" (p "crypto_key") "\"" > n
 "  role          = \"" (p "role") "\"" > n
 "  member        = \"user:" (p "user") "\"" > n
 "}" > n
 n
 q
 :ann "google_kms_crypto_key_iam_member")

;; terraform-mode/google/key_management_service_resources/google_kms_key_ring
(goog_kms_key_ring "resource \"google_kms_key_ring\" \"" (p "name") "\" {" n
 "  name     = \"" (p "name") "\"" > n
 "  location = \"" (p "location") "\"" > n
 "}" > n
 n
 q
 :ann "google_kms_key_ring")

;; terraform-mode/google/key_management_service_resources/google_kms_key_ring_iam_binding
(goog_kms_key_ring_iam_binding "resource \"google_kms_key_ring_iam_binding\" \"" (p "name") "\" {" n
 "  key_ring_id = \"" (p "key_ring") "\"" > n
 "  role        = \"" (p "role") "\"" > n
 "  members     = [" > n
 "    \"user:" (p "user") "\"," > n
 "  ]" > n
 "}" > n
 n
 q
 :ann "google_kms_key_ring_iam_binding")

;; terraform-mode/google/key_management_service_resources/google_kms_key_ring_iam_member
(goog_kms_key_ring_iam_member "resource \"google_kms_key_ring_iam_member\" \"" (p "name") "\" {" n
 "  key_ring_id = \"" (p "key_ring") "\"" > n
 "  role        = \"" (p "role") "\"" > n
 "  member      = \"user:" (p "user") "\"" > n
 "}" > n
 n
 q
 :ann "google_kms_key_ring_iam_member")

;; terraform-mode/google/key_management_service_resources/google_kms_key_ring_iam_policy
(goog_kms_key_ring_iam_policy "resource \"google_kms_key_ring_iam_policy\" \"" (p "name") "\" {" n
 "  key_ring_id = \"" (p "key_ring") "\"" > n
 "  policy_data = \"" (p "policy_data") "\"" > n
 "}" > n
 n
 q
 :ann "google_kms_key_ring_iam_policy")

;; terraform-mode/google/pubsub_resources/google_pubsub_subscription
(goog_pubsub_subscription "resource \"google_pubsub_subscription\" \"" (p "name") "\" {" n
 "  name  = \"" (p "name") "\"" > n
 "  topic = \"" (p "topic") "\"" > n
 "}" > n
 n
 q
 :ann "google_pubsub_subscription")

;; terraform-mode/google/pubsub_resources/google_pubsub_subscription_iam_binding
(goog_pubsub_subscription_iam_binding "resource \"google_pubsub_subscription_iam_binding\" \"" (p "name") "\" {" n
 "  subscription = \"" (p "subscription") "\"" > n
 "  role         = \"" (p "role") "\"" > n
 "  members      = [" > n
 "    \"user:" (p "user") "\"," > n
 "  ]" > n
 "}" > n
 n
 q
 :ann "google_pubsub_subscription_iam_binding")

;; terraform-mode/google/pubsub_resources/google_pubsub_subscription_iam_member
(goog_pubsub_subscription_iam_member "resource \"google_pubsub_subscription_iam_member\" \"" (p "name") "\" {" n
 "  subscription = \"" (p "subscription") "\"" > n
 "  role         = \"" (p "role") "\"" > n
 "  member       = \"user:" (p "user") "\"" > n
 "}" > n
 n
 q
 :ann "google_pubsub_subscription_iam_member")

;; terraform-mode/google/pubsub_resources/google_pubsub_subscription_iam_policy
(goog_pubsub_subscription_iam_policy "resource \"google_pubsub_subscription_iam_policy\" \"" (p "name") "\" {" n
 "  subscription = \"" (p "subscription") "\"" > n
 "  policy_data  = \"" (p "policy_data") "\"" > n
 "}" > n
 n
 q
 :ann "google_pubsub_subscription_iam_policy")

;; terraform-mode/google/pubsub_resources/google_pubsub_topic
(goog_pubsub_topic "resource \"google_pubsub_topic\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_pubsub_topic")

;; terraform-mode/google/pubsub_resources/google_pubsub_topic_iam_binding
(goog_pubsub_topic_iam_binding "resource \"google_pubsub_topic_iam_binding\" \"" (p "name") "\" {" n
 "  topic   = \"" (p "topic") "\"" > n
 "  role    = \"" (p "role") "\"" > n
 "  members = [" > n
 "    \"user:" (p "user") "\"," > n
 "  ]" > n
 "}" > n
 n
 q
 :ann "google_pubsub_topic_iam_binding")

;; terraform-mode/google/pubsub_resources/google_pubsub_topic_iam_member
(goog_pubsub_topic_iam_member "resource \"google_pubsub_topic_iam_member\" \"" (p "name") "\" {" n
 "  topic  = \"" (p "topic") "\"" > n
 "  role   = \"" (p "role") "\"" > n
 "  member = \"user:" (p "user") "\"" > n
 "}" > n
 n
 q
 :ann "google_pubsub_topic_iam_member")

;; terraform-mode/google/pubsub_resources/google_pubsub_topic_iam_policy
(goog_pubsub_topic_iam_policy "resource \"google_pubsub_topic_iam_policy\" \"" (p "name") "\" {" n
 "  topic       = \"" (p "topic") "\"" > n
 "  policy_data = \"" (p "policy_data") "\"" > n
 "}" > n
 n
 q
 :ann "google_pubsub_topic_iam_policy")

;; terraform-mode/google/redis_resources/google_redis_instance
(goog_redis_instance "resource \"google_redis_instance\" \"" (p "name") "\" {" n
 "  name           = \"" (p "name") "\"" > n
 "  memory_size_gb = " (p "size") > n
 "}" > n
 n
 q
 :ann "google_redis_instance")

;; terraform-mode/google/runtimeconfig_resources/google_runtimeconfig_config
(goog_runtimeconfig_config "resource \"google_runtimeconfig_config\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_runtimeconfig_config")

;; terraform-mode/google/runtimeconfig_resources/google_runtimeconfig_variable
(goog_runtimeconfig_variable "resource \"google_runtimeconfig_variable\" \"" (p "name") "\" {" n
 "  name   = \"" (p "name") "\"" > n
 "  parent = \"" (p "parent_config_name") "\"" > n
 "  text   = \"" (p "text") "\"" > n
 "}" > n
 n
 q
 :ann "google_runtimeconfig_variable")

;; terraform-mode/google/service_networking_resources/google_service_networking_connection
(goog_service_networking_connection "resource \"google_service_networking_connection\" \"" (p "name") "\" {" n
 "  network                 = \"" (p "network") "\"" > n
 "  service                 = \"" (p "service") "\"" > n
 "  reserved_peering_ranges = [\"" (p "reserved_peering_ranges") "\"]" > n
 "}" > n
 n
 q
 :ann "google_service_networking_connection")

;; terraform-mode/google/source_repositories_resources/google_sourcerepo_repository
(goog_sourcerepo_repository "resource \"google_sourcerepo_repository\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_sourcerepo_repository")

;; terraform-mode/google/spanner_resources/google_spanner_database
(goog_spanner_database "resource \"google_spanner_database\" \"" (p "name") "\" {" n
 "  instance  = \"" (p "instance") "\"" > n
 "  name      = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_spanner_database")

;; terraform-mode/google/spanner_resources/google_spanner_database_iam_binding
(goog_spanner_database_iam_binding "resource \"google_spanner_database_iam_binding\" \"" (p "name") "\" {" n
 "  instance = \"" (p "instance") "\"" > n
 "  database = \"" (p "database") "\"" > n
 "  role     = \"" (p "role") "\"" > n
 "  members  = [" > n
 "    \"user:" (p "user") "\"," > n
 "  ]" > n
 "}" > n
 n
 q
 :ann "google_spanner_database_iam_binding")

;; terraform-mode/google/spanner_resources/google_spanner_database_iam_member
(goog_spanner_database_iam_member "resource \"google_spanner_database_iam_member\" \"" (p "name") "\" {" n
 "  instance = \"" (p "instance") "\"" > n
 "  database = \"" (p "database") "\"" > n
 "  role     = \"" (p "role") "\"" > n
 "  member   = \"user:" (p "user") "\"" > n
 "}" > n
 n
 q
 :ann "google_spanner_database_iam_member")

;; terraform-mode/google/spanner_resources/google_spanner_database_iam_policy
(goog_spanner_database_iam_policy "resource \"google_spanner_database_iam_policy\" \"" (p "name") "\" {" n
 "  instance    = \"" (p "instance") "\"" > n
 "  database    = \"" (p "database") "\"" > n
 "  policy_data = \"" (p "policy_data") "\"" > n
 "}" > n
 n
 q
 :ann "google_spanner_database_iam_policy")

;; terraform-mode/google/spanner_resources/google_spanner_instance
(goog_spanner_instance "resource \"google_spanner_instance\" \"" (p "name") "\" {" n
 "  config       = \"" (p "config") "\"" > n
 "  display_name = \"" (p "display_name") "\"" > n
 "}" > n
 n
 q
 :ann "google_spanner_instance")

;; terraform-mode/google/spanner_resources/google_spanner_instance_iam_binding
(goog_spanner_instance_iam_binding "resource \"google_spanner_instance_iam_binding\" \"" (p "name") "\" {" n
 "  instance  = \"" (p "instance") "\"" > n
 "  role      = \"" (p "role") "\"" > n
 "  members   = [" > n
 "    \"user:" (p "user") "\"," > n
 "  ]" > n
 "}" > n
 n
 q
 :ann "google_spanner_instance_iam_binding")

;; terraform-mode/google/spanner_resources/google_spanner_instance_iam_member
(goog_spanner_instance_iam_member "resource \"google_spanner_instance_iam_member\" \"" (p "name") "\" {" n
 "  instance  = \"" (p "instance") "\"" > n
 "  role      = \"" (p "role") "\"" > n
 "  member    = \"user:" (p "user") "\"" > n
 "}" > n
 n
 q
 :ann "google_spanner_instance_iam_member")

;; terraform-mode/google/spanner_resources/google_spanner_instance_iam_policy
(goog_spanner_instance_iam_policy "resource \"google_spanner_instance_iam_policy\" \"" (p "name") "\" {" n
 "  instance    = \"" (p "instance") "\"" > n
 "  policy_data = \"" (p "policy_data") "\"" > n
 "}" > n
 n
 q
 :ann "google_spanner_instance_iam_policy")

;; terraform-mode/google/sql_resources/google_sql_database
(goog_sql_database "resource \"google_sql_database\" \"" (p "name") "\" {" n
 "  name     = \"" (p "name") "\"" > n
 "  instance = \"" (p "instance") "\"" > n
 "}" > n
 n
 q
 :ann "google_sql_database")

;; terraform-mode/google/sql_resources/google_sql_database_instance
(goog_sql_database_instance "resource \"google_sql_database_instance\" \"" (p "name") "\" {" n
 "  region = \"" (p "region") "\"" > n
 "  settings {" > n
 "    tier = \"" (p "tier") "\"" > n
 "  }" > n
 "}" > n
 n
 q
 :ann "google_sql_database_instance")

;; terraform-mode/google/sql_resources/google_sql_ssl_cert
(goog_sql_ssl_cert "resource \"google_sql_ssl_cert\" \"" (p "name") "\" {" n
 "  common_name = \"" (p "name") "\"" > n
 "  instance    = \"" (p "instance") "\"" > n
 "}" > n
 n
 q
 :ann "google_sql_ssl_cert")

;; terraform-mode/google/sql_resources/google_sql_user
(goog_sql_user "resource \"google_sql_user\" \"" (p "name") "\" {" n
 "  name     = \"" (p "name") "\"" > n
 "  instance = \"" (p "instance") "\"" > n
 "  password = \"" (p "changeme") "\"" > n
 "}" > n
 n
 q
 :ann "google_sql_user")

;; terraform-mode/google/stackdriver_logging_resources/google_logging_billing_account_exclusion
(goog_logging_billing_account_exclusion "resource \"google_logging_billing_account_exclusion\" \"" (p "name") "\" {" n
 "  name            = \"" (p "name") "\"" > n
 "  billing_account = \"" (p "billing_account") "\"" > n
 "  filter          = \"" (p "filter") "\"" > n
 "}" > n
 n
 q
 :ann "google_logging_billing_account_exclusion")

;; terraform-mode/google/stackdriver_logging_resources/google_logging_billing_account_sink
(goog_logging_billing_account_sink "resource \"google_logging_billing_account_sink\" \"" (p "name") "\" {" n
 "  name            = \"" (p "name") "\"" > n
 "  billing_account = \"" (p "billing_account") "\"" > n
 "  destination     = \"" (p "destination") "\"" > n
 "}" > n
 n
 q
 :ann "google_logging_billing_account_sink")

;; terraform-mode/google/stackdriver_logging_resources/google_logging_folder_exclusion
(goog_logging_folder_exclusion "resource \"google_logging_folder_exclusion\" \"" (p "name") "\" {" n
 "  name   = \"" (p "name") "\"" > n
 "  folder = \"" (p "folder") "\"" > n
 "  filter = \"" (p "filter") "\"" > n
 "}" > n
 n
 q
 :ann "google_logging_folder_exclusion")

;; terraform-mode/google/stackdriver_logging_resources/google_logging_folder_sink
(goog_logging_folder_sink "resource \"google_logging_folder_sink\" \"" (p "name") "\" {" n
 "  name        = \"" (p "name") "\"" > n
 "  folder      = \"" (p "folder") "\"" > n
 "  destination = \"" (p "destination") "\"" > n
 "}" > n
 n
 q
 :ann "google_logging_folder_sink")

;; terraform-mode/google/stackdriver_logging_resources/google_logging_organization_exclusion
(goog_logging_organization_exclusion "resource \"google_logging_organization_exclusion\" \"" (p "name") "\" {" n
 "  name   = \"" (p "name") "\"" > n
 "  org_id = \"" (p "org_id") "\"" > n
 "  filter = \"" (p "filter") "\"" > n
 "}" > n
 n
 q
 :ann "google_logging_organization_exclusion")

;; terraform-mode/google/stackdriver_logging_resources/google_logging_organization_sink
(goog_logging_organization_sink "resource \"google_logging_organization_sink\" \"" (p "name") "\" {" n
 "  name        = \"" (p "name") "\"" > n
 "  org_id      = \"" (p "org_id") "\"" > n
 "  destination = \"" (p "destination") "\"" > n
 "}" > n
 n
 q
 :ann "google_logging_organization_sink")

;; terraform-mode/google/stackdriver_logging_resources/google_logging_project_exclusion
(goog_logging_project_exclusion "resource \"google_logging_project_exclusion\" \"" (p "name") "\" {" n
 "  name   = \"" (p "name") "\"" > n
 "  filter = \"" (p "filter") "\"" > n
 "}" > n
 n
 q
 :ann "google_logging_project_exclusion")

;; terraform-mode/google/stackdriver_logging_resources/google_logging_project_sink
(goog_logging_project_sink "resource \"google_logging_project_sink\" \"" (p "name") "\" {" n
 "  name        = \"" (p "name") "\"" > n
 "  destination = \"" (p "destination") "\"" > n
 "}" > n
 n
 q
 :ann "google_logging_project_sink")

;; terraform-mode/google/stackdriver_monitoring_resources/google_monitoring_alert_policy
(goog_monitoring_alert_policy "resource \"google_monitoring_alert_policy\" \"" (p "name") "\" {" n
 "  display_name = \"" (p "display_name") "\"" > n
 "  combiner     = \"" (p "combiner") "\"" > n
 "  conditions   = [" > n
 "    {" > n
 "      display_name = \"" (p "display_name") "\"" > n
 "    }" > n
 "  ]" > n
 "}" > n
 n
 q
 :ann "google_monitoring_alert_policy")

;; terraform-mode/google/stackdriver_monitoring_resources/google_monitoring_group
(goog_monitoring_group "resource \"google_monitoring_group\" \"" (p "name") "\" {" n
 "  display_name = \"" (p "display_name") "\"" > n
 "  filter       = \"" (p "filter") "\"" > n
 "}" > n
 n
 q
 :ann "google_monitoring_group")

;; terraform-mode/google/stackdriver_monitoring_resources/google_monitoring_notification_channel
(goog_monitoring_notification_channel "resource \"google_monitoring_notification_channel\" \"" (p "name") "\" {" n
 "  display_name = \"" (p "display_name") "\"" > n
 "  type         = \"" (p "type") "\"" > n
 "}" > n
 n
 q
 :ann "google_monitoring_notification_channel")

;; terraform-mode/google/stackdriver_monitoring_resources/google_monitoring_uptime_check_config
(goog_monitoring_uptime_check_config "resource \"google_monitoring_uptime_check_config\" \"" (p "name") "\" {" n
 "  display_name = \"" (p "display_name") "\"" > n
 "  timeout      = \"" (p "timeout") "\"" > n
 "}" > n
 n
 q
 :ann "google_monitoring_uptime_check_config")

;; terraform-mode/google/storage_resources/google_storage_bucket
(goog_storage_bucket "resource \"google_storage_bucket\" \"" (p "name") "\" {" n
 "  name = \"" (p "name") "\"" > n
 "}" > n
 n
 q
 :ann "google_storage_bucket")

;; terraform-mode/google/storage_resources/google_storage_bucket_acl
(goog_storage_bucket_acl "resource \"google_storage_bucket_acl\" \"" (p "name") "\" {" n
 "  bucket = \"" (p "bucket") "\"" > n
 "}" > n
 n
 q
 :ann "google_storage_bucket_acl")

;; terraform-mode/google/storage_resources/google_storage_bucket_iam_binding
(goog_storage_bucket_iam_binding "resource \"google_storage_bucket_iam_binding\" \"" (p "name") "\" {" n
 "  bucket  = \"" (p "bucket") "\"" > n
 "  role    = \"" (p "role") "\"" > n
 "  members = [" > n
 "    \"user:" (p "user") "\"," > n
 "  ]" > n
 "}" > n
 n
 q
 :ann "google_storage_bucket_iam_binding")

;; terraform-mode/google/storage_resources/google_storage_bucket_iam_member
(goog_storage_bucket_iam_member "resource \"google_storage_bucket_iam_member\" \"" (p "name") "\" {" n
 "  bucket = \"" (p "bucket name") "\"" > n
 "  member = \"" (p "member") "\"" > n
 "  role   = \"" (p "role") "\"" > n
 "}" > n
 n
 q
 :ann "google_storage_bucket_iam_member")

;; terraform-mode/google/storage_resources/google_storage_bucket_iam_policy
(goog_storage_bucket_iam_policy "resource \"google_storage_bucket_iam_policy\" \"" (p "name") "\" {" n
 "  bucket      = \"" (p "bucket") "\"" > n
 "  policy_data = \"" (p "policy_data") "\"" > n
 "}" > n
 n
 q
 :ann "google_storage_bucket_iam_policy")

;; terraform-mode/google/storage_resources/google_storage_bucket_object
(goog_storage_bucket_object "resource \"google_storage_bucket_object\" \"" (p "name") "\" {" n
 "  name   = \"" (p "name") "\"" > n
 "  bucket = \"" (p "bucket") "\"" > n
 "}" > n
 n
 q
 :ann "google_storage_bucket_object")

;; terraform-mode/google/storage_resources/google_storage_default_object_access_control
(goog_storage_default_object_access_control "resource \"google_storage_default_object_access_control\" \"" (p "name") "\" {" n
 "  bucket = \"" (p "bucket") "\"" > n
 "  role   = \"" (p "role") "\"" > n
 "  entity = \"" (p "entity") "\"" > n
 "}" > n
 n
 q
 :ann "google_storage_default_object_access_control")

;; terraform-mode/google/storage_resources/google_storage_default_object_acl
(goog_storage_default_object_acl "resource \"google_storage_default_object_acl\" \"" (p "name") "\" {" n
 "  bucket      = \"" (p "bucket") "\"" > n
 "  role_entity = [" > n
 "    \"OWNER:" (p "owner") "\"," > n
 "  ]" > n
 "}" > n
 n
 q
 :ann "google_storage_default_object_acl")

;; terraform-mode/google/storage_resources/google_storage_notification
(goog_storage_notification "resource \"google_storage_notification\" \"" (p "name") "\" {" n
 "  bucket         = \"" (p "bucket") "\"" > n
 "  payload_format = \"" (p "payload") "\"" > n
 "  topic          = \"" (p "topic") "\"" > n
 "}" > n
 n
 q
 :ann "google_storage_notification")

;; terraform-mode/google/storage_resources/google_storage_object_access_control
(goog_storage_object_access_control "resource \"google_storage_object_access_control\" \"" (p "name") "\" {" n
 "  object = \"" (p "object") "\"" > n
 "  bucket = \"" (p "bucket") "\"" > n
 "  role   = \"" (p "role") "\"" > n
 "  entity = \"" (p "entity") "\"" > n
 "}" > n
 n
 q
 :ann "google_storage_object_access_control")

;; terraform-mode/google/storage_resources/google_storage_object_acl
(goog_storage_object_acl "resource \"google_storage_object_acl\" \"" (p "name") "\" {" n
 "  bucket = \"" (p "bucket") "\"" > n
 "  object = \"" (p "object") "\"" > n
 "}" > n
 n
 q
 :ann "google_storage_object_acl")

;; terraform-mode/import
(import "import {" n
 "  to = \"" (p "to") "\"" > n
 "  id = \"" (p "id") "\"" > n
 "}" > n
 q)

;; terraform-mode/locals
(locals "locals {" n
 "  {" p ":name} = " (p "value") > n
 "}" q >)

;; terraform-mode/module
(mod "module \"" (p "name") "\" {" n
 "  source = \"" (p "location") "\"" > n
 "  " > q n
 "}" > n
 
 :ann "module")

;; terraform-mode/output
(output "output \"" (p "name") "\" {" n
 "  value = " (p "value") > n
 "}" > n
 q)

;; terraform-mode/provider
(prov "provider \"" (p "name") "\" {" n
 "  " > q n
 "}" > n
 
 :ann "provider")

;; terraform-mode/resource
(res "resource \"" (p "type") "\" \"" (p "name") "\" {" n
 "         " > q n
 "}" > n
 
 :ann "resource")

;; terraform-mode/terraform
(tf "terraform {" n
 "  backend \"" (p "backend") "\" {" > n
 "    " > q n
 "  }" > n
 "}" > n
 
 :ann "terraform")

;; terraform-mode/variable
;; WARNING: Nested fields flattened into field 2
(var "variable \"" (p "name") "\" {" n
 "  " > (p "default = \"\"") n
 "}" q >
 :ann "variable")


tuareg-mode typerex-mode

;; tuareg-mode/assert
(as "assert " q ";;"
 :ann "assert")

;; tuareg-mode/docstring
(d "(* " q " *)"
 :ann "docstring")

;; tuareg-mode/for
(for "for " (p "cond") " do" n
 "    " > q n
 "done" >)

;; tuareg-mode/fun
(fun "fun " (p "args") " -> " q)

;; tuareg-mode/guard
(| "| " (p "match") " -> " q
 :ann "guard")

;; tuareg-mode/ifthen
(if "if " (p "cond") " then" n
 "   " > q n
 
 :ann "ifthen")

;; tuareg-mode/ifthenelse
;; WARNING: Duplicate key ‘if’ renamed to ‘if-2’
(if-2 "if " (p "cond") " then" n
 "   " > p n
 "else" > n
 "        " > q
 :ann "ifthenelse")

;; tuareg-mode/let
(let "let " (p "var") " " q)

;; tuareg-mode/list_comprehension
(l "[? " p " | " q " ?]"
 :ann "list_comprehension")

;; tuareg-mode/main
(m "let main =" n
 "    " > q
 :ann "main")

;; tuareg-mode/match
(match "match " (p "to_match") " with" n
 "      | " (p "matching") " -> " q >)

;; tuareg-mode/module
(mod "module " (p "A") " =" n
 "       struct" > n
 "       " > (p "type t = { name : string; phone : string ") "}" n
 "       " > q n
 "end;;" > n
 
 :ann "module")

;; tuareg-mode/open
(op "open " (p "Module")
 :ann "open")

;; tuareg-mode/printf
(pr "Printf.printf \"" (p "string") "\" " (p "vals") ";;" q
 :ann "printf")

;; tuareg-mode/rec
(rec "let rec " (p "fun") " " (p "args") " =" n
 "    " > q)

;; tuareg-mode/try
;; WARNING: $0 is not the last field; kept as a plain field
(try "try" n
 "        " > p n
 "with" > n
 "        " > p)

;; tuareg-mode/type
;; WARNING: $0 directly follows a field; Tempel exits at the template end
(type "type " (p "name") " = {" (p "var") ": " (p "int") "}"
 :ann "type_record")

;; tuareg-mode/type_type
;; WARNING: Duplicate key ‘type’ renamed to ‘type-2’
(type-2 "type " (p "expr") " =" n
 "     | " q >
 :ann "type_type")

;; tuareg-mode/val
(val "val " (p "fun") " : " q)

;; tuareg-mode/while
(wh "while " (p "cond") " do" n
 "      " > q n
 "done" >
 :ann "while")


udev-mode

;; udev-mode/ENV
(env "ENV{" p "}" q
 :ann "ENV")

;; udev-mode/GOTO
(goto "GOTO=\"" (s f1) "\"" n
 q n
 n
 "LABEL=\"" f1 "\"" >
 :ann "GOTO")

;; udev-mode/KERNEL
(ker "KERNEL!=\"" q "\""
 :ann "KERNEL")

;; udev-mode/add
(add "ACTION==\"add\", " q)

;; udev-mode/env$
($ "$env{" p "} " q
 :ann "env$")

;; udev-mode/run
(run "RUN+=\"" q "\"")


vhdl-mode

;; vhdl-mode/architecture
(arch "architecture " (p "Type" f1) " of " (p "Name") " is" n
 "begin" > n
 "  " > q n
 "end " f1 ";" > n
 
 :ann "architecture")

;; vhdl-mode/assignation
(asg (p "variable") " <= " (p "value") ";" n
 q
 :ann "assignation")

;; vhdl-mode/case
(case "case " (p "cond") " is" n
 "    when " (p "Value") " =>" > n
 "        " > q n
 n
 "end case;" >)

;; vhdl-mode/component
(comp "component " (p "Name") n
 "  " > q n
 "end component;" > n
 
 :ann "component")

;; vhdl-mode/constant
(const "constant " (p "Name") ": " (p "Type") " := " (p "Value") ";" q
 :ann "constant")

;; vhdl-mode/downto
(dto (p "name") "(" (p "start") " downto " (p "end") ")" q
 :ann "downto")

;; vhdl-mode/entity
(ent "entity " (p "Name" f1) " is" n
 "    " > q n
 "end " f1 ";" >
 :ann "entity")

;; vhdl-mode/if
(if "if " (p "cond") " then" n
 "   " > q n
 "end if;" >)

;; vhdl-mode/ifelif
;; WARNING: $0 is not the last field; kept as a plain field
(ifelif "if " (p "cond1") " then" n
 "   " > p n
 "elsif " (p "cond2") " then" > n
 "      " > n
 "end if;" >)

;; vhdl-mode/ifelse
(ifel "if " (p "cond1") " then" n
 "   " > q n
 "else" > n
 "   " > n
 "end if;" >
 :ann "ifelse")

;; vhdl-mode/lib
(lib "library IEEE;" n
 "use IEEE.std_logic_1164.all;" > n
 
 :ann "library")

;; vhdl-mode/port
(port "port(" (p "name") ": " (p "IO") " " (p "type") ");" q)

;; vhdl-mode/process
(process (p "Name" f1) ": process(" (p "Sensitivity List") ")" n
 "begin" > n
 "  " > q n
 "end process " f1 ";" > n
 )

;; vhdl-mode/signal
(signal "signal " (p "Names") ": " (p "Type") ";" q)

;; vhdl-mode/to
(to (p "name") "(" (p "start") " to " (p "end") ")" q)

;; vhdl-mode/type
(type "type " (p "Name") " is (" (p "Value list") ");" n
 q)

;; vhdl-mode/when
(when "when " (p "Value") " =>" n
 "    " > q n
 )


yaml-mode

;; yaml-mode/entry
(entry (p "entry") ": " (p "value") n
 q)

;; yaml-mode/list
(list "[" p "]" n
 q)

;; yaml-mode/section
(-- "--- # " (p "section") n
 q
 :ann "section")


zig-mode

;; zig-mode/elif
(elif "else if (" p ") {" n
 "     " > q n
 "}" >
 :ann "else if statement")

;; zig-mode/else
(el "else {" n
 "    " > q n
 "}" >
 :ann "else sttement")

;; zig-mode/enum
;; WARNING: Nested fields flattened into field 1
(enum "enum" (p "(type)") " {" n
 "    " > q n
 "}" >
 :ann "enum declaration")

;; zig-mode/fn
(fn "fn " (p "name") "(" p ") " (p "!void") " {" n
 "   " > q n
 "}" >
 :ann "fn name() type { ... }")

;; zig-mode/for
;; WARNING: Nested fields flattened into field 2
(for "for (" p ") " (p "|value|") " {" n
 "    " > q n
 "}" >
 :ann "for statement")

;; zig-mode/if
(if "if (" p ") {" n
 "    " > q n
 "}" > n
 
 :ann "if statement")

;; zig-mode/imp
(imp "const " (p "name") " = @import(\"" (p "lib") "\");" q
 :ann "import statement")

;; zig-mode/main
;; WARNING: Nested fields flattened into field 2
(main "pub fn main(" p ") " (p "!void") " {" n
 "    " > q n
 "}" >
 :ann "main pub function")

;; zig-mode/struct
(struct "struct {" n
 "    " > q n
 "}" >
 :ann "struct declaration")

;; zig-mode/switch
(switch "switch (" p ") {" n
 "    " > p "=> " q n
 "}" >
 :ann "switch statement")

;; zig-mode/test
(test "test \"" p "\" {" n
 "    " > q n
 "}" >
 :ann "test statement")

;; zig-mode/union
(union "union " (p "(enum)") " {" n
 "    " > q n
 "}" >
 :ann "union statement")

