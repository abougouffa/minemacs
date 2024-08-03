#!/usr/bin/python
"""
Generate MISRA C guidelines (Dirs and Rules) from pdf for cppcheck.

Arguments:
    filename - MISRA pdf file
Example:
    python cppcheck_misra_parse_pdf.py "/path/to/MISRA_C_2023.pdf"

----------------------------------------------------------------------------------------------------

Code formatting
===============

- Style guide check: `flake8 --max-line-length 99 cppcheck_misra_parse_pdf.py`
- Format file: `black cppcheck_misra_parse_pdf.py`

Changelog
=========

v1.0.0 - 2023/05/21
-------------------

Other
=====

- Modified from
  https://github.com/ChisholmKyle/SublimeLinter-cppcheck-misra/blob/master/scripts/cppcheck-misra-parsetexts.py

- How cppcheck .txt file must be formatted
  https://github.com/danmar/cppcheck/blob/main/addons/test/misra/misra_rules_structure.txt
----------------------------------------------------------------------------------------------------

Modified from: https://gist.github.com/bzgec/9ff8338f344d1a6a8dea4c50e2dbf54e
"""

import os
import re
import sys
import json
import datetime
import tempfile
import subprocess

SCRIPT_NAME = "cppcheck_misra_parse_pdf.py"
SCRIPT_VERSION = "1.0.0"
DATE_FORMAT = "%Y/%m/%d"

APPENDIX_A_LINE = "Appendix A Summary of guidelines\n"
APPENDIX_B_LINE = "Appendix B Guideline attributes\n"
_rule_regex = re.compile(
    r"(Rule|Dir).(\d+)\.(\d+).(Advisory|Required|Mandatory).([^\n]+)\n"
)


def misra_list_to_text(misra_list: list[dict]) -> str:
    """
    Convert dict to string readable by cppcheck's misra.py addon.

    list elements:
      {
          "guideline_type": "Dir",
          "numb": "1.1",
          "category": "Required",
          "text": "Guideline text for Dir 1.1 ...",
      },
      {
          "guideline_type": "Rule",
          "numb": "23.8",
          "category": "Required",
          "text": "Guideline text for Rule 23.8 ...",
      },
    """
    misra_str = ""
    for elem in misra_list:
        misra_str += f'\n{elem["guideline_type"]} {elem["numb"]} {elem["category"]}\n{elem["text"]}\n'  # noqa: E501

    return misra_str


def parse_misra_xpdf_output(misra_file: str) -> list[dict]:
    """
    Extract misra rules texts from xPDF output.

    list elements:
      {
          "guideline_type": "Dir",
          "numb": "1.1",
          "category": "Required",
          "text": "Guideline text for Dir 1.1 ...",
      },
      {
          "guideline_type": "Rule",
          "numb": "23.8",
          "category": "Required",
          "text": "Guideline text for Rule 23.8 ...",
      },
    """
    misra_list = []

    appendix_a_start_line = 0
    appendix_b_start_line = 0
    with open(misra_file, encoding="utf-8") as my_file:
        guideline_on_prev_line = False

        ruletype = ""
        rulenum1 = 0
        rulenum2 = 0
        category = ""
        ruletext = ""
        for num, line in enumerate(my_file, 1):
            if APPENDIX_A_LINE in line:
                appendix_a_start_line = num
            if APPENDIX_B_LINE in line:
                appendix_b_start_line = num
                break

            if appendix_a_start_line != 0:
                res = _rule_regex.search(line, 0)
                if res:
                    if guideline_on_prev_line is True:
                        misra_list.append(
                            {
                                "guideline_type": ruletype,
                                "numb": f"{rulenum1}.{rulenum2}",
                                "category": category,
                                "text": ruletext,
                            }
                        )
                    guideline_on_prev_line = True
                    ruletype = res.group(1)
                    rulenum1 = res.group(2)
                    rulenum2 = res.group(3)
                    category = res.group(4)
                    ruletext = res.group(5).strip()
                else:
                    if guideline_on_prev_line is True:
                        # Exception: some lines which start with "Standard" or
                        # "Library" but are still guidelines
                        if (
                            line[0].isnumeric() or line[0].isupper()
                        ) and "Library" not in line:
                            guideline_on_prev_line = False
                            misra_list.append(
                                {
                                    "guideline_type": ruletype,
                                    "numb": f"{rulenum1}.{rulenum2}",
                                    "category": category,
                                    "text": ruletext,
                                }
                            )
                        else:
                            ruletext += " " + line.strip()

    if appendix_a_start_line == 0:
        print("Can't find Appendix A line")
        sys.exit(1)
    if appendix_b_start_line == 0:
        print("Can't find Appendix B line")
        sys.exit(2)

    return misra_list


def misra_parse_pdf(misra_pdf):
    """Extract misra rules texts from Misra-C-2012 pdf."""
    file_name = ""
    with tempfile.NamedTemporaryFile(delete=False) as fp:
        file_name = fp.name

    print("Calling `pdftotext`...")
    subprocess.call(
        ["pdftotext", "-enc", "UTF-8", "-eol", "unix", "-raw", misra_pdf, file_name]
    )
    misra_text_from_pdf = parse_misra_xpdf_output(file_name)
    os.remove(file_name)

    return misra_text_from_pdf


if __name__ == "__main__":
    MISRA_PDF_FILENAME = sys.argv[1]

    misra_list = misra_parse_pdf(MISRA_PDF_FILENAME)

    misra_json_fout = f"{os.path.splitext(MISRA_PDF_FILENAME)[0]}_guidelines.json"
    misra_text_fout = f"{os.path.splitext(MISRA_PDF_FILENAME)[0]}_guidelines.txt"

    with open(misra_json_fout, "w", encoding="utf-8") as fp:
        fp.write(json.dumps(misra_list, indent=4))
        print('Done creating "' + misra_json_fout + '"')

    with open(misra_text_fout, "w", encoding="utf-8") as fp:
        misra_text = (
            f"Generated from: {MISRA_PDF_FILENAME}\n"
            + f"Generated on: {datetime.date.today().strftime(DATE_FORMAT)}\n"
            + f"Generated with: {SCRIPT_NAME} v{SCRIPT_VERSION}\n\n"
            + "Appendix A Summary of guidelines\n\n"
            + misra_list_to_text(misra_list)
        )
        fp.write(misra_text)
        print('Done creating "' + misra_text_fout + '"')
