;;-*-coding: utf-8;-*-
(define-abbrev-table 'c++-ts-mode-abbrev-table
  '(
    ("cl" "class {
}" nil :count 1)
    ("newf" "#include <iostream>
#include <iostream>

using std::cout;
using std::cin;
using std::endl;


int main(void)
{
     return 0;
}
" nil :count 1)
    ("pr" "private" nil :count 0)
    ("pu" "public" nil :count 1)
    ("scerr" "std::cerr << " nil :count 2)
    ("scin" "std::cin >> " nil :count 6)
    ("scout" "std::cout << " nil :count 8)
    ("sendl" "<< std::endl " nil :count 9)
   ))

(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '(
    ("(use-package )" "up" nil :count 0)
    ("up" "(use-package" nil :count 0)
    ("upg" "(use-package )" nil :count 1)
   ))

(define-abbrev-table 'fundamental-mode-abbrev-table
  '(
    ("abb" "abbrev" nil :count 0)
   ))

(define-abbrev-table 'global-abbrev-table
  '(
    ("ltx" "LaTeX" nil :count 0)
   ))

(define-abbrev-table 'latex-mode-abbrev-table
  '(
    ("Bmch" "\\gtab{Bm}{021202}" nil :count 0)
    ("Dch" "\\gtab{E}{022100}" nil :count 0)
    ("Emch" "\\gtab{Em}{022000}" nil :count 1)
    ("Gch" "\\gtab{G}{320033}" nil :count 1)
   ))

(define-abbrev-table 'org-mode-abbrev-table
  '(
    ("bs" "#+begin_src " nil :count 7)
    ("bsb" "#+begin_src bash " nil :count 1)
    ("bse" "#+begin_src emacs-lisp" nil :count 3)
    ("el" "emacs-lisp" nil :count 2)
    ("eml" "emacs-lisp" nil :count 6)
    ("es" "#+end_src" nil :count 4)
    ("esb" "#+begin_src  emacs-lisp
   %CHANGEME%  
#+end_src" nil :count 1)
    ("mls" "ehll\\nthere\\nend" nil :count 1)
    ("otem" "#+TITLE: %CHANGEME%
#+AUTHOR: rileyrg
#+EMAIL: rileyrg at g m x dot de

#+LANGUAGE: en
#+STARTUP: showall

#+EXPORT_FILE_NAME: README.md
#+OPTIONS: toc:8 num:nil

#+category: %CHANGEME%
#+FILETAGS: :%CHANGEME%:

#+PROPERTY: header-args:bash :tangle-mode (identity #o755)" nil :count 3)
   ))

