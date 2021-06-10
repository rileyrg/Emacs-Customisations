;;-*-coding: utf-8;-*-
(define-abbrev-table 'c++-mode-abbrev-table
  '(
    ("endl" "std::endl;" nil :count 0)
    ("newf" "#include <iostream>
#include <ostream>

using std::cout;
using std::cin;
using std::endl;


int main(void)
{
     return 0;
}
" nil :count 1)
    ("sin" "std::cin >>" nil :count 4)
    ("sout" "std::cout <<" nil :count 4)
   ))

(define-abbrev-table 'org-mode-abbrev-table
  '(
    ("bs" "#+begin_src " nil :count 2)
    ("bsb" "#+begin_src bash " nil :count 1)
    ("bse" "#+begin_src emacs-lisp" nil :count 3)
    ("eml" "emacs-lisp" nil :count 6)
    ("es" "#+end_src" nil :count 2)
    ("mls" "ehll\\nthere\\nend" nil :count 1)
   ))

