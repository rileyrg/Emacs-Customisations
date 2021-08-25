;;-*-coding: utf-8;-*-
(define-abbrev-table 'c++-mode-abbrev-table
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

(define-abbrev-table 'global-abbrev-table
  '(
    (">>i" "cin >> " nil :count 0)
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

