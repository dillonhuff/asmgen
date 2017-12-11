#include "cond_move.h"

#include <cassert>
#include <stdint.h>

#include <iostream>

using namespace std;

int main() {

  cout << "size of mx = " << sizeof(layout) << endl;
  layout s;

  s.in0 = 12;
  s.in1 = 3;
  s.sel = 0;
  s.out = 0;

  cond_move(&s);

  cout << "s.out = " << s.out << endl;
  assert(s.out == 12);

  s.sel = 1;
  s.out = 0;

  cond_move(&s);

  assert(s.out == 3);
  
  return 0;
}
