#include "cond_move.h"

#include <cassert>
#include <stdint.h>

#include <iostream>

using namespace std;

//struct __attribute__((packed)) mx {
struct mx {
  uint16_t a;
  uint16_t b;
  uint16_t sel;
  uint16_t res;
  int pad;
};


int main() {

  cout << "size of mx = " << sizeof(mx) << endl;
  mx s;

  s.a = 12;
  s.b = 3;
  s.sel = 0;
  s.res = 0;

  cond_move(&s);

  cout << "s.res = " << s.res << endl;
  assert(s.res == 12);

  s.sel = 1;
  s.res = 0;

  cond_move(&s);

  assert(s.res == 3);
  
  return 0;
}
