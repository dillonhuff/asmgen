#include "two_adds.h"

#include <cassert>

int main() {
  layout s;
  s.a = 1;
  s.b = 3;

  s.c = 4;

  two_adds(&s);

  // assert(s.r1 == (s.a + s.b));
  // assert(s.r2 == (s.b + s.c));

  return 0;
}
