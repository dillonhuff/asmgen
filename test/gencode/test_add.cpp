#include "simd_add.h"

#include <cassert>

#include <immintrin.h>

struct tstruct {
  __m128i a;
  __m128i b;
  __m128i c;
};

int main() {
  __m128i a;
  a[0] = 23;
  a[1] = 0;
  
  __m128i b;
  b[0] = 4;
  b[1] = 0;

  tstruct s;
  s.a = a;
  s.b = b;

  simd_add(&s);

  __m128i correct;
  correct[0] = 23 + 4;
  correct[1] = 0;

  assert(s.c[0] == correct[0]);
  assert(s.c[1] == correct[1]);
}
