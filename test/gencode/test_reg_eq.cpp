#include "reg_eq.h"

#include <cassert>
#include <iostream>
#include <bitset>

using namespace std;

int main() {
  layout s;
  s.self_in_sel = 10;
  s.self_in_1 = 5;
  s.self_in_2 = 10;
  s.self_out_0 = 1;
  s.self_clk = 1;
  s.self_clk_last = 0;

  reg_eq(&s);

  cout << "s.self_out_0 = " << (int) s.self_out_0 << endl;

  assert(s.self_out_0 == 10);

  s.self_in_sel = 6;

  reg_eq(&s);

  cout << "s.self_out_0 = " << (int) s.self_out_0 << endl;

  assert(s.self_out_0 == 5);
}
